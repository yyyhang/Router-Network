-module(router).

-export([init/1, start/1]).

%% Create local table, pull up process
start(RouterName) -> spawn(router, init, [RouterName]).

init(RouterName) ->
    LocalTable = ets:new(undef, [private]),
    TwoPCTable = ets:new(undef, [private]),
    TentativeTable = ets:new(undef, [private]),
    io:format("Created proc ~p~n", [RouterName]),
    ets:insert(LocalTable, {'$NoInEdges', 0}),
    handleRcv(RouterName, LocalTable, TwoPCTable,
	      TentativeTable).

handleRcv(RouterName, LocalTable, TwoPCTable,
	  TentativeTable) ->
    % io:format("CurrNode: ~p~n", [RouterName]),
    receive
      % forward msg
      {message, Dest, From, Pid, Trace} ->
	  handleMsg(RouterName, Dest, Pid, Trace, LocalTable),
	  handleRcv(RouterName, LocalTable, TwoPCTable,
		    TentativeTable);
      % handle control. Using seqNum to decide the control type
      {control, From, Pid, SeqNum, ControlFun} ->
	  io:format("SeqNum: ~p~n", [SeqNum]),
	  handleControl(SeqNum, From, Pid, ControlFun, RouterName,
			LocalTable, TwoPCTable, TentativeTable),
	  handleRcv(RouterName, LocalTable, TwoPCTable,
		    TentativeTable);
      %handle dump
      {dump, From} ->
	  handleDump(LocalTable, From),
	  handleRcv(RouterName, LocalTable, TwoPCTable,
		    TentativeTable);
      % handle2PCRes
      {twoPC, From, Msg, Phase, Edges} ->
	  Check = ets:member(TwoPCTable, seqnum),
	  if Msg =:= canCommit, Check =:= false ->
		 From ! {twoPC, self(), committed, 2};
	     true ->
		 handle2PCRes(From, Msg, Phase, TwoPCTable, LocalTable,
			      TentativeTable, Edges)
	  end,
	  handleRcv(RouterName, LocalTable, TwoPCTable,
		    TentativeTable);
      % other situations
      _ ->
	  handleRcv(RouterName, LocalTable, TwoPCTable,
		    TentativeTable)
    end.

handleMsg(RouterName, Dest, Pid, Trace, LocalTable) ->
    CurrTrace = [RouterName | Trace],
    io:format("CurrNode: ~p~n", [RouterName]),
    if RouterName =:= Dest ->
	   % io:format("CurrTrace: ~p~n", [CurrTrace]),
	   Pid ! {trace, self(), lists:reverse(CurrTrace)};
       true ->
	   %    Qump = ets:match(LocalTable, '$1'),
	   %    io:format("Taking a qump ~p~n", [Qump]),
	   [{_, NextPid}] = ets:lookup(LocalTable, Dest),
	   io:format("Start to routing: ~p~n", [NextPid]),
	   NextPid ! {message, Dest, self(), Pid, CurrTrace}
    end.

%  to init the local table
handleControl(0, From, Pid, ControlFun, RouterName,
	      LocalTable, TwoPCTable, TentativeTable) ->
    ControlFun(RouterName, LocalTable),
    io:format("From ~p to init local table~n", [From]),
    From ! {committed, self(), 0};
%  progagates control requests
handleControl(SeqNum, From, Pid, ControlFun, RouterName,
	      LocalTable, TwoPCTable, TentativeTable) ->
    % [{_, Flag}] = ets:lookup(TwoPCTable, flag),
    Vist = ets:member(TwoPCTable, seqnum),
    io:format("~p - Vist:~p from ~p ~n",
	      [RouterName, Vist, From]),
    % if Visited
    if Vist =:= true ->
	   From ! {twoPC, self(), committed, 1, []};
       % not visited
       true ->
	   ets:delete_all_objects(TentativeTable),
	   ControlFun(RouterName, TentativeTable),
	   TempTab = ets:match(TentativeTable, '$1'),
	   io:format("Taking a TentativeTable ~p~n", [TempTab]),
	   UniEdges = lists:usort([NodePid
				   || [{Name, NodePid}] <- TempTab,
				      Name /= '$NoInEdges']),
	   %    Outgoing = length(lists:usort(ets:match(LocalTable,
	   % 					   '$1'))),
	   Table = ets:match(LocalTable, '$1'),
	   Neibors = [NodePid
		      || [{Name, NodePid}] <- Table, Name /= '$NoInEdges'],
	   UniNeibors = lists:usort(Neibors),
	   Outgoing = length(UniNeibors),
	   case Outgoing of
	     0 ->
		 %   if not visited and worker has no outgoing edges
		 if From /= Pid ->
			start2pc(From, Pid, SeqNum, Outgoing, TwoPCTable,
				 UniEdges),
			From ! {twoPC, self(), committed, 1, UniEdges},
			ets:insert(TwoPCTable, {flag, false}),
			ets:insert(TwoPCTable, {count, 0}),
			ets:delete(TwoPCTable, from);
		    %  root node has no outgoing
		    true ->
			doCommit(TentativeTable, LocalTable),
			From ! {committed, self(), SeqNum}
		 end;
	     % if not visited, bcast to all outgoing edges
	     _ ->
		 start2pc(From, Pid, SeqNum, Outgoing, TwoPCTable,
			  UniEdges),
		 Wump = ets:match(TwoPCTable, '$1'),
		 io:format("Taking a TwoPCTable ~p~n", [Wump]),
		 progagates(control, Pid, ControlFun, UniNeibors,
			    SeqNum),
		 io:format("~p COMMITED ~n", [RouterName])
	   end
    end.

start2pc(From, Pid, SeqNum, Outgoing, TwoPCTable,
	 UniEdges) ->
    io:format("~p:From ~p to Pid~p ~n",
	      [self(), From, Pid]),
    if Pid =:= From ->
	   ets:insert(TwoPCTable, {control, Pid}),
	   ets:insert(TwoPCTable, {from, self()});
       true -> ets:insert(TwoPCTable, {from, From})
    end,
    ets:insert(TwoPCTable, {flag, true}),
    ets:insert(TwoPCTable, {seqnum, SeqNum}),
    ets:insert(TwoPCTable, {count, 0}),
    ets:insert(TwoPCTable, {edges, UniEdges}),
    ets:insert(TwoPCTable, {outgoing, Outgoing}).

phaseTwoTable(From, TwoPCTable) ->
    io:format("Phase two node ~p:From ~p ~n",
	      [self(), From]),
    ets:insert(TwoPCTable, {from, From}),
    ets:insert(TwoPCTable, {flag, true}).

handleDump(LocalTable, From) ->
    Table = ets:match(LocalTable, '$1'),
    From ! {table, self(), Table}.

%  broadcast msg
progagates(msg, LocalTable, Msg, Phase, AllEdges) ->
    Table = ets:match(LocalTable, '$1'),
    Neibors = [NodePid
	       || [{Name, NodePid}] <- Table, Name /= '$NoInEdges'],
    io:format("Taking a Neibors ~p~n", [Neibors]),
    UniNeibors = lists:usort(Neibors),
    io:format("Taking a progagates ~p~n", [UniNeibors]),
    [Nbor ! {twoPC, self(), Msg, Phase, AllEdges}
     || Nbor <- UniNeibors];
%  broadcast controlFun
progagates(control, Pid, ControlFun, UniNeibors,
	   SeqNum) ->
    % io:format("Table 2 ~p~n", [Table]),
    io:format("Taking a progagates 2 ~p~n", [UniNeibors]),
    [Nbor ! {control, self(), Pid, SeqNum, ControlFun}
     || Nbor <- UniNeibors].

handle2PCRes(From, Msg, Phase, TwoPCTable, LocalTable,
	     TentativeTable, Edges) ->
    io:format("~p recv 2pc msg: ~p~n", [self(), Msg]),
    [{_, Flag}] = ets:lookup(TwoPCTable, flag),
    [{_, Cnt}] = ets:lookup(TwoPCTable, count),
    [{_, Outgoing}] = ets:lookup(TwoPCTable, outgoing),
    [{_, PrevPid}] = ets:lookup(TwoPCTable, from),
    [{_, SeqNum}] = ets:lookup(TwoPCTable, seqnum),
    [{_, CurrEdges}] = ets:lookup(TwoPCTable, edges),
    if Msg =:= committed, Cnt + 1 /= Outgoing ->
	   io:format("committed not enough ~p ~n", [Cnt + 1]),
	   ets:insert(TwoPCTable, {count, Cnt + 1}),
	   ets:insert(TwoPCTable, {edges, CurrEdges ++ Edges});
       % coordinator collected all committed msg
       Msg =:= committed, Cnt + 1 =:= Outgoing,
       PrevPid =:= self() ->
	   io:format("coor enough ~n"),
	   ets:insert(TwoPCTable, {edges, CurrEdges ++ Edges}),
	   [{_, Pid}] = ets:lookup(TwoPCTable, control),
	   [{_, AllEdges}] = ets:lookup(TwoPCTable, edges),
	   %  ~~~~~~~~~~ WE NEED TO HANDEL THE LISTS HERE
	   InEdges = [Edge || Edge <- AllEdges, self() =:= Edge],
	   io:format("InEdges ~p ~n", [InEdges]),
	   io:format("NoInEdges ~p ~n", [length(InEdges)]),
	   ets:insert(TentativeTable,
		      {'$NoInEdges', length(InEdges)}),
	   Wump = ets:match(TentativeTable, '$1'),
	   io:format("################ ~p~n", [Wump]),
	   coordinator(committed, Phase, Pid, SeqNum, LocalTable,
		       TentativeTable, AllEdges),
	   ets:insert(TwoPCTable, {count, 0});
       % worker collected all committed msg <-
       Msg =:= committed, Cnt + 1 =:= Outgoing ->
	   io:format("committed enough ~n"),
	   ets:insert(TwoPCTable, {edges, CurrEdges ++ Edges}),
	   PrevPid !
	     {twoPC, self(), committed, Phase, CurrEdges ++ Edges},
	   %    Complete Phase 2
	   if Phase =:= 2 ->
		  io:format("~p do commit ~n", [self()]),
		  doCommit(TentativeTable, LocalTable),
		  Wump = ets:match(LocalTable, '$1'),
		  io:format("~p taking a LocalTable ~p~n",
			    [self(), Wump]),
		  clean2CPTable(TwoPCTable);
	      %   Complete Voting
	      Phase =:= 1 ->
		  ets:insert(TwoPCTable, {flag, false}),
		  ets:insert(TwoPCTable, {count, 0}),
		  ets:insert(TwoPCTable, {from, false})
	   end;
       % coordinator recv abort msg
       Msg =:= abort, PrevPid =:= self() ->
	   [{_, Pid}] = ets:lookup(TwoPCTable, control),
	   coordinator(abort, Phase, Pid, SeqNum, LocalTable),
	   ets:insert(TwoPCTable, {count, 0}),
	   ets:insert(TwoPCTable, {flag, false});
       % doCommit ->
       Msg =:= canCommit ->
	   %    check if visited this node
	   if Flag =:= true ->
		  From ! {twoPC, self(), committed, 2, []};
	      true ->
		  phaseTwoTable(From, TwoPCTable),
		  %    if node has no edges
		  if Outgoing =:= 0 ->
			 io:format("end node do commit ~n"),
			 InEdges = [Edge || Edge <- Edges, self() =:= Edge],
			 ets:insert(TentativeTable,
				    {'$NoInEdges', length(InEdges)}),
			 doCommit(TentativeTable, LocalTable),
			 From ! {twoPC, self(), committed, 2, []},
			 clean2CPTable(TwoPCTable);
		     true -> progagates(msg, LocalTable, canCommit, 2, Edges)
		  end
	   end;
       % worker recv abort msg
       Msg =:= abort ->
	   PrevPid ! {twoPC, self(), abort},
	   ets:insert(TwoPCTable, {count, 0}),
	   ets:insert(TwoPCTable, {flag, false})
    end.

coordinator(abort, _, Pid, SeqNum, LocalTable) ->
    progagates(msg, LocalTable, abort, 0, []),
    Pid ! {abort, self(), SeqNum}.

coordinator(committed, Phase, Pid, SeqNum, LocalTable,
	    TentativeTable, AllEdges) ->
    if Phase =:= 2 ->
	   io:format("**** COMPLETED 2PC ****~n"),
	   doCommit(TentativeTable, LocalTable),
	   Pid ! {committed, self(), SeqNum};
       Phase =:= 1 ->
	   io:format("**** Start Phase 2 ****~n"),
	   progagates(msg, LocalTable, canCommit, 2, AllEdges)
    end.

doCommit(TentativeTable, LocalTable) ->
    List = ets:match(TentativeTable, '$1'),
    ets:delete_all_objects(LocalTable),
    ets:insert(LocalTable,
	       [Entry || Each <- List, Entry <- Each]),
    Wump = ets:match(LocalTable, '$1'),
    io:format("doCommit a LocalTable ~p~n", [Wump]),
    ets:delete(TentativeTable).

clean2CPTable(TwoPCTable) ->
    ets:insert(TwoPCTable, {flag, false}),
    ets:delete(TwoPCTable, outgoing),
    ets:delete(TwoPCTable, count),
    ets:delete(TwoPCTable, sqenum),
    ets:delete(TwoPCTable, from).

% update2PCTable(TwoPCTable, Visited, Count, From) ->
%     ets:insert(TwoPCTable, {flag, Visited}),
%     ets:insert(TwoPCTable, {from, From}),
%     ets:insert(TwoPCTable, {count, 0}).

