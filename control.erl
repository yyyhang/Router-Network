-module(control).

-export([graphToNetwork/1]).

graphToNetwork(Graph) ->
    % get all nodes name
    RouterNames = [RouterName || {RouterName, _} <- Graph],
    PidTable = ets:new(undef, [private]),
    InEdgesTable = ets:new(undef, [duplicate_bag, private]),
    countInEdges(Graph, InEdgesTable),
    % set up process for each node, and then return the root pid
    [RootPid | R] = [setUp(RouterName, PidTable,
			   InEdgesTable)
		     || RouterName <- RouterNames],
    io:format("PidTable: ~w ~n",
	      [ets:match(PidTable, {'$1', '$2'})]),
    % io:format("InEdgesTable: ~w ~n",
    %       [ets:match(InEdgesTable, {'$1', '$2'})]),
    %  After setting up all nodes, construct their routing table
    [init(RouterName, InEdgesTable, Graph, PidTable)
     || RouterName <- RouterNames],
    RootPid.

setUp(RouterName, PidTable, InEdgesTable) ->
    %  spawn this node and get the pid. Store the pid in a table
    Pid = router:start(RouterName),
    % io:format("node ~p's pid is: ~p~n", [RouterName, Pid]),
    ets:insert(PidTable, {RouterName, Pid}),
    Pid.

init(RouterName, InEdgesTable, Graph, PidTable) ->
    % Count the number of incoming edges of this node
    Dests = ets:match(InEdgesTable, {RouterName, '$1'}),
    Cnt = length(Dests),
    % send control to this node
    [{_, Pid}] = ets:lookup(PidTable, RouterName),
    Routing = findRouting(RouterName, Graph, PidTable),
    % Dump = ets:match(Routing, '$1'),
    % io:format("Taking a dump ~p~n", [Routing]),
    Pid !
      {control, self(), self(), 0,
       fun (Name, Table) ->
	       ets:insert(Table, {'$NoInEdges', Cnt}),
	       ets:insert(Table, Routing)
       end},
    receive
      {committed, From, 0} ->
	  io:format("recv init committed ~n")
    end.

% Record all dest nodesin InEdgesTable, in order to cnt them later
countInEdges(Graph, InEdgesTable) ->
    [ets:insert(InEdgesTable, {Edge, Edge})
     || {_, Edges} <- Graph, {Edge, _} <- Edges].

findRouting(RouterName, Graph, PidTable) ->
    % Routings = ets:new(undef, [private]),
    OutGoings = [DeEdges
		 || {Name, Edges} <- Graph, Name =:= RouterName,
		    DeEdges <- Edges],
    Routings = [{To, Pid}
		|| {DesName, OutGo} <- OutGoings, To <- OutGo,
		   {_, Pid} <- ets:lookup(PidTable, DesName)],
    % Dump = ets:match(Routings, '$1'),
    % io:format("Taking a dump ~p~n", [Dump]),
    Routings.
