-module(aTest).

% -export([aTest/0, probeNetwork/2, verifyNetwork/2]).
-export([aTest/0]).

simpleNetworkGraph() ->
    [{red, [{white, [white, green]}, {blue, [blue]}]},
     {white, [{red, [blue]}, {blue, [green, red]}]},
     {blue, [{green, [white, green, red]}]},
     {green, [{red, [red, blue, white]}]}].

% aTest() ->
%     io:format("*** Starting router network...~n"),
%     Graph = simpleNetworkGraph(),
%     RedPid = control:graphToNetwork(Graph),
%     RedPid ! {dump, self()},
%     receive
%       {table, RedPid, Table} ->
% 	  io:format("*** Table Dump of red:~n"),
% 	  io:format("~w~n", [Table])
%       after 1000 -> io:format("!!! Can't obtain dump~n")
%     end,
%     io:format("*** Sending message red -> green...~n"),
%     RedPid ! {message, green, self(), self(), []},
%     receive
%       {trace, GreenPid, Trace} ->
% 	  io:format("Received trace: ~w~n", [Trace])
%       after timeout() ->
% 		io:format("!!! Message to green seems lost~n")
%     end.

% aTest() ->
%     io:format("*** Starting router network...~n"),
%     Graph = simpleNetworkGraph(),
%     A = ets:new(undef, [private]),
%     ets:insert(A, {red, 12}),
%     ets:insert(A, {green, 13}),
%     Qump = ets:match(A, '$1'),
%     io:format("Taking a qump ~p~n", [Qump]),
%     [{_, Te}] = ets:lookup(A, red),
%     C = {rty, Te},
%     io:format("updated table: ~p~n", [C]).

timeout() -> 5000.

aTest() ->
    A = ets:new(undef, [private]),
    B = ets:new(undef, [private]),
    ets:insert(A, {red, 12}),
    ets:insert(A, {green, 13}),
    ets:insert(B, {green, 9}),
    ets:insert(B, {black, 9}),
    Qump = ets:match(A, '$1'),
    io:format("Taking a qump ~p~n", [Qump]),
    ets:delete_all_objects(B),
    ets:insert(B, [Ea || Each <- Qump, Ea <- Each]),
    Dump = ets:match(B, '$1'),
    io:format("Taking a Dump ~p~n", [Dump]).
