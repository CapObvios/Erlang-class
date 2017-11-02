-module(ring).
-export([init/0, init2/2, loop/3]).

-define(PROCESSES, 5).
-define(MESSAGES, 3).

init() -> register(first, spawn(?MODULE, init2, [?MESSAGES, ?PROCESSES])).

init2(M,1) -> loop(M, 1, whereis(first));
init2(M,N) -> Other = spawn(?MODULE, init2, [M, N-1]), loop(M,N, Other).

loop(0, _Id, _Pid) -> ok;
loop(M, Id, Pid) -> 
	receive
		{msg, Msg} -> io:format("proc ~w ~s~n", [Id, Msg]),
			Pid ! {msg, Msg}, loop(M - 1, Id, Pid)
	end.