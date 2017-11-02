-module(tempo).
-export([loop/1, go/0]).

go() -> register(center, spawn(tempo, loop, [5])).

loop(M) ->
	receive
	{msg, Msg} ->
		io:format("message: ~w~n", [Msg]), loop(M-1);
	stop -> ok
	after
	10000 ->
		io:format("End of wait: ~w~n", [M]), loop(M)
	end.