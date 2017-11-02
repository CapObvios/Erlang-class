-module(echo).
-export([go/0, loop/0, merge_sort/1, test/0, go2/0]).


go() ->
	 Pid = spawn(echo, loop, []),
	 io:format("~w,~w~n",[Pid, self()]),
	 Pid ! {self(), hola},
 receive
	{Pid, Msg} ->
	 io:format("~s~n",[Msg])
 end.
 %Pid ! stop.

go2() -> 
	Pid1 = spawn(echo, loop, []),
	Pid2 = spawn(echo, loop, []),
	Pid1 ! {sort, [10,2,3,11,34,-2], self()},
	Pid2 ! {sort, [-1,-2,30,21,35,-20], self()},
	LR1 = receive
		{'I was', L2} -> io:format("~w~n",[L2]), L2
	end,
	LR2 = receive
		{'I was', L3} -> io:format("~w~n",[L3]), L3
	end,
	LR3 = lists:merge(LR1, LR2),
	io:format("~w~n",[LR3]),
	Pid1 ! stop,
	Pid2 ! stop.


loop() ->
 	receive
		{From, Msg} ->
			Msg1 = "Del otro proceso " ++ atom_to_list(Msg),
		From ! {self(), Msg1},
			loop();
		{sort, L, Pid} ->
			io:format("inside sort~n"),
			L2 = merge_sort(L),
			Pid ! {'I was', L2},
			loop();
	stop ->
		true
	end.

%-------------------------

merge_sort([]) -> [];
merge_sort([A]) -> [A];
merge_sort(L) -> 
	Half = length(L) div 2,
	Lst1 = lists:sublist(L, Half),
	Lst2 = lists:sublist(L, Half + 1, length(L)),
	lists:merge(merge_sort(Lst1), merge_sort(Lst2)).

test() -> L = [12,23,5,-1,10], merge_sort(L).










