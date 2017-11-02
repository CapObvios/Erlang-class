-module(calc).

-export([add/2, sub/2,mul/2,divd/2, loop/0,start/0,stop/0, svloop/0, ini/0]).

add(A,B) -> 
	calc ! {add, {A,B}, self()},
	receive 
		Res -> Res
	end.

sub(A,B) -> 
	calc ! {sub, {A,B}, self()},
	receive 
		Res -> Res
	end.

mul(A,B) -> 
	calc ! {mul, {A,B}, self()},
	receive 
		Res -> Res
	end.

divd(A,B) -> case B of 0 -> 'MONKEY BY THE KEYBOARD DETECTED';
	_ -> 
	calc ! {divd, {A,B}, self()},
	receive 
		Res -> Res
	end
	end.



start() -> 	register(svp, spawn(calc, ini, [])).

ini() -> process_flag(trap_exit, true),
		register(calc, spawn_link(calc, loop, [])),
		svloop().
		

stop() -> 	case whereis(svp) of 
			undefined -> 
					case whereis(calc) of 
						undefined -> 'NO PROC FOUND'; 
						_ -> exit(whereis(calc), kill),
							'DONE BUT NO SV PROC EXISTING' 
					end;
			_ -> 						
				svp ! {stop, self()},
				receive 
					Mes -> Mes
				end
		end.

loop() -> 
	receive
		{add, {A,B}, R} -> 
			Res = A + B,
			R ! Res,
			loop();
		{sub, {A,B}, R} ->
			Res = A - B,
			R ! Res,
			loop();
		{mul, {A,B}, R} -> 
			Res = A * B,
			R ! Res,
			loop();
		{divd, {A,B}, R} -> 
			Res = A / B,
			R ! Res,
			loop();
		stop -> 
			ok
	end.

svloop() -> receive 
			{'EXIT', _Pid, _Reason} ->
				case whereis(calc) of
					undefined -> 
						register(calc, spawn_link(calc, loop, [])),
						'PROC RESTARTED',
						svloop();
					_ -> 
						svloop()
				end;
			{stop, From} -> 
				Pid = whereis(calc),
				case Pid of 
					undefined -> 
						From ! 'NO PROC FOUND';
					_ -> 
						exit(Pid, kill),
						From ! ok
				end
			end.






