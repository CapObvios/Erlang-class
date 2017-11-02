%------------------------------------------------------------------------
% SERGEY PAVLOV and AUGUSTO MONGE
%------------------------------------------------------------------------
-module(my_db).

-export([start/0, stop/0, write/2, delete/1, read/1, match/1, loop/1]).


% Define timeout time. By default is 10.000 = 10 sec.
-define(TIMEOUTTIME, 10000).



%---------------------------------------------------------------
%my_db:start() ? ok. 

start() ->
	register(loopp, spawn(my_db, loop, [[]])),
	ok.

	
%---------------------------------------------------------------
%my_db:stop() ? ok. 

stop() -> loopp ! {stop, self()},
	receive
		_ -> ok
		after ?TIMEOUTTIME -> timeout
	end.


%---------------------------------------------------------------
%my_db:write(Key, Element) ? ok. 

write(Key, Element) -> 
	loopp ! {write, {Key, Element}, self()},
	receive 
		Res -> case Res of ok -> {ok, Element}; _ -> Res end
		after ?TIMEOUTTIME -> timeout
	end.


%---------------------------------------------------------------
%my_db:delete(Key) ? ok. 

delete(Key) -> 
	loopp ! {delete, Key, self()},
	receive
		Res -> Res
	after ?TIMEOUTTIME -> timeout
	end.


%---------------------------------------------------------------
%my_db:read(Key) ? {ok, Element} | {error, instance}. 

read(Key) -> 
	loopp ! {read, Key, self()},
	receive
		Res -> case Res of [] -> {error, instance}; _ -> {ok, Res} end
	after ?TIMEOUTTIME -> timeout
	end.


%---------------------------------------------------------------
%my_db:match(Element) ? [Key1, ..., KeyN]. 

match(Element) -> 
	loopp ! {match, Element, self()},
	receive
		{res, L} -> L
	after ?TIMEOUTTIME -> timeout
	end.


%-----------------------SERVER-PROCESS---------------------------------------

loop(Data) ->
 	receive
		{write, {Key, Element}, Backadd} ->
				case readins(Data, Key) of 
					[] -> 
						Newdata = [{Key, Element}| Data],	
						Backadd ! ok,	
						loop(Newdata);
					_ -> 
						Backadd ! {error, 'ELEMENT ALREADY EXISTS'},	
						loop(Data)
				end;
		{read, Key, Backadd} -> 	
			Backadd ! readins(Data, Key),
			loop(Data);
		{match, Elem, Backadd} -> 
			Backadd ! {res, matchins(Data, Elem, [])},
			loop(Data);
		{delete, Keytodel, Backadd} -> 
			Newdata = delins(Data, Keytodel, []),
			case Newdata of noelem -> Backadd ! {error, 'NO ELEMENT EXISTS'}, loop(Data); 
				_ -> Backadd ! ok, loop(Newdata)
			end;
		{stop, Backadd} -> 
			Backadd ! ok,
			true
	end.


%--------------INSIDE-FUNCS-------------------------------------------------

readins([], _) -> [];
readins([{Key, Val}| T], Keytm) -> 
			case Key of 
				Keytm -> Val;
				_ -> readins(T, Keytm)
			end.


%---------------------------------------------------------------

matchins([], _, Res) -> Res;
matchins([{Key, Val}| T], Valtm, Res) -> 
			case Val of 
				Valtm -> matchins(T, Valtm, [Key|Res]);
				_ -> matchins(T, Valtm, Res)
			end.


%---------------------------------------------------------------

delins([], _, _Res) -> noelem;
delins([{Key, Val}| T], Keytm, Res) -> 
			case Key of 
				Keytm -> Res ++ T;
				_ -> delins(T, Keytm, [{Key, Val} |Res])
			end.

