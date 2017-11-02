-module(subscription).

-compile(export_all).

%Sdt -> [{Pid, Topic}, ...]
%Tdt -> [Topic1, Topic2]

start(Name) -> register(Name, spawn(subscription, loop, [[],[programming, math]])).

loop(Tdt, Sdt) -> receive 
		{subscribe, Topic, Pid} ->
			case chsub(Topic, Pid, Sdt) of false -> 
					Pid ! ok,
					loop(Tdt, [{Pid, Topic}|Sdt]);
					true -> Pid ! error, loop(Tdt, Sdt)
			end;
		{unsubscribe, Topic, Pid} ->
			case delsub(Topic, Pid, Sdt, []) of {ok, Ans} -> 
					Pid ! ok,
					loop(Tdt, Ans);
					{error, _} -> Pid ! error, loop(Tdt, Sdt)
			end;
		{send, Topic, Message} ->
			sendmes(Sdt, Topic, Message),
			loop(Tdt, Sdt);			
		{topics, Pid} ->
			Pid ! listofpids(Sdt, Pid, []),
			loop(Tdt, Sdt);	
		{pids, Topics, Pid} -> 
			Pid ! listoftopics(Sdt, Topics, []),
			loop(Tdt, Sdt)
		end.

chsub(Topic, Pid, []) -> false;
chsub(Topic, Pid, [D|Sdt]) ->  case D of 
						{Pid, Topic} -> true;
						_ -> chsub(Topic, Pid, Sdt)
				end.

delsub(Topic, Pid, [], Ans) -> {error, Ans};
delsub(Topic, Pid, [D|Sdt], Ans) ->  case D of 
						{Pid, Topic} -> {ok,[Ans|Sdt]};
						_ -> delsub(Topic, Pid, Sdt, [D|Ans])
				end.

sendmes([], _Topic, _Mes) -> ok;
sendmes([{P,T}| Sdt], Topic, Mes) -> 
	case Topic of 
	T -> 
		P ! Mes, 
		sendmes(Sdt, Topic, Mes);
	_ -> 
		sendmes(Sdt, Topic, Mes)
	end.

listofpids([], _, Res )-> Res;
listofpids([{Piid | Topiic} | Sdt], P, Res)->
	case Piid  of
		P -> listofpids(Sdt, P, [Topiic|Res]);
	 	_ -> listofpids(Sdt, P, Res)
	end.

listoftopics([], _, Res )-> Res;
listoftopics([{Piid | Topiic} | Sdt], T, Res)->
	case Topiic  of
		T -> listoftopics(Sdt, T, [Piid|Res]);
	 	_ -> listoftopics(Sdt, T, Res)
	end.

