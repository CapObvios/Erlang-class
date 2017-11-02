-module(emc).

-compile([export_all]).

%----------Video 2--------------

-type expr() :: {'num', integer()} 
		| {'var', atom()}
		| {'add', expr(), expr()}
		| {'mul', expr(), expr()}.


%----------Video 3--------------

-spec print(expr()) -> string().

print({'num', I}) -> integer_to_list(I);
print({'var', V}) -> atom_to_list(V);
print({'add', E1, E2}) -> "(" ++ print(E1) ++ "+" ++ print(E2) ++ ")";
print({'mul', E1, E2}) -> "(" ++ print(E1) ++ "*" ++ print(E2) ++ ")".

pp1() -> print({'num', 5}).
pp2() -> print({add, {'num',10}, {mul, {'num', 5}, {var, a}}}). 

%----------Video 4--------------

-type env() :: [{atom(), integer()}].

-spec eval(env(), expr()) -> integer().

-spec lookup(atom(), env()) -> integer().

lookup(A,[{A,Val}|_]) -> Val;
lookup(A,[_|T]) -> lookup(A,T).

eval(_Env, {'num', N}) -> N;
eval(Env, {'var',A}) -> lookup(A,Env);
eval(Env, {'add',E1,E2}) -> eval(Env,E1) + eval(Env,E2);
eval(Env, {'mul',E1,E2}) -> eval(Env,E1) * eval(Env,E2).

%----------Video 5--------------

-type instr() :: {'push', integer()}
		| {'fetch', atom()}
		| {'add2'}
		| {'mul2'}.

-type program() :: [instr()].

-type stack() :: [integer()].

-spec compile(expr()) -> program().

compile({'num', N}) -> [{push, N}];
compile({'var', A}) -> [{fetch, A}];
compile({'add', E1,E2}) -> compile(E1) ++ compile(E2) ++ [{'add2'}];
compile({'mul', E1,E2}) -> compile(E1) ++ compile(E2) ++ [{'mul2'}].

-spec run(program(), env(), stack()) -> integer().

run([], _Env, N) -> N;
run([{'push', N} | Continue], Env, Stack) -> run(Continue, Env, [N| Stack]);
run([{'fetch',A} | Continue], Env, Stack) -> run(Continue, Env, [lookup(A,Env) | Stack]);
run([{'add2'} | Continue], Env, [N1, N2 | Stack]) -> run(Continue, Env, [(N1 + N2)| Stack]);
run([{'mul2'} | Continue], Env, [N1, N2 | Stack]) -> run(Continue, Env, [(N1 * N2)| Stack]).

%----------Video 6--------------

-spec parse(string()) -> {expr(), string()}.

parse([$(|Rest]) ->
		{E1,Rest1} = parse(Rest),
		[Op|Rest2] = Rest1,
		{E2,Rest3} = parse(Rest2),
		[$)|RestFinal] = Rest3,
		{case Op of 
			$+ -> {'add', E1, E2};
			$* -> {'mul', E1,E2}
		end,
		RestFinal};


parse([Ch|Rest]) when $a =< Ch andalso Ch =< $z ->
	{Succeeds,Remainder} = get_while(fun is_alpha/1, Rest),
	{{'var', list_to_atom([Ch|Succeeds])}, Remainder};

parse([Ch|Rest]) when $0 =< Ch andalso Ch =< $9 ->
	{Succeeds,Remainder} = get_while(fun is_num/1, Rest),
	{{'num', list_to_atom([Ch|Succeeds])}, Remainder}.

is_num(Ch) -> $0 =< Ch andalso Ch =< $9.

is_alpha(Ch) -> $a =< Ch andalso Ch =< $z.


-spec get_while(fun((T) -> boolean()), [T]) -> {[T],[T]}.

get_while(P, [Ch| Rest]) -> 
	case P(Ch) of 
		true ->
			{Succeeds, Remainder} = get_while(P, Rest),
			{[Ch|Succeeds], Remainder};
		false ->
			{[],[Ch|Rest]}
	end;
get_while(_P,[]) -> {[],[]}.


%----------Video 7--------------

%-spec test1() -> integer().

%test1() -> eval(env1(), expr1()).

-spec expr2() -> expr().

expr2() ->
	{'add',{'mul',{'num',1},{'var',b}},{'mul',{'add',
		{'mul',{'num',2},{'var',b}},{'mul',{num,1},{'var',b}}},{'num',0}}}.


zeroA({'add',E,{'num',0}}) -> E;
zeroA({'add',{'num',0},E}) -> E;
zeroA(E) -> E.

mul0({'mul',E,{'num',1}}) -> E;
mul0({'mul',{'num',1},E}) -> E;
mul0(E) -> E.

mulZ({'mul',_,{'num',0}}) -> {'num',0};
mulZ({'mul',{'num',0},_}) -> {'num',0};
mulZ(E) -> E.

compose([]) -> fun (E) -> E end;
compose([Rule|Rules]) -> fun (E) -> (compose(Rules))(Rule(E)) end.

rules() -> [ fun zeroA/1, fun mul0/1, fun mulZ/1].

simp(F, {'add', E1,E2}) -> F({'add', simp(F,E1), simp(F,E2)});
simp(F, {'mul', E1,E2}) -> F({'mul', simp(F,E1), simp(F,E2)});
simp(_,E) -> E.

simplify(E) -> simp(compose(rules()),E).