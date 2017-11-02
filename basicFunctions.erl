-module(basicFunctions).

-export([len1/1, len2/1, len3/1, max1/1, max2/1, max3/1, reverse1/1, reverse2/1, reverse3/1, sum1/1, sum2/1, sum3/1, last1/1, last2/1, last3/1, last4/1, dpdummy/2, reduce1/3, add/2, cons/2, factorial/1, factorial2/1, factorial3/1, factorial4/1, map1/2, map2/2, map3/2]).

-export([multiplybyn/1, qs/2, curry2/1, mystery/1]).

-export([f1/1, f2/2]).

f1([]) -> 1;
f1([H | T]) -> if
		   		is_list(H) -> max(f1(H) + 1,f1(T));
		   		true -> f1(T)
	       	end.

f2(X,[]) ->
 [[X]];
f2(X,[Y | Ys]) ->
 [[X,Y | Ys] | [ [Y | Z] || Z <- f2(X,Ys)]].

%-------------------------------------------------------
%custom curry

curry2(F) -> fun(X) -> fun(Y) -> F(X,Y) end end.

%-------------------------------------------------------
%

mystery(A) -> 
	F =  case A > 0 of
			true -> fun(X, Y) -> X + Y end;
			false -> fun(X, Y) -> X - Y end
		end,
	F(0,A).
	

%-------------------------------------------------------
%custom length of a list with a usual recursion

len1([]) -> 0;
len1([_| T]) -> 1 + len1(T).

%-------------------------------------------------------
%custom length of a list with a tail recursion

len2(Lst) -> len2(Lst, 0).

%len2(Lst, Res)

len2([], Res) -> Res;
len2([_| T], Res) -> len2(T, Res + 1).

%-------------------------------------------------------
%custom length of a list with continuation

len3(L) -> len3(L, fun(X) -> X end).

len3([], K) -> K(0);
len3([_|T], K) -> len3(T, fun(X) -> K(X + 1) end).


%-------------------------------------------------------
%custom max with a usual recursion

max1([]) -> 0;
max1([A]) -> A;
max1([H|T]) -> max(max1(T), H).

%-------------------------------------------------------
%custom max with a tail recursion

max2([]) -> 0;
max2([H|T]) -> max2([H|T], H).

max2([], Res) -> Res;
max2([H|T], Res) -> 
	case H > Res of 
		true -> max2(T, H);
		false -> max2(T, Res)
	end.

%-------------------------------------------------------
%custom max with using reduce

max3([]) -> 0;
max3([A]) -> A;
max3([H|T]) -> reduce1(fun (A,B) -> case A > B of true -> A; false -> B end end, [H|T], H).

%-------------------------------------------------------
%custom reverse with a usual recursion

reverse1([]) -> [];
reverse1([A]) -> [A];
reverse1([H|T]) -> reverse1(T) ++ [H].

%-------------------------------------------------------
%custom reverse with a tail recursion

reverse2(Lst) -> reverse2(Lst, []).

%reverse2(Lst, Res)

reverse2([], R) -> R;
reverse2([H|T], R) -> reverse2(T, [H] ++ R).

%-------------------------------------------------------
%custom reverse with using reduce

reverse3([]) -> [];
reverse3([A]) -> [A];
reverse3([H|T]) -> reduce1(fun (A,B) -> B ++ [A] end, [H|T], []).

%-------------------------------------------------------
%custom sum with a usual recursion

sum1([]) -> 0;
sum1([A]) -> A;
sum1([H|T]) -> sum1(T) + H.

%-------------------------------------------------------
%custom sum with a tail recursion

sum2(Lst) -> sum2(Lst, 0).

sum2([], Res) -> Res;
sum2([H|T], Res) -> sum2(T, Res + H).

%-------------------------------------------------------
%custom sum with reduce usage

sum3([]) -> 0;
sum3([A]) -> A;
sum3([H|T]) -> reduce1(fun (A,B) -> A + B end, T, H).

%-------------------------------------------------------
%custom last with a usual == tail recursion

last1([]) -> [];
last1([A]) -> A;
last1([_|T]) -> last1(T).

%-------------------------------------------------------
%custom last with a usual == tail recursion

last2([]) -> [];
last2([A]) -> A;
last2([_|T]) -> last2(T).

%-------------------------------------------------------
%custom last with reduce usage

last3([]) -> [];
last3([A]) -> A;
last3([_|T]) -> reduce1(fun replace/2, T, []).

%-------------------------------------------------------
%custom last with reduce usage

last4([]) -> [];
last4([A]) -> A;
last4([_|T]) -> reduce1(fun (A,B) -> case B of [] -> A; _ -> B end end, T, []).

%-------------------------------------------------------
%custom ++ with a usual recursion

dpdummy([],[]) -> [];
dpdummy(Lst1, []) -> Lst1;
dpdummy([], Lst2) -> Lst2; %%[H| dpdummy([],Lst2)];
dpdummy([H|Lst1], Lst2) -> [H | dpdummy(Lst1, Lst2)]. 

%-------------------------------------------------------
%custom add func

add(A,B) -> A + B.
cons(H,T) -> [H|T].

%-------------------------------------------------------
%custom reduce func

reduce1(_, [], EL) -> EL;
reduce1(F, [H | T], EL) -> F(H, reduce1(F, T, EL)).

%-------------------------------------------------------
%replace

replace(A,B) -> case B of [] -> A; _ -> B end.

%-------------------------------------------------------
%factorial

factorial(0) -> 1;
factorial(N) -> factorial(N-1)*N.


%-------------------------------------------------------
%Second factorial modification

factorial2(X) -> 
  case X of
   0 -> 1;
   N -> factorial2(N-1)*N
  end.

%-------------------------------------------------------
%Third factorial modification TAIL RECURSION

factorial3(N) -> factorial3(N, 1).

factorial3(0, Res) -> Res;
factorial3(N, Res) -> factorial3(N-1, Res*N).

%-------------------------------------------------------
%Fourth factorial

factorial4(N) -> factorial4(N, fun (X) -> X end).

factorial4(0, F) -> F(1);
factorial4(N, F) -> factorial4(N-1, fun (X) -> F(X * N) end).

%-------------------------------------------------------
% custom map with usual recursion

map1(_,[]) -> [];
map1(F, [H|T]) -> [F(H) | map1(F, T)].

%-------------------------------------------------------
% custom map with tail recursion

map2(F, L) -> map2(F,L,[]).

map2(_, [], Res) -> Res;
map2(F, [H|T], Res) -> map2(F, T, Res ++ [F(H)]).

%-------------------------------------------------------
% map using reduce

map3(F, L) -> reduce1(fun (H, T) -> [F(H)|T] end, L, []).



%-------------------------------------------------------
%multiply by n

multiplybyn(N) -> fun (X) -> X * N end.


%-------------------------------------------------------

qs([],_) -> [];
qs([H|T],F) -> qs([X || X <- T, F(X, H)], F) ++ [H] ++ qs([X || X <- T, not F(X, H)], F). 

%-------------------------------------------------------
%-------------------------------------------------------
%-------------------------------------------------------


%max, reverse, sum, last, ++. read an article Why functional
%programming matters about erlang to 11th page.
% cons(1, []) == [1]
% add(1,add(10,add(5,0)))\
%GOOGLE CPS
%max, reverse, sum, last
% L = [1,4,5,6]. 
% [X * X || X <- L].

%processes().
%i().
%self().
% Pid2 ! {self(), foo}
%flush().

