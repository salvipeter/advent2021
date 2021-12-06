% -*- mode: prolog -*-

% Solution using memoization
:- table births/3.
births(0, _, 1).
births(K, 0, B) :-
    K > 0, K1 is K - 1,
    births(K1, 6, B1), births(K1, 8, B2),
    B is B1 + B2.
births(K, N, B) :-
    K > 0, K1 is K - 1,
    N > 0, N1 is N - 1,
    births(K1, N1, B).

% Solution using tail-recursion
births1(K, N, B) :- K1 is K - N, births2(K1, [1,1,1,1,1,1,1,1,1], B).
births2(0, [A|_], A).
births2(K, [A1,A2,A3,A4,A5,A6,A7,A8,A9], B) :-
    K > 0, K1 is K - 1,
    A0 is A7 + A9,
    births2(K1, [A0,A1,A2,A3,A4,A5,A6,A7,A8], B).

adv06(X) :- fish(F), maplist(births1(80), F, L), sum_list(L, X).

adv06b(X) :- fish(F), maplist(births1(256), F, L), sum_list(L, X).

fish([3,4,3,1,2,1,5,1,1,1,1,4,1,2,1,1,2,1,1,1,3,4,4,4,1,3,2,1,3,4,1,1,3,4,2,5,5,3,3,3,5,1,4,1,2,3,1,1,1,4,1,4,1,5,3,3,1,4,1,5,1,2,2,1,1,5,5,2,5,1,1,1,1,3,1,4,1,1,1,4,1,1,1,5,2,3,5,3,4,1,1,1,1,1,2,2,1,1,1,1,1,1,5,5,1,3,3,1,2,1,3,1,5,1,1,4,1,1,2,4,1,5,1,1,3,3,3,4,2,4,1,1,5,1,1,1,1,4,4,1,1,1,3,1,1,2,1,3,1,1,1,1,5,3,3,2,2,1,4,3,3,2,1,3,3,1,2,5,1,3,5,2,2,1,1,1,1,5,1,2,1,1,3,5,4,2,3,1,1,1,4,1,3,2,1,5,4,5,1,4,5,1,3,3,5,1,2,1,1,3,3,1,5,3,1,1,1,3,2,5,5,1,1,4,2,1,2,1,1,5,5,1,4,1,1,3,1,5,2,5,3,1,5,2,2,1,1,5,1,5,1,2,1,3,1,1,1,2,3,2,1,4,1,1,1,1,5,4,1,4,5,1,4,3,4,1,1,1,1,2,5,4,1,1,3,1,2,1,1,2,1,1,1,2,1,1,1,1,1,4]).
