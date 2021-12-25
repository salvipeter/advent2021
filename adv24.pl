data([d(13,3,1),d(11,12,1),d(15,9,1),d(-6,12,26),d(15,2,1),d(-8,1,26),d(-4,1,26),d(15,13,1),d(10,1,1),d(11,6,1),d(-11,2,26),d(0,11,26),d(-8,10,26),d(-7,3,26)]).

% Which digits can be used to force division
% (see adv24.sml for more explanation)
important([n,n,n,y,n,y,y,n,n,n,y,y,y,y]).

try([], _, [], Z, []) :- Z =:= 0.
try([d(A,_,M)|Ds], Ws, [y|YNs], Z, [Z1|R]) :-
    Z0 is Z // M, Z1 is Z mod 26 + A,
    between(1, 9, Z1),
    try(Ds, Ws, YNs, Z0, R).
try([d(_,B,M)|Ds], Ws, [n|YNs], Z, [W|R]) :-
    member(W, Ws),
    Z0 is 26 * (Z // M) + W + B,
    try(Ds, Ws, YNs, Z0, R).

adv24(X) :-
    data(D), important(I),
    try(D, [9,8,7,6,5,4,3,2,1], I, 0, L), !,
    atomic_list_concat(L, X).

adv24b(X) :-
    data(D), important(I),
    try(D, [1,2,3,4,5,6,7,8,9], I, 0, L), !,
    atomic_list_concat(L, X).

% Simulation (for testing)

get(A, R, X) :- member(A-X, R), !.
get(A, _, A).

run([], [], R, Z) :- member(z-Z, R).
run([inp(_)|_], [], R, Z) :- member(z-Z, R).
run([inp(A)|Ps], [X|Xs], R, Z) :-
    select(A-_, R, R1),
    run(Ps, Xs, [A-X|R1], Z).
run([add(A,B)|Ps], Xs, R, Z) :-
    get(B, R, Vb), select(A-Va, R, R1),
    V is Va + Vb,
    run(Ps, Xs, [A-V|R1], Z).
run([mul(A,B)|Ps], Xs, R, Z) :-
    get(B, R, Vb), select(A-Va, R, R1),
    V is Va * Vb,
    run(Ps, Xs, [A-V|R1], Z).
run([mul(A,B)|Ps], Xs, R, Z) :-
    get(B, R, Vb), select(A-Va, R, R1),
    V is Va * Vb,
    run(Ps, Xs, [A-V|R1], Z).
run([div(A,B)|Ps], Xs, R, Z) :-
    get(B, R, Vb), select(A-Va, R, R1),
    V is Va // Vb,
    run(Ps, Xs, [A-V|R1], Z).
run([mod(A,B)|Ps], Xs, R, Z) :-
    get(B, R, Vb), select(A-Va, R, R1),
    V is Va mod Vb,
    run(Ps, Xs, [A-V|R1], Z).
run([eql(A,B)|Ps], Xs, R, Z) :-
    get(B, R, Vb), select(A-Va, R, R1),
    ( Va =:= Vb -> V = 1 ; V = 0 ),
    run(Ps, Xs, [A-V|R1], Z).

validate(Xs, Z) :- program(P), run(P, Xs, [x-0,y-0,z-0,w-0], Z), !.

program([inp(w),mul(x,0),add(x,z),mod(x,26),div(z,1),add(x,13),eql(x,w),eql(x,0),mul(y,0),add(y,25),mul(y,x),add(y,1),mul(z,y),mul(y,0),add(y,w),add(y,3),mul(y,x),add(z,y),inp(w),mul(x,0),add(x,z),mod(x,26),div(z,1),add(x,11),eql(x,w),eql(x,0),mul(y,0),add(y,25),mul(y,x),add(y,1),mul(z,y),mul(y,0),add(y,w),add(y,12),mul(y,x),add(z,y),inp(w),mul(x,0),add(x,z),mod(x,26),div(z,1),add(x,15),eql(x,w),eql(x,0),mul(y,0),add(y,25),mul(y,x),add(y,1),mul(z,y),mul(y,0),add(y,w),add(y,9),mul(y,x),add(z,y),inp(w),mul(x,0),add(x,z),mod(x,26),div(z,26),add(x,-6),eql(x,w),eql(x,0),mul(y,0),add(y,25),mul(y,x),add(y,1),mul(z,y),mul(y,0),add(y,w),add(y,12),mul(y,x),add(z,y),inp(w),mul(x,0),add(x,z),mod(x,26),div(z,1),add(x,15),eql(x,w),eql(x,0),mul(y,0),add(y,25),mul(y,x),add(y,1),mul(z,y),mul(y,0),add(y,w),add(y,2),mul(y,x),add(z,y),inp(w),mul(x,0),add(x,z),mod(x,26),div(z,26),add(x,-8),eql(x,w),eql(x,0),mul(y,0),add(y,25),mul(y,x),add(y,1),mul(z,y),mul(y,0),add(y,w),add(y,1),mul(y,x),add(z,y),inp(w),mul(x,0),add(x,z),mod(x,26),div(z,26),add(x,-4),eql(x,w),eql(x,0),mul(y,0),add(y,25),mul(y,x),add(y,1),mul(z,y),mul(y,0),add(y,w),add(y,1),mul(y,x),add(z,y),inp(w),mul(x,0),add(x,z),mod(x,26),div(z,1),add(x,15),eql(x,w),eql(x,0),mul(y,0),add(y,25),mul(y,x),add(y,1),mul(z,y),mul(y,0),add(y,w),add(y,13),mul(y,x),add(z,y),inp(w),mul(x,0),add(x,z),mod(x,26),div(z,1),add(x,10),eql(x,w),eql(x,0),mul(y,0),add(y,25),mul(y,x),add(y,1),mul(z,y),mul(y,0),add(y,w),add(y,1),mul(y,x),add(z,y),inp(w),mul(x,0),add(x,z),mod(x,26),div(z,1),add(x,11),eql(x,w),eql(x,0),mul(y,0),add(y,25),mul(y,x),add(y,1),mul(z,y),mul(y,0),add(y,w),add(y,6),mul(y,x),add(z,y),inp(w),mul(x,0),add(x,z),mod(x,26),div(z,26),add(x,-11),eql(x,w),eql(x,0),mul(y,0),add(y,25),mul(y,x),add(y,1),mul(z,y),mul(y,0),add(y,w),add(y,2),mul(y,x),add(z,y),inp(w),mul(x,0),add(x,z),mod(x,26),div(z,26),add(x,0),eql(x,w),eql(x,0),mul(y,0),add(y,25),mul(y,x),add(y,1),mul(z,y),mul(y,0),add(y,w),add(y,11),mul(y,x),add(z,y),inp(w),mul(x,0),add(x,z),mod(x,26),div(z,26),add(x,-8),eql(x,w),eql(x,0),mul(y,0),add(y,25),mul(y,x),add(y,1),mul(z,y),mul(y,0),add(y,w),add(y,10),mul(y,x),add(z,y),inp(w),mul(x,0),add(x,z),mod(x,26),div(z,26),add(x,-7),eql(x,w),eql(x,0),mul(y,0),add(y,25),mul(y,x),add(y,1),mul(z,y),mul(y,0),add(y,w),add(y,3),mul(y,x),add(z,y)]).
