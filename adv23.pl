% Notations:
%   ..........................
%   ..l1l2  ab  bc  cd  r2r1..
%   ......a4..b4..c4..d4......
%       ..a3..b3..c3..d3..
%       ..a2..b2..c2..d2..
%       ..a1..b1..c1..d1..
%       ..................

% solution(State, A, A) :- solved(State).
solution(State, A, _) :- solved(State), print(A), nl, fail.
solution(State, A, X) :-
    move(State, C, State1),
    A1 is A + C,
    solution(State1, A1, X).

move(State, C, State1) :-
    member(From, [l1,l2,ab,bc,cd,r2,r1]),
    member(From-A, State),
    ownroom(A, R),
    \+ (member(X, R), member(X-B, State), B \= A),
    member(To, R), free(State, To),
    trymove(State, From, To, N, State1),
    cost(A, N, C).
move(State, C, State1) :-
    member(X, [a,b,c,d]),
    ownroom(X, R),
    member(From, R),
    member(From-A, State),
    once((A \= X ; (member(Y, R), member(Y-B, State), B \= A))),
    member(To, [l1,l2,ab,bc,cd,r2,r1]),
    trymove(State, From, To, N, State1),
    cost(A, N, C).

ownroom(a, [a1,a2,a3,a4]).
ownroom(b, [b1,b2,b3,b4]).
ownroom(c, [c2,c2,c3,c4]).
ownroom(d, [d1,d2,d3,d4]).

solved(State) :-
    State = [R-A|_], ownroom(A, Rs), memberchk(R, Rs),
    memberchk(a1-a, State), memberchk(a2-a, State),
    memberchk(a3-a, State), memberchk(a4-a, State),
    memberchk(b1-b, State), memberchk(b2-b, State),
    memberchk(b3-b, State), memberchk(b4-b, State),
    memberchk(c1-c, State), memberchk(c2-c, State),
    memberchk(c3-c, State), memberchk(c4-c, State),
    memberchk(d1-d, State), memberchk(d2-d, State),
    memberchk(d3-d, State), memberchk(d4-d, State).

cost(a, N, N).
cost(b, N, C) :- C is N * 10.
cost(c, N, C) :- C is N * 100.
cost(d, N, C) :- C is N * 1000.

trymove(State, X, Y, N, [Y-A|State1]) :-
    find_path(X, Y, [X|S]),
    maplist(free(State), S),
    select(X-A, State, State1),
    length(S, N).

free(State, X) :- \+ member(X-_, State).

:- table(find_path/3).
find_path(X, Y, P) :- route(R), segment(R, X, Y, P), !.
find_path(X, Y, P) :- route(R), segment(R, Y, X, P0), reverse(P0, P), !.

segment([Y|_], ok, Y, [Y]).
segment([X|Rs], X, Y, [X|S]) :- segment(Rs, ok, Y, S).
segment([R|Rs], X, Y, S) :- R \= X, X \= ok, segment(Rs, X, Y, S).
segment([R|Rs], ok, Y, [R|S]) :- R \= Y, segment(Rs, ok, Y, S).

route([a1,a2,a3,a4,x,l2,l1]).
route([a1,a2,a3,a4,x,ab,x,bc,x,cd,x,r2,r1]).
route([b1,b2,b3,b4,x,ab,x,l2,l1]).
route([b1,b2,b3,b4,x,bc,x,cd,x,r2,r1]).
route([c1,c2,c3,c4,x,bc,x,ab,x,l2,l1]).
route([c1,c2,c3,c4,x,cd,x,r2,r1]).
route([d1,d2,d3,d4,x,cd,x,bc,x,ab,x,l2,l1]).
route([d1,d2,d3,d4,x,r2,r1]).

adv23(X) :- amphipods(A), solution(A, 0, X).

amphipods([a1-c,a2-d,a3-d,a4-a,
           b1-c,b2-b,b3-c,b4-d,
           c1-d,c2-a,c3-b,c4-a,
           d1-b,d2-c,d3-a,d4-b]).
