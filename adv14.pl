add(V, K, L, [count(V,K1)|L1]) :- select(count(V,N), L, L1), !, K1 is K + N.
add(V, K, L, [count(V,K)|L]).

init_counts(Xs, Ys) :- init_counts(Xs, [], Ys).
init_counts([_], A, A).
init_counts([X,Y|Xs], A, Ys) :-
    add(X-Y, 1, A, A1),
    init_counts([Y|Xs], A1, Ys).

step(Xs, Ys) :- step(Xs, Xs, Ys).
step([], A, A).
step([count(X-Y,N)|Xs], A, Ys) :-
    rule(X-Y, Z),
    N1 is -N,
    add(X-Y, N1, A,  A1),
    add(X-Z, N,  A1, A2),
    add(Z-Y, N,  A2, A3),
    step(Xs, A3, Ys).

steps(0, Xs, Xs).
steps(N, Xs, Ys) :-
    N > 0, N1 is N - 1,
    step(Xs, Xs1),
    steps(N1, Xs1, Ys).

compare_counts(count(_,N1), count(_,N2)) :- N1 =< N2.

score(Original, Counts, N) :- score(Original, Counts, [], N).
score([C0|Cs], [], A, N) :-
    last(Cs, Cn),
    add(C0, 1, A,  A1),
    add(Cn, 1, A1, A2),
    max_member(compare_counts, count(_,Max), A2),
    min_member(compare_counts, count(_,Min), A2),
    N is (Max - Min) // 2.
score(O, [count(X-Y,K)|Cs], A, N) :-
    add(X, K, A,  A1),
    add(Y, K, A1, A2),
    score(O, Cs, A2, N).

adv14(N, Result) :-
    template(T),
    init_counts(T, Xs),
    steps(N, Xs, Ys),
    score(T, Ys, Result), !.

template([b,n,b,b,n,c,f,h,h,k,o,s,c,h,b,k,k,s,h,n]).

rule(c-h, s).
rule(k-k, v).
rule(f-s, v).
rule(c-n, p).
rule(v-c, n).
rule(c-b, v).
rule(v-k, h).
rule(c-f, n).
rule(p-o, o).
rule(k-c, s).
rule(h-c, p).
rule(p-p, b).
rule(k-o, b).
rule(b-k, p).
rule(b-h, n).
rule(c-c, n).
rule(p-c, o).
rule(f-k, n).
rule(k-f, f).
rule(f-h, s).
rule(s-s, v).
rule(o-n, k).
rule(o-v, k).
rule(n-k, h).
rule(b-o, c).
rule(v-p, o).
rule(c-s, v).
rule(k-s, k).
rule(s-k, b).
rule(o-p, s).
rule(p-k, s).
rule(h-f, p).
rule(s-v, p).
rule(s-b, c).
rule(b-c, c).
rule(f-p, h).
rule(f-c, p).
rule(p-b, n).
rule(n-v, f).
rule(v-o, f).
rule(v-h, p).
rule(b-b, n).
rule(s-f, f).
rule(n-b, k).
rule(k-b, s).
rule(v-v, s).
rule(n-p, n).
rule(s-o, o).
rule(p-n, b).
rule(b-p, h).
rule(b-v, v).
rule(o-b, c).
rule(h-v, n).
rule(p-f, b).
rule(s-p, n).
rule(h-n, n).
rule(c-v, h).
rule(b-n, v).
rule(p-s, v).
rule(c-o, s).
rule(b-s, n).
rule(v-b, h).
rule(p-v, p).
rule(n-n, p).
rule(h-s, c).
rule(o-s, p).
rule(f-b, s).
rule(h-o, c).
rule(k-h, h).
rule(h-b, k).
rule(v-f, s).
rule(c-k, k).
rule(f-f, h).
rule(f-n, p).
rule(o-k, f).
rule(s-c, b).
rule(h-h, n).
rule(o-h, o).
rule(v-s, n).
rule(f-o, n).
rule(o-c, h).
rule(n-f, f).
rule(p-h, s).
rule(h-k, k).
rule(n-h, h).
rule(f-v, s).
rule(o-f, v).
rule(n-c, o).
rule(h-p, o).
rule(k-p, b).
rule(b-f, n).
rule(n-o, s).
rule(c-p, c).
rule(n-s, n).
rule(v-n, k).
rule(k-v, n).
rule(o-o, v).
rule(s-n, o).
rule(k-n, c).
rule(s-h, f).
