path(end, _, _).
path(Pos, Caves, Visited) :-
    Pos \= end,
    ( member(Pos-Pos1, Caves) ; member(Pos1-Pos, Caves) ),
    Pos1 \= start,
    ( Pos1 = small(S) -> \+ member(S, Visited),
                         Visited1 = [S|Visited]
    ; Visited1 = Visited
    ),
    path(Pos1, Caves, Visited1).

adv12(X) :- caves(C), aggregate_all(count, path(start, C, []), X).

update_visits(end, VO, VT, VO, VT).
update_visits(large(_), VO, VT, VO, VT).
update_visits(small(S), VO, VT, [S|VO], VT) :- \+ member(S, VO), S \= VT.
update_visits(small(S), VO, none, VO1, S) :- select(S, VO, VO1).

path1(end, _, _, _).
path1(Pos, Caves, VisitedOnce, VisitedTwice) :-
    Pos \= end,
    ( member(Pos-Pos1, Caves) ; member(Pos1-Pos, Caves) ),
    Pos1 \= start,
    update_visits(Pos1, VisitedOnce, VisitedTwice, VisitedOnce1, VisitedTwice1),
    path1(Pos1, Caves, VisitedOnce1, VisitedTwice1).

adv12b(X) :- caves(C), aggregate_all(count, path1(start, C, [], none), X).

caves([small(pg)-large(ch),small(pg)-small(yd),small(yd)-start,small(fe)-small(hv),small(bi)-large(ch),large(ch)-small(yd),end-small(bi),small(fe)-large(ry),small(ng)-large(ch),small(fe)-large(ch),small(ng)-small(pg),small(hv)-large(fl),large(fl)-small(fe),small(hv)-small(pg),small(bi)-small(hv),large(ch)-end,small(hv)-small(ng),small(yd)-small(ng),small(pg)-small(fe),start-small(ng),end-large(fl),small(fe)-small(bi),large(fl)-small(ks),small(pg)-start]).
