board([ [sssss, t, sssss, t],
        [ssss, ub, sl, db, sss, ub, sl, db],
        [ss, t, ub, sss, db, t, ub, sss, db, t],
        [sl, ub, sl, db, sss, ub, sl, db, sss, ub, sl, db],
        [ub, sss, db, t, ub, sss, db, t, ub, sss, db],
        [db, sss, ub, sl, db, sss, ub, sl, db, sss, ub],
        [sl, db, t, ub, sss, db, t, ub, sss, db, t, ub],
        [sss, db, sss, ub, sl, db, sss, ub],
        [ssss, db, t, ub, sss, db, t, ub]]).

translate(sssss, '     ').
translate(ssss, '    ').
translate(sss, '   ').
translate(ss, '  ').
translate(sl, ' ').
translate(t, '_').
translate(ub, '/').
translate(db, '\\').
translate(nl, '\n').

display_lineA(L1):-
    translate(sssss, C),
    write(C),
    translate(t, C),
    write(C),
    translate(sssss, C),
    write(C),
    translate(t, C),
    write(C).

display_line([]):-
    write('').

display_line([E1|Es]):-
    translate(E1, C),
    write(C),
    display_line(Es).

display_board([]):-
    nl.

display_board([L1|Ls]):-
    display_line(L1),
    nl,
    display_board(Ls).

display:- board(B), display_board(B).