board([ [sssss, t, sssss, t],
        [ssss, ub, sl, db, sss, ub, sl, db],
        [ss, t, ub, sl, c, sl, db, t, ub, sl, x, sl, db, t],
        [sl, ub, sl, db, sss, ub, sl, db, sss, ub, sl, db],
        [ub, sss, db, t, ub, sss, db, t, ub, sss, db],
        [db, sss, ub, sl, db, sss, ub, sl, db, sss, ub],
        [sl, db, t, ub, sss, db, t, ub, sss, db, t, ub],
        [sss, db, sss, ub, sl, db, sss, ub],
        [ssss, db, t, ub, sss, db, t, ub]]).

boardT([[x, x, x],
        [x, x],
        [x, x, x]]).

% Adicionar coordenadas

translate(s8, '        ').
translate(s7, '       ').
translate(s5, '     ').
translate(s3, '   ').
translate(sl, ' ').
translate(t, '___').
translate(ub, '/').
translate(db, '\\').
translate(nl, '\n').
translate(c, '1').

translate(x, '').

generate_empty_space(Spaces, NumberOfTimes):-
    NumberOfTimes = 0,
    write('').

generate_empty_space(Spaces, NumberOfTimes):-
    N1 is NumberOfTimes - 1,
    translate(Spaces, EmptySpace),
    write(EmptySpace),
    generate_empty_space(Spaces, N1).

display_line_a_aux(N):-
    N=0,
    nl.

display_line_a_aux(NumberHexagons):-
    N1 is NumberHexagons - 1,
    translate(t, A),
    translate(s7, SpaceBetweenHex),
    write(A),
    write(SpaceBetweenHex),
    display_line_a_aux(N1).

display_line_a(EmptySpace, NumberEmptySpaces, NumberHexagons):-
    generate_empty_space(EmptySpace, NumberEmptySpaces),
    display_line_a_aux(NumberHexagons).

display_line_b_aux(N):-
    N = 0,
    nl.

display_line_b_aux(N):-
    N1 is N-1,
    translate(ub, OpenHex),
    translate(s3, SpaceInsideHex),
    translate(db, CloseHex),
    translate(s5, SpaceBetweenHex),
    write(OpenHex),
    write(SpaceInsideHex),
    write(CloseHex),
    write(SpaceBetweenHex),
    display_line2(N1).

display_line_b(EmptySpace, NumberEmptySpaces, NumberHexagons):-
    generate_empty_space(EmptySpace, NumberEmptySpaces),
    display_line_b_aux(NumberHexagons).

display_board:-
    display_line_a(s8, 2, 4),
    display_line_b(s7, 2, 4).

display:- display_board.