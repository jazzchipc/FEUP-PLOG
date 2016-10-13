board([[x, x, x],
        [x, x],
        [x, x, x]]).

% Adicionar coordenadas

translate(s8, '        ').
translate(s7, '       ').
translate(s6, '      ').
translate(s5, '     ').
translate(s3, '   ').
translate(s2, '  ').
translate(s1, ' ').
translate(s0, '').
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

display_line_1_aux(N):-
    N=0,
    nl.

display_line_1_aux(NumberHexagons):-
    N1 is NumberHexagons - 1,
    translate(t, A),
    translate(s7, SpaceBetweenHex),
    write(A),
    write(SpaceBetweenHex),
    display_line_1_aux(N1).

display_line_1(NumberEmptySpaces, NumberHexagons):-
    generate_empty_space(s7, NumberEmptySpaces),
    display_line_1_aux(NumberHexagons).

display_line_2_aux(N):-
    N = 0,
    nl.

display_line_2_aux(N):-
    N1 is N-1,
    translate(ub, OpenHex),
    translate(s3, SpaceInsideHex),
    translate(db, CloseHex),
    translate(s5, SpaceBetweenHex),
    write(OpenHex),
    write(SpaceInsideHex),
    write(CloseHex),
    write(SpaceBetweenHex),
    display_line_2_aux(N1).

display_line_2(NumberEmptySpaces, NumberHexagons):-
    generate_empty_space(s6, NumberEmptySpaces),
    display_line_2_aux(NumberHexagons).

display_line_3_aux(N):-
    N = 0,
    nl.

display_line_3_aux(N):-
    N1 is N-1,
    translate(t, A),
    translate(ub, OpenHex),
    translate(s5, SpaceInsideHex),
    translate(db, CloseHex),
    write(A),
    write(OpenHex),
    write(SpaceInsideHex),
    write(CloseHex),
    display_line_3_aux(N1).

display_line_3(NumberEmptySpaces, NumberHexagons):-
    generate_empty_space(s2, NumberEmptySpaces),
    display_line_3_aux(NumberHexagons).





display_line_4_aux(N):-
    N = 0,
    nl.

display_line_4_aux(NumberHexagons):-
    N1 is NumberHexagons - 1,
    translate(ub, OpenHex),
    translate(s3, SpaceInsideHex),
    translate(db, CloseHex),
    translate(s5, SpaceInsideHex2),
    write(CloseHex),
    write(SpaceInsideHex2),
    write(OpenHex),
    write(SpaceInsideHex),
    display_line_4_aux(N1).

display_line_4(NumberEmptySpaces, NumberHexagons):-
    generate_empty_space(s1, NumberEmptySpaces),
    translate(ub, OpenHex),
    translate(s3, SpaceInsideHex),
    translate(db, CloseHex),
    write(OpenHex),
    write(SpaceInsideHex),
    display_line_4_aux(NumberHexagons).





display_line_5_aux(N):-
    N = 0,
    nl.

display_line_5_aux(NumberHexagons):-
    N1 is NumberHexagons - 1,
    translate(t, A),
    translate(ub, OpenHex),
    translate(s5, SpaceInsideHex),
    translate(db, CloseHex),
    write(CloseHex),
    write(A),
    write(OpenHex),
    write(SpaceInsideHex),
    display_line_5_aux(N1).

display_line_5(NumberEmptySpaces, NumberHexagons):-
    generate_empty_space(s0, NumberEmptySpaces),
    translate(ub, OpenHex),
    translate(s5, SpaceInsideHex),
    translate(db, CloseHex),
    write(OpenHex),
    write(SpaceInsideHex),
    display_line_5_aux(NumberHexagons).






display_line_6_aux(N):-
    N = 0,
    nl.

display_line_6_aux(NumberHexagons):-
    N1 is NumberHexagons - 1,
    translate(ub, OpenHex),
    translate(s3, SpaceInsideHex),
    translate(db, CloseHex),
    translate(s5, SpaceInsideHex2),
    write(OpenHex),
    write(SpaceInsideHex),
    write(CloseHex),
    write(SpaceInsideHex2),
    display_line_6_aux(N1).

display_line_6(NumberEmptySpaces, NumberHexagons):-
    generate_empty_space(s0, NumberEmptySpaces),
    translate(ub, OpenHex),
    translate(s5, SpaceInsideHex),
    translate(db, CloseHex),
    translate(s5, SpaceInsideHex2),
    write(CloseHex),
    write(SpaceInsideHex),
    display_line_6_aux(NumberHexagons).





display_line_7_aux(N):-
    N = 0,
    nl.

display_line_7_aux(NumberHexagons):-
    N1 is NumberHexagons - 1,
    translate(t, A),
    translate(ub, OpenHex),
    translate(s5, SpaceInsideHex),
    translate(db, CloseHex),
    write(A),
    write(OpenHex),
    write(SpaceInsideHex),
    write(CloseHex),
    display_line_7_aux(N1).

display_line_7(NumberEmptySpaces, NumberHexagons):-
    generate_empty_space(s1, NumberEmptySpaces),
    translate(db, CloseHex),
    write(CloseHex),
    display_line_7_aux(NumberHexagons).





display_line_8_aux(N):-
    N = 0,
    nl.

display_line_8_aux(N):-
    N1 is N-1,
    translate(ub, OpenHex),
    translate(s5, SpaceInsideHex),
    translate(db, CloseHex),
    translate(s3, SpaceInsideHex2),
    write(CloseHex),
    write(SpaceInsideHex),
    write(OpenHex),
    write(SpaceInsideHex2),
    display_line_8_aux(N1).

display_line_8(NumberEmptySpaces, NumberHexagons):-
    generate_empty_space(s5, NumberEmptySpaces),
    display_line_8_aux(NumberHexagons).






display_line_9_aux(N):-
    N = 0,
    nl.

display_line_9_aux(N):-
    N1 is N-1,
    translate(t, A),
    translate(ub, OpenHex),
    translate(db, CloseHex),
    translate(s5, SpaceBetweenHex),
    write(CloseHex),
    write(A),
    write(OpenHex),
    write(SpaceBetweenHex),
    display_line_9_aux(N1).

display_line_9(NumberEmptySpaces, NumberHexagons):-
    generate_empty_space(s6, NumberEmptySpaces),
    display_line_9_aux(NumberHexagons).



display_num_linhas(N):-
    N = 0,
    write('').

display_num_linhas(NumLinhasAdicionais):-
    N1 is NumLinhasAdicionais - 1,
    display_line_4(1, 4),
    display_line_5(1, 4),
    display_line_6(1, 4),
    display_line_7(1, 4),
    display_num_linhas(N1).



display_board:-
    display_line_1(1, 4),
    display_line_2(1, 4),
    display_line_3(1, 4),
    display_num_linhas(10),
    display_line_8(1, 4),
    display_line_9(1, 4).

display:- display_board.