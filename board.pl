%%% Case example: [<type>, <owner>, [<listOfShips], <building>]

/* TYPES OF CASE 
h - home system
b - blackhole
c - wormhole
l1 - level one system (1 point)
l2 - level two system (2 points)
l3 - level three system (3 points)
n - nebula (5 points)
*/

/* OWNER
1 - player one
2 - player two
*/

/* LIST OF SHIPS
Player one: ABCD or abcd (damaged)
Player two: WXYZ or wxyz (damaged)
*/

/* BUILDING
t - trade station
c - colony
n - none
*/

/* UTILITIES */

prefix([],Ys).
prefix([X|Xs], [X|Ys]):- prefix(Xs,Ys).

sufix(Xs, Xs).
sufix(Xs, [Y|Ys]):- suffix(Xs, Ys).

sublist(Xs, Ys):- prefix(Xs, Ys).
sublist(Xs, [Y|Ys]):- sublist(Xs,Ys).

length([],0);
length([X|Xs], s(N)):- length(Xs, N).

/* END OF UTILITIES */


/* DISPLAY GAME CASE FUNCTIONS*/ 

%Verify errors example function

isTypeOfCase(C):-
    C == h;
    C == b;
    C == c;
    C == l1;
    C == l2;
    C == l3;
    C == n.

case_example([h, 1, ['A', 'B', 'C', 'D'], n]).

board_example([
    [h, 1, ['A', 'B', 'C', 'D'], n], [], [h, 2, ['W', 'X', 'Y', 'Z'], n]
]).

display_list([]).

display_list([E1|Es]):-
    write(E1),
    display_list(Es).

display_type([C,_,_,_|_]):- write(C).

display_owner([_,O,_,_|_]):- write(O).

display_ships([_,_,S,_|_]):- display_list(S).

display_building([_,_,_,B|_]):- write(B).

display_board_case([]).

display_board_case(C):-
    display_type(C),
    display_owner(C),
    display_ships(C),
    display_building(C).

display_board_test([B1|Bs]):-
    display_board_case(B1),
    display_board_test(Bs).

test_display_board:-
    board_example(B), display_board_test(B).


board([[x, x, x],
        [x, x],
        [x, x, x]]).

% Adicionar coordenadas

translate(s10, '          ').
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
    generate_empty_space(s7, 1),
    generate_empty_space(s10, NumberEmptySpaces),
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
    generate_empty_space(s6, 1),
    generate_empty_space(s10, NumberEmptySpaces),
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
    generate_empty_space(s2, 1),
    generate_empty_space(s10, NumberEmptySpaces),
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
    generate_empty_space(s1, 1),
    generate_empty_space(s10, NumberEmptySpaces),
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
    generate_empty_space(s0, 1),
    generate_empty_space(s10, NumberEmptySpaces),
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
    generate_empty_space(s0, 1),
    generate_empty_space(s10, NumberEmptySpaces),
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
    generate_empty_space(s1, 1),
    generate_empty_space(s10, NumberEmptySpaces),
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
    generate_empty_space(s5, 1),
    generate_empty_space(s10, NumberEmptySpaces),
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
    generate_empty_space(s6, 1),
    generate_empty_space(s10, NumberEmptySpaces),
    display_line_9_aux(NumberHexagons).



display_num_linhas(N):-
    N = 0,
    write('').

display_num_linhas(NumLinhasAdicionais):-
    N1 is NumLinhasAdicionais - 1,
    display_line_4(0, 4),
    display_line_5(0, 4),
    display_line_6(0, 4),
    display_line_7(0, 4),
    display_num_linhas(N1).



display_start_lines:-
    display_line_1(0, 4),
    display_line_2(0, 4),
    display_line_3(0, 4).

display_end_lines:-
    display_line_8(0, 4),
    display_line_9(0, 4).

display_board:-
    display_start_lines,
    display_num_linhas(10),
    display_end_lines.

display:- display_board.