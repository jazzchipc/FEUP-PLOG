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

/* UTILITIES */

prefix([],Ys).
prefix([X|Xs], [X|Ys]):- prefix(Xs,Ys).

sufix(Xs, Xs).
sufix(Xs, [Y|Ys]):- suffix(Xs, Ys).

sublist(Xs, Ys):- prefix(Xs, Ys).
sublist(Xs, [Y|Ys]):- sublist(Xs,Ys).

length([],0);
length([X|Xs], s(N)):- length(Xs, N).

first(X, [X|_]).

last(X, [X]).
last(X, [_|Z]) :- last(X, Z).

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
    [h, 1, ['A', 'B', 'C', 'D'], n], x, [h, 2, ['W', 'X', 'Y', 'Z'], n]
]).

display_list([]).

display_list([E1|Es]):-
    write(E1),
    display_list(Es).

getLine(X, [_|Xs], N):-
    N == 1 ->
        write('');
    N == 0 ->
        N1 is N+1,
        getline(X, Xs, N1).
    


/* These next functions add space to fill the hexagon awhile displaying information */

display_type(X):-
    translate(X, WHY),      % WHY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    translate(s3, SpaceBetweenHex),
    write(SpaceBetweenHex).

display_type([C,_,_,_|_]):-
    (C == 'h'; C == 'b'; C == 'c'; C == 'n') ->
        translate(s1, S),
        write(S),
        write(C),
        write(S);
    (C == 'l1'; C == 'l2'; C == 'l3') ->
        translate(s1, S),
        write(S),
        write(C).

display_owner(X):-
    translate(X, FreeHouse),
    write(FreeHouse).

display_owner([_,O,_,_|_]):- 
    write(O).

display_ships(X):-
    translate(X, WHY),      % WHY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    translate(s5, SpaceBetweenHex),
    write(SpaceBetweenHex).

display_ships([_,_,S,_|_]):- 
    length(S, L),
    L1 is 5-L,
    generate_empty_space(s1, L1),
    display_list(S).

display_building([_,_,_,B|_]):- 
    translate(s1, S),
    write(S),
    write(B),
    write(S).

empty_case(C):-
    C==x.

display_board_case([]).

display_board_case(C):-
    display_type(C),
    display_owner(C),
    display_ships(C),
    display_building(C).

/*** WRITE FUNCTIONS ***/

/* Write whole element */
write_element_coord([B1|Bs], X, Y):-
    Y == 0 -> write_element(B1, X);
    Y > 0 -> Y1 is Y-1, write_element_coord(Bs, X, Y1).

write_element([R1|Rs], X):-
    X == 0 -> display_board_case(R1);
    X > 0 -> X1 is X-1, write_element(Rs, X1).

/* Write element type */
write_type_coord([B1|Bs], X, Y):-
    Y == 0 -> write_element_type(B1, X);
    Y > 0 -> Y1 is Y-1, write_type_coord(Bs, X, Y1).

write_element_type([R1|Rs], X):-
    X == 0 -> display_type(R1);
    X > 0 -> X1 is X-1, write_element_type(Rs, X1).

/* Write element owner */

write_owner_coord([B1|Bs], X, Y):-
    Y == 0 -> write_element_owner(B1, X);
    Y > 0 -> Y1 is Y-1, write_owner_coord(Bs, X, Y1).

write_element_owner([R1|Rs], X):-
    X == 0 -> display_owner(R1);
    X > 0 -> X1 is X-1, write_element_owner(Rs, X1).

/* Write element ships*/

write_ships_coord([B1|Bs], X, Y):-
    Y == 0 -> write_element_ships(B1, X);
    Y > 0 -> Y1 is Y-1, write_ships_coord(Bs, X, Y1).

write_element_ships([R1|Rs], X):-
    X == 0 -> display_ships(R1);
    X > 0 -> X1 is X-1, write_element_ships(Rs, X1).

/* Write element building */

write_building_coord([B1|Bs], X, Y):-
    Y == 0 -> write_element_building(B1, X);
    Y > 0 -> Y1 is Y-1, write_building_coord(Bs, X, Y1).

write_element_building([R1|Rs], X):-
    X == 0 -> display_building(R1);
    X > 0 -> X1 is X-1, write_element_building(Rs, X1).

/*** END OF WRITE FUNCTIONS ***/

display_board_test([B1|Bs]):-
    display_board_case(B1),
    display_board_test(Bs).

test_display_board:-
    board_example(B), display_board_test(B).

/* Each element of the board is a line. Each element within the line is a piece.*/
board([
    [[l1, 1, ['A', 'D'], n], [b, 1, ['A', 'B', 'C', 'D'], n], x],
    [[h, 1, ['A', 'B', 'C', 'D'], n], x, x],
    [x, x, [h, 2, ['W', 'X', 'Y', 'Z'], n]]]
    ).

generate_empty_space(Spaces, NumberOfTimes):-
    NumberOfTimes = 0,
    write('').

generate_empty_space(Spaces, NumberOfTimes):-
    N1 is NumberOfTimes - 1,
    translate(Spaces, EmptySpace),
    write(EmptySpace),
    generate_empty_space(Spaces, N1).

display_line_1_aux(NumberHexagons):-
    NumberHexagons > 0 ->
        N1 is NumberHexagons - 1,
        translate(t, A),
        translate(s7, SpaceBetweenHex),
        write(A),
        write(SpaceBetweenHex),
        display_line_1_aux(N1);
    NumberHexagons == 0 -> nl.

display_line_1(NumberEmptySpaces, NumberHexagons):-
    generate_empty_space(s7, 1),
    generate_empty_space(s10, NumberEmptySpaces),
    display_line_1_aux(NumberHexagons).

getCurrentPiece([X|Xs], X, Xs).

display_line_2_aux(NumberHexagons, FirstRow):-
    NumberHexagons > 0 ->
        N1 is NumberHexagons-1,
        getCurrentPiece(FirstRow, CurrentPiece, RemainingPieces),
        translate(ub, OpenHex),
        translate(s3, SpaceInsideHex),
        translate(db, CloseHex),
        translate(s5, SpaceBetweenHex),
        write(OpenHex),
        display_type(CurrentPiece),
        % write(SpaceInsideHex),
        write(CloseHex),
        write(SpaceBetweenHex),
        display_line_2_aux(N1, RemainingPieces);
    NumberHexagons == 0 -> nl.

display_line_2(NumberEmptySpaces, NumberHexagons, FirstRow):-
    generate_empty_space(s6, 1),
    generate_empty_space(s10, NumberEmptySpaces),
    display_line_2_aux(NumberHexagons, FirstRow).

display_line_3_aux(NumberHexagons, FirstRow):-
    NumberHexagons > 0 ->
        N1 is NumberHexagons-1,
        getCurrentPiece(FirstRow, CurrentPiece, RemainingPieces),
        translate(t, A),
        translate(ub, OpenHex),
        translate(s5, SpaceInsideHex),
        translate(db, CloseHex),
        write(A),
        write(OpenHex),
        display_ships(CurrentPiece),
        % write(SpaceInsideHex),
        write(CloseHex),
        display_line_3_aux(N1, RemainingPieces);
    NumberHexagons == 0 -> nl.

display_line_3(NumberEmptySpaces, NumberHexagons, FirstRow):-
    generate_empty_space(s2, 1),
    generate_empty_space(s10, NumberEmptySpaces),
    display_line_3_aux(NumberHexagons, FirstRow).

display_line_4_aux(NumberHexagons, MatrixLine):-
    NumberHexagons > 0 ->
        N1 is NumberHexagons - 1,
        getLine(Current),
        translate(ub, OpenHex),
        translate(s3, SpaceInsideHex),
        translate(db, CloseHex),
        translate(s5, SpaceInsideHex2),
        write(CloseHex),
        write(SpaceInsideHex2),
        write(OpenHex),
        write(SpaceInsideHex),
        display_line_4_aux(N1);
    NumberHexagons == 0 -> nl.

display_line_4(NumberEmptySpaces, NumberHexagons):-
    generate_empty_space(s1, 1),
    generate_empty_space(s10, NumberEmptySpaces),
    translate(ub, OpenHex),
    translate(s3, SpaceInsideHex),
    translate(db, CloseHex),
    write(OpenHex),
    write(SpaceInsideHex),
    display_line_4_aux(NumberHexagons).

display_line_5_aux(NumberHexagons):-
    NumberHexagons > 0 ->
        N1 is NumberHexagons - 1,
        translate(t, A),
        translate(ub, OpenHex),
        translate(s5, SpaceInsideHex),
        translate(db, CloseHex),
        write(CloseHex),
        write(A),
        write(OpenHex),
        write(SpaceInsideHex),
        display_line_5_aux(N1);
    NumberHexagons == 0 -> nl.

display_line_5(NumberEmptySpaces, NumberHexagons):-
    generate_empty_space(s0, 1),
    generate_empty_space(s10, NumberEmptySpaces),
    translate(ub, OpenHex),
    translate(s5, SpaceInsideHex),
    translate(db, CloseHex),
    write(OpenHex),
    write(SpaceInsideHex),
    display_line_5_aux(NumberHexagons).

display_line_6_aux(NumberHexagons):-
    NumberHexagons > 0 ->
        N1 is NumberHexagons - 1,
        translate(ub, OpenHex),
        translate(s3, SpaceInsideHex),
        translate(db, CloseHex),
        translate(s5, SpaceInsideHex2),
        write(OpenHex),
        write(SpaceInsideHex),
        write(CloseHex),
        write(SpaceInsideHex2),
        display_line_6_aux(N1);
    NumberHexagons == 0 -> nl.

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

display_line_7_aux(NumberHexagons):-
    NumberHexagons > 0 ->
        N1 is NumberHexagons - 1,
        translate(t, A),
        translate(ub, OpenHex),
        translate(s5, SpaceInsideHex),
        translate(db, CloseHex),
        write(A),
        write(OpenHex),
        write(SpaceInsideHex),
        write(CloseHex),
        display_line_7_aux(N1);
    NumberHexagons == 0 -> nl.

display_line_7(NumberEmptySpaces, NumberHexagons):-
    generate_empty_space(s1, 1),
    generate_empty_space(s10, NumberEmptySpaces),
    translate(db, CloseHex),
    write(CloseHex),
    display_line_7_aux(NumberHexagons).

display_line_8_aux(NumberHexagons):-
    NumberHexagons > 0 ->
        N1 is NumberHexagons-1,
        translate(ub, OpenHex),
        translate(s5, SpaceInsideHex),
        translate(db, CloseHex),
        translate(s3, SpaceInsideHex2),
        write(CloseHex),
        write(SpaceInsideHex),
        write(OpenHex),
        write(SpaceInsideHex2),
        display_line_8_aux(N1);
    NumberHexagons == 0 -> nl.

display_line_8(NumberEmptySpaces, NumberHexagons):-
    generate_empty_space(s5, 1),
    generate_empty_space(s10, NumberEmptySpaces),
    display_line_8_aux(NumberHexagons).

display_line_9_aux(NumberHexagons):-
    NumberHexagons > 0 ->
        N1 is NumberHexagons-1,
        translate(t, A),
        translate(ub, OpenHex),
        translate(db, CloseHex),
        translate(s5, SpaceBetweenHex),
        write(CloseHex),
        write(A),
        write(OpenHex),
        write(SpaceBetweenHex),
        display_line_9_aux(N1);
    NumberHexagons == 0 -> nl.

display_line_9(NumberEmptySpaces, NumberHexagons):-
    generate_empty_space(s6, 1),
    generate_empty_space(s10, NumberEmptySpaces),
    display_line_9_aux(NumberHexagons).

display_num_linhas(NumLinhasAdicionais, NumOfCols):-
    NumLinhasAdicionais > 0 ->
        N1 is NumLinhasAdicionais - 1,
        display_line_4(0, NumOfCols),
        display_line_5(0, NumOfCols),
        display_line_6(0, NumOfCols),
        display_line_7(0, NumOfCols),
        display_num_linhas(N1);
    NumLinhasAdicionais == 0 -> write('').

display_start_lines(NumOfCols, FirstRow):-
    display_line_1(0, NumOfCols),
    display_line_2(0, NumOfCols, FirstRow),
    display_line_3(0, NumOfCols, FirstRow).

display_end_lines(NumOfCols):-
    display_line_8(0, NumOfCols),
    display_line_9(0, NumOfCols).

display_board:-
    board(B),
    length(B, NumOfRows), 

    first(FirstRow, B),
    length(FirstRow, NumOfElementsFirstRow),

    last(LastRow, B),
    length(LastRow, NumOfElementsLastRow),

    display_start_lines(NumOfElementsFirstRow, FirstRow),
    display_num_linhas(NumOfRows//2 , NumOfElementsFirstRow),
    display_end_lines(NumOfElementsLastRow).

display:- display_board.