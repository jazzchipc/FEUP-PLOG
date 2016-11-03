%%% Case example: [<type>, <owner>, [<listOfShips], <building>]

/* TYPES OF CASE 
h - home system
b - blackhole
w - wormhole
l1 - level one system (1 point)
l2 - level two system (2 points)
l3 - level three system (3 points)
n - nebula (5 points)
*/

/* OWNER
1 - player one
2 - player two
f - free
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
translate(t1, '_').
translate(t2, '__').
translate(t3, '___').
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

getLine(X, [X|_], 0).

getLine(X, [_|L], LineToSend):-
    getLine(X, L, K1), LineToSend is K1 + 1.

/* These next functions add space to fill the hexagon awhile displaying information */

display_type(X):-
    translate(X, WHY),      % WHY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    translate(s3, SpaceBetweenHex),
    write(SpaceBetweenHex).

display_type([C,_,_,_|_]):-
    (C == 'h'; C == 'b'; C == 'n'; C == 'w') ->
        translate(s1, S),
        write(S),
        write(C),
        write(S);
    (C == 'l1'; C == 'l2'; C == 'l3') ->
        translate(s1, S),
        write(S),
        write(C).

display_owner(X):-
    translate(X, WHY),      % WHY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    translate(s5, SpaceBetweenHex),
    write(SpaceBetweenHex).

display_owner([_,O,_,_|_]):- 
    translate(s2, SpaceBetweenHex),
    write(SpaceBetweenHex),
    write(O),
    write(SpaceBetweenHex).

display_ships(X):-
    translate(X, WHY),      % WHY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    translate(s5, SpaceBetweenHex),
    write(SpaceBetweenHex).

display_ships([_,_,S,_|_]):- 
    length(S, L),
    L1 is 5-L,
    generate_empty_space(s1, L1),
    display_list(S).

display_building(X):-
    translate(X, WHY),      % WHY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    translate(t2, SpaceBetweenHex),
    write(SpaceBetweenHex).

display_building([_,_,_,B|_]):- 
    translate(t1, S),
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

/*** BEGIN OF DISPLAY OF BOARD ***/

%% Wormhole displays

display_wormhole_line_2:-
    write(@@@).

display_wormhole_line_3:-
    write(@),
    translate(s3, S),
    write(S),
    write(@).

display_wormhole_line_4:-
    display_wormhole_line_3.

display_wormhole_line_5:-
    display_wormhole_line_2.

display_wormhole_test:-
    display_wormhole_line_2, nl,
    display_wormhole_line_3, nl,
    display_wormhole_line_4, nl,
    display_wormhole_line_5.

%% Black hole displays

display_blackhole_line_2:-
    write(###).

display_blackhole_line_3:-
    write(#####).

display_blackhole_line_4:-
    display_blackhole_line_3.

display_blackhole_line_5:-
    display_blackhole_line_2.

display_blackhole_test:-
    display_blackhole_line_2, nl,
    display_blackhole_line_3, nl,
    display_blackhole_line_4, nl,
    display_blackhole_line_5.

display_board_test([B1|Bs]):-
    display_board_case(B1),
    display_board_test(Bs).

test_display_board:-
    board_example(B), display_board_test(B).

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
        translate(t3, A),
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
        translate(t3, A),
        translate(ub, OpenHex),
        translate(s5, SpaceInsideHex),
        translate(db, CloseHex),
        write(A),
        write(OpenHex),
        display_ships(CurrentPiece),
        write(CloseHex),
        display_line_3_aux(N1, RemainingPieces);
    NumberHexagons == 0 -> nl.

display_line_3(NumberEmptySpaces, NumberHexagons, FirstRow):-
    generate_empty_space(s2, 1),
    generate_empty_space(s10, NumberEmptySpaces),
    display_line_3_aux(NumberHexagons, FirstRow).

display_line_4_aux(NumberHexagons, UpperMatrixLine, MiddleMatrixLine):-
    NumberHexagons > 0 ->
        N1 is NumberHexagons - 1,
        getCurrentPiece(UpperMatrixLine, CurrentUpperPiece, RemainingUpperPieces),
        getCurrentPiece(MiddleMatrixLine, CurrentMiddlePiece, RemainingMiddlePieces),
        translate(ub, OpenHex),
        translate(s3, SpaceInsideHex),
        translate(db, CloseHex),
        translate(s5, SpaceInsideHex2),
        display_type(CurrentMiddlePiece),
        write(CloseHex),
        display_owner(CurrentUpperPiece),
        write(OpenHex),
        display_line_4_aux(N1, RemainingUpperPieces, RemainingMiddlePieces);
    NumberHexagons == 0 -> nl.

display_line_4(NumberEmptySpaces, NumberHexagons, UpperMatrixLine, MiddleMatrixLine):-
    generate_empty_space(s1, 1),
    generate_empty_space(s10, NumberEmptySpaces),
    translate(ub, OpenHex),
    translate(s3, SpaceInsideHex),
    translate(db, CloseHex),
    write(OpenHex),
    display_line_4_aux(NumberHexagons, UpperMatrixLine, MiddleMatrixLine).

display_line_5_aux(NumberHexagons, UpperMatrixLine, MiddleMatrixLine):-
    NumberHexagons > 0 ->
        N1 is NumberHexagons - 1,
        getCurrentPiece(UpperMatrixLine, CurrentUpperPiece, RemainingUpperPieces),
        getCurrentPiece(MiddleMatrixLine, CurrentMiddlePiece, RemainingMiddlePieces),
        translate(t1, A),
        translate(ub, OpenHex),
        translate(s5, SpaceInsideHex),
        translate(db, CloseHex),
        display_ships(CurrentMiddlePiece),
        write(CloseHex),
        write(A),
        display_building(CurrentUpperPiece),
        write(OpenHex),
        display_line_5_aux(N1, RemainingUpperPieces, RemainingMiddlePieces);
    NumberHexagons == 0 -> nl.

display_line_5(NumberEmptySpaces, NumberHexagons, UpperMatrixLine, MiddleMatrixLine):-
    generate_empty_space(s0, 1),
    generate_empty_space(s10, NumberEmptySpaces),
    translate(ub, OpenHex),
    translate(s5, SpaceInsideHex),
    translate(db, CloseHex),
    write(OpenHex),
    display_line_5_aux(NumberHexagons, UpperMatrixLine, MiddleMatrixLine).

display_line_6_aux(NumberHexagons, MiddleMatrixLine, LowerMatrixLine):-
    NumberHexagons > 0 ->
        N1 is NumberHexagons - 1,
        getCurrentPiece(MiddleMatrixLine, CurrentMiddlePiece, RemainingMiddlePieces),
        getCurrentPiece(LowerMatrixLine, CurrentLowerPiece, RemainingLowerPiece),        
        translate(ub, OpenHex),
        translate(s3, SpaceInsideHex),
        translate(db, CloseHex),
        translate(s5, SpaceInsideHex2),
        display_owner(CurrentMiddlePiece),
        write(OpenHex),
        display_type(CurrentLowerPiece),
        write(CloseHex),
        display_line_6_aux(N1, RemainingMiddlePieces, RemainingLowerPiece);
    NumberHexagons == 0 -> nl.

display_line_6(NumberEmptySpaces, NumberHexagons, MiddleMatrixLine, LowerMatrixLine):-
    generate_empty_space(s0, 1),
    generate_empty_space(s10, NumberEmptySpaces),
    translate(ub, OpenHex),
    translate(s5, SpaceInsideHex),
    translate(db, CloseHex),
    translate(s5, SpaceInsideHex2),
    write(CloseHex),
    display_line_6_aux(NumberHexagons, MiddleMatrixLine, LowerMatrixLine).

display_line_7_aux(NumberHexagons, MiddleMatrixLine, LowerMatrixLine):-
    NumberHexagons > 0 ->
        N1 is NumberHexagons - 1,
        getCurrentPiece(MiddleMatrixLine, CurrentMiddlePiece, RemainingMiddlePieces),
        getCurrentPiece(LowerMatrixLine, CurrentLowerPiece, RemainingLowerPiece),
        translate(t1, A),
        translate(ub, OpenHex),
        translate(s5, SpaceInsideHex),
        translate(db, CloseHex),
        write(A),
        display_building(CurrentMiddlePiece),
        write(OpenHex),
        display_ships(CurrentLowerPiece),
        write(CloseHex),
        display_line_7_aux(N1, RemainingMiddlePieces, RemainingLowerPiece);
    NumberHexagons == 0 -> nl.

display_line_7(NumberEmptySpaces, NumberHexagons, MiddleMatrixLine, LowerMatrixLine):-
    generate_empty_space(s1, 1),
    generate_empty_space(s10, NumberEmptySpaces),
    translate(db, CloseHex),
    write(CloseHex),
    display_line_7_aux(NumberHexagons, MiddleMatrixLine, LowerMatrixLine).

display_line_8_aux(NumberHexagons, LastRow):-
    NumberHexagons > 0 ->
        N1 is NumberHexagons-1,
        getCurrentPiece(LastRow, CurrentPiece, RemainingPieces),
        translate(ub, OpenHex),
        translate(s5, SpaceInsideHex),
        translate(db, CloseHex),
        translate(s3, SpaceInsideHex2),
        write(CloseHex),
        display_owner(CurrentPiece),
        write(OpenHex),
        write(SpaceInsideHex2),
        display_line_8_aux(N1, RemainingPieces);
    NumberHexagons == 0 -> nl.

display_line_8(NumberEmptySpaces, NumberHexagons, LastRow):-
    generate_empty_space(s5, 1),
    generate_empty_space(s10, NumberEmptySpaces),
    display_line_8_aux(NumberHexagons, LastRow).

display_line_9_aux(NumberHexagons, LastRow):-
    NumberHexagons > 0 ->
        N1 is NumberHexagons-1,
        getCurrentPiece(LastRow, CurrentPiece, RemainingPieces),
        translate(t1, A),
        translate(ub, OpenHex),
        translate(db, CloseHex),
        translate(s5, SpaceBetweenHex),
        write(CloseHex),
        write(A),
        display_building(CurrentPiece),
        write(OpenHex),
        write(SpaceBetweenHex),
        display_line_9_aux(N1, RemainingPieces);
    NumberHexagons == 0 -> nl.

display_line_9(NumberEmptySpaces, NumberHexagons, LastRow):-
    generate_empty_space(s6, 1),
    generate_empty_space(s10, NumberEmptySpaces),
    display_line_9_aux(NumberHexagons, LastRow).

display_num_linhas(NumLinhasAdicionais, NumOfCols, MatrixLineToStart, Board):-
    NumLinhasAdicionais > 0 ->
        N1 is NumLinhasAdicionais - 1,
        HexMiddleLine is MatrixLineToStart+1,
        HexLowerLine is MatrixLineToStart+2,
        getLine(UpperMatrixLine, Board, MatrixLineToStart),
        getLine(MiddleMatrixLine, Board, HexMiddleLine),
        getLine(LowerMatrixLine, Board, HexLowerLine),
        display_line_4(0, NumOfCols, UpperMatrixLine, MiddleMatrixLine),
        display_line_5(0, NumOfCols, UpperMatrixLine, MiddleMatrixLine),
        display_line_6(0, NumOfCols, MiddleMatrixLine, LowerMatrixLine),
        display_line_7(0, NumOfCols, MiddleMatrixLine, LowerMatrixLine),
        display_num_linhas(N1, NumOfCols, HexLowerLine, Board);
    NumLinhasAdicionais == 0.

display_start_lines(NumOfCols, FirstRow):-
    display_line_1(0, NumOfCols),
    display_line_2(0, NumOfCols, FirstRow),
    display_line_3(0, NumOfCols, FirstRow).

display_end_lines(NumOfCols, LastRow):-
    display_line_8(0, NumOfCols, LastRow),
    display_line_9(0, NumOfCols, LastRow).

display_board:-
    board(B),
    length(B, NumOfRows),

    1 is mod(NumOfRows, 2), /**** Until we can work with even rows ****/

    first(FirstRow, B),
    length(FirstRow, NumOfElementsFirstRow),

    last(LastRow, B),
    length(LastRow, NumOfElementsLastRow),

    display_start_lines(NumOfElementsFirstRow, FirstRow),
    display_num_linhas(NumOfRows//2 , NumOfElementsFirstRow, 0, B),
    display_end_lines(NumOfElementsLastRow, LastRow).

display:- display_board.

/*** END OF DISPLAY OF BOARD ***/

/**********
*  BOARD  *
***********/

/* Each element of the board is a line. Each element within the line is a piece.*/
initial_board([
    [[l2, f, [], n], [l2, f, [], n], [w, f, [], n]],
    [[l1, f, [], n], [l2, f, [], n], [l2, f, [], n]],
    [[h, 1, ['A', 'B', 'C', 'D'], n], [l2, f, [], n], [l2, f, [], n]],
    [[l3, f, [], n], [n, f, [], n], [h, 2, ['W', 'X', 'Y', 'Z'], n]],
    [[b, f, [], n], [w, f, [], n], [b, f, [], n]],
    [[l3, f, [], n], [n, f, [], n], [l1, f, [], n]],
    [[l1, f, [], n], [l2, f, [], n], [l2, f, [], n]]
    ]
    ).

mid_board([
    [[l2, f, [], n], [l2, f, [], n], [w, f, [], n]],
    [[l1, f, [], n], [l2, f, [], n], [l2, 1, [], t]],
    [[h, 1, ['C', 'D'], n], [l2, 1, ['A'], c], [l2, 2, ['W'], c]],
    [[l3, f, [], n], [n, 1, ['B'], t], [h, 2, ['Y', 'Z'], n]],
    [[b, f, [], n], [w, f, [], n], [b, f, [], n]],
    [[l3, f, [], n], [n, f, [], n], [l1, 2, ['X'], c]],
    [[l1, f, [], n], [l2, f, [], n], [l2, f, [], n]]
    ]
    ).

board([
    [[l2, 1, ['C'], n], [l2, f, [], n], [w, f, [], n]],
    [[l1, f, [], n], [l2, f, [], n], [l2, 1, [], t]],
    [[h, 1, ['D'], n], [l2, 1, ['A'], c], [l2, 2, ['W'], c]],
    [[l3, 1, [], t], [n, 1, ['B'], t], [h, 2, ['Y', 'Z'], n]],
    [[b, f, [], n], [w, f, [], n], [b, f, [], n]],
    [[l3, 1, ['D'], t], [n, 2, [], t], [l1, 2, [], c]],
    [[l1, 2, ['X'], c], [l2, 2, [], c], [l2, f, [], n]]
    ]
    ).