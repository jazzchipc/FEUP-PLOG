:- use_module(library(lists)).

%%% TRANSLATIONS

translate(0, '###').
translate(1, '   ').

%%% DISPLAY LINE SEPARATOR 

display_horizontal_separator(0):- write(+), nl.

display_horizontal_separator(NumOfCols):-
    write(+),
    write(---),
    NumOfCols1 is NumOfCols - 1,
    display_horizontal_separator(NumOfCols1).

%%% DISPLAY ROW

display_row_aux(Row, Counter):-
    length(Row, Counter),   % if counter is equal to the lentgh it is the last element of row

    write('|'),
    nth1(Counter, Row, Cell),
    translate(Cell, ToWrite),
    write(ToWrite), 
    write('|'),
    nl.

display_row_aux(Row, Counter):-
    write('|'),
    nth1(Counter, Row, Cell),
    translate(Cell, ToWrite),
    write(ToWrite),
    Counter1 is Counter + 1,    % next element
    display_row_aux(Row, Counter1).

display_row(Row):-
    display_row_aux(Row, 1).

%%% DISPLAY BOARD

display_board_aux(Board, Counter):-
    length(Board, Counter),     % last row
    nth1(Counter, Board, Row),

    length(Row, NumOfCols),
    display_horizontal_separator(NumOfCols),
    display_row(Row),
    display_horizontal_separator(NumOfCols).


display_board_aux(Board, Counter):-
    nth1(Counter, Board, Row),

    length(Row, NumOfCols),
    display_horizontal_separator(NumOfCols),
    display_row(Row),
    
    Counter1 is Counter + 1,    % next row
    display_board_aux(Board, Counter1).

display_board(Board):-
    display_board_aux(Board, 1).

board(B):-
    B = [
        [0, 0, 1],
        [0, 1, 1],
        [0, 0, 1]
        ].
