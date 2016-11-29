:- use_module(library(lists)).
:- use_module(library(clpfd)).

make_rows(1, N, Board):- 
    nth1(1, Board, Row),
    length(Row, N).

make_rows(M, N, Board):- 
    M > 1,
    nth1(M, Board, Row),
    length(Row, N),             % each row has N columns
    M1 is M-1,
    make_rows(M1, N, Board).

make_board(NumOfRows, NumOfCols, Board):-
    length(Board, NumOfRows),      
    make_rows(NumOfRows, NumOfCols, Board).

get_cell(Board, Row, Column, Cell):-
    nth1(Row, Board, RowList),
    nth1(Column, RowList, Cell).

get_row(Board, Row, R):-
    nth1(Row, Board, R).

set_domain(Board):-
    get_row(Board, X, Row),
    domain(Row, 0, 1). % either black (0) or white (1)

set_constraints(Board):-
    get_cell(Board, X, Y, Cell),
    Cell #= 1.

set_square_constraint(Board):-  %   no 2x2 group can have cells of a single color
    get_cell(Board, X, Y, Cell).


yin_yang(Board, N):-
    /*Area is N * N,
    length(Board, Area),*/
    Board = [
        A1, A2, A3, A4,
        B1, B2, B3, B4,
        C1, C2, C3, C4,
        D1, D2, D3, D4
    ];
    domain(Board, 1, 2),
    constrain(Board, 0, Board),

    labeling([], Row).    % 0 is white, 1 is black


constrain([X|Xs], Index, L):-
    element(Index, L, NewX),