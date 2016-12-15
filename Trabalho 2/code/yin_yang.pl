:- use_module(library(lists)).
:- use_module(library(clpfd)).

/*
0 --> black
1 --> white

LastEvaluatorNumber --> last column of penultima line
*/

test_board_4x4([
    A1, A2, A3, A4,
    B1, B2, B3, B4,
    C1, C2, C3, C4,
    D1, D2, D3, D4
]).

test_board_3x3([
    A1, A2, A3,
    B1, B2, B3,
    C1, C2, C3
]).

test_board_2x2([
    A1, A2,
    B1, B2
]).

yin_yang_auto(Board):-
    test_board_4x4(Board),
    length(Board, Length),
    LengthRowAux is sqrt(Length),
    LengthRow is round(LengthRowAux),
    
    domain(Board, 0, 1),
    setConstrains(Board, 1, Length, LengthRow),
    labeling([], Board),

    display_board(Board, LengthRow, 1).

yin_yang_manual(Board, Length):-
    BoardLength is Length * Length,

    length(Board, BoardLength),
    LengthRowAux is sqrt(BoardLength),
    LengthRow is round(LengthRowAux),
    
    domain(Board, 0, 1),
    setConstrains(Board, 1, BoardLength, LengthRow),
    labeling([], Board),

    display_board(Board, LengthRow, 1).

% cell that ends predicate. Last cell in the board
setConstrains(Board, Length, Length, LengthRow).
setConstrains(Board, LengthRow, Length, LengthRow):-
    % cell that appears in the first row, last column
    NewIndex is LengthRow + 1,

    LeftIndex is LengthRow - 1,
    BelowIndex is LengthRow + LengthRow,

    element(LengthRow, Board, CurrElem),
    element(LeftIndex, Board, LeftElem),
    element(BelowIndex, Board, BelowElem),

    (
        CurrElem #= LeftElem
        #\/ CurrElem #= BelowElem
    ),
    setConstrains(Board, NewIndex, Length, LengthRow).
setConstrains(Board, Index, Length, LengthRow):-
    % cell that appears in the last row, first column
    LastRowFirstColumn is Length - LengthRow + 1,
    Index =:= LastRowFirstColumn,
    NewIndex is Index + 1,

    UpperIndex is Index - LengthRow,
    RightIndex is Index + 1,

    element(Index, Board, CurrElem),
    element(UpperIndex, Board, UpperElem),
    element(RightIndex, Board, RightElem),

    (
        CurrElem #= UpperElem
        #\/ CurrElem #= RightElem
    ),
    setConstrains(Board, NewIndex, Length, LengthRow).
setConstrains(Board, Index, Length, LengthRow):-
    % cells between first and last line in the last column (excluded)
    0 =:= mod(Index, LengthRow),
    NewIndex is Index + 1,

    UpperIndex is Index - LengthRow,
    BelowIndex is Index + LengthRow,
    LeftIndex is Index - 1,

    element(Index, Board, CurrElem),
    element(UpperIndex, Board, UpperElem),
    element(BelowIndex, Board, BelowElem),
    element(LeftIndex, Board, LeftElem),

    (
        CurrElem #= UpperElem
        #\/ CurrElem #= BelowElem
        #\/ CurrElem #= LeftElem
    ),
    setConstrains(Board, NewIndex, Length, LengthRow).
setConstrains(Board, Index, Length, LengthRow):-
    % cells in the last row between first and last columns (excluded)
    LastRowIndex is Length - LengthRow,
    Index > LastRowIndex,
    NewIndex is Index + 1,

    LeftIndex is Index - 1,
    RightIndex is Index + 1,
    UpperIndex is Index - LengthRow,

    element(Index, Board, CurrElem),
    element(LeftIndex, Board, LeftElem),
    element(RightIndex, Board, RightElem),
    element(UpperIndex, Board, UpperElem),

    (
        CurrElem #= LeftElem
        #\/ CurrElem #= RightElem
        #\/ CurrElem #= UpperElem
    ),
    setConstrains(Board, NewIndex, Length, LengthRow).
setConstrains(Board, Index, Length, LengthRow):-
    % intermediate board cells
    element(Index, Board, CurrElem),
    NextIndex is Index + 1,
    element(NextIndex, Board, ElemRight),
    NextRow is Index + LengthRow,
    element(NextRow, Board, ElemBelow),
    NextRowPlus is NextRow + 1,
    element(NextRowPlus, Board, ElemSE),
    
    (
        CurrElem #= ElemRight
        #\/ CurrElem #= ElemBelow
    ),

    nvalue(2, [CurrElem, ElemRight, ElemBelow, ElemSE]),

    NewIndex is Index + 1,
    setConstrains(Board, NewIndex, Length, LengthRow).

display_board([], _, _).
display_board([X|Xs], LengthRow, Index):-
    0 =:= mod(Index, LengthRow),
    !,
    write(X), nl,
    NewIndex is Index + 1,
    display_board(Xs, LengthRow, NewIndex).
display_board([X|Xs], LengthRow, Index):-
    write(X), write(' '),
    NewIndex is Index + 1,
    display_board(Xs, LengthRow, NewIndex).