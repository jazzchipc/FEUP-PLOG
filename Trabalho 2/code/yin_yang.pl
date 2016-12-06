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

solve(Board):-
    test_board_4x4(Board),

    length(Board, Length),
    LengthRowAux is sqrt(Length),
    LengthRow is round(LengthRowAux),
    
    domain(Board, 0, 1),
    setConstrains(Board, 1, Length, LengthRow),
    labeling([], Board),
    
    display_board(Board, LengthRow, 1).

setConstrains(Board, Index, Length, LengthRow):-
    Aux is Length - LengthRow,
    LastEvaluatorNumber is Aux - 1,
    Index =:= LastEvaluatorNumber.
setConstrains(Board, Index, Length, LengthRow):-
    0 =:= mod(Index, LengthRow),
    NewIndex is Index + 1,
    setConstrains(Board, NewIndex, Length, LengthRow).
setConstrains(Board, Index, Length, LengthRow):-
    element(Index, Board, CurrElem),
    NextIndex is Index + 1,
    element(NextIndex, Board, ElemRight),
    NextRow is Index + LengthRow,
    element(NextRow, Board, ElemBelow),
    NextRowPlus is NextRow + 1,
    element(NextRowPlus, Board, ElemSE),
    
    (CurrElem #= ElemRight #\/ CurrElem #= ElemBelow #\/ (CurrElem #\= ElemRight #/\ CurrElem #\= ElemBelow)),
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