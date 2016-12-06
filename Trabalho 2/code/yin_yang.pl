:- use_module(library(lists)).
:- use_module(library(clpfd)).

/*
0 --> black
1 --> white

LastEvaluatorNumber --> last column of penultima line
*/

test_board_4x4([
    A1, A2, A3, A4,
    B1, 1, 1, B4,
    C1, C2, 0, C4,
    D1, D2, D3, D4
]).

test_board_3x3([
    A1, A2, A3,
    B1, 1, B3,
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
    length(Board, Length),
    LengthRowAux is sqrt(Length),
    LengthRow is round(LengthRowAux),
    
    domain(Board, 0, 1),
    setConstrains(Board, 1, Length, LengthRow),
    labeling([], Board),

    display_board(Board, LengthRow, 1).

setConstrains(Board, LengthRow, Length, LengthRow):-
    NewIndex is LengthRow + 1,
    setConstrains(Board, NewIndex, Length, LengthRow).
setConstrains(Board, Index, Length, LengthRow):-
    LastEvaluatorNumber is Length - LengthRow,
    Index =:= LastEvaluatorNumber,
    NewIndex is Index + 1,

    UpperIndex is Index - LengthRow,
    BelowIndex is Index + LengthRow,

    element(Index, Board, CurrElem),
    element(UpperIndex, Board, UpperElem),
    element(BelowIndex, Board, BelowElem),

    (
        CurrElem #= UpperElem
        #\/ CurrElem #= BelowElem
    ),
    setConstrains(Board, NewIndex, Length, LengthRow).
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
    
    (
        CurrElem #= ElemRight
        #\/ CurrElem #= ElemBelow
        %#\/ (CurrElem #\= ElemSE #/\ ElemRight #/\ ElemSE #/\ ElemBelow #\= ElemSE)
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