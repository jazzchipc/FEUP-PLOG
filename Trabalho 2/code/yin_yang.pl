:- use_module(library(lists)).
:- use_module(library(clpfd)).

/*
0 --> black
1 --> white
*/

initial_board([
    A1, A2, A3, A4,
    B1, B2, B3, B4,
    C1, C2, C3, C4,
    D1, D2, D3, D4
]).

display:-
    initial_board(Board),
    domain(Board, 0, 1),

    setConstrains(Board, 1),

    labeling([], Board).

setConstrains([_, _, _, _, _], Index).
setConstrains(Board, Index):-
    0 =:= mod(Index, 2),
    NewIndex is Index + 1,
    setConstrains(Board, NewIndex).
setConstrains(Board, Index):-
    element(Index, Board, CurrElem),
    NextIndex is Index + 1,
    element(NextIndex, Board, ElemRight),
    NextRow is Index + 4,
    element(NextRow, Board, ElemBelow),
    NextRowPlus is NextRow + 1,
    element(NextRowPlus, Board, ElemSE),
    
    (CurrElem #= ElemRight #\/ CurrElem #= ElemBelow #\/ (CurrElem #\= ElemRight #/\ CurrElem #\= ElemBelow)),
    nvalue(2, [CurrElem, ElemRight, ElemBelow, ElemSE]),

    NewIndex is Index + 1,
    setConstrains(Board, NewIndex).