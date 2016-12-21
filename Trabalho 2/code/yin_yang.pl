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
    setConstrains(Board, 1, BoardLength, LengthRow, NList),
    count(1, NList, #<, 5),
    labeling([], Board),
    write(NList), nl,

    display_board(Board, LengthRow, 1).

% cell that ends predicate. Last cell in the board
setConstrains(Board, Length, Length, LengthRow, [N]):-
    % cell that appears in the last row, last column (X = Last, Y = Last)
    LeftIndex is Length - 1,
    UpperIndex is Length - LengthRow,

    element(Length, Board, CurrElem),
    element(LeftIndex, Board, LeftElem),
    element(UpperIndex, Board, UpperElem),


    (
        CurrElem #= LeftElem #<=>B,
        CurrElem #= UpperElem #<=>C,
        N #= B+C,
        N #>= 1 
    ).

setConstrains(Board, 1, Length, LengthRow, [N|Ns]):-
    % cell that appears in the first row, first column (X = 0, Y = 0)
    NewIndex is 1 + 1,

    RightIndex is 1 + 1,
    BelowIndex is 1 + LengthRow,
    SEIndex is BelowIndex + 1,

    element(1, Board, CurrElem),
    element(RightIndex, Board, RightElem),
    element(BelowIndex, Board, BelowElem),
    element(SEIndex, Board, SEElem),

    (
        CurrElem #= RightElem #<=>B,
        CurrElem #= BelowElem #<=>C,
        N #= B+C,
        N #>= 1 
    ),

    % special case for 2x2 boards
    nvalue(2, [CurrElem, RightElem, BelowElem, SEElem]),

    setConstrains(Board, NewIndex, Length, LengthRow, Ns).

setConstrains(Board, LengthRow, Length, LengthRow, [N|Ns]):-
    % cell that appears in the first row, last column (X = Last, Y = 0)
    NewIndex is LengthRow + 1,

    LeftIndex is LengthRow - 1,
    BelowIndex is LengthRow + LengthRow,

    element(LengthRow, Board, CurrElem),
    element(LeftIndex, Board, LeftElem),
    element(BelowIndex, Board, BelowElem),

    (
        CurrElem #= LeftElem #<=>B,
        CurrElem #= BelowElem #<=>C,
        N #= B+C,
        N #>= 1 
    ),
    setConstrains(Board, NewIndex, Length, LengthRow, Ns).

setConstrains(Board, Index, Length, LengthRow, [N|Ns]):-
    % cell that appears in the last row, first column (X = 0, Y = Last)
    LastRowFirstColumn is Length - LengthRow + 1,
    Index =:= LastRowFirstColumn,
    NewIndex is Index + 1,

    UpperIndex is Index - LengthRow,
    RightIndex is Index + 1,

    element(Index, Board, CurrElem),
    element(UpperIndex, Board, UpperElem),
    element(RightIndex, Board, RightElem),

    (
        CurrElem #= UpperElem #<=>B,
        CurrElem #= RightElem #<=>C,
        N #= B+C,
        N #>= 1
    ),
    setConstrains(Board, NewIndex, Length, LengthRow, Ns).

setConstrains(Board, Index, Length, LengthRow, [N|Ns]):-
    % cells between first and last line in the last column (excluded) (X = Last, Y = [1, Last - 1]) (Right side)
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
        CurrElem #= UpperElem #<=> B,
        CurrElem #= BelowElem #<=> C,
        CurrElem #= LeftElem #<=> D,
        N #= B + C + D,
        N #>= 1
    ),
    setConstrains(Board, NewIndex, Length, LengthRow, Ns).

setConstrains(Board, Index, Length, LengthRow, [N|Ns]):-
    % cells in the last row between first and last columns (excluded) (X = [1, Last - 1], Y = Last) (Below Side)
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
        CurrElem #= LeftElem #<=> B, 
        CurrElem #= RightElem #<=> C,
        CurrElem #= UpperElem #<=> D,
        N #= B + C + D,
        N #>= 1
    ),
    setConstrains(Board, NewIndex, Length, LengthRow, Ns).

setConstrains(Board, Index, Length, LengthRow, [N|Ns]):-
    % cells in the first column between first and last rows (excluded) (X = 0, Y = [1, Last - 1]) (Left Side)
    CheckIndex is Index - 1,
    0 =:= mod(CheckIndex, LengthRow),
    NewIndex is Index + 1,

    RightIndex is Index + 1,
    UpperIndex is Index - LengthRow,
    BelowIndex is Index + LengthRow,

    element(Index, Board, CurrElem),
    element(RightIndex, Board, RightElem),
    element(UpperIndex, Board, UpperElem),
    element(BelowIndex, Board, BelowElem),

    (
        CurrElem #= UpperElem #<=> B,
        CurrElem #= RightElem #<=> C,
        CurrElem #= BelowElem #<=> D,
        N #= B + C + D,
        N #>= 1
    ),
    setConstrains(Board, NewIndex, Length, LengthRow, Ns).

setConstrains(Board, Index, Length, LengthRow, [N|Ns]):-
    % cells in the first line between first and last columns (excluded) (X = [1, Last - 1], Y = 0) (Upper Side)
    Index < LengthRow,
    NewIndex is Index + 1,

    LeftIndex is Index - 1,
    BelowIndex is Index + LengthRow,
    RightIndex is Index + 1,

    element(Index, Board, CurrElem),
    element(LeftIndex, Board, LeftElem),
    element(BelowIndex, Board, BelowElem),
    element(RightIndex, Board, RightElem),

    (
        CurrElem #= LeftElem #<=> B,
        CurrElem #= BelowElem #<=> C,
        CurrElem #= RightElem #<=> D,
        N #= B + C + D,
        N #>= 1
    ),
    setConstrains(Board, NewIndex, Length, LengthRow, Ns).

setConstrains(Board, Index, Length, LengthRow, [N|Ns]):-
    NewIndex is Index + 1,

    UpperIndex is Index - LengthRow,
    RightIndex is Index + 1,
    BelowIndex is Index + LengthRow,
    LeftIndex is Index - 1,

    NWIndex is UpperIndex - 1,
    NEIndex is UpperIndex + 1,
    SWIndex is BelowIndex - 1,
    SEIndex is BelowIndex + 1,

    element(Index, Board, CurrElem),
    element(UpperIndex, Board, UpperElem),
    element(RightIndex, Board, RightElem),
    element(BelowIndex, Board, BelowElem),
    element(LeftIndex, Board, LeftElem),

    element(NWIndex, Board, NWElem),
    element(NEIndex, Board, NEElem),
    element(SWIndex, Board, SWElem),
    element(SEIndex, Board, SEElem),


    (
        CurrElem #= UpperElem #<=> B,
        CurrElem #= RightElem #<=> C,
        CurrElem #= BelowElem #<=> D,
        CurrElem #= LeftElem #<=> E,
        N #= B + C + D + E,
        N #>= 1
    ),

    % top left
    nvalue(2, [CurrElem, UpperElem, NWElem, LeftElem]),

    % top right
    nvalue(2, [CurrElem, UpperElem, NEElem, RightElem]),

    % bottom left
    nvalue(2, [CurrElem, BelowElem, SWElem, LeftElem]),

    %bottom right
    nvalue(2, [CurrElem, BelowElem, SEElem, RightElem]),

    setConstrains(Board, NewIndex, Length, LengthRow, Ns).

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