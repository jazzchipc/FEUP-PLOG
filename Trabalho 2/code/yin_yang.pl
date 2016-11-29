:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- include('display_board.pl').

%% UTILITIES %%

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

get_cell(Board, Column, Row, Cell):-
    nth1(Row, Board, RowList),
    nth1(Column, RowList, Cell).

get_row(Board, Row, R):-
    nth1(Row, Board, R).

%% SET DOMAIN OF BOARD %%

% 1 is white
% 2 is black

set_domain_aux(Board, 1):-
    get_row(Board, 1, Row),
    domain(Row, 1, 2).

set_domain_aux(Board, RowY):-
    get_row(Board, RowY, Row),
    domain(Row, 1, 2),
    RowY1 is RowY-1,
    set_domain_aux(Board, RowY1).

set_domain(Board):-
    length(Board, N),
    set_domain_aux(Board, N).

%% SET CONSTRAINTS %%

set_constraints_in_cell(Board, X, Y):-
    get_cell(Board, X, Y, CurrentCell),

    Xright is X+1, Xleft is X-1,
    Ybottom is Y+1, Ytop is Y-1,

    %% direct contact cells
    get_cell(Board, Xright, Y, CellRight),
    get_cell(Board, Xleft, Y, CellLeft),
    get_cell(Board, X, Ytop, CellTop),
    get_cell(Board, X, Ybottom, CellBottom),

    %% diagonal cells
    get_cell(Board, Xright, Ytop, NE),
    get_cell(Board, Xleft, Ytop, NW),
    get_cell(Board, Xright, Ybottom, SE),
    get_cell(Board, Xleft, Ybottom, SW),

    %% one direct contact cell must be the same color

    (CurrentCell #= CellRight #\/ CurrentCell #= CellLeft #\/ CurrentCell #= CellTop #\/ CurrentCell #= CellBottom),

    %% no 2x2 can be the same color

    %top left square
    ((CurrentCell #\= CellLeft #\/ CurrentCell #\= CellTop #\/ CurrentCell #\= NW) #\/
    %top right
    (CurrentCell #\= CellRight #\/ CurrentCell #\= CellTop #\/ CurrentCell #\= NE) #\/
    %bottom left
    (CurrentCell #\= CellLeft #\/ CurrentCell #\= CellBottom #\/ CurrentCell #\= SW) #\/
    %bottom right
    (CurrentCell #\= CellRight #\/ CurrentCell #\= CellBottom #\/ CurrentCell #\= SE)).


%% set constraints to all row elements
set_board_constraints_row_aux(Board, RowY, 1):-
    set_board_constraints_row_aux(Board, RowY, 1).

set_board_constraints_row_aux(Board, RowY, ElementX):-
    set_constraints_in_cell(Board, ElementX, RowY),

    ElementX1 is ElementX-1,
    set_board_constraints_row_aux(Board, RowY, ElementX1).

%% set constraints to all rows

set_board_constraints_aux(Board, 1):-
    length(Board, XElements),
    set_board_constraints_row_aux(Board, RowY, XElements).

set_board_constraints_aux(Board, RowY):-
    length(Board, XElements),
    set_board_constraints_row_aux(Board, RowY, XElements),

    RowY1 is RowY-1,
    set_board_constraints_row_aux(Board, RowY1).

set_board_constraints(Board):-
    length(Board, YRows),
    set_board_constraints_aux(Board, YRows).
 
%% find solutions

yin_yang(Board):-

    make_board(4, 4, Board),

    set_domain(Board),
    
    set_constraints_in_cell(Board, 2, 2),
    set_constraints_in_cell(Board, 3, 2),
    set_constraints_in_cell(Board, 2, 3),
    set_constraints_in_cell(Board, 3, 3),

    append(Board, ListOfVars),

    labeling([], ListOfVars).

%% display

display:-
    yin_yang(Board),
    display_board(Board).