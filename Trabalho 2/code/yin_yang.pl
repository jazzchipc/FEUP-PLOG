:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- include('display_board.pl').

%% NÃO É NECESSÁRIO UMA INTERFACE COM O UTILIZADOR %%

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

    Xright is X+1,
    Ybottom is Y+1,

    %% direct contact cells
    get_cell(Board, Xright, Y, CellRight),
    get_cell(Board, X, Ybottom, CellBottom),

    %% diagonal cells
    get_cell(Board, Xright, Ybottom, CellSE),

    %% one direct contact cell must be the same color

    (
        (nvalue(1, [CurrentCell, CellRight]) #\/ nvalue(1, [CurrentCell, CellBottom])) % must be connected to one cell of the same color
        #/\ 
        (nvalue(2, [CurrentCell, CellLeft, CellRight, CellSE]))  % no 2x2 can have a single color
    ).  


%% set constraints to all row elements
set_board_constraints_row_aux(Board, RowY, 2):- % 2 instead of 1 so it doesn't apply to the last column
    set_board_constraints_row_aux(Board, RowY, 2).

set_board_constraints_row_aux(Board, RowY, ElementX):-
    set_constraints_in_cell(Board, ElementX, RowY),

    ElementX1 is ElementX-1,
    set_board_constraints_row_aux(Board, RowY, ElementX1).

%% set constraints to all rows

set_board_constraints_aux(Board, 2):-   % 2 instead of 1 so it doesn't appply to last row
    length(Board, XElements),
    set_board_constraints_row_aux(Board, 2, XElements).

set_board_constraints_aux(Board, RowY):-
    length(Board, XElements),
    set_board_constraints_row_aux(Board, RowY, XElements),

    RowY1 is RowY-1,
    set_board_constraints_row_aux(Board, RowY1).

set_board_constraints(Board):-
    length(Board, YRows),
    set_board_constraints_aux(Board, YRows).
 
%% find solutions

yin_yang(Board, N):-

    make_board(N, N, Board),

    set_domain(Board),
    
    set_board_constraints(Board),

    append(Board, ListOfVars),

    labeling([], ListOfVars).

%% display

display:-
    yin_yang(Board, 4),
    display_board(Board).