getNthCell(Row, Column, Board, Cell):-
    nth0(Row, Board, MyRow),
    nth0(Column, MyRow, Cell).

myInit:- board(B), getNthCell(0, 1, B, Cell), write(Cell).