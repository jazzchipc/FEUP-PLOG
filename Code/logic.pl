:- include('utils.pl').

getPiece(Row, Column, Board, Piece):-
    nth0(Row, Board, MyRow),
    nth0(Column, MyRow, Piece).

setPieceAux(Row, Column, BoardIn, Piece, BoardOut):-
    Column

setPieceAux(Row, Column, BoardIn, Piece, BoardOut):-
    NewColumn is Column - 1,
    setPieceAux(Row, NewColumn, BoardIn, Piece, BoardOut).

setPiece(Row, Column, [X|Xs], Piece, BoardOut):-
    Row == 0,
    setPieceAux(Row, Column, [X|Xs], Piece, BoardOut).

setPiece(Row, Column, [X|Xs], Piece, BoardOut):-
    NewRow is Row - 1,
    setPiece(NewRow, Column, Xs, Piece, BoardOut).


% myInit:- board(B), getNthCell(0, 1, B, Cell), write(Cell).

playerTurn(WhoIsPlaying):-
    write('*** Player '),
    write(WhoIsPlaying),
    write(' turn ***'), nl,

    write('Select ship: '),
    read(ShipSelection), nl,
    % check if ship can indeed travel

    write('Select row to travel to: '),
    read(RowSelection), nl,
    % check row limits

    write('Select column to travel to: '),
    read(ColumnSelection), nl.
    % check column limits