/*** LOGICAL ARRAY KEYWORDS ***/

/*** TYPES OF SYSTEMS 
home - Homeworld system 
starX - Star system with X planets
nebula - Nebula system 
emptyS - Empty system 
wormhole - Wormhole system 
blackhole - Blackhole system 
*/

/*** OWNER
player1 - player one
player2 - player two
free - free system
*/

/*** LIST OF SHIPS
Player one: 
    -shipA, shipB, shipC, shipD
    -shipAdamaged, shipBdamaged, shipCdamaged, shipDdamaged

Player two:
    -shipW, shipX, shipY, shipZ
    -shipWdamaged, shipXdamaged, shipYdamaged, shipZdamaged
*/

/*** BUILDING
trade - trade station
colony - colony
none - none
*/

/* Element example: [<type of system>, <owner>, <list of ships>, <constructions>] */

initial_logic_board([
    [[star2, free, [], none], [star2, free, [], none], [wormhole]],
    [[star1, free, [], none], [star2, free, [], none], [star2, free, [], none]],
    [[home, player1, [shipA, shipB, shipC, shipD], none], [star2, free, [], none], [emptyS, free, [], none]],
    [[star3, free, [], none], [nebula, free, [], none], [home, player2, [shipW, shipX, shipY, shipZ], none]],
    [[blackhole], [wormhole], [blackhole]],
    [[star3, free, [], none], [nebula, free, [], none], [star1, free, [], none]],
    [[star1, free, [], none], [star2, free, [], none], [star2, free, [], none]]
    ]
    ).

/*** GET INFORMATION FROM CELLS ***/

%% Get cell type

isStarSystem1(star1).
isStarSystem2(star2).
isStarSystem3(star3).

isStarSystem(X):-
    isStarSystem1(X);
    isStarSystem2(X);
    isStarSystem3(X).

isEmptySystem(emptyS).

isNebulaSystem(nebula).

isBlackhole(blackhole). 

isWormhole(wormhole). 

isCellBlackhole([X|[]]):-
    isBlackhole(X).

isCellWormhole([X|[]]):-
    isWormhole(X).

%% Get owner

isSystemFree(free).

%% Get buildings

isSystemNotColonized(none).

hasSystemColony(colony).
hasSystemTradeStation(trade).

isSystemColonized(X):-
    hasSystemColony(X);
    hasSystemTrade(X).
    

getPiece(Row, Column, Board, Piece):-
    nth0(Row, Board, MyRow),
    nth0(Column, MyRow, Piece).

%setPieceAux(Row, Column, BoardIn, Piece, BoardOut):-
%    Column

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