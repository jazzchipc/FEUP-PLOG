:- use_module(library(lists)).
:- include('board.pl').
:- include('utils.pl').

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
1 - player one
2 - player two
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
    [[home, 1, [shipA, shipB, shipC, shipD]], [star2, free, [], none], [emptyS, free, [], none]],
    [[star3, free, [], none], [nebula, free, [], none], [home, 2, [shipW, shipX, shipY, shipZ]]],
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

isSystemPlayer1(1).

isSystemPlayer2(2).

isSystemOwned(X):-
    isSystemPlayer1(X);
    isSystemPlayer2(X).

%% Get ships

ship(shipA).
ship(shipB).
ship(shipC).
ship(shipD).

ship(shipW).
ship(shipX).
ship(shipY).
ship(shipZ).

shipDamaged(shipAdamaged).
shipDamaged(shipBdamaged).
shipDamaged(shipCdamaged).
shipDamaged(shipDdamaged).

shipDamaged(shipWdamaged).
shipDamaged(shipXdamaged).
shipDamaged(shipYdamaged).
shipDamaged(shipZdamaged).

isShipNotDamaged(X):-
    ship(X).

isShipDamaged(X):-
    shipDamaged(X).

player1Ship(shipA).
player1Ship(shipB).
player1Ship(shipC).
player1Ship(shipD).

player1Ship(shipAdamaged).
player1Ship(shipBdamaged).
player1Ship(shipCdamaged).
player1Ship(shipDdamaged).

shipBelongsToPlayer1(X):-
    player1Ship(X).

player2Ship(shipW).
player2Ship(shipX).
player2Ship(shipY).
player2Ship(shipZ). 

player2Ship(shipWdamaged).
player2Ship(shipXdamaged).
player2Ship(shipYdamaged).
player2Ship(shipZdamaged).

shipBelongsToPlayer2(X):-
    player2Ship(X).

%% Get buildings

isSystemNotColonized(none).

hasSystemColony(colony).
hasSystemTradeStation(trade).

isSystemColonized(X):-
    hasSystemColony(X);
    hasSystemTrade(X).


/** BOARD CELL FUNCTIONS **/

getPiece(Row, Column, Board, Piece):-
    nth0(Row, Board, MyRow),
    nth0(Column, MyRow, Piece).

replaceElement(_, _, [], []).
replaceElement(O, R, [O|Xs], [R|Ys]):-
    replaceElement(O, R, Xs, Ys).
replaceElement(O, R, [X|Xs], [X|Ys]):-
    X \= O,
    replaceElement(O, R, Xs, Ys).

replace(_, _, [], []).
replace(OldPiece, NewPiece, [X|Xs], [Y|Ys]):-
    replaceElement(OldPiece, NewPiece, X, Y),
    replace(OldPiece, NewPiece, Xs, Ys).

getShip([_,_,Ship,_], Ship).

getPieceGivenShipAux(Ship, [], Column).
getPieceGivenShipAux(Ship, [_, _, Ship, _|Xs], Column).
getPieceGivenShipAux(Ship, [X|Xs], Column):-
    NewColumn is Column + 1,
    getPieceGivenShipAux(Ship, Xs, NewColumn).

/*Given the ship and the board, returns the row and column that ships is situated*/
getPieceGivenShip(Ship, [X|Xs], Row, Column, Counter):-
    getPieceGivenShipAux(Ship, X, 0),
    NewRow is Counter + 1,
    getPieceGivenShip(Ship, Xs, NewRow, Column).

insertShipOnPiece(Ship, [_,_,Ship_]).

playerTurn(WhoIsPlaying):-
    board(BoardIn),

    write('*** Player '),
    write(WhoIsPlaying),
    write(' turn ***'), nl, nl,

    /*write('Select ship: '),
    read(ShipSelection),
    write('Escrevi esta merda: '),
    write(ShipSelection).
    getPieceGivenShip(ShipSelection, BoardIn, CurrentRow, CurrentColumn, 0),*/
    % check if ship can indeed travel

    write('Select row the ship is in now: '),
    read(CurrentRow),
    % check row limits

    write('Select column the ship is in now: '),
    read(CurrentColumn),
    % check column limits

    write('Select row to travel to: '),
    read(DestinationRow),
    % check row limits

    write('Select column to travel to: '),
    read(DestinationColumn), nl,
    % check column limits

    getPiece(CurrentRow, CurrentColumn, BoardIn, CurrentPiece),
    getPiece(DestinationRow, DestinationColumn, BoardIn, DestinationPiece),
    getShip(CurrentPiece, Ship),
    insertShipOnPiece(Ship, DestinationPiece),
    % replace(CurrentPiece, DestinationPiece, BoardIn, BoardOut),

    write('OldBoard: '),
    write(BoardIn), nl,
    write('OldPiece: '),
    write(CurrentPiece), nl,
    write('NewPiece: '),
    write(DestinationPiece), nl,
    write('NewBoard: '),
    write(BoardOut), nl, nl.