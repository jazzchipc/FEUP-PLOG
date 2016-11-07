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
    [[star1, player1, [], none], [star2, free, [], none], [star2, free, [], none]],
    [[home, player1, [shipA, shipB, shipC, shipD], none], [star2, free, [], none], [emptyS, free, [], none]],
    [[star3, free, [], none], [nebula, free, [], none], [home, player2, [shipW, shipX, shipY, shipZ], none]],
    [[blackhole], [wormhole], [blackhole]],
    [[star3, free, [], none], [nebula, free, [], none], [star1, free, [], none]],
    [[star1, free, [], none], [star2, free, [], none], [star2, player1, [], none]]
    ]
    ).

/*** GET INFORMATION FROM CELLS ***/

%% Get cell type

isStarSystem1([star1, _ , _, _]).
isStarSystem2([star2, _ , _, _]).
isStarSystem3([star3, _ , _, _]).

isStarSystem(X):-
    isStarSystem1(X);
    isStarSystem2(X);
    isStarSystem3(X).

isEmptySystem([emptyS, _ , _, _]).

isNebulaSystem([nebula, _ , _, _]).

isBlackhole([blackhole]). 

isWormhole([wormhole]). 

%% Get owner

isSystemFree([_, free, _, _]).

systemBelongsToPlayer(Player, [_, Player, _, _]).

systemHasShip(Ship, [_, _, Ships, _]):-
    member(Ship, Ships).

isSystemOwned(System):-
    systemBelongsToPlayer(player1, System);
    systemBelongsToPlayer(player2, System).

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

isSystemNotColonized([_, _, _, none]).

hasSystemColony([_, _, _, colony]).
hasSystemTradeStation([_, _, _, trade]).

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

getPieceGivenShipAux(shipA, [], PieceToMove).
insertShipOnPiece(Ship, [_,_,Ship_]).
    getPieceGivenShipAux(shipA, Xs).
getPieceGivenShipAux(shipA, [Type,Owner,shipA,Ocupation|Xs], PieceToMove):-
    setPieceGivenShip([Type,Owner,shipA,Ocupation], PieceToMove).

getPieceGivenShip(shipA, [], PieceToMove).
getPieceGivenShip(shipA, [X|Xs], PieceToMove):-
    getPieceGivenShipAux(shipA, X, PieceToMove),
    getPieceGivenShip(shipA, Xs, PieceToMove).

% Does player turn

assignShip(a).
assignShip(b).
assignShip(c).
assignShip(d).

playerTurn(WhoIsPlaying):-
    board(BoardIn),

    write('*** Player '),
    write(WhoIsPlaying),
    write(' turn ***'), nl, nl,

    /*write('Select ship: '),
    read(ShipToMove),
    read(ShipSelection),
    write('Escrevi esta merda: '),
    write(ShipSelection).
    getPieceGivenShip(ShipSelection, BoardIn, CurrentRow, CurrentColumn, 0),*/
    % check if ship can indeed travel

    write('Select row the ship is in now: '),
    read(CurrentRow).
    % check row limits

    /*write('Select row to travel to: '),
    read(DestinationRow),
    % check row limits

    write('Select column to travel to: '),
    read(DestinationColumn), nl,
    % check column limits

    getPiece(CurrentRow, CurrentColumn, BoardIn, CurrentPiece),
    getPiece(DestinationRow, DestinationColumn, BoardIn, DestinationPiece),
    getShip(CurrentPiece, Ship),
    % replace(CurrentPiece, DestinationPiece, BoardIn, BoardOut),

    write('OldBoard: '),
    write(BoardIn), nl,
    write('OldPiece: '),
    write(CurrentPiece), nl,
    write('NewPiece: '),
    write(DestinationPiece), nl,
    write('NewBoard: '),
    write(BoardOut), nl, nl,*/

    %BoardIn is BoardOut.



/**** CALCULATE SCORE FUNCTIONS ****/

starSystemScore(StarSystem, Score):-
    (isStarSystem1(StarSystem), Score is 1);
    (isStarSystem2(StarSystem), Score is 2);
    (isStarSystem3(StarSystem), Score is 3).

%%score(nebula) --> depends on how many the player has

getRowPieces(Board, NumOfRow, Piece):-
    getPiece(NumOfRow, _, Board, Piece).

getBoardPieces(Board, Piece):-
    getRowPieces(Board, _, Piece).

getScoreFromPiece(Piece, Score):-
    isStarSystem(Piece), starSystemScore(Piece, Score).

:- dynamic total_score/1.

total_score(0).

getScoreOfPlayer(Player, Board, TotalScore):-
    getBoardPieces(Board, Piece),
    systemBelongsToPlayer(Player, Piece),
    getScoreFromPiece(Piece, Score),

    retract(total_score(C)),
    C1 is (C + Score) /* or C1 is C+1 */,
    assertz(total_score(C1)),
    
    total_score(TotalScore).

/**** GET SHIP POSITION ****/

%% initial_logic_board(Board), getBoardPieces(Board, Piece), systemHasShip(shipA, Piece), getPiece(Y, X, Board, Piece) == retornar X e Y da shipA

