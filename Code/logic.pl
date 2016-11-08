:- use_module(library(lists)).
:- use_module(library(aggregate)).
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
    [[home, player1, [shipAdamaged], none], [star2, free, [], none], [emptyS, free, [], none]],
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

% Does player turn

assignShip(a, shipAdamaged).
assignShip(b).
assignShip(c).
assignShip(d).

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

getScoreOfPlayerPiece(Player, Board, Piece, Score):-
    getBoardPieces(Board, Piece),
    systemBelongsToPlayer(Player, Piece),
    getScoreFromPiece(Piece, Score).

getTotalScoreOfPlayer(Player, Board, TotalScore):-
    findall(Score, getScoreOfPlayerPiece(Player, Board, Piece, Score), List),

    list_sum(List, Total),
    TotalScore is Total.

/**** GET SHIP POSITION ****/

% Copies only what is needed to NewPiece
apply0([A, _, _, _], A).
apply1([_, A, _, _], A).
apply2([_, _, [A], _], A).
apply3([_, _, _, A], A).

setPieceToMove([X|Xs], [Y|Ys], Ship, NewPiece, 3):-
    apply3(NewPiece, none).

setPieceToMove([X|Xs], [Y|Ys], Ship, NewPiece, 2):-
    apply2(NewPiece, Ship),
    setPieceToMove(Xs, Ys, Ship, NewPiece, 3).

setPieceToMove([X|Xs], [Y|Ys], Ship, NewPiece, 1):-
    apply1(NewPiece, X),
    setPieceToMove(Xs, Ys, Ship, NewPiece, 2).

setPieceToMove([X|Xs], [Y|Ys], Ship, NewPiece, 0):-
    apply0(NewPiece, Y),
    setPieceToMove(Xs, Ys, Ship, NewPiece, 1).

% Removes ship on the old piece
removeShipFromPiece([Type, Owner, Ships, Building], Ship, [Type, Owner, NewShips, Building]):-
    delete(Ships, Ship, NewShips).

playerTurn(WhoIsPlaying):-
    initial_logic_board(Board),

    write('*** Player '),
    write(WhoIsPlaying),
    write(' turn ***'), nl, nl,

    display_board(Board), nl, nl,

    write('Select ship: '),
    read(UserShipToMove),
    assignShip(UserShipToMove, ShipToMove),
    % check if ship can indeed travel

    getBoardPieces(Board, PieceToMove),
    systemHasShip(ShipToMove, PieceToMove),
    getPiece(PieceToMoveY, PieceToMoveX, Board, PieceToMove),
    write('This is the piece to move: '),
    write(PieceToMove), nl,

    write('Select row to travel to: '),
    read(DestinationRow),
    % check row limits

    write('Select column to travel to: '),
    read(DestinationColumn), nl,
    % check column limits

    getPiece(DestinationRow, DestinationColumn, Board, DestinationPiece),
    write('This is the destination piece: '),
    write(DestinationPiece), nl,

    setPieceToMove(PieceToMove, DestinationPiece, ShipToMove, NewPiece, 0),
    write('This is the new piece: '),
    write(NewPiece), nl,

    removeShipFromPiece(PieceToMove, ShipToMove, OldPiece),
    write('This is the old piece: '),
    write(OldPiece), nl,

    replace(PieceToMove, OldPiece, Board, UpdatedBoard1),
    replace(DestinationPiece, NewPiece, UpdatedBoard1, FinalUpdatedBoard),
    display_board(FinalUpdatedBoard).