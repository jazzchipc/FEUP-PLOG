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
    [[star1, free, [], none], [star2, free, [], none], [star2, free, [], none]],
    [[home, player1, [shipAdamaged, shipBdamaged, shipCdamaged, shipDdamaged], none], [star2, free, [], none], [emptyS, free, [], none]],
    [[star3, free, [], none], [nebula, free, [], none], [home, player2, [shipWdamaged, shipXdamaged, shipYdamaged, shipZdamaged], none]],
    [[blackhole], [wormhole], [blackhole]],
    [[star3, free, [], none], [nebula, free, [], none], [star1, free, [], none]],
    [[star1, free, [], none], [star2, free, [], none], [star2, free, [], none]]
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
    hasSystemTradeStation(X).

%% Directions of movement

north(n).
south(s).
northwest(nw).
southwest(sw).
northeast(ne).
southeast(se).

/** BOARD CELL FUNCTIONS **/

% Auxiliar functions for getting adjacent cells

getAdjacentEvenRow(Xin, Yin, Xout, Yout):-
    (Xout is Xin, (Yout is Yin -2; Yout is Yin -1; Yout is Yin +1; Yout is Yin +2))
    ;
    (Xout is Xin +1, (Yout is Yin +1; Yout is Yin -1)).

getAdjacentOddRow(Xin, Yin, Xout, Yout):-
    (Xout is Xin, (Yout is Yin -2; Yout is Yin -1; Yout is Yin +1; Yout is Yin +2))
    ;
    (Xout is Xin -1, (Yout is Yin +1; Yout is Yin -1)).

getAdjacent(Xin, Yin, Xout, Yout):-
    (1 =:= mod(Yin, 2), getAdjacentOddRow(Xin, Yin, Xout, Yout))
    ;
    (0 =:= mod(Yin, 2), getAdjacentEvenRow(Xin, Yin, Xout, Yout)).

% Auxiliar functions for verifying adjacent cells

adjacentEvenRow(X, Y, AdjX, AdjY):-
    (AdjX =:= X, AdjY \= Y, abs(AdjY - Y) =< 2);
    (AdjX =:= X+1, abs(AdjY - Y) =:= 1).

adjacentOddRow(X, Y, AdjX, AdjY):-
    (AdjX =:= X, AdjY \= Y, abs(AdjY - Y) =< 2);
    (AdjX =:= X-1, abs(AdjY - Y) =:= 1).

% Returns adjacent cell coordinates to cell with (X, Y) coordinates 
adjacent(X, Y, AdjX, AdjY):-
    (1 =:= mod(Y, 2), adjacentOddRow(X, Y, AdjX, AdjY))
    ;
    (0 =:= mod(Y, 2), adjacentEvenRow(X, Y, AdjX, AdjY)).

% Returns all adjacent cells to cell(Column, Row) and saves them in List
getAdjacentList(X, Y, ListX, ListY):-
    findall((AdjX), getAdjacent(X, Y, AdjX, AdjY), ListX),
    findall((AdjY), getAdjacent(X, Y, AdjX, AdjY), ListY).

% Auxiliar function to get all the cell to be played
getOddCell(Board, X, Y, Xs, Ys, ListX, ListY, MovType):-
    (X < 0; Y < 0),
        ListX = Xs,
        ListY = Ys;
    length(Board, NumOfRows),
    Y > NumOfRows - 1,
        ListX = Xs,
        ListY = Ys;
    getColumnLength(Board, Y, NumOfColumns),
    X > NumOfColumns - 1,
        ListX = Xs,
        ListY = Ys;
    MovType == topLeft,
        NewX is X - 1,
        NewY is Y - 1,
        getEvenCell(Board, NewX, NewY, [X|Xs], [Y|Ys], ListX, ListY, MovType);
    MovType == topRight,
        NewY is Y - 1,
        getEvenCell(Board, X, NewY, [X|Xs], [Y|Ys], ListX, ListY, MovType);
    MovType == bottomLeft,
        NewX is X - 1,
        NewY is Y + 1,
        getEvenCell(Board, NewX, NewY, [X|Xs], [Y|Ys], ListX, ListY, MovType);
    MovType == bottomRight,
        NewY is Y + 1,
        getEvenCell(Board, X, NewY, [X|Xs], [Y|Ys], ListX, ListY, MovType).

% Auxiliar function to get all the cell to be played
getEvenCell(Board, X, Y, Xs, Ys, ListX, ListY, MovType):-
    (X < 0; Y < 0),
        ListX = Xs,
        ListY = Ys;
    length(Board, NumOfRows),
    Y > NumOfRows - 1,
        ListX = Xs,
        ListY = Ys;
    getColumnLength(Board, Y, NumOfColumns),
    X > NumOfColumns - 1,
        ListX = Xs,
        ListY = Ys;
    MovType == topLeft,
        NewY is Y - 1,
        getOddCell(Board, X, NewY, [X|Xs], [Y|Ys], ListX, ListY, MovType);
    MovType == topRight,
        NewX is X + 1,
        NewY is Y - 1,
        getOddCell(Board, NewX, NewY, [X|Xs], [Y|Ys], ListX, ListY, MovType);
    MovType == above,
        NewY is Y - 2,
        getEvenCell(Board, X, NewY, [X|Xs], [Y|Ys], ListX, ListY, MovType);
    MovType == below,
        NewY is Y + 2,
        getEvenCell(Board, X, NewY, [X|Xs], [Y|Ys], ListX, ListY, MovType);
    MovType == bottomLeft,
        NewY is Y + 1,
        getOddCell(Board, X, NewY, [X|Xs], [Y|Ys], ListX, ListY, MovType);
    MovType == bottomRight,
        NewX is X + 1,
        NewY is Y + 1,
        getOddCell(Board, NewX, NewY, [X|Xs], [Y|Ys], ListX, ListY, MovType).

% Returns all the top left cells that can be played given a specific X and Y
getTopLeft(Board, X, Y, ListX, ListY):-
    1 =:= mod(Y, 2),
        getEvenCell(Board, X - 1, Y - 1, [], [], ListX, ListY, topLeft);
    0 =:= mod(Y, 2),
        getOddCell(Board, X, Y - 1, [], [], ListX, ListY, topLeft).

% Returns all the top right cells that can be played given a specific X and Y
getTopRight(Board, X, Y, ListX, ListY):-
    1 =:= mod(Y, 2),
        getEvenCell(Board, X, Y - 1, [], [], ListX, ListY, topRight);
    0 =:= mod(Y, 2),
        getOddCell(Board, X + 1, Y - 1, [], [], ListX, ListY, topRight).

% Returns all the above cells that can be played given a specific X and Y
getAbove(Board, X, Y, ListX, ListY):-
    getEvenCell(Board, X, Y - 2, [], [], ListX, ListY, above).

% Returns all the below cells that can be played given a specific X and Y
getBelow(Board, X, Y, ListX, ListY):-
    getEvenCell(Board, X, Y + 2, [], [], ListX, ListY, below).

% Returns all the bottom left cells that can be played given a specific X and Y
getBottomLeft(Board, X, Y, ListX, ListY):-
    1 =:= mod(Y, 2),
        NewX is X - 1,
        NewY is Y + 1,
        getEvenCell(Board, NewX, NewY, [], [], ListX, ListY, bottomLeft);
    0 =:= mod(Y, 2),
        NewY is Y + 1,
        getOddCell(Board, X, NewY, [], [], ListX, ListY, bottomLeft).

% Returns all the bottom right cells that can be played given a specific X and Y
getBottomRight(Board, X, Y, ListX, ListY):-
    1 =:= mod(Y, 2),
        NewY is Y + 1,
        getEvenCell(Board, X, Y + 1, [], [], ListX, ListY, bottomRight);
    0 =:= mod(Y, 2),
        NewX is X + 1,
        NewY is Y + 1,
        getOddCell(Board, NewX, NewY, [], [], ListX, ListY, bottomRight).

% Returns the X on the ListX and the Y on the ListY of all the cells that can be playeed given a specific X and Y
getAllPossibleCellsToMove(Board, X, Y, ListX, ListY):-
    getTopLeft(Board, X, Y, TopLeftX, TopLeftY),
    getTopRight(Board, X, Y, TopRightX, TopRightY),
    getAbove(Board, X, Y, AboveX, AboveY),
    getBelow(Board, X, Y, BelowX, BelowY),
    getBottomLeft(Board, X, Y, BottomLeftX, BottomLeftY),
    getBottomRight(Board, X, Y, BottomRightX, BottomRightY),

    append(TopLeftX, TopRightX, X1),
    append(X1, AboveX, X2),
    append(X2, BelowX, X3),
    append(X3, BottomLeftX, X4),
    append(X4, BottomRightX, ListX),

    append(TopLeftY, TopRightY, Y1),
    append(Y1, AboveY, Y2),
    append(Y2, BelowY, Y3),
    append(Y3, BottomLeftY, Y4),
    append(Y4, BottomRightY, ListY).

% Returns Piece on Row and Column
getPiece(Row, Column, Board, Piece):-
    nth0(Row, Board, MyRow),
    nth0(Column, MyRow, Piece).

% Replaces element O to R on Board only one time
replaceElement(_, _, _, [], []).
replaceElement(O, R, 0, [O|Xs], [R|Ys]):-
    replaceElement(O, R, 20, Xs, Ys).
replaceElement(O, R, Column, [X|Xs], [X|Ys]):-
    NewColumn is Column - 1,
    replaceElement(O, R, NewColumn, Xs, Ys).

replace(_, _, _, _, [], []).
replace(OldPiece, NewPiece, 0, Column, [X|Xs], [Y|Ys]):-
    replaceElement(OldPiece, NewPiece, Column, X, Y),
    replace(OldPiece, NewPiece, 20, Column, Xs, Ys).
replace(OldPiece, NewPiece, Row, Column, [X|Xs], [X|Ys]):-
    NewRow is Row - 1,
    replace(OldPiece, NewPiece, NewRow, Column, Xs, Ys).

% Makes NewPiece as the final piece to display in the board
apply0([A, _, _, _], A).
apply1([_, A, _, _], A).
apply2([_, _, [A], _], A).
apply3([_, _, _, A], A).

setPieceToMove([X|Xs], [Y|Ys], Ship, Building, NewPiece, 3):-
    apply3(NewPiece, Building).
setPieceToMove([X|Xs], [Y|Ys], Ship, Building, NewPiece, 2):-
    apply2(NewPiece, Ship),
    setPieceToMove(Xs, Ys, Ship, Building, NewPiece, 3).
setPieceToMove([X|Xs], [Y|Ys], Ship, Building, NewPiece, 1):-
    apply1(NewPiece, X),
    setPieceToMove(Xs, Ys, Ship, Building, NewPiece, 2).
setPieceToMove([X|Xs], [Y|Ys], Ship, Building, NewPiece, 0):-
    apply0(NewPiece, Y),
    setPieceToMove(Xs, Ys, Ship, Building, NewPiece, 1).

% Removes ship on the old piece
removeShipFromPiece([Type, Owner, Ships, Building], Ship, [Type, Owner, NewShips, Building]):-
    delete(Ships, Ship, NewShips).

% Assigns ship based on user input
assignShip(a, shipAdamaged).
assignShip(b, shipBdamaged).
assignShip(c, shipCdamaged).
assignShip(d, shipDdamaged).
assignShip(w, shipWdamaged).
assignShip(x, shipXdamaged).
assignShip(y, shipYdamaged).
assignShip(z, shipZdamaged).

% Assigns building based on user input
assignBuilding(t, trade).
assignBuilding(c, colony).

% Writes N newlines
clearScreen(0).
clearScreen(N):-
    N1 is N - 1,
    nl,
    clearScreen(N1).

getShip([_,_,Ship,_], Ship).

/**** CALCULATE SCORE FUNCTIONS ****/

getRowPieces(Board, NumOfRow, Piece):-
    getPiece(NumOfRow, _, Board, Piece).

getRowPiece(Board, X, Y, Piece):-
    getPiece(Y, X, Board, Piece).

%% GETS ALL BOARD PIECES
% Regardless of coordinates
getBoardPieces(Board, Piece):-
    getRowPieces(Board, _, Piece).

% With coordinates
getBoardPiece(Board, Piece, X, Y):-
    getRowPieces(Board, X, Y, Piece).

%% Get score from star system cells
starSystemScore(StarSystem, Score):-
    (isStarSystem1(StarSystem), Score is 1);
    (isStarSystem2(StarSystem), Score is 2);
    (isStarSystem3(StarSystem), Score is 3).

getScoreFromStarSystemPiece(Piece, Score):-
    isStarSystem(Piece), starSystemScore(Piece, Score).

%% Get score from nebula system cells
getPlayerNebulaScore(Player, NumOfOwnedNebulas, NebulaScore):-
    (NumOfOwnedNebulas =:= 0, NebulaScore is 0);
    (NumOfOwnedNebulas =:= 1, NebulaScore is 2);
    (NumOfOwnedNebulas =:= 2, NebulaScore is 5);
    (NumOfOwnedNebulas =:= 3, NebulaScore is 8).

getNumOfOwnedNebulas(Player, Board, NumOfOwnedNebulas):-
    findall(Piece, (getBoardPieces(Board, Piece), systemBelongsToPlayer(Player, Piece), isNebulaSystem(Piece)), ListOfNebulasOwned),
    length(ListOfNebulasOwned, NumOfOwnedNebulas).


getScoreOfPlayerStarSystemPiece(Player, Board, Piece, Score):-
    getBoardPieces(Board, Piece),
    systemBelongsToPlayer(Player, Piece),
    (getScoreFromStarSystemPiece(Piece, Score)) .

%% Get score from adjacent cells
getCoordsOfTradeStationsAdjacents(Player, Board, ListOfCoords):-
    getBoardPiece(Board, Piece, X, Y),
    systemBelongsToPlayer(Player, Piece),
    hasSystemTradeStation(Piece),

    getAdjacent(X, Y, Xadj, Yadj),
    getBoardPiece(Board, AdjPiece, Xadj, Yadj),
    isSystemOwned(AdjPiece),
    (\+(systemBelongsToPlayer(Player, AdjPiece))),
    ListOfCoords = [Xadj, Yadj].

getScoreFromAdjacentsToTradeStations(Player, Board, ScoreFromAdjacents):-
    findall(ListOfCoords, getCoordsOfTradeStationsAdjacents(Player, Board, ListOfCoords), ListOfAdjacents),

    length(ListOfAdjacents, ScoreFromAdjacents).

%% Get player total score

getTotalScoreOfPlayer(Player, Board, TotalScore):-
    %star systems
    %% findall(<o que quero procurar>, <que condição tem que obedecer>, <onde guardar soluções>).
    findall(Score, getScoreOfPlayerStarSystemPiece(Player, Board, Piece, Score), StarSystemList), 

    ((length(StarSystemList, 0), TotalStarSystemsScore is 0)
    ;
    list_sum(StarSystemList, TotalStarSystemsScore)),
    
    %nebulas systems
    getNumOfOwnedNebulas(Player, Board, NumOfOwnedNebulas),
    getPlayerNebulaScore(Player, NumOfOwnedNebulas, NebulaScore),
    
    %adjacent systems
    getScoreFromAdjacentsToTradeStations(Player, Board, ScoreFromAdjacents),

    %total 
    TotalScore is (TotalStarSystemsScore+NebulaScore+ScoreFromAdjacents).

/******************VALID MOVE FUNCTIONS******************/

% Checks if ship to move belongs to the player who is playing
canPlayerMoveSelectedShip(1, Ship):-
    (Ship == shipAdamaged; Ship == shipBdamaged; Ship == shipCdamaged; Ship == shipDdamaged);
    !,
    write('***** You entered a ship that is not yours to command! *****'), nl,
    fail.
canPlayerMoveSelectedShip(2, Ship):-
    (Ship == shipWdamaged; Ship == shipXdamaged; Ship == shipYdamaged; Ship == shipZdamaged);
    !,
    write('***** You entered a ship that is not yours to command! *****'), nl,
    fail.

% Checks if the row inserted by the player is in the board
checkRowLimits(Board, DestinationRow):-
    length(Board, NumOfRows),

    (DestinationRow > NumOfRows,
    !,
    format('***** The board only has ~d rows, cant go to row ~d*****~n', [NumOfRows, DestinationRow]),
    fail)
    ;
    (DestinationRow < 0,
    !,
    write('***** There are no negative coordinates. ******'), nl, 
    fail).
checkRowLimits(Board, DestinationRow).

% Checks if the column inserted by the player is in the board
checkColumnLimits([X|Xs], 0, DestinationColumn):-
    length(X, NumOfColumns),
    
    (DestinationColumn > NumOfColumns,
    !,
    format('***** The board only has ~d columns, cant go to column ~d*****~n', [NumOfColumns, DestinationColumn]),
    fail)
    ;
    (DestinationColumn < 0,
    !,
    write('***** There are no negative coordinates. ******'), nl, 
    fail).
checkColumnLimits([X|Xs], 0, DestinationColumn).
checkColumnLimits([X|Xs], DestinationRow, DestinationColumn):-
    NewRow is DestinationRow - 1,
    checkColumnLimits(Xs, NewRow, DestinationColumn).

% Checks if the building typed by the player is either a colony ir a trade station
checkValidBuilding(Building):-
    (Building == trade; Building == colony);
    !,
    write('***** Invalid input, please only type t or c for the building!*****'), nl,
    fail.

/**** MOVE WITH DIRECTIONS ****/

moveNCellsInDirectionOddRow(Xi, Yi, Direction, NumberOfCells, Xf, Yf):-

    ((northwest(Direction), Yf is(Yi - NumberOfCells), Xf is(Xi - ((NumberOfCells + 1) // 2)))
    ;
    (southwest(Direction), Yf is(Yi + NumberOfCells), Xf is(Xi - ((NumberOfCells + 1) // 2)))
    ;
    (northeast(Direction), Yf is(Yi - NumberOfCells), Xf is(Xi + (NumberOfCells // 2)))
    ;
    (southeast(Direction), Yf is(Yi + NumberOfCells), Xf is(Xi + (NumberOfCells // 2)))),

    verifyValidGeometricDirection(Xi, Yi, Xf, Yf).

moveNCellsInDirectionEvenRow(Xi, Yi, Direction, NumberOfCells, Xf, Yf):-

    ((northwest(Direction), Yf is(Yi - NumberOfCells), Xf is(Xi - (NumberOfCells // 2)))
    ;
    (southwest(Direction), Yf is(Yi + NumberOfCells), Xf is(Xi - (NumberOfCells // 2)))
    ;
    (northeast(Direction), Yf is(Yi - NumberOfCells), Xf is(Xi + ((NumberOfCells + 1) // 2)))
    ;
    (southeast(Direction), Yf is(Yi + NumberOfCells), Xf is(Xi + ((NumberOfCells + 1) // 2)))),

    verifyValidGeometricDirection(Xi, Yi, Xf, Yf).

moveNCellsInDirection(Xi, Yi, Direction, NumberOfCells, Xf, Yf):-
    NumberOfCells \= 0,
    
    %north
    ((north(Direction), Yf is (Yi - (NumberOfCells*2)), Xf is Xi, verifyValidGeometricDirection(Xi, Yi, Xf, Yf))
    ;
    %south
    (south(Direction), Yf is (Yi + (NumberOfCells*2)), Xf is Xi, verifyValidGeometricDirection(Xi, Yi, Xf, Yf))
    ;
    %other Directions
    ((1 =:= mod(Yi, 2), moveNCellsInDirectionOddRow(Xi, Yi, Direction, NumberOfCells, Xf, Yf)))
    ;
    ((0 =:= mod(Yi, 2), moveNCellsInDirectionEvenRow(Xi, Yi, Direction, NumberOfCells, Xf, Yf)))).
    
/**** VERIFY MOVE ****/

verifyValidDirectionOddRow(Xi, Yi, Xf, Yf):- 
    DifY is (Yf - Yi), DifX is(Xf-Xi),

    ((DifX >= 0, abs(DifX) =:= (abs(DifY//2)))
    ;
    (DifX =< 0, abs(DifX) =:= ((abs(DifY) + 1)//2))).

verifyValidDirectionEvenRow(Xi, Yi, Xf, Yf):- 
    DifY is (Yf - Yi), DifX is(Xf-Xi),

    ((DifX =< 0, abs(DifX) =:= (abs(DifY//2)))
    ;
    (DifX >= 0, abs(DifX) =:= ((abs(DifY) + 1)//2))).


/**** VERIFY END OF THE GAME ****/

gameEnd(Board):-

    %% While any ship can be moved    
    \+((ship(Ship),
    getBoardPieces(Board, PieceWithShip),
    systemHasShip(Ship, PieceWithShip),
    getPiece(Y, X, Board, PieceWithShip),

    moveNCellsInDirection(X, Y, Direction, 1, Xf, Yf),
    getPiece(Yf, Xf, Board, AdjPiece),
    checkValidLandingCell(AdjPiece))).



/**** USE THIS FUNCTION TO VERIFY THE MOVEMENT OF A SHIP ****/
verifyValidGeometricDirection(Xi, Yi, Xf, Yf):-
    (((Xi =:= Xf), (mod(Yi, 2) =:= mod(Yf,2)), Yf \= Yi);
    ((1 =:= mod(Yi, 2), verifyValidDirectionOddRow(Xi, Yi, Xf, Yf)));
    ((0 =:= mod(Yi, 2), verifyValidDirectionEvenRow(Xi, Yi, Xf, Yf))))
    ;
    (write('The given cell is not in a possible direction'), nl, fail).

% Checks if the landing cell is valid
checkValidLandingCell([_, free, _, _]).
checkValidLandingCell([wormhole]):-
    write('You cant land in a wormhole!'), nl,
    fail.
checkValidLandingCell([blackhole]):-
    write('You cant land in a blackhole!'), nl,
    fail.
checkValidLandingCell(Cell):-
    isSystemOwned(Cell),
    write('You cant land in an occupied cell!'), nl,
    fail.

myDebug(ShipToMove, PieceToMove, DestinationPiece, NewPiece, OldPiece):-
    write('This is the selected ship: '),
    write(ShipToMove), nl,
    write('This is the piece to move: '),
    write(PieceToMove), nl,
    write('This is the destination piece: '),
    write(DestinationPiece), nl,
    write('This is the new piece: '),
    write(NewPiece), nl,
    write('This is the old piece: '),
    write(OldPiece), nl.

% Reads player input
readPlayerInput(Board, WhoIsPlaying, OldPiece, NewPiece, PieceToMove, PieceToMoveRow, PieceToMoveColumn, DestinationPiece, DestinationRow, DestinationColumn):-
    !,
    repeat,
    write('Select ship'), nl,
    read(UserShipToMove), nl,
    assignShip(UserShipToMove, ShipToMove),
    canPlayerMoveSelectedShip(WhoIsPlaying, ShipToMove),

    getBoardPieces(Board, PieceToMove),
    systemHasShip(ShipToMove, PieceToMove),
    getPiece(PieceToMoveRow, PieceToMoveColumn, Board, PieceToMove),

    !,
    repeat,
    
    write('Select direction to travel'), nl,
    read(Direction), nl,

    write('Select number of cells to travel'), nl,
    read(NumOfCells), nl,
    
    moveNCellsInDirection(PieceToMoveColumn, PieceToMoveRow, Direction, NumOfCells, DestinationColumn, DestinationRow),

    (getPiece(DestinationRow, DestinationColumn, Board, DestinationPiece)
    ;
    (write('There is no cell in those coordinates.'), nl, fail)),

    % check if the destination cell is a valid one
    checkValidLandingCell(DestinationPiece),

    !,
    repeat,
    format('Player ~p, what building would you like to construct?~n   t --> Trade Station~n   c --> Colony~n', [WhoIsPlaying]),
    read(UserBuilding),
    assignBuilding(UserBuilding, Building),
    checkValidBuilding(Building),

    setPieceToMove(PieceToMove, DestinationPiece, ShipToMove, Building, NewPiece, 0),
    removeShipFromPiece(PieceToMove, ShipToMove, OldPiece).

% Updates board
updateBoard(Board, OldPiece, NewPiece, PieceToMove, PieceToMoveRow, PieceToMoveColumn, DestinationPiece, DestinationRow, DestinationColumn, UpdatedBoard):-
    replace(PieceToMove, OldPiece, PieceToMoveRow, PieceToMoveColumn, Board, BoardChange1),
    replace(DestinationPiece, NewPiece, DestinationRow, DestinationColumn, BoardChange1, UpdatedBoard).

% Returns only the valid adjacent cells. The X list is D, and the Y list is C
restrictValidCells(_, _, [], [], A, B, A, B).
restrictValidCells(Board, NumOfRows, [Y|Ys], [X|Xs], A, B, C, D):-
    X >= 0,
    Y >= 0,
    getColumnLength(Board, X, NumOfColumns),
    X < NumOfColumns,
    Y < NumOfRows,
    restrictValidCells(Board, NumOfRows, Ys, Xs, [Y|A], [X|B], C, D);
    restrictValidCells(Board, NumOfRows, Ys, Xs, A, B, C, D).


getBestCellToMoveTo(Board):-
    getAdjacentList(2, 0, AdjacentListX, AdjacentListY),

    length(Board, NumOfRows),
    restrictValidCells(Board, NumOfRows, AdjacentListY, AdjacentListX, [], [], ValidListY, ValidListX).

calculateBestMove(Board):-
    getBestCellToMoveTo(Board).

% Does AI turn
playerTurn(Board, ai, UpdatedBoard):-
    %write('*************** AI turn ***************'), nl, nl,
    %display_board(Board), nl, nl,

    getAllPossibleCellsToMove(Board, 2, 6, ListX, ListY),
    write('Total:'), nl,
    writeXY(ListX, ListY),

    %calculateBestMove(ListX, ListY),
    UpdatedBoard = Board.
    %clearScreen(60).

% Does player turn
playerTurn(Board, WhoIsPlaying, UpdatedBoard):-
    format('*************** Player ~d turn *************** ~n~n', [WhoIsPlaying]),
    display_board(Board), nl, nl,

    readPlayerInput(Board, WhoIsPlaying, OldPiece, NewPiece, PieceToMove, PieceToMoveRow, PieceToMoveColumn, DestinationPiece, DestinationRow, DestinationColumn),
    updateBoard(Board, OldPiece, NewPiece, PieceToMove, PieceToMoveRow, PieceToMoveColumn, DestinationPiece, DestinationRow, DestinationColumn, UpdatedBoard),
   
    clearScreen(60).