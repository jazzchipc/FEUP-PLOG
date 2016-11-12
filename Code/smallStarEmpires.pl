:- include('logic.pl').

assignGameMode(1, playerVSplayer).
assignGameMode(2, playerVSai).
assignGameMode(3, aiVSai).

displayMenu(GameMode):-
        nl, nl,
        write('**************************************************************************************'), nl,
        write('********************************* SMALL STAR EMPIRES *********************************'), nl,
        write('**************************************************************************************'), nl,
        nl, nl,

        write('***** Main Menu *****'), nl, nl,
        write('Player VS Player --> Type 1'), nl,
        write('Player VS AI --> Type 2'), nl,
        write('AI VS AI --> Type 3'), nl, nl,
        write('Select Game Mode'), nl,
        read(UserGameMode), nl,
        assignGameMode(UserGameMode, GameMode).

askForYoungestPlayer(YoungestPlayer):-
        write('Who is the youngest player?'), nl,
        write('Player 1 --> 1'), nl,
        write('Player 2 --> 2'), nl,
        read(YoungestPlayer), nl, nl.

startGame:- 
    displayMenu(GameMode),
    playGameMode(GameMode).

playGameMode(playerVSplayer):-
        askForYoungestPlayer(YoungestPlayer),
        write('********************* THE BATTLE IS ON! *********************'), nl, nl,
        initial_logic_board(Board),
        \+(playPlayerPlayer(Board, YoungestPlayer)),
        !,
        getTotalScoreOfPlayer(player1, Board, TotalScore1),
        format('Player 1 has a score of ~d points.~n', [TotalScore1]),
        
        getTotalScoreOfPlayer(player2, Board, TotalScore2),
        format('Player 2 has a score of ~d points.~n', [TotalScore2])
        .

playGameMode(playerVSai):-
        initial_logic_board(Board),
        %StartingPlayer is random(1),
        playPlayerAI(Board, ai).

playGameMode(aiVSai):-
        initial_logic_board(Board),
        playGame(Board, 2).

playPlayerPlayer(Board, WhoIsPlaying):-
        (\+ endGame(Board)),

        (WhoIsPlaying == 1,
        playerTurn(Board, 1, UpdatedBoard),
        !,
        playPlayerPlayer(UpdatedBoard, 2);
        
        playerTurn(Board, 2, UpdatedBoard),
        !,
        playPlayerPlayer(UpdatedBoard, 1)).

playPlayerAI(Board, WhoIsPlaying):-
        (\+ endGame(Board)),

        WhoIsPlaying == 1,
        playerTurn(Board, 1, UpdatedBoard),
        !,
        playPlayerAI(UpdatedBoard, ai);
        
        playerTurn(Board, ai, UpdatedBoard),
        !,
        playPlayerAI(UpdatedBoard, 1).

