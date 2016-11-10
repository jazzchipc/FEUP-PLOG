:- include('logic.pl').

startGame:- 
    write('The game started!'),
    nl, nl,

    initial_logic_board(Board),
    playGame(Board, 1).

playGame(Board, WhoIsPlaying):-
        playerTurn(Board, WhoIsPlaying, UpdatedBoard),
        !, 
        WhoIsPlaying == 1,
        playGame(UpdatedBoard, 2);
        playGame(UpdatedBoard, 1).