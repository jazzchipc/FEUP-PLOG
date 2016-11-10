:- include('logic.pl').

startGame:- 
    write('The game started!'),
    nl, nl,

    initial_logic_board(Board),
    playGame(Board, 2).

playGame(Board, 1):-
        playerTurn(Board, 1, UpdatedBoard),
        !,
        playGame(UpdatedBoard, 2).
playGame(Board, 2):-
        playerTurn(Board, 2, UpdatedBoard),
        !,
        playGame(UpdatedBoard, 1).