:- include('logic.pl').

startGame:- 
    nl, nl,
    write('**************************************************************************************'), nl,
    write('********************************* SMALL STAR EMPIRES *********************************'), nl,
    write('**************************************************************************************'), nl,
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