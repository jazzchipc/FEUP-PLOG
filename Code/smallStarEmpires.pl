:- include('logic.pl').

startGame:- 
    write('The game started!'),
    nl, nl,

    initial_logic_board(Board),

    repeat,
        playerTurn(Board, 1, UpdatedBoard),
        playerTurn(UpdatedBoard, 1, UpdatedBoard2),
        playerTurn(UpdatedBoard2, 1, UpdatedBoard3).