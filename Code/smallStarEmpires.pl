:- include('logic.pl').

startGame:- 
    write('The game started!'),
    nl, nl,

    playerTurn(1).