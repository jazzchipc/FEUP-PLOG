:- use_module(library(lists)).
:- include('board.pl').
:- include('utils.pl').
:- include('logic.pl').

startGame:- 
    write('The game started!'),
    nl, nl,
    playerTurn(1).