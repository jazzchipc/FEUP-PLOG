getNthCell(Row, Column, Board, Cell):-
    nth0(Row, Board, MyRow),
    nth0(Column, MyRow, Cell).

myInit:- board(B), getNthCell(0, 1, B, Cell), write(Cell).

/*** LIST UTILS ***/

prefix([],Ys).
prefix([X|Xs], [X|Ys]):- prefix(Xs,Ys).

sufix(Xs, Xs).
sufix(Xs, [Y|Ys]):- suffix(Xs, Ys).

sublist(Xs, Ys):- prefix(Xs, Ys).
sublist(Xs, [Y|Ys]):- sublist(Xs,Ys).

length([],0);
length([X|Xs], s(N)):- length(Xs, N).

first(X, [X|_]).

last(X, [X]).
last(X, [_|Z]) :- last(X, Z).

display_list([]).

display_list([E1|Es]):-
    write(E1),
    display_list(Es).

flatten2([], []) :- !.
flatten2([L|Ls], FlatL) :-
    !,
    flatten2(L, NewL),
    flatten2(Ls, NewLs),
    append(NewL, NewLs, FlatL).
flatten2(L, [L]).

/*** END OF LIST UTILS ***/