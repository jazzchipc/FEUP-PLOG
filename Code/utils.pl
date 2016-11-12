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

list_sum([Item], Item).
list_sum([Item1,Item2 | Tail], Total) :-
    list_sum([Item1+Item2|Tail], Total).

% Writes the whole list
writeList([]):-
    write('Vazio'),
    nl.
writeList([X|Xs], N):-
    format('Element num ~d: ~d~n', [N, X]),
    N1 is N + 1,
    writeList(Xs, N1).

% Gets the length of the selected row
getColumnLength([X|Xs], 0, Length):-
    length(X, Length).
getColumnLength([X|Xs], Row, Length):-
    NewRow is Row - 1,
    getColumnLength(Xs, NewRow, Length).

/*** END OF LIST UTILS ***/