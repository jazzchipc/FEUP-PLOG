get_longest_list([], MaxSize):-
    MaxSize is 0.

get_longest_list([L|Ls], MaxSize):-
    length(L, MaxSize),
    MaxSize1 is MaxSize,
    MaxSize is max(MaxSize, get_longest_list([Ls], MaxSize1)).