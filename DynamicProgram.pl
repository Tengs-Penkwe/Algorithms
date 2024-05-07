

denomination(365).
denomination(91).
denomination(52).
denomination(28).
denomination(13).
denomination(7).
denomination(4).
denomination(1).

denominations([365, 91, 52, 28, 13, 7, 4, 1]).

max_denomination(N, Smallest) :-
    denominations(Values), % Retrieve the list of values
    findall(Value, (member(Value, Values), Value =< N), Filtered), % Find all values greater than N
    max_list(Filtered, Smallest). % Find the minimum of those values

replicate(Term, Length, List):-
    length(List, Length),
    maplist(=(Term), List).

greedyChange(0, []) :- !.
greedyChange(Amount, _):-
    Amount < 0, !, fail.
greedyChange(Amount, Change) :-
    % max_denomination(Amount, D),
    denomination(D),
    D =< Amount,
    Count is Amount // D,
    Reminder is Amount mod D,
    greedyChange(Reminder, RestChange),
    replicate(D, Count, Replicated),
    append(Replicated, RestChange, Change).

bestChange(0, []) :- !.
bestChange(Amount, _):-
    Amount < 0, !, fail.
bestChange(Amount, Change) :-
    denominations(Values),
