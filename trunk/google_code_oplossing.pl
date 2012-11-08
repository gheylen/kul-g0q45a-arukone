% Arukone in SWI-Prolog Copyright (c) 2011 Glenn Heylen
% 
% This file is part of Arukone in SWI-Prolog.
% 
% Arukone in SWI-Prolog is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
% 
% Arukone in SWI-Prolog is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with Arukone in SWI-Prolog.  If not, see <http://www.gnu.org/licenses/>.

% Solution to practicum of course G0Q45a @ Katholieke Universiteit Leuven, Belgium.

:- ensure_loaded(puzzels).
:- ensure_loaded(visualisatie).

% solve/2
solve(PuzzleId, Solution) :-
    puzzle(PuzzleId, Grid, Links),
    arukone(Grid, Links, Solution).
	
% arukone/3
% First, we create a functor to remember all the positions of the grid used/marked/selected/part of connection/path.
% Secondly, we fill the functor's variables with the input data (link positions).
% Thirdly, we order the input links basic on several heuristics. Orderings are based on which links are
% 	easier to connect, and on which links are less prone to creating an invalid gridstate (state in which
%		other connections are impossible).
% At last, we try to find a solutions for the grid.
% Consider the fact that every puzzle has a single result, we *could* put a "cut" at the end of this
% 	predicate. ("!")
arukone(Grid, Links, Solution) :-
	grid_as_functor(Grid, MarkedNodesFunctor),
    grid_functor_with_init_links(Grid, Links, MarkedNodesFunctor),
    links_optimization_sort(Links, Grid, OrderedLinks),
    solution_for_all_links(Grid, OrderedLinks, MarkedNodesFunctor, Solution).


% Create a functor from the input "grid", which will be used throughout the code
% 	to mark positions which have been selected already as a part of a connection for a link.
grid_as_functor(grid(X, Y), MarkedNodesFunctor) :-
    Size is X * Y,
    functor(MarkedNodesFunctor, marked, Size).

% Fill the functor's variables with the start and end position of the link for which we will find a connection.
% Thus, hereby invalidating those positions as (im)possible neighbours to extend a subpath when searching for a path (connection).
grid_functor_with_init_links(_, [], _).
grid_functor_with_init_links(Grid, [link(_, FirstPos, SecondPos) | Links], MarkedNodesFunctor) :-
    mark_grid_pos(FirstPos, Grid, MarkedNodesFunctor),
    mark_grid_pos(SecondPos, Grid, MarkedNodesFunctor),
    grid_functor_with_init_links(Grid, Links, MarkedNodesFunctor).

% Basic predicate which describes whether or not two positions are to be defined as "neighbours".
% In our case, this means whether or not they are located next to eachother without considering the
%	diagonal relation. (Only horizontal and vertical)
neighbour(pos(X, Y), pos(X2, Y), _) :-
    X > 1,
    X2 is X - 1.
neighbour(pos(X, Y), pos(X2, Y), grid(N, _)) :-
    X < N,
    X2 is X + 1.
neighbour(pos(X, Y), pos(X, Y2), _) :-
    Y > 1,
    Y2 is Y - 1.
neighbour(pos(X, Y), pos(X, Y2), grid(_, N)) :-
    Y < N,
    Y2 is Y + 1.
	
% Basic predicate which will mark a given position in the given functor as selected/marked/used/part of a path.
mark_grid_pos(Pos, grid(X, _), MarkedNodesFunctor) :-
    pos_coord_to_grid_functor_index(Pos, X, Index),
    arg(Index, MarkedNodesFunctor, true).

% Basic predicate which will determine whether or not a given position is still available to be marked/used/selected.
grid_pos_marked(Pos, grid(_, Y), MarkedNodesFunctor) :-
    pos_coord_to_grid_functor_index(Pos, Y, Index),
    arg(Index, MarkedNodesFunctor, Result),
    Result == true.

% Basic predicate which will convert a given position to the correct index of the variable to be marked.
pos_coord_to_grid_functor_index(pos(X, Y), GridWidth, Index) :-
	Index is ((X - 1) * GridWidth) + Y.

% Loop in which we will use backtracking to determine the solution for the resp. puzzle.
% We will try to find valid connections for all the given links by using the dimensions of the grid,
% 	and the currently already marked/selected/used positions of that same grid.
solution_for_all_links(_, [], _, []).
solution_for_all_links(Grid, [link(LinkId, FirstPos, SecondPos)|Links], MarkedNodesFunctor, [connects(LinkId, PathForLink)|Paths]) :-
    solution_for_single_link(Grid, FirstPos, SecondPos, MarkedNodesFunctor, PathForLink),
    solution_for_all_links(Grid, Links, MarkedNodesFunctor, Paths).

% Helper predicate which will find a possible connection for a given link. If not possible, backtracking
% 	will occur in the calling predicate ("solution_for_all_links") to find alternative solutions for
%		previous calculated connections for other links.
% Every valid puzzle has a solution, so backtracking will eventually find a working/valid/legal combination
%	of connections/paths for every link of the resp. puzzle.
% A connection/path is considered to be found whenever the current node/position is a neighbour of the 
%	node/pos which we are trying to reach.
solution_for_single_link(Grid, FromPos, ToPos, _, [FromPos, ToPos]) :-
    neighbour(FromPos, ToPos, Grid).
solution_for_single_link(Grid, FromPos, ToPos, MarkedNodesFunctor, [FromPos|PathSoFar]) :-
    \+ neighbour(FromPos, ToPos, Grid),
    next_best_pos_to_extend_path(FromPos, ToPos, Grid, MarkedNodesFunctor, NextPos),
    mark_grid_pos(NextPos, Grid, MarkedNodesFunctor),
    solution_for_single_link(Grid, NextPos, ToPos, MarkedNodesFunctor, PathSoFar).



% Select the next best neighbour (by heuristic evaluation) for the current subpath (FromPos) considering
%	we are trying to reach ToPos. 	
next_best_pos_to_extend_path(FromPos, ToPos, Grid, MarkedNodesFunctor, NextBestPos) :-
    findall(
		(FromPos, ToPos, Grid, NextBestPos),
        (neighbour(FromPos, NextBestPos, Grid), \+ grid_pos_marked(NextBestPos, Grid, MarkedNodesFunctor)),
		Neighbours
	),
	predsort(ordering_touching_border_first, Neighbours, SortedNeighbours),
    member((FromPos, ToPos, Grid, NextBestPos), SortedNeighbours).

% Whether or not the given position ("pos") is located on the border of the grid.
touching_border(pos(1, _), _).
touching_border(pos(X, 1), _) :- 
    X \== 1.
touching_border(pos(X, Y), grid(X, _)) :-
    Y \== 1.
touching_border(pos(X, Y), grid(XDim, Y)):- 
    X \== 1,
    X \== XDim.

% Whether or not both given positions are located on the border of the grid.
both_touching_border(link(_, From, To), Grid) :-
    touching_border(From, Grid),
    touching_border(To, Grid).
	
% This will optimize the order in which the links are selected to find a path/connections for that resp. link.
% This predicate will be used in the main predicate "arukone", but it is completely optional.
% An advantage over the overhead of this code will be achieved when solving the bigger/more difficult puzzles.
links_optimization_sort(Links, Grid, OrderedLinks) :-
    findall((Link, Grid), member(Link, Links), NewLinks),
    predsort(ordering_links_all_combined, NewLinks, TmpOrderedLinks),
    findall(Link, member((Link, Grid), TmpOrderedLinks), OrderedLinks).

% This will call multiple (ordered) ordering-predicates whenever one doesn't offer a decisive answer.
% This means that whenever a comparison returns "equal", a next ordering-pred will be called, until we get
% 	a conclusive result.
mutliple_orderings_combined([P|Redicates], Pred1, Pred2, D) :-
    Callable =.. [P, D2, Pred1, Pred2],
    call(Callable),
    ( D2 == (=) ->
        mutliple_orderings_combined(Redicates, Pred1, Pred2, D)
    ;
        D = D2
    ).

% Ordering which will prioritize positions on the grid which are located on the border of the resp. grid.
ordering_touching_border_first(>, (_, _, Grid, _), (_, _, Grid, PossibleNeighbour)) :-
    touching_border(PossibleNeighbour, Grid).
ordering_touching_border_first(<, (_, _, Grid, _), (_, _, Grid, PossibleNeighbour)) :-
    \+ touching_border(PossibleNeighbour, Grid).

% A combination of different sorting heuristics. (The two orderings directly underneath this predicate)
ordering_links_all_combined(D, Link1, Link2) :-
        AllSorts = [ordering_link_both_pos_along_border, ordering_links_longest_path_first],
            mutliple_orderings_combined(AllSorts, Link1, Link2, D).
	
% Shortcut heuristic for connections between links which will probably be completely along the border.
ordering_link_both_pos_along_border(<, (Link, Grid), _) :-
    both_touching_border(Link, Grid).
ordering_link_both_pos_along_border(>, (Link1, Grid), (Link2, Grid)) :-
    \+ both_touching_border(Link1, Grid),
    both_touching_border(Link2, Grid).
ordering_link_both_pos_along_border(=, (Link1, Grid), (Link2, Grid)) :-
    \+ both_touching_border(Link1, Grid),
    \+ both_touching_border(Link2, Grid).

% Manhattan heuristic used to sort links.
ordering_links_longest_path_first(D,
                             (link(_, pos(X1, Y1), pos(X2, Y2)), _),
                             (link(_, pos(X11, Y11), pos(X21, Y22)), _)) :-
    PathLength1 is abs(X1 - X2) + abs(Y1 - Y2),
    PathLength2 is abs(X11 - X21) + abs(Y11 - Y22),
    compare_shortest_first(D, PathLength2, PathLength1).

compare_shortest_first(<, PathLength2, PathLength1) :-
	PathLength2 < PathLength1.
compare_shortest_first(>, PathLength2, PathLength1) :-
	PathLength2 >= PathLength1.
compare_shortest_first(=, PathLength2, PathLength1) :-
	PathLength2 == PathLength1.
