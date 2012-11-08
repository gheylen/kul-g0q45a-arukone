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

% show_puzzle/1
show_puzzle(PuzzleId) :-
    puzzle(PuzzleId,Grid,Links),
    generateEmptyGrid(Grid,Array),
    addLinksToGrid(Links,Array),
    showGrid(Array).

% show_solution/1
show_solution(PuzzleId) :-
    puzzle(PuzzleId,Grid,_),
    generateEmptyGrid(Grid,Array),
    solve(PuzzleId,Solution),
    addSolutionToGrid(Solution,Array),
    showGrid(Array).

% generateEmptyGrid/2
generateEmptyGrid(grid(H,W),Array) :-
    length(Array,H),
    maplist(lengthReverse(W),Array).

% lengthReverse/2
lengthReverse(Length,List) :-
    length(List,Length).

% addLinksToGrid/2
addLinksToGrid([],_).

% addLinksToGrid/2
addLinksToGrid([link(Label,pos(R1,C1),pos(R2,C2))|RestOfLinks],Rows) :-
    nth1(R1,Rows,Row1),
    nth1(C1,Row1,Label),
    nth1(R2,Rows,Row2), 
    nth1(C2,Row2,Label),
    addLinksToGrid(RestOfLinks,Rows).

% addSolutionToGrid/2
addSolutionToGrid([],_).

% addSolutionToGrid/2
addSolutionToGrid([connects(_,[])|RestOfSolution],Rows) :-
    addSolutionToGrid(RestOfSolution,Rows).

% addSolutionToGrid/2
addSolutionToGrid([connects(Label,[pos(R,C)|RestOfPath])|RestOfSolution],Rows) :-
    nth1(R,Rows,Row),
    nth1(C,Row,Label),
    addSolutionToGrid([connects(Label,RestOfPath)|RestOfSolution],Rows).

% showCell/1
showCell(Cell) :-
    ( var(Cell) -> 
        write(' _')
    ; 
        writef(' %w',[Cell])
    ).

% showRow/1
showRow(Row) :-
    maplist(showCell,Row), 
    nl.

% showGrid/1
showGrid(Rows) :-
    nl, 
    maplist(showRow,Rows), 
    nl.

