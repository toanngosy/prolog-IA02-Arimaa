:- module(bot,
      [  get_moves/3
      ]).
	
% A few comments but all is explained in README of github

% get_moves signature
% get_moves(Moves, gamestate, board).

% Exemple of variable
% gamestate: [side, [captured pieces]] (e.g. [silver, [ [0,1,rabbit,silver],[0,2,horse,silver] ]) 
% board: [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]

% Call exemple:
% get_moves(Moves, [silver, []], [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).

% default call

%get_type([_,_,Y,_], Y).

%get_moves([[[2,0],[3,0]],[[1,0],[2,0]],[[1,1],[2,1]],[[2,1],[2,2]]], Gamestate, Board).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%fundamental function%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%list in lists
member(X, [X|_]) :- !.
member(L, [_|Q]) :- member(L,Q). 

%list_length
listLength([], 0).
listLength([_|Q], L) :- listLength(Q, N), L is N+1.
%empty list
is_empty_list(L) :- listLength(L, Length), Length = 0.
%is not empty list
is_not_empty_list(L) :- listLength(L, Length), Length =\= 0.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%describing function%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%board
board_height(8).
board_width(8).
%in-board
in_board(Coordinate) :- get_x(Coordinate, X), get_y(Coordinate, Y), X >= 0, Y >= 0, board_height(H), board_width(W), X < H, Y < W.

%traps
traps([[2,2], [2,5], [5,2], [5,5]]).

%directions
west([-1, 0]).
east([1, 0]).
north([0, -1]).
south([0, 1]).
directions([[-1,0], [1,0], [0,-1], [0,1]]).

%silver homerow
silver_homerow(0).
%gold homerow
gold_homerow(X) :- board_height(H), X is H-1.

%Piece type
type([_,_,X,_], X).
is_rabbit([_,_,rabbit,_]).
is_cat([_,_,cat,_]).
is_dog([_,_,dog,_]).
is_horse([_,_,horse,_]).
is_camel([_,_,camel,_]).
is_elephant([_,_,elephant,_]).

%force(Piece,F) --- return Piece Force 
force([_,_,rabbit,_], 1).
force([_,_,cat,_], 2).
force([_,_,dog,_], 3).
force([_,_,horse,_], 4).
force([_,_,camel,_], 5).
force([_,_,elephant,_], 6).
force(rabbit, 1).
force(cat, 2).
force(dog, 3).
force(horse, 4).
force(camel, 5).
force(elephant, 6).
 


%get x, y coordination
get_x([X,_], X).
get_x([X,_,_,_],X).
get_y([_,Y], Y).
get_y([_,Y,_,_],Y).

%get coordination of a piece
get_coordination([X, Y], [X, Y]).
get_coordination([X, Y, _, _], [X, Y]).

%is in trap
is_in_trap(Coordinate) :- traps(Trap), member(Coordinate,Traps),!. 

%get_piece -> get piece from coordinate and Board - line 
get_piece_at_coordinate(X, Coordinate) :- board(B), get_piece(X, Coordinate, B).
get_piece([], _, []).
get_piece(T, Coordinate, [T|_]) :- get_x(Coordinate, X1), get_y(Coordinate, Y1), get_x(T, X), get_y(T, Y), X =:= X1, Y =:= Y1 ,!.
get_piece(R, Coordinate, [_|Q]) :- get_piece(R, Coordinate, Q).

%is_stronger
is_stronger(Piece1, Piece2) :- force(Piece1,F1), force(Piece2,F2), F1>F2.

%is_friendly between 2 pieces
is_friendly([_,_,_,X], [_,_,_,X]).

%get_neighbour_coordinate -> get coordinate on 1 direction (N or S or W or E) 
get_neighbour_coordinate([X,Y], Coordinate, Direction) :- get_x(Coordinate, XC), get_y(Coordinate, YC), get_x(Direction, XD), get_y(Direction, YD),
															A is XC+XD, B is YC+YD, is_in_board([A,B]), X is A, Y is B,!. 
get_neighbour_coordinate([], _, _) :- !.  

%get_neighbour -> get piece on that coordinate
get_neighbour(Piece, Coordinate, Direction) :- get_neighbour_coordinate(NeiCoordinate, Coordinate, Direction), 
												is_not_empty_list(NeiCoordinate), get_piece_at_coordinate(Piece, NeiCoordinate), 
												is_not_empty_list(Piece),!.
get_neighbour([], _, _) :- !.

%is_near_ally
is_near_ally(Coordinate) :- directions(Directions), ina(Coordinate, Directions).
ina(_,[]) :- fail, !.
ina(Coordinate, [T|_]) :- get_piece_at_coordinate(P, Coordinate), is_not_empty_list(P),
									get_neighbour(Nei, Coordinate, T), is_not_empty_list(Nei), is_friendly(Nei,P),!.
ina(Coordinate, [_|Q]) :- ina(Coordinate, Q). 

%is_near_enemy_stronger
is_near_enemy_stronger(Coordinate) :- directions(Directions), ines(Coordinate, Directions).
ines(_,[]) :- fail, !.
ines(Coordinate, [T|_]) :- get_piece_at_coordinate(P, Coordinate), is_not_empty_list(P),
									get_neighbour(Nei, Coordinate, T), is_not_empty_list(Nei), \+is_friendly(Nei,P), is_stronger(Nei, P),!.
ines(Coordinate, [_|Q]) :- ines(Coordinate, Q). 

%is_near_enemy_weaker
is_near_enemy_weaker(Coordinate) :- directions(Directions), inew(Coordinate, Directions).
inew(_,[]) :- fail, !.
inew(Coordinate, [T|_]) :- get_piece_at_coordinate(P, Coordinate), is_not_empty_list(P),
									get_neighbour(Nei, Coordinate, T), is_not_empty_list(Nei), \+is_friendly(Nei,P), is_stronger(P, Nei),!.
inew(Coordinate, [_|Q]) :- ines(Coordinate, Q). 

%is_frozen
is_frozen(Coordinate) :- get_piece_at_coordinate(P, Coordinate), is_not_empty_list(P), 
							\+is_near_ally(Coordinate), is_near_enemy_stronger(Coordinate). 
							
%same_coordinates(a, b) 
same_coordinates(C1, C2) :- get_x(C1,X1), get_y(C1,Y1), get_x(C2,X2), get_y(C2,Y2), X1 =:= X2, Y1 =:= Y2.
 




 













