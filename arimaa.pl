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

board([[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).

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

%deleteElement : delete the first occurence of the element
deleteElement(_, [], []).
deleteElement(X,[X|Q],Q).
deleteElement(X,[T|Q],[T|R]):-deleteElement(X,Q,R).

%deleteElement : delete all occurences of the element
deleteElements(X, L, L):- \+ member(X,L).
deleteElements(X, L, R):-deleteElement(X,L,R1), deleteElements(X,R1,R).

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

%Piece side
ally(silver).
enemy(gold).
side([_,_,_,X], X).


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
                                                                                                                        A is XC+XD, B is YC+YD, in_board([A,B]), X is A, Y is B,!.
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

%get_piece_from_side(Board, Side, SidePiece)
get_piece_from_side([[Row, Col, Type, Side]|B], Side,[[Row, Col, Type, Side]|Q]) :- get_piece_from_side(B, Side, Q), !.
get_piece_from_side([_|B], Side, Q) :- get_piece_from_side(B, Side, Q).
get_piece_from_side([], Side, []).

%get_piece_from_type(B, Type, Res)
get_piece_from_type([[Row, Col, Type, Side]|B], Type, [[Row, Col, Type, Side]|Q]) :- get_piece_from_type(B, Type, Q), !.
get_piece_from_type([_|B], Type, Q) :- get_piece_from_type(B, Type, Q).
get_piece_from_type([], Type, []).

%get_piece_from_row(Board, Row, RowPiece)
get_piece_from_row([[Row, Col, Type, Side]|B], Row,[[Row, Col, Type, Side]|Q]) :- get_piece_from_row(B, Row, Q), !.
get_piece_from_row([_|B], Row, Q) :- get_piece_from_row(B, Row, Q).
get_piece_from_row([], Row, []).

%get_max_row_piece(B, Type, Side, Res)
get_max_row_piece(B, Type, Side, Res) :- get_piece_from_side(B, Side, SPiece), get_piece_from_type(SPiece, Type, TSPiece),
                        get_max_row_piece2(8, TSPiece, Res).

%get_max_row_piece(B, Side, Res)
get_max_row_piece(B, Side, Res) :- get_piece_from_side(B, Side, SPiece),
                        get_max_row_piece2(8, SPiece, Res).

%get_max_row_piece2(Row, B, Res)
get_max_row_piece2(Row, B, Res) :- Row < 0, fail,!.
get_max_row_piece2(Row, B, Res) :- get_piece_from_row(B, Row, X), X\=[], Res = X, !.
get_max_row_piece2(Row, B, Res) :- RowTmp is Row-1, get_max_row_piece2(RowTmp, B, Res).

%get_piece_order_by_row_desc(B, Type, Side, Res)
%Res = [T|Q] => T is the list of piece from B at max row
get_piece_order_by_row_desc(B, Type, Side, Res) :- get_piece_from_side(B, Side, SPiece), get_piece_from_type(SPiece, Type, TSPiece),
                        get_piece_order_by_row_desc(8, TSPiece, ResTmp), deleteElements([], ResTmp, Res).
get_piece_order_by_row_desc(Row, B, []) :- Row < 0,!.
get_piece_order_by_row_desc(Row, B, [X|Res]) :- get_piece_from_row(B, Row, X), RowTmp is Row-1, get_piece_order_by_row_desc(RowTmp, B, Res).

%get_strongest_neighbour
%get_strongest_neighbour(Coord, Side, Nei) :- get_one_step_moves(Coord, NeiCases), !,

%:- dynamic moves/1.
add_move(NewMove) :-  moves(M), retract(moves(M)), asserta(moves([NewMove|M])).

%call get_possible_one_step_moves with the chosen move to generate move of 2 or more steps
get_possible_one_step_moves(Cood, MovePossible) :- get_one_step_moves(Cood, Moves), get_possible_one_step_moves(Cood, Moves, MovePossible), !.

%get_possible_one_step_moves(Cood, Moves, MovePossible)
get_possible_one_step_moves([X,Y], _, []) :- is_frozen([X,Y]), !.
get_possible_one_step_moves(_, [], []).
get_possible_one_step_moves([X,Y], [T|MovesInit], [T|MovePossible]) :-  get_piece_at_coordinate(Piece, T), is_empty_list(Piece),
                                                                is_in_trap(T), is_near_ally(T), get_possible_one_step_moves([X,Y], MovesInit, MovePossible), !.
get_possible_one_step_moves([X,Y], [T|MovesInit], [T|MovePossible]) :- get_piece_at_coordinate(Piece, T), is_empty_list(Piece), \+is_in_trap(T),
                                                                get_possible_one_step_moves([X,Y], MovesInit, MovePossible),!.
get_possible_one_step_moves([X,Y], [T|MovesInit], [T|MovePossible]) :- get_piece_at_coordinate([_, _, TypeNei, SideNei], T), Side = gold,
                                                                get_piece_at_coordinate([_, _, Type, Side], [X,Y]), is_stronger(Type, TypeNei),
                                                                get_possible_one_step_moves([X,Y], MovesInit, MovePossible), !.
get_possible_one_step_moves([X,Y], [T|MovesInit], MovePossible) :- get_possible_one_step_moves([X,Y], MovesInit, MovePossible).

%get_one_step_moves(Cood, Moves)
get_one_step_moves([X,Y], Moves) :- get_south_move([X,Y], S), get_north_move([X,Y], N), get_east_move([X,Y], E), get_west_move([X,Y], W),
                                                                asserta(moves([])), add_move(S), add_move(N), add_move(E), add_move(W), moves(Temp), deleteElements([], Temp, Moves).

get_south_move([X,Y], [Xres, Yres]) :- south(Direction), get_x(Direction, XD), get_y(Direction, YD), Xres is X+XD, Yres is Y+YD,in_board([Xres, Yres]),!.
get_south_move(_,[]).
get_north_move([X,Y], [Xres, Yres]) :- north(Direction), get_x(Direction, XD), get_y(Direction, YD), Xres is X+XD, Yres is Y+YD,in_board([Xres, Yres]),!.
get_north_move(_,[]).
get_east_move([X,Y], [Xres, Yres]) :- east(Direction), get_x(Direction, XD), get_y(Direction, YD), Xres is X+XD, Yres is Y+YD,in_board([Xres, Yres]),!.
get_east_move(_,[]).
get_west_move([X,Y], [Xres, Yres]) :- get_piece_at_coordinate([_, _, Type, _], [X,Y]), Type \= rabbit, west(Direction), get_x(Direction, XD), get_y(Direction, YD), Xres is X+XD, Yres is Y+YD,in_board([Xres, Yres]),!.
get_west_move(_,[]).

%test(X, Side) :- board(B), get_max_row_piece(B, Side, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% PUSH MOVE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_enemy_near_trap([X,Y, _, Side], TrapHole) :- enemy(Side), directions(Directions), traps(Traps), isen([X,Y], Directions, Traps, TrapHole).
isen(_, [], _, _) :- !, fail.
isen([X,Y], [[Xd, Yd]|_], Traps, [Xt, Yt]) :- Xt is X+Xd, Yt is Y+Yd, member([Xt, Yt], Traps), !.
isen([X,Y], [[Xd, Yd]|Q], Traps, Trap) :- Xt is X+Xd, Yt is Y+Yd, \+member([Xt, Yt], Traps), isen([X,Y], Q, Traps, Trap).

count_enemy_piece_around_trap(TrapHole, N) :- directions(Directions), cgpat(TrapHole, Directions, N), !.
cgpat(_, [], 0) :- !.
cgpat(TrapHole, [Hd|Td], N) :- get_neighbour(Piece, TrapHole, Hd), is_empty_list(Piece), cgpat(TrapHole, Td, N).
cgpat(TrapHole, [Hd|Td], N) :- get_neighbour(Piece, TrapHole, Hd), 
								is_not_empty_list(Piece), side(Piece, Side), ally(Side), cgpat(TrapHole, Td, N).
cgpat(TrapHole, [Hd|Td], N) :- get_neighbour(Piece, TrapHole, Hd), 
								is_not_empty_list(Piece), side(Piece, Side), enemy(Side), cgpat(TrapHole, Td, M), N is M+1.  

neighbour_silver_stronger(PieceVictim, PiecePush) :- directions(Directions), nss(PieceVictim, PiecePush, Directions).
nss(_, [], []) :- !.
nss(PieceVictim, PiecePush, [Hd|_]) :- get_coordination(PieceVictim, Cv), get_neighbour(PiecePush, Cv, Hd), is_not_empty_list(PiecePush), 
										side(PiecePush, Side), ally(Side), is_stronger(PiecePush, PieceVictim), !. 
nss(PieceVictim, PiecePush, [_|Td]) :- nss(PieceVictim, PiecePush, Td).

is_pushable(PieceVictim, PiecePush) :- is_enemy_near_trap(PieceVictim, TrapHole), count_enemy_piece_around_trap(TrapHole, N), N =:= 1,
										neighbour_silver_stronger(PieceVictim, PiecePush).

push_detect(Victim, Push) :- board(Board), enemy(Side), get_piece_from_side(Board, Side, TotalPiece), pd(Victim, Push, TotalPiece).
pd(_, _, []) :- !, fail.
pd(Victim, Push, [Victim|_]) :- is_pushable(Victim, Push), is_not_empty_list(Push),!.
pd(Victim, Push, [_|Q]) :- pd(Victim, Push, Q).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% APPEL A PUSH MOVE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%move_push(M) :- asserta(moves([])), push_detect(Victim, Push), is_enemy_near_trap(Victim, TrapHole), get_coordination(Victim, Vc), get_coordination(Push, Pc), add_move([Pc, Vc]), add_move([Vc, TrapHole]), moves(M).













