# prolog-IA02-Arimaa
Project for IA02 - UTC - creating an AI to play Arimaa.

Describing Piece:
Piece: [Coordinate X, Coordinate Y, Type, Side]

Coordinate: [Coordinate X, Coordinate Y]

# Fundamental function
```prolog
member(Element, List).
```
* Check if an Element in a List. (same as element(E,L)).
```prolog
listLength(List, Length).
```
* Get the Length of a List.
```prolog
is_empty_list(List).
```
* True if List is empty ([]).
```prolog
is_not_empty_list(List).
```
* True if List is not empty.

# Useful function
```prolog
type(Piece, Type).
```
* Get Type of a Piece.
```prolog
is_rabbit(Piece).
```
* True if Piece is Rabbit (similar is_cat(Piece)...).
```prolog
Force(Piece, Force).
Force(Type, Force).
```
* Get the Force of a Piece.
```prolog
is_stronger(Piece1, Piece2).
```
* True if Piece1 stronger than Piece2.
```prolog
get_x(Piece, X).
get_y(Coordinate, X).
```
* Get the X Coordinate of the Piece (similar get_y(Piece,Y)...).
```prolog
is_in_trap(Coordinate).
```
* True if Coordinate is at trap.
```prolog
get_piece_at_coordinate(Piece, Coordinate).
```
* Get Piece at the Coordinate, return List empty if empty Square.
```prolog
is_friendly(Piece1, Piece2).
```
* Check if 2 Pieces same side.
```prolog
is_frozen(Coordinate).
```
* True if the Piece is frozen.

# Strategy to search for moves
From observation: captured Piece of silver already in Gamestate - Why we need gamestate?

Receive (gamestate, board) -> 

TO DO:

   * Implement Push/Pull
