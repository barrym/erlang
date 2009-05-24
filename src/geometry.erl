-module(geometry).
-export([area/1]).

area({rectangle, Width, Ht}) -> Width * Ht;
area({circle, R}) -> 3.15159 * R * R;
area({square, Side}) -> Side * Side.
