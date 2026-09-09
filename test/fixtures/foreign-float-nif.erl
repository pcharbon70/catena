-module(foreign_float_nif).
-export([load/1, number/1, decode/3, encode/3]).
-nifs([number/1]).
load(Path) -> erlang:load_nif(Path, 0).
number(_) -> erlang:nif_error(not_loaded).
decode(Codec, Value, Limits) -> 'Elixir.Catena.Foreign.Codec':decode(Codec, Value, Limits).

encode(Codec, Value, Limits) -> 'Elixir.Catena.Foreign.Codec':encode(Codec, Value, Limits).
