-module(catena_native_service).
-export([init_library/1,open/0,call/2,close/1,stats/0]).
init_library(Path) -> erlang:load_nif(Path, 0).
open() -> erlang:nif_error(not_loaded).
call(_,_) -> erlang:nif_error(not_loaded).
close(_) -> erlang:nif_error(not_loaded).
stats() -> erlang:nif_error(not_loaded).
