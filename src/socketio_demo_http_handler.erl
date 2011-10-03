-module(socketio_demo_http_handler).
-export([handle_request/3]).

handle_request(_Method, _Path, Req) ->
    Req:respond(404).
