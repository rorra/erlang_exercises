-module(hello_web).
-author("rorra").

-export([start/1, stop/0]).

-define(CONTENT, <<"<html><head><title>Hello world</title></head><body><h1>Test</h1></body></html>">>).

start(Port) ->
  mochiweb_http:start([{port, Port}, {loop, fun(Req) ->
    Req:ok({"text/html", ?CONTENT}) end}]).

stop() ->
  mochiweb_http:stop().
