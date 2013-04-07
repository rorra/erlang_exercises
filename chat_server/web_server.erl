-module(web_server).
-author("rorra@rorra.com.ar").

-define(OK, <<"ok">>).

-export([start/1, stop/0, dispatch_requests/1]).

start(Port) ->
  mochiweb_http:start([{port, Port},
    {loop, fun dispatch_requests/1}]).

stop() ->
  mochiweb_http:stop().

dispatch_requests(Req) ->
  Path = Req:get(path),
  Action = clean_path(Path),
  handle(Action, Req).

handle("/register", Req) ->
  Params = Req:parse_qs(),
  NickName = proplists:get_value("nick", Params),
  case mucc:register_nickname(NickName) of
    ok ->
      Req:respond({200, [{"Content-Type", "text/plain"}], ?OK});
    Error ->
      Req:respond({500, [{"Content-Type", "text/plain"}], subst("Error: ~s~n", [Error])})
  end.

handle(Unknown, Req) ->
  Req:respond({404, [{"Content-Type", "text/plain"}], subst("Unknown action: ~s~n", [Unknown])}).

subst(Template, Values) when is_list(Values) ->
  list_to_binary(lists:flatten(io_lib:fwrite(Template, Values))).

clean_path(Path) ->
  case string:str(Path, "?") of
    0 ->
      Path;
    N ->
      string:substr(Path, 1, string:len(Path) - (N + 1))
  end.