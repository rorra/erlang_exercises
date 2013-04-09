-module(web_server).
-author("rorra@rorra.com.ar").

-define(OK, <<"ok">>).

-export([start/1, stop/0, dispatch_requests/1]).

-compile({no_auto_import,[error/2]}).

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
      success(Req, ?OK);
    Error ->
      error(Req, subst("Error: ~s", [Error]))
  end;

handle("/unregister", Req) ->
  Params = Req:parse_qs(),
  Nickname = proplists:get_value("nick", Params),
  mucc:unregister(Nickname),
  success(Req, ?OK);

handle("/poll", Req) ->
  Params = Req:parse_qs(),
  NickName = proplists:get_value("nick", Params),
  case mucc:poll(NickName) of
    {error, Error} ->
      error(Req, subst("Error: ~s~n", Error));
    Messages ->
      io:format("~s~n", [Messages]),
      case length(Messages) == 0 of
        true ->
          success(Req, <<"none">>);
        false ->
          Template = lists:foldl(fun(_, Acc) -> ["~s~n"|Acc] end, [], Messages),
          success(Req, subst(lists:flatten(Template), Messages))
      end
  end;

handle("/send", Req) ->
  Params = Req:parse_qs(),
  Sender = proplists:get_value("nick", Params),
  Addressee = proplists:get_value("to", Params),
  Message = proplists:get_value("msg", Params),
  mucc:send_message(Sender, Addressee, Message),
  success(Req, ?OK);

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

error(Req, Body) when is_binary(Body) ->
  Req:respond({500, [{"Content-Type", "text/plain"}], Body}).

success(Req, Body) when is_binary(Body) ->
  Req:respond({200, [{"Content-Type", "text/plain"}], Body}).
