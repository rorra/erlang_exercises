-module(mucc).

-author("rorra").

-define(SERVER, mucc).

-compile(export_all).

start() ->
  server_util:start(?SERVER, {mucc, server_loop, [dict:new()]}).

stop() ->
  server_util:stop(?SERVER).

register_nickname(NickName) ->
  global:send(?SERVER, {register, NickName, self()}),
  receive
    ok ->
      ok;
    {error, Error} ->
      Error
  end.

poll(NickName) ->
  global:send(?SERVER, {poll, NickName, self()}),
  receive
    {ok, Messages} ->
      Messages;
    Error ->
      Error
  end.

send_message(Sender, Addressee, Message) ->
  global:send(?SERVER, {send_message, Sender, Addressee, Message}).

server_loop(Proxies) ->
  receive
    {register, NickName, Caller} ->
      case dict:find(NickName, Proxies) of
        error ->
          Pid = spawn(fun() -> proxy_client([]) end),
          message_router:register_nick(NickName, Pid),
          Caller ! ok,
          server_loop(dict:store(NickName, Pid, Proxies));
        {ok, _} ->
          Caller ! {error, duplicate_nick_found},
          server_loop(Proxies)
      end;
    {poll, NickName, Caller} ->
      case dict:find(NickName, Proxies) of
        error ->
          Caller ! {error, unknown_nick};
        {ok, Pid} ->
          Pid ! {get_messages, self()},
          receive
            {messages, Messages} ->
              Caller ! {ok, Messages}
          end
      end,
      server_loop(Proxies);
    {send_message, Sender, Addressee, Message} ->
      case dict:find(Sender, Proxies) of
        error ->
          ok;
        {ok, Pid} ->
          Pid ! {send_message, Addressee, Message}
      end,
      server_loop(Proxies)
  end.

proxy_client(Messages) ->
  receive
    {printmsg, MessageBody} ->
      proxy_client([MessageBody | Messages]);
    {get_messages, Caller} ->
      Caller ! {messages, lists:reverse(Messages)},
      proxy_client([]);
    {send_message, Addressee, Message} ->
      message_router:send_chat_message(Addressee, Message),
      proxy_client(Messages);
    stop ->
      io:format("Proxy stopping...~n"),
      ok
  end.
