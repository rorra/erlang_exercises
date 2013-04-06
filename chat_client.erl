-module(chat_client).

-compile(export_all).

register_nickname(NickName) ->
    message_router:register_nick(NickName, fun(Msg) -> chat_client:print_message(NickName, Msg) end).

unregister_nickname(NickName) ->
    message_router:unregister_nick(NickName).

send_message(Addressee, MessageBody) ->
    message_router:send_chat_message(Addressee, MessageBody).

print_message(Who, MessageBody) ->
    io:format("~p received: ~p~n", [Who, MessageBody]).

start_router() ->
    message_router:start().
