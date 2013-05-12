-module(mucc).

-behavior(gen_server).

-define(SERVER, ?MODULE).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API

-export([start_link/0, register_nickname/1, poll/1, send_message/3, unregister/1]).

%% Public API
register_nickname(NickName) ->
  case gen_server:call({global, ?SERVER}, {register, NickName}) of
      ok ->
	  ok;
      {error, Error} ->
	  Error
  end.

poll(NickName) ->
  case gen_server:call({global, ?SERVER}, {poll, NickName}) of
      {ok, Messages} ->
	  Messages;
      Error ->
	  Error
  end.

send_message(Sender, Addressee, Message) ->
  gen_server:cast({global, ?SERVER}, {send_message, Sender, Addressee, Message}).

unregister(Nickname) ->
  gen_server:cast({global, ?SERVER}, {unregister, Nickname}).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    io:format("~p (~p) starting....~n", [?MODULE, self()]),
    {ok, dict:new()}.


handle_call({register, NickName}, _From, State) ->
    case dict:find(NickName, State) of
        error ->
	    Pid = spawn(fun() -> 
				process_flag(trap_exit, true),
				proxy_client([]) end),
	    erlang:monitor(process, Pid),
	    message_router:register_nick(NickName, Pid),
	    {reply, ok, dict:store(NickName, Pid, State)};
        {ok, _} ->
	    {reply, {error, duplicate_nick_found}, State}
    end;

handle_call({poll, NickName}, _From, State) ->
      case dict:find(NickName, State) of
        error ->
	      {reply, {error, unknown_nick}, State};
        {ok, Pid} ->
          Pid ! {get_messages, self()},
          receive
            {messages, Messages} ->
		  {reply, {ok, Messages}, State}
          end
      end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({unregister, Nickname}, State) ->
    case dict:find(Nickname, State) of
        error ->
	    ok;
        {ok, Pid} ->
	    Pid ! stop
    end,
    {noreply, State};

handle_cast({send_message, Sender, Addressee, Message}, State) ->
    case dict:find(Sender, State) of
        error ->
	    ok;
        {ok, Pid} ->
	    Pid ! {send_message, Addressee, Message}
    end,
    {noreply, State};    

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldSvn, State, _Extra) ->
    {ok, State}.

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
