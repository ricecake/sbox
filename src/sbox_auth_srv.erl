-module(sbox_auth_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, dict:new(), []).

auth(User, IP, Pass) ->
	gen_server:call(?SERVER, {auth, {User, IP, Pass, timestamp()}}).

expire(User, IP) ->
	gen_server:call(?SERVER, {expire, {User, IP}}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call({auth, {User, IP, Pass, Time} = Attempt}, State) ->
	Auth = case dict:find(Key, Dict) of
		{ok, Value} -> checkLimit(Attempt, Value);
		error       -> checkAuth(Attempt)
	end,
	NewState = dict:store({User, IP}, {Time, Auth}, State),
	{reply, {ok, Auth}, NewState}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

createOrReturn(Dict, Key, Default) ->
	case dict:find(Key, Dict) of
		{ok, Value} -> {Dict, Value};
		error       -> {dict:store(Key, Default, Dict), Default}
	end.

timestamp() -> {Mega, Secs, Micro} = erlang:now(),  Mega*1000*1000*1000*1000 + Secs * 1000 * 1000 + Micro.
