-module(sbox_auth_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, auth/2, expire/1, fetch/1]).

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

auth(User, Pass) ->
	gen_server:call(?SERVER, {auth, {User, Pass, timestamp()}}).

fetch(User) ->
	gen_server:call(?SERVER, {fetch, User}).

expire(User) ->
	gen_server:call(?SERVER, {expire, User}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	self() ! {start, ssh_shell},
	{ok, Args}.

handle_call({auth, {User, _Pass, Time} = Attempt}, _From, State) ->
	Auth = case dict:find(User, State) of
		{ok, {Value, Tref}} ->
				{ok, cancel} = timer:cancel(Tref),
				checkLimit(Attempt, Value);
		error       -> checkAuth(Attempt)
	end,
	{ok, Timer} = timer:apply_after(5000, ?MODULE, expire, [User]),
	NewState = dict:store(User, {{Time, Auth}, Timer}, State),
	Result = case Auth of
		noauth -> false;
		_      -> true
	end,
	{reply, {ok, Result}, NewState};
handle_call({fetch, User}, _From, State) ->
	{reply, {ok, dict:fetch(User, State)}, State};
handle_call({expire, User}, _From, State) ->
	NewState = dict:erase(User, State),
	{reply, ok, NewState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({start, ssh_shell}, State) ->
	{ok, _pid} = ssh_shell:listen(8888),
	{noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

timestamp() -> {Mega, Secs, _Micro} = erlang:now(),  Mega*1000*1000 + Secs.

checkLimit({_User, _Pass, Time}, {LastTime, _Auth}) when Time < LastTime + 5 -> noauth;
checkLimit(Attempt, _Cached) -> checkAuth(Attempt).

checkAuth({User, Pass, _Time}) -> sbox_user:auth(User, Pass).
