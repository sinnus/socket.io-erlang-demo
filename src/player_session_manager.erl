-module(player_session_manager).

-behaviour(gen_server).

%% API
-export([start_link/0, create_session/1, send_message/2, disconnect_player/1, event_manager/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {sessions,
                event_manager}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create_session(PlayerId) ->
    gen_server:call(?SERVER, {create_session, PlayerId}).

send_message(PlayerId, Message) ->
    gen_server:call(?SERVER, {send_message, PlayerId, Message}).

disconnect_player(PlayerId) ->
    gen_server:call(?SERVER, {disconnect_player, PlayerId}).

event_manager() ->
    gen_server:call(?SERVER, {event_manager}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, EventManager} = gen_event:start_link(),
    {ok, #state{sessions = ets:new(player_sessions, [public]),
                event_manager = EventManager}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({create_session, PlayerId}, _From, #state{event_manager = EventManager, sessions = Sessions} = State) ->
    ExistedPid = get_session_by_player(Sessions, PlayerId),
    case ExistedPid of
        undefined ->
            ok;
        Pid ->
            ets:delete(Sessions, Pid),
            ets:delete(Sessions, PlayerId),
            gen_event:notify(EventManager, {disconnect, PlayerId}),
            player_session:disconnect_async(Pid),
            ok
    end,

    {ok, NewSessionPid} = player_session:start_link(PlayerId),
    %% Read about ets insert behaviour if row already exists
    ets:insert(Sessions, [{PlayerId, NewSessionPid},
                          {NewSessionPid, PlayerId}]),
    gen_event:notify(EventManager, {connect, PlayerId, NewSessionPid}),
    {reply, NewSessionPid, State};

handle_call({send_message, PlayerId, Message}, _From, #state{sessions = Sessions} = State) ->
    case get_session_by_player(Sessions, PlayerId) of
        undefined ->
            ok;
        Pid ->
            player_session:send_message(Pid, Message),
            ok
    end,
    {reply, ok, State};

handle_call({disconnect_player, PlayerId}, _From, #state{sessions = Sessions} = State) ->
    case get_session_by_player(Sessions, PlayerId) of
        undefined ->
            ok;
        Pid ->
            player_session:disconnect(Pid),
            ok
    end,
    {reply, ok, State};

handle_call({event_manager}, _From, #state{event_manager = EventManager} = State) ->
    {reply, EventManager, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, _}, #state{ event_manager = EventManager, sessions = Sessions } = State) ->
    error_logger:info_msg("EXIT", []),
    case ets:lookup(Sessions, Pid) of
        [{Pid, PlayerId}] ->
            ets:delete(Sessions, PlayerId),
            ets:delete(Sessions, Pid),
            gen_event:notify(EventManager, {disconnect, PlayerId});
        _ ->
            ignore
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_session_by_player(Sessions, PlayerId) ->
    case ets:lookup(Sessions, PlayerId) of
        [{PlayerId, Pid}] -> 
            Pid;
        _ ->
            undefined
    end.
