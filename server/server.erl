-module(server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop({global, ?SERVER}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
                              {ok, State :: term(), Timeout :: timeout()} |
                              {ok, State :: term(), hibernate} |
                              {stop, Reason :: term()} |
                              ignore.
init([]) ->
    process_flag(trap_exit, true),
    {ok, {ok, ok, []}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
                         {reply, Reply :: term(), NewState :: term(), hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.

handle_cast(_Request, State) ->
    {noReply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: normal | term(), NewState :: term()}.

handle_info({join, Client}, {Word, Drawer, Players}) ->
    NewPlayers = [{Client, 0} | Players],
    {noreply, {Word, Drawer, NewPlayers}};

% Player has correctly guessed drawing word
handle_info({guess, Client, Word}, {Word, Drawer, Players}) ->
    NewPlayers = add_points(Players, Client),
    send_guesses(Players, Word, Client),
    % Alert all clients the round is over
    % Update word to new one
    % Clear all pixels
    % Send the drawer their word
    % {noreply, {NewWord, NewDrawer, NewPlayers}};
    {noreply, {Word, Drawer, NewPlayers}};

% Player incorrectly guessed drawing
handle_info({guess, Client, IncorrectDrawing}, {Word, Drawer, Players}) ->
    send_guesses(Players, IncorrectDrawing, Client),
    {noreply, {Word, Drawer, Players}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% Sends a message to all players except the one who made the guess
%%--------------------------------------------------------------------
send_guesses([], _Guess, _Exclude) ->
    ok;
send_guesses([{Client, _Points} | Rest], Guess, Client) ->
    send_guesses(Rest, Guess, Client);
send_guesses([{Pid, _Points} | Rest], Guess, Client) ->
    Pid ! {guess, Guess},
    send_guesses(Rest, Guess, Client).

%%--------------------------------------------------------------------
%% Given a list of all the players and their points, returns a new
%% list with the winner's points updated
%%--------------------------------------------------------------------
add_points([], _Winner) ->
    ok;
add_points([{Winner, Points} | Rest], Winner) ->
    [{Winner, Points + 1} | Rest];
add_points([{Other, Points} | Rest], Winner) ->
    [{Other, Points} | add_points(Rest, Winner)].
