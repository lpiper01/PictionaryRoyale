-module(server).

-behaviour(gen_server).

%% Public functions
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% PUBLIC FUNCTIONS
%%%===================================================================

%%--------------------------------------------------------------------
%% Starts the server
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
%%% GEN_SERVER CALLBACKS
%%%===================================================================

%%--------------------------------------------------------------------
%% Initializes the server
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
%% Handles synchronous call messages that require a reply
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
%% Handles asynchronous cast messages with no reply
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.

handle_cast(_Request, State) ->
    {noReply, State}.

%%--------------------------------------------------------------------
%% Handles all messages sent directly to server
%% Supports join, start, guess, and draw commands
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: normal | term(), NewState :: term()}.

%---------------------------------------------------------------------
% Messages of the form {join, Pid}
% Adds the client to the current game of pictionary
%---------------------------------------------------------------------
handle_info({join, Client}, {Word, Drawer, Players}) ->
    NewPlayers = [{Client, 0} | Players],
    {noreply, {Word, Drawer, NewPlayers}};

%---------------------------------------------------------------------
% Messages of the form {start, Pid}
% Starts a game of pictionary with all the current players
%---------------------------------------------------------------------
handle_info({start, Client}, {State}) ->
    % TODO: Pick the first word
    % TODO: Send that word to the first player
    {noreply, {State}};

%---------------------------------------------------------------------
% Messages of the form {guess, Pid, Message}
% Takes in Message as a guess of what the current drawing is. 
% Shows the message on every player's screen, and ends the current
% round if the guess is correct
%---------------------------------------------------------------------
% Player has correctly guessed drawing word
handle_info({guess, Client, Word}, {Word, Drawer, Players}) ->
    NewPlayers = add_points(Players, Client),
    send_message(Players, {guess, Word}, Client),
    % TODO: Alert all clients the round is over
    % TODO: Update word to new one
    % TODO: Clear all pixels
    % TODO: Send the drawer their word
    % TODO: {noreply, {NewWord, NewDrawer, NewPlayers}};
    {noreply, {Word, Drawer, NewPlayers}};

% Player incorrectly guessed drawing
handle_info({guess, Client, IncorrectDrawing}, {Word, Drawer, Players}) ->
    send_message(Players, {guess, IncorrectDrawing}, Client),
    {noreply, {Word, Drawer, Players}};

%---------------------------------------------------------------------
% Messages of the form {draw, Pid, Changes}
% Sends the drawers pixel changes to all of the guessers
%---------------------------------------------------------------------
handle_info({draw, Client, Changes}, {Word, Drawer, Players}) ->
    send_message(Players, {draw, Changes}, Client),
    {noreply, {Word, Drawer, Players}}.

%%--------------------------------------------------------------------
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% HELPER FUNCTIONS
%%%===================================================================

%%--------------------------------------------------------------------
%% Sends given message to all players except the specified client
%% that sent the message.
%%--------------------------------------------------------------------
send_message([], _Message, _Exclude) ->
    ok;
send_message([{Client, _Points} | Rest], Message, Client) ->
    send_message(Rest, Message, Client);
send_message([{Pid, _Points} | Rest], Message, Client) ->
    Pid ! Message,
    send_message(Rest, Message, Client).

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
