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

    % {roundNum, currentWord,  currentDrawer, listOfPlayers, numPlayers}
    {ok, {0, ok, ok, [], 0}}.

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
    {noreply, State}.

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
% Only works before the game has started or after it has stopped
%---------------------------------------------------------------------
handle_info({join, Client}, {0, Word, Drawer, Players, NumPlayers}) ->
    io:format("~s~n", ["asdf"]),
    NewPlayers = [{Client, 0} | Players],
    NewNum = NumPlayers + 1,
    {noreply, {0, Word, Drawer, NewPlayers, NewNum}};

%---------------------------------------------------------------------
% Messages of the form {guess, Pid, Message}
% Takes in Message as a guess of what the current drawing is. 
% Shows the message on every player's screen, and ends the current
% round if the guess is correct
%---------------------------------------------------------------------
% Player is starting the game
handle_info({guess, Client, start}, {0, _Word, _Drawer, Players, Num}) ->
    NewDrawer = find_drawer(Players, 0),
    NewWord = find_word(0),
    NewDrawer ! {word, Client, NewWord},
    send_message(Players, {guess, Client, start}, Client),
    {noreply, {1, NewWord, NewDrawer, Players, Num}};

% The game is over
handle_info({guess, Client, Word}, {Round, Word, Drawer, Players, Round}) ->
    NewPlayers = add_points(Players, Drawer),
    send_message(Players, {guess, Client, Word}, Client),
    send_message(Players, {correct, Client, Word}, ok),
    NewWord = ok,
    NewDrawer = ok,
    % TODO: Clear pixels
    Winner = find_winner(NewPlayers),
    send_message(Players, {loser, Client, ok}, Winner),
    Winner ! {winner, Client, ok},
    ResetPlayers = zero_points(Players),
    {noreply, {0, NewWord, NewDrawer, ResetPlayers, Round}};
    
% Player has correctly guessed drawing word
handle_info({guess, Client, Word}, {Round, Word, Drawer, Players, Num}) ->
    NewPlayers = add_points(Players, Drawer),
    send_message(Players, {guess, Client, Word}, Client),
    send_message(Players, {correct, Client, Word}, ok),
    NewWord = find_word(Round rem Num),
    NewDrawer = find_drawer(Players, Round),
    NewDrawer ! {word, Client, NewWord},
    NewRound = Round + 1,
    {noreply, {NewRound, NewWord, NewDrawer, NewPlayers, Num}};


% Player incorrectly guessed drawing
handle_info({guess, Client, Incorrect}, {Round, Word, Drawer, Players, Num}) ->
    send_message(Players, {guess, Client, Incorrect}, Client),
    {noreply, {Round, Word, Drawer, Players, Num}};

%---------------------------------------------------------------------
% Messages of the form {draw, Pid, Changes}
% Sends the drawers pixel changes to all of the guessers
%---------------------------------------------------------------------
handle_info({draw, Client, Changes}, {Round, Word, Drawer, Players, Num}) ->
    send_message(Players, {draw, Client, Changes}, Client),
    {noreply, {Round, Word, Drawer, Players, Num}}.

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
    [];
add_points([{Winner, Points} | Rest], Winner) ->
    [{Winner, Points + 1} | Rest];
add_points([{Other, Points} | Rest], Winner) ->
    [{Other, Points} | add_points(Rest, Winner)].

%%--------------------------------------------------------------------
%% Given a round number, returns the next word to be drawn
%%--------------------------------------------------------------------
find_word(Num) ->
    Words = [house, cat, star, face, sun, flower, shoe, hat, dog, 
             guitar, pie, spider],
    lists:nth((Num rem lists:flatlength(Words)) + 1, Words).
 
%%--------------------------------------------------------------------
%% Given a list of players and a round number (n), returns the PID
%% of the nth player in the list
%%--------------------------------------------------------------------
find_drawer([{Client, _Points} | _Rest], 0) ->
    Client;
find_drawer([{_Client, _Points} | Rest], Num) ->
    find_drawer(Rest, Num - 1).

%%--------------------------------------------------------------------
%% Given a list of players and their points, resets all of the point
%% counters to zero
%%--------------------------------------------------------------------
zero_points([]) ->
    [];
zero_points([{Client, Points} | Rest]) ->
    [{Client, 0} | zero_points(Rest)].

%%--------------------------------------------------------------------
%% Given a list of players and their points, determine who has the
%% most points. In the case of a tie, picks one to be the winner
%%--------------------------------------------------------------------
find_winner([]) ->
    none;
find_winner([{Client, Points} | Rest]) ->
    find_winner(Rest, Client, Points).
find_winner([], Client, Points) ->
    Client;
find_winner([{Client, Points} | Rest], Other, Best) when Points >= Best ->
    find_winner(Rest, Client, Points);
find_winner([{Client, Points} | Rest], Other, Best) when Points < Best ->
    find_winner(Rest, Other, Best).