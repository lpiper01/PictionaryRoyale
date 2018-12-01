%%%-------------------------------------------------------------------
%%% @author Louis A. Piper Carson <lpiper01@vm-hw04.eecs.tufts.edu>
%%% @copyright (C) 2018, Louis A. Piper Carson
%%% @doc A simple chatserver. Allows --quit and --list commands.
%%%
%%% @end
%%% Created :  2 Oct 2018 by Louis A. Piper Carson <lpiper01@vm-hw04.eecs.tufts.edu>
%%%-------------------------------------------------------------------
-module(chat).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, join_room/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, client_loop/4, receive_loop/0]).

-define(SERVER, ?MODULE).

%% State = [{Pid1, Node1, Room1, Name1}, {Pid2, Node2, Room2, Name 2}, ...]

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

join_room(Node, Room, Name) ->
    net_adm:ping(Node),
    RNode = node(),
    Receiver = spawn(?MODULE, receive_loop, []),
    gen_server:cast({global, ?SERVER}, {subscribe, {Receiver, RNode, Room, Name}}),
    client_loop(Receiver, RNode, Room, Name).

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
    {ok, []}.

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


%% A list message is sent to the server every time a user enters --list.
handle_call({list, Receiver, Room}, _Parent, State) ->
    People = "[" ++
        [People ++ " " || {_R, _N, SRoom, People} <- State, SRoom == Room]
        ++ "]",
    Receiver ! People,
    Reply = ok,
    {reply, Reply, State};

%% A dispatch message is sent to the server every time a user enters a string.
handle_call({dispatch, Receiver, _Node, Room, Message}, _Parent, State) ->
    %% For every user who is not Receiver and is in Room...
    lists:foreach(fun (Elem) -> sendif(Message, Elem, Receiver, Room) end, State),
    Reply = ok,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = unknown,
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

%% A subscribe message is sent to the server whenever chat:join_room
%% is called. Adds user to the server State.
handle_cast({subscribe, Details}, State) ->
    {noreply, [Details | State]};

%% Inverse of subscribe; called whenever a client process dies. If
%% multiple users have the same name... bad things might happen.
handle_cast({unsubscribe, Name}, State) ->
    {noreply, lists:keydelete(Name, 4, State)};

handle_cast(_Request, State) ->
    {noreply, State}.

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
                         {stop, Reason :: normal | term(), NewState :: term(handle_info(_Info, State) ->
    {noreply, State}.

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

%% Take user input. Send it to the server.
%% Leave room if --quit entered. Call list from gen_server if
%% --list entered.
client_loop(Receiver, Node, Room, Name) ->
    Message = io:get_line(Name ++ ": "),
    case Message of
        "--quit\n" -> ok;
        "--list\n" -> gen_server:call({global, ?SERVER}, {list, Receiver, Room}),
                      client_loop(Receiver, Node, Room, Name);
        Message ->
            gen_server:call({global, ?SERVER},
                            {dispatch, Receiver, Node,
                             Room, Name ++ ": " ++ Message}),
            client_loop(Receiver, Node, Room, Name)
    end,
    gen_server:cast({global, ?SERVER}, {unsubscribe, Name}).


%% Receives messages, forever.
%% I'm not sure of a good way to terminate it. Any message sent
%% has the potential of matching, which is horrible for security.
%% A user could potentially kick everyone out in this way.
receive_loop() ->
    receive
        Message ->
            io:format("~s~n", [string:trim(Message)]),
            receive_loop()
    end.

%% Sends Message to Receiver if Receiver is not SReceiver and SRoom is Room.
%% This filters out the user's own messages, and prevents messages from
%% being sent cross-room.
sendif(Message, {SReceiver, _SNode, SRoom, _SName}, Receiver, Room) when
      (Receiver /= SReceiver) and (SRoom == Room) ->
    SReceiver ! Message;

sendif(_Message, {_SReceiver, _SNode, _SRoom, _SName}, _Receiver, _Node) ->
    pass.

