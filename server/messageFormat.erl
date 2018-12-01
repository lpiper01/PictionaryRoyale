% State needs to include:
%     List of all the current players (Node and Pid? Just Pid?)
%     Pid of the current drawer
%     Current Drawing
%     Points

{CurrentWord, CurrentDrawer, RemainingRounds, [{Pid1, Node1, Points}, {Pid2, Node2, Points}...]}


% Timer can be added in the form of a process that just keeps track of
% time (possibly sending update to each client's GUI every second or
% something) that sends a {timeout, PID} message to the server, which
% causes the server to award no points and go to the next round


wait_loop(CurrentState) ->
    receive
        {join, Client} -> 
            % Add to game, update state
            wait_loop(NewState);
        {start, Client} -> 
            % Make sure game is setup correctly to begin
            game_loop(CurrentState).


game_loop(CurrentState) ->
    receive
        {guess, Client, CurrentDrawing} ->
            % Update point counter
            % Send guess to every other player
            % Alert all clients that the round is over
            % Update the word to a new one
            % Send the next drawer their word, and make everyone's pixel board blank
            game_loop(NewState);
        {guess, Client, IncorrectDrawing} ->
            % Send out the incorrect guess to every other player
            game_loop(CurrentState);
        {drawing, CurrentDrawer, Changes} ->
            % Send changes to all guessers
            % Update state
            game_loop(NewState).
        {requestState, Client} -> 