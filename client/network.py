import sys

ERLPORT_PATH = './resources/erlport-0.9.8/priv/python2/'
sys.path.insert(0, ERLPORT_PATH)

from erlport.erlterms import Atom
import erlport.erlang
from collections import deque

from Queue import Queue

class NetworkHandler:
    """Wrapper for Erlang server communication process"""
    def __init__(self, pid):
        self.serverPID = pid
        self.PID = erlport.erlang.self()
        self.inbox = deque()
        self.outbox = deque()

        erlport.erlang.cast(self.serverPID, (Atom("join"), self.PID))

    def put(self, message):
        """Add message to outbox"""
        self.outbox.append(message)

    def process(self):
        """Access network to send/receive new messages"""
        self._send()

    def get(self):
        """Update state based on messages in inbox"""
        return self.inbox

    def _send(self):
        """Send outbox messages to server through Erlang process"""
        pass

    def receive(self, message):
        """Receive message that has been sent by Erlang process

        Get message and store it in inbox
        """
        command, client, args = message
        client = ""

        print "Gottem"
        if command == Atom('guess'):
            self.inbox.append(("GUESS", [client, args]))
        elif command == Atom('draw'):
            self.inbox.append(("DRAW", [client, args]))
        elif command == Atom('word'):
            self.inbox.append(("WORD", [client, args]))
        elif command == Atom('correct'):
            self.inbox.append(("CORRECT", [client, args]))
        elif command == Atom('winner'):
            self.inbox.append(("WINNER", [client, args]))
        elif command == Atom('loser'):
            self.inbox.append(("LOSER", [client, args]))
        else:
            print "unknown"
