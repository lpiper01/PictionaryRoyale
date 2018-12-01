

from Queue import Queue
class NetworkHandler:
    """Wrapper for Erlang server communication process"""
    def __init__(self, node):
        self.server = None #spawn an erlang process here
        self.inbox = []
        self.outbox = []

    def put(self, message):
        """Add message to outbox"""
        self.outbox.append(message)

    def process(self):
        """Access network to send/receive new messages"""
        self._send()
        self._receive()

    def get(self):
        """Update state based on messages in inbox"""
        return self.inbox

    def _send(self):
        """Send outbox messages to server through Erlang process"""
        pass

    def _receive(self):
        """Receive messages that have been sent to Erlang process

        Gets all messages that were sent to the Erlang subprocess; stores
        them in inbox
        """
        pass
