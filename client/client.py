#!/usr/bin/env python2

import pygame
from panel import Panel
from chat  import Chat
from Queue import Queue
pygame.init()

SIZE = (WIDTH, HEIGHT) = (580, 260)

class Client:
    """Client for Pictionary

    This defines the player's viewable area. It is comprised
    of panels, at minimum the CHAT panel and the CLIENT panel.
    The CHAT panel sends and receives chat messages.
    The CLIENT panel sends mouse events IF it is the client's turn
    """

    def __init__(self):
        self.screen = pygame.display.set_mode(SIZE)
        self.turn = False
        self.size = SIZE
        self.server = None
        self.inbox = Queue()
        self.outbox = Queue()
        self.chat = Chat()
        self.panel = Panel(None)

    def add_panel(self, panel_id):
        """Adds a panel with the given ID to the Client.
        """
        pass

    def remove_panel(self, panel_id):
        """Removes the panel with given ID.
        """
        pass

    def _send(self, event):
        """Sends the given event to the server
        """
        pass

    def _receive(self):
        """Receives a list of events from the server and processes them.
        """
        pass

    def loop(self):
        """Sends messages to server. Receives messages from server. Updates.
        """
        pass


if __name__ == "__main__":
    client = Client();

