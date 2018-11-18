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

        # Panel which belongs to the client
        self.main_panel = Panel(True, 'CLIENT')
        # Panels owned by other connected clients
        self.other_panels = []

    def add_panel(self, panel_id):
        """Adds a panel with the given ID to the Client.
        """
        new_panel = Panel(panel_id)
        other_panels.append(new_panel)

    def remove_panel(self, panel_id):
        """Removes the panel with given ID.
        """
        index = None
        for current, panel in enumerate(other_panels):
            if panel.getID() == panel_id:
                index = current;



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

