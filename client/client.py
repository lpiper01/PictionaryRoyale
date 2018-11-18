#!/usr/bin/env python2
import pygame
from pygame.locals import *
from panel import Panel
from chat  import Chat
from Queue import Queue
pygame.init()

SIZE = (WIDTH, HEIGHT) = (1080, 900)
PANELSIZE = (300, 300)
# TWEAKS:
# - producer/consumer model between server and client?
# - resizable?

def rel_to_abs(rel_x, rel_y, gap = 10):
    offset_x = rel_x * PANELSIZE[0]
    offset_y = rel_y * PANELSIZE[1]
    gap_x = (rel_x + 1) * gap
    gap_y = (rel_y + 1) * gap
    return (offset_x + gap_x, offset_y + gap_y)

class Client:
    """Client for Pictionary

    This defines the player's viewable area. It is comprised
    of panels, at minimum the CHAT panel and the CLIENT panel.
    The CHAT panel sends and receives chat messages.
    The CLIENT panel sends mouse events IF it is the client's turn
    """

    def __init__(self):
        self.size = SIZE
        self.running = True
        self.turn = False
        self.screen = pygame.display.set_mode(self.size)

        self.server = None
        self.inbox = Queue()
        self.outbox = Queue()

        # Client's canvas + chat
        panel_location = rel_to_abs(0, 0)
        chat_location = rel_to_abs(1, 0)
        self.chat = Chat(chat_location, PANELSIZE, self.screen)
        self.main_panel = Panel('CLIENT', panel_location, PANELSIZE, self.screen)
        self.main_panel.toggle()
        
        # TMP TESTING
        self.chat.static_broadcast("E", "E")
        self.chat.local_broadcast("Louis", "Hello1")
        self.chat.static_broadcast("E", "F")
        self.chat.local_broadcast("Louis", "Hello2")
        self.chat.static_broadcast("E", "G")

        # Panels 'owned' by other connected clients
        self.other_panels = []

    def add_panel(self, panel_id):
        """Add a panel with the given ID to the Client.
        """
        new_panel = Panel(panel_id)
        other_panels.append(new_panel)

    def remove_panel(self, panel_id):
        """Remove the panel with given ID.
        """
        # WOULD PROBABLY BREAK STUFF RIGHT NOW
        index = None
        for current, panel in enumerate(other_panels):
            if panel.getID() == panel_id:
                index = current;

    def _post(self, event):
        """Add event to outbox, to be sent later
        """
        pass

    def _send(self):
        """Send all events in outbox to server
        """
        pass

    def _receive(self):
        """Receive a list of events from the server and adds to inbox
        """
        pass

    def _process(self):
        """Update state based on received messages
        """
        pass

    def _draw(self):
        """Draw all components and update display
        """
        self.screen.fill((0,0,0))
        self.main_panel.clear()
        self.main_panel.draw()
        self.chat.draw()
        for panel in self.other_panels:
            panel.clear()
            panel.draw()

        pygame.display.update()

    def _pygame_process(self):
        """Handles keyboard, mouse, other internal stuff"""
        for event in pygame.event.get():
            if event.type == QUIT:
                self.running = False
            # TODO: Keyboard + Mouse events -> 'package' + send to server

    def loop(self):
        """Sends messages to server. Receives messages from server. Updates.
        """
        while self.running:
            self._send()
            self._receive()
            self._process()
            self._draw()
            self._pygame_process()

        pygame.quit()


if __name__ == "__main__":
    client = Client()
    client.loop()
