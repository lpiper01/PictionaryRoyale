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

def rel_to_abs(rel_x, rel_y, gap = 5):
    return (rel_x * (gap + PANELSIZE[0]), rel_y * (gap + PANELSIZE[1]))

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
        self.running = True
        # Client's canvas + chat
        panel_location = rel_to_abs(0, 0)
        chat_location = rel_to_abs(1, 0)
        print panel_location
        print chat_location
        self.chat = Chat(chat_location, PANELSIZE)
        self.main_panel = Panel(True, 'CLIENT', panel_location, PANELSIZE)

        # Panels 'owned' by other connected clients
        self.other_panels = []

        self._loop()

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


    def _post(self, event):
        """Adds event to outbox, to be sent later
        """
        pass
    
    def _send(self):
        """Sends all events in outbox to server
        """
        pass

    def _receive(self):
        """Receives a list of events from the server and adds to inbox
        """
        pass

    def _process(self):
        """Updates state based on received messages
        """
        pass

    def _draw(self):
        # TODO: draw at specific locations
        self.screen.fill((0,0,0))
        self.main_panel.draw(self.screen)
        self.chat.draw(self.screen)
        for panel in self.other_panels:
            panel.draw(self.screen)

        pygame.display.update()

    def _pygame_process(self):
        """Handles keyboard, mouse, other internal stuff"""
        for event in pygame.event.get():
            if event.type == QUIT:
                self.running = False

    def _loop(self):
        """Sends messages to server. Receives messages from server. Updates.
        """
        while self.running:
            self._send()
            self._receive()
            self._process()
            self._draw()
            self._pygame_process()
        # TODO: event handling
        pygame.quit()


if __name__ == "__main__":
    
    client = Client();

