import pygame
from pygame.locals import *
import sys
pygame.init()

KEYEVENTS = [pygame.KEYDOWN, pygame.KEYUP]
MOUSEEVENTS = [pygame.MOUSEBUTTONDOWN, pygame.MOUSEBUTTONUP, pygame.MOUSEMOTION]

class EventHandler:
    """Container of gui events

    Events are of form (COMMAND, [PARAMLIST])
    """
    def __init__(self):
        self.events = []

    def process(self):
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                self._handle_quit()
            elif event.type in KEYEVENTS:
                self._handle_keyboard(event)
            elif event.type in MOUSEEVENTS:
                self._handle_mouse(event)

    def get(self):
        return self.events

    def _handle_quit(self):
        self.events.append(("EXIT", [None]))

    def _handle_keyboard(self, event):
        pass

    def _handle_mouse(self, event):
        if event.type == pygame.MOUSEMOTION and event.buttons[0]:
            self.events.append(("DRAW", [(0, 0, 0), event.pos]))

