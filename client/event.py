import pygame
from pygame.locals import *
from collections import deque
import sys
pygame.init()

KEYEVENTS = [pygame.KEYDOWN, pygame.KEYUP]
MOUSEEVENTS = [pygame.MOUSEBUTTONDOWN, pygame.MOUSEBUTTONUP, pygame.MOUSEMOTION]
CLIENT_NAME = 'CLIENT'

class EventHandler:
    """Container of gui events

    Events are of form (COMMAND, [PARAMLIST])
    """

    def __init__(self):
        self.mousedown = False
        self.events = deque()

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
        if event.type == pygame.MOUSEBUTTONDOWN and not self.mousedown:
            self.mousedown = True
            self.events.append(("STARTLINES", [CLIENT_NAME, event.pos]))
        elif event.type == pygame.MOUSEBUTTONUP and self.mousedown:
            self.mousedown = False
            self.events.append(("ENDLINES", [CLIENT_NAME, event.pos]))
        elif event.type == pygame.MOUSEMOTION and self.mousedown:
            self.events.append(("ADDPOINT", [CLIENT_NAME, event.pos]))
