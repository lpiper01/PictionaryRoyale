import pygame
from collections import deque
from panel import Panel

MAX_MESSAGES = 10

class Chat:
    """Specialized panel which sends and receives chat messages
    """

    def __init__(self, location, size):
        self.panel = Panel(True, 'CHAT', location, size)
        self.messages = deque()

    def local_broadcast(self, status, message):
        '''Displays a message without sending it to the server
        '''
        pass

    def global_broadcast(self, message):
        '''Sends a message to the server and local_broadcast it
        '''
        pass

    def display_message(self, message):
        '''Draw a message to the Panel
        '''
        pass

    def draw(self, surface):
        for message in self.messages:
            # Write each message to self.panel
            pass

        self.panel.draw(surface)
