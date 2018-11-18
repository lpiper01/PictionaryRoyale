import pygame
from recentlist import RecentList
from collections import deque
from panel import Panel

MAX_MESSAGES = 10
FONTSIZE = 16
X_OFFSET = 5
Y_OFFSET = 5
FONT_GAP = 3
STATIC_COLOR = (0, 0, 0)
CHAT_COLOR = (0, 0, 255)

class Chat:
    """Specialized panel which sends and receives chat messages
    """

    def __init__(self, location, size, parent):
        self.panel = Panel('CHAT', location, size, parent)
        self.history_size = MAX_MESSAGES
        self.messages = RecentList(self.history_size)
        self.font = pygame.font.Font(None, 24)
        self.permanent = []

    def static_broadcast(self, status, message):
        """Displays a permanent static message (local-only)
        """
        self.history_size -= 1
        self.messages.set_limit(self.history_size)
        self.permanent.append(status + ": " + message)
        pass

    def local_broadcast(self, status, message):
        '''Displays a message without sending it to the server
        '''
        self.messages.add(str(status) + ": " + str(message))

    def global_broadcast(self, message):
        '''Sends a message to the server and local_broadcast it
        '''
        self.messages.add(str(status) + ": " + str(message))
        # TODO: send to server, remove above line (wait for server reply to
        # write our input to prevent out of order)

    def display_message(self, message, num, color):
        '''Draw the num-th message to the Panel (lower = prior message)
        '''
        text = self.font.render(message, 1, color)
        pos = self.font_pos(num)

        self.panel.blit(text, pos)

    def font_pos(self, num):
        '''Determines where the nth message ought to be placed'''
        _, height = self.font.size("A") # Need Y height - any string works
        return (X_OFFSET, Y_OFFSET + num * (height + FONT_GAP))

    def draw(self):
        self.panel.clear()

        num = 0
        for message in self.permanent:
            # Write each message to self.panel
            self.display_message(message, num, STATIC_COLOR)
            num += 1

        for message in self.messages:
            num += 1 # adds gap between permanent and chat
            self.display_message(message, num, CHAT_COLOR)

        self.panel.draw()
