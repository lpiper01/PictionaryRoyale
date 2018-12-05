import pygame
from recentlist import RecentList
from collections import deque
from panel import Panel
import erlport.erlang
from erlport.erlterms import Atom

MAX_MESSAGES = 11
LAST_SPOT = 12
FONTSIZE = 16
FONT = "resources/fonts/ubuntu.ttf"
X_OFFSET = 5
Y_OFFSET = 0
FONT_GAP = 3
STATIC_COLOR = (0, 0, 0)
ENTRY_COLOR = (0, 127, 127)
CHAT_COLOR = (0, 0, 255)
PROMPT = list(">>> ")

class Chat:
    """Send/Receive messages; draw to panel
    """

    def __init__(self, location, size, parent):
        self.panel = Panel(location, size, parent)
        self.history_size = MAX_MESSAGES
        self.messages = RecentList(self.history_size)
        self.font = pygame.font.Font(FONT, FONTSIZE)
        self.permanent = []
        self.name = "NULL"
        self.text_entry = PROMPT[:] # want a copy

    def static_message(self, status, message):
        """Displays a permanent static message (local-only)
        """
        self.history_size -= 1
        self.messages.set_limit(self.history_size)
        self.permanent.append(status + "" + message)

    def local_message(self, status, message):
        '''Displays a message without sending it to the server
        '''
        self.messages.add(str(status) + ": " + str(message))

    def global_message(self, status, message, pid):
        '''Sends a message to the server and local_broadcast it
        '''
        erlport.erlang.cast(pid, (Atom('guess'), Atom('garbage'), Atom(str(message))))
#        self.local_message(status, message)

    def draw(self):
        self.panel.clear()

        num = 0
        for message in self.permanent:
            # Write each message to self.panel
            self._display_message(message, num, STATIC_COLOR)
            num += 1

        for message in self.messages:
            num += 1 # adds gap between permanent and chat
            self._display_message(message, num, CHAT_COLOR)

        num = LAST_SPOT
        text = ''.join(self.text_entry)

        self._display_message(text, num, ENTRY_COLOR)
        self.panel.draw()

    def sendchar(self, char):
        self.text_entry.append(char)

    def delchar(self):
        if len(self.text_entry) > len(PROMPT):
            self.text_entry.pop()

    def endstr(self, pid):
        text = self.text_entry[len(PROMPT):]
        text = ''.join(text)
        self.global_message(self.name, text, pid)
        self.text_entry = PROMPT[:] # want a copy

    def _display_message(self, message, num, color):
        '''Draw the num-th message to the Panel (lower = prior message)
        '''
        text = self.font.render(message, 1, color)
        pos = self._font_pos(num)

        self.panel.blit(text, pos)

    def _font_pos(self, num):
        '''Determines where the nth message ought to be placed'''
        _, height = self.font.size("A") # Need Y height - any string works
        return (X_OFFSET, Y_OFFSET + num * (height + FONT_GAP))
