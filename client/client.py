#!/usr/bin/env python2
import pygame
import sys
from pygame.locals import *
from panel         import Panel
from chat          import Chat
from network       import NetworkHandler
from event         import EventHandler
from Queue         import Queue

pygame.init()

DEBUG = True
SIZE = (WIDTH, HEIGHT) = (1080, 900)
PANELSIZE = (300, 300)
CLIENT_NAME = 'CLIENT'
FPS_LIMIT = 100
LINE_DIVISOR = 10

PANEL_INDEX = 0
LINES_INDEX = 1

def rel_to_abs(rel_x, rel_y, gap = 10):
    offset_x = rel_x * PANELSIZE[0]
    offset_y = rel_y * PANELSIZE[1]
    gap_x = (rel_x + 1) * gap
    gap_y = (rel_y + 1) * gap
    return (offset_x + gap_x, offset_y + gap_y)

class App:
    def __init__(self, node):
        self.nw_handler = NetworkHandler(node)
        self.event_handler = EventHandler()
        self.client = Client()
        self.running = True
        self.actions = {"EXIT" : self._exit, "STARTLINES" : self._startline,
                        "ENDLINES" : self._endline, "ADDPOINT" : self._addpoint,
                        "KEYDOWN" : self._keydown}

    def loop(self):
        """Main execution loop"""
        while self.running:
            # Handle input & changes
            self._process()

            # communicate changes to user
            self.client.process()

        # TODO: send termination signal to child processes
        sys.exit()

    def _process(self):
        """Get changes from handlers and update program state"""
        self.event_handler.process()
        self.nw_handler.process()

        gui_events = self.event_handler.get()
        network_events = self.nw_handler.get()

        self._do_events(gui_events)
        self._do_events(network_events)

    def _do_events(self, events):
        """Execute the commands defined by a list of events"""
        while len(events) > 0:
            event = events.popleft()
            command = event[0]
            params = event[1]

            self._do(command, params)

    def _do(self, command, params):
        """Translate a string into a function and execute it"""
        cmd_func = self.actions[command]
        cmd_func(params)


    def _exit(self, params):
        self.running = False

    def _startline(self, params):
        panel_id = params[0]
        pos = params[1]
        self.client.startline(panel_id, pos)

    def _endline(self, params):
        panel_id = params[0]
        pos = params[1]
        self.client.endline(panel_id, pos)

    def _addpoint(self, params):
        panel_id = params[0]
        pos = params[1]
        self.client.addpoint(panel_id, pos)

    def _keydown(self, params):
        char = params[0]

        if char == "return":
            self.client.endstr()
        elif char == "backspace":
            self.client.delchar()
        # Quick and dirty - filter out nondesirable keys
        # (left ctrl, escape, alt, etc)
        elif len(char) == 1:
            self.client.sendchar(char)

class Client:
    """Client for Pictionary

    This defines the player's viewable area. It is comprised
    of panels, at minimum the CHAT, CLIENT and one OTHER panel

    The CHAT panel sends and receives chat messages.
    The CLIENT panel sends mouse events IF it is the client's turn
    The OTHER panel(s) draw(s) the images sent by the server
    """

    def __init__(self):
        self.size = SIZE
        self.running = True
        self.turn = False
        self.screen = pygame.display.set_mode(self.size)
        self.clock = pygame.time.Clock()

        # Client's canvas + chat
        panel_location = rel_to_abs(0, 0)
        chat_location = rel_to_abs(1, 0)
        self.chat_panel = Chat(chat_location, PANELSIZE, self.screen)

        # Panels 'owned' by other connected clients
        # form: {panel_id : (Panel, [list of lines])}
        main_panel =  Panel(panel_location, PANELSIZE, self.screen)
        main_panel.set_enable(True)
        self.panels = {CLIENT_NAME : (main_panel, [])}

        # TMP TESTING
        self.chat_panel.static_message("E", "E")
        self.chat_panel.local_message("Louis", "Hello1")
        self.chat_panel.static_message("E", "F")
        self.chat_panel.local_message("Louis", "Hello2")
        self.chat_panel.static_message("E", "G")
        self.chat_panel.local_message("Louis", "Hello3")
        self.chat_panel.local_message("Louis", "Hello4")
        self.chat_panel.local_message("Louis", "Hello5")
        self.chat_panel.local_message("Louis", "Hello6")
        self.chat_panel.local_message("Louis", "Hello3")
        self.chat_panel.local_message("Louis", "Hello4")
        self.chat_panel.local_message("Louis", "Hello5")
        self.chat_panel.local_message("Louis", "Hello6")
        self.chat_panel.local_message("Louis", "Hello3")
        self.chat_panel.local_message("Louis", "Hello4")
        self.chat_panel.local_message("Louis", "Hello5")
        self.chat_panel.local_message("Louis", "Hello6")

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
        for current, panel in enumerate(self.other_panels):
            if panel.getID() == panel_id:
                index = current;

        self.other_panels.pop(index)

    def process(self):
        """Draw updates to screen, handle events
        """
        self._draw()

    def startline(self, panel_id, pos):
        """Begins a new line on the given panel"""
        target_panel = self.panels[panel_id]
        target_panel[LINES_INDEX].append([pos])

    # TODO: instead of adding then removing, just don't add certain points
    def endline(self, panel_id, pos):
        """Ends the last line on the given panel"""
        target_panel = self.panels[panel_id]
        target_linenum = len(target_panel[LINES_INDEX]) - 1
        target_line = target_panel[LINES_INDEX][target_linenum]
        target_line.append(pos)
        target_panel[LINES_INDEX][target_linenum]= target_line[0::LINE_DIVISOR]

    def addpoint(self, panel_id, pos):
        """Adds a point to the last line on the given panel"""
        target_panel = self.panels[panel_id]
        target_line = len(target_panel[LINES_INDEX]) - 1
        print "hi"
        target_panel[LINES_INDEX][target_line].append(pos)

    def sendchar(self, char):
        """Add a character to the chat panel entry box"""
        self.chat_panel.sendchar(char)

    def endstr(self):
        """Send string in the chat panel entry box"""
        self.chat_panel.endstr()

    def delchar(self):
        """Delete a character from the chat panel entry box"""
        self.chat_panel.delchar()

    def _draw(self):
        """Draw all components and update display
        """
        self.screen.fill((0,0,0))
        self.chat_panel.draw()

        for user in self.panels:
            panel = self.panels[user][PANEL_INDEX]
            lines = self.panels[user][LINES_INDEX]
            panel.clear()
            panel.draw(lines)

        pygame.display.update()
        self.clock.tick(FPS_LIMIT)


if __name__ == "__main__":
    app = App(None)
    app.loop()
