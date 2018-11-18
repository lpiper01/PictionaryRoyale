import pygame
pygame.init()

class Chat:
    """Specialized panel which sends and receives chat messages
    """

    def __init__(self):
        self.panel = Panel(True, 'CHAT')

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
