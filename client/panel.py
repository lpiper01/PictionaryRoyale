import pygame

class Panel:
    """A Panel is a NxM canvas supporting client and server drawing.

    An ACTIVE Panel receives mouse events. These are propagated up to the
    parent, who then may send them to the server.

    An INACTIVE Panel receives lists of points from the server. It then draws
    lines between these points.
    """
    def __init__(self, active, ID, location, size):
        self.active = active
        self.ID = ID
        self.location = location

        self.canvas = pygame.Surface(size)
        # all objects that need to be drawn to the screen
        self.color = (255, 255, 255)

    def connect_points(self, points):
        """Draws a solid line between the points

        E.G. [(0,0), (1, 1), (3, 4)] draws a line between (0,0) and (1, 1)
        and then between (1, 1) and (3, 4)
        """
        pass

    def contains(self, point):
        """If the panel is ACTIVE and point is within it, returns true.

        Else returns false
        """
        pass;

    def draw(self, window):
        """Draws the Panel onto window"""
        self.canvas.fill(self.color)
        window.blit(self.canvas, self.location)

