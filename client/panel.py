import pygame
pygame.init()

class Panel:
    """A Panel is a 100x100 canvas supporting client and server drawing.

    An ACTIVE Panel receives mouse events. These are propagated up to the
    parent, who then may send them to the server.

    An INACTIVE Panel receives lists of points from the server. It then draws
    lines between these points.
    """
    def __init__(self, active):
        pass

    def connect_points(self, points):
        """Draws a solid line between the points

        E.G. [(0,0), (1, 1), (3, 4)] draws a line between (0,0) and (1, 1)
        and then between (1, 1) and (3, 4)
        """
        pass

    def contains(point):
        """If the panel is ACTIVE and point is within it, returns true.

        Else returns false
        """
        pass;
