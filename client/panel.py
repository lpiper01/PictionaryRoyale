import pygame

ACTIVECOLOR = (255, 255, 102)
INACTIVECOLOR = (128, 128, 128)

class Panel:
    """A Panel is a NxM canvas supporting client and server drawing.

    An ACTIVE Panel receives mouse events. These are propagated up to the
    parent, who then may send them to the server.

    An INACTIVE Panel receives lists of points from the server. It then draws
    lines between these points.
    """
    def __init__(self, location, size, parent):
        self.active = False
        self.location = location
        self.size = size
        self.canvas = pygame.Surface(self.size)
        self.rect = self.canvas.get_rect()
        self.rect.move(self.location)
        self.parent = parent
        self.color = (255, 255, 255)

    def connect_points(self, points):
        """Draws a solid line between the points
        E.G. [(0,0), (1, 1), (3, 4)] draws a line between (0,0) and (1, 1)
        and then between (1, 1) and (3, 4)
        """
        pygame.draw.lines(self.canvas, False, points, 3)

    def contains(self, point):
        """If the panel is ACTIVE and point is within it, returns true.

        Else returns false
        """

        return self.rect.collidepoint(point)

    def blit(self, surface, pos):
        self.canvas.blit(surface, pos)
        self.parent.blit(self.canvas, self.location)

    def set_enable(self, val):
        self.active = val

    def get_enable(self):
        return self.active

    def clear(self):
        self.canvas.fill(self.color)

    def draw(self, lines = []):
        """Draws the Panel onto window
        """
        left, top = self.location
        width, height = self.size
        outline = pygame.Rect(left - 5, top - 5, width + 10, height + 10)
        outline_color = ACTIVECOLOR if self.active else INACTIVECOLOR

        pygame.draw.rect(self.parent, outline_color, outline, 0)
        for line in lines:
            if len(line) > 1:
                print line
                pygame.draw.lines(self.canvas, (0, 0, 0), False, line, 3)
        self.parent.blit(self.canvas, self.location)
