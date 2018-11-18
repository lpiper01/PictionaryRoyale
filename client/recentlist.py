from collections import deque

class RecentList:
    """A list with a maximum age - elements older than that age are removed

    'Age' is merely order of insertion - elements added first fall off when new
    elements get added
    """
    def __init__(self, limit):
        self.limit = limit
        self.data = deque()

    def add(self, elem):
        """Add elem and removes oldest if limit surpassed"""
        self.data.append(elem)
        self._prune()

    def pop(self):
        """Removes oldest"""
        self.data.popleft()

    def remove(self, index):
        """Remove item at index"""
        self.data.pop(index)

    def display(self):
        """Display the data"""
        print self.data

    def set_limit(self, limit):
        """Change the age limit and update the list"""
        self.limit = limit
        self._prune()

    def to_list(self):
        """Convert the data to a normal list"""
        return list(self.data)

    def _prune(self):
        """Remove data older than the limit"""
        while len(self.data) > self.limit:
            self.data.popleft()

    def __getitem__(self, index):
        """Return the given data item"""
        return self.data[index]

if __name__ == "__main__":
    limit = 20
    rlist = RecentList(limit)
    for i in range(100):
        rlist.add(i)
        rlist.display()
