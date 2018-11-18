from collections import deque

class RecentList:
    """Kicks the oldest message off when cap is 
    reached and new item appended
    """
    def __init__(self, limit):
        self.limit = limit
        self.data = deque()

    def add(self, elem):
        """Adds elem and removes oldest if limit surpassed"""
        self.data.append(elem)
        self._prune()

    def pop(self):
        """Removes oldest"""
        self.data.popleft()

    def remove(self, index):
        """Removes item at index"""
        self.data.pop(index)

    def display(self):
        print self.data

    def set_limit(self, limit):
        self.limit = limit
        self._prune()

    def to_list(self):
        return list(self.data)
        
    def _prune(self):
        while len(self.data) > self.limit:
            self.data.popleft()

    def __getitem__(self, index):
        return self.data[index]

if __name__ == "__main__":
    limit = 20
    rlist = RecentList(limit)
    for i in range(100):
        rlist.add(i)
        rlist.display()
