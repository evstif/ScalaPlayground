def set1 = new NonEmpty(10)
def set2 = new NonEmpty(3)
def set3 = set1 add 5
def set4 = set1 union (set3 union set2)
