class simpleClass:
    y = 2

class complexClass(simpleClass):
    pass

p2 = complexClass()

print(p2.y)