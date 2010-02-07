#!/usr/bin/python

class drawnData:
    
    drawnLine  = "5"
    drawnSpace = "6"
    drawnCd    = "7"

    def __init__(self):
        print "Calling the init function"
        self.drawnLine   = -1
        self.drawnSpace  = -3
        self.drawnCd     = -5

    def disp(self):
        print self.drawnLine
        print self.drawnSpace
        print self.drawnCd
        
    
if __name__ == '__main__':

    fileName = "data.txt"
    FILE     = open(fileName, "r")
    data     = FILE.read()
    FILE.close()

    A = drawnData()
    A.disp()

    print "Assigning to the class"
    
    A.drawnLine  = 13
    A.drawnSpace = 14
    A.disp()

    A.l = 12
    print "New value is ", A.l
    
 
