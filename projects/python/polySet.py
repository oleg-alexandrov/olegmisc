#!/usr/bin/python

import sys
import os
import re # Perl-style regular expressions
import copy

class point:

    def __init__(self, x = 0, y = 0):
        self.x = x
        self.y = y
        
    def set(self, x, y):
        self.x = x
        self.y = y

    def printPoint(self):
        print "(" + str(self.x) + ", " + str(self.y) + ")"


class poly:

    def __init__(self):
        self.points = []

    def append(self, P):
        self.points.append(P)

    def printPoly(self):
        points = self.points
        for P in points:
            P.printPoint()
        if len(points) > 0 and points[0] != points[len(points)-1]:
            points[0].printPoint()
        print "NEXT"

            
if __name__ == "__main__":

    print "testing the polySet class"

    a = point(0, 3)

    b = point(3, 2)

    
    c = a
    a.set(7, 4)
    c.printPoint()

    A = poly()
    A.append(a)
    A.append(b)
    
    B = copy.deepcopy(A)
    print "\nB is:"
    B.printPoly()
    print ""
    
    A.append(point(13, 14))
    print "B is:"
    B.printPoly()
    print ""
