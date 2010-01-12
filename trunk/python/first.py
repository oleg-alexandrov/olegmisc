#!/usr/bin/python

import sys

print "hi!\n"

print "input arguments are ", sys.argv

vals = sys.argv;

print "program name is ", vals[0]
print "input option is ", vals[1]

A = ['pqr', 'stu'];

print "The array is ", A
print "number of elements is ", len(A)
print "First element is ", A[1]

a = 0;
while ( a < 10 ):
    print "a is ", a
    a += 1

    if a > 5:
        print "We went beyond 5"
    else:
       print "We are under 5"

    if a > 8:
        print "We are breaking"
        break
    
for x in A:
    print "Listing the elements of A = ", A, " element is ", x

    
def first_fun(n):

    print "number is ", n

first_fun(5)

