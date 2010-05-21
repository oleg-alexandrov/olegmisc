#!/usr/bin/python

import sys

print "hi there"

print "input arguments are ", sys.argv

vals = sys.argv;

if len(vals) > 0:
    print "program name is ", vals[0]
    
if len(vals) > 1:
    print "input option is ", vals[1]

A = ['abc', 'def'];

print "The array is ", A
print "number of elements is ", len(A)
print "Elements 0 and 1 are", A[0], A[1]

for x in A:
    print "Listing the elements of A = ", A, " element is:", x

a = 0;
while ( a < 10 ):
    print "a is ", a
    a += 1

    if a > 2:
        print "We went beyond 2"
    else:
       print "We are under 2"

    if a > 3:
        print "break the loop"
        break
    
    
def first_fun(n):

    print "First function: number is ", n

# Call the function we just defined
first_fun(5)

