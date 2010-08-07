#include ../Makefile.arpack

CPP = g++ -O3 -Wall
CC = gcc -O3
FC = g77 -O3
XTRA_OBJ=

#start: the lines between start and end will be updated every time hdr is run.

OBJ1  =  $(XTRA_OBJ)


linked_ints: linked_ints.o $(OBJ1) $(HDR) 
	$(CPP)  -o $@ $@.o $(OBJ1)  $(LIBS)

permIndex: permIndex.o $(OBJ1) $(HDR) 
	$(CPP)  -o $@ $@.o $(OBJ1)  $(LIBS)

stack_as_linked_list: stack_as_linked_list.o $(OBJ1) $(HDR) 
	$(CPP)  -o $@ $@.o $(OBJ1)  $(LIBS)

# object files which contain headers must be recompiled when headers change

#end

%.o:    %.cpp
	$(CPP)  -c $<

%.o:    %.f
	$(FC)  -c $<
