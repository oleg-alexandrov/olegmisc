#include ../Makefile.arpack

CPP = g++ -O3 -Wall
CC = gcc -O3
FC = g77 -O3
XTRA_OBJ=cutPoly.cpp dPoint.cpp dPoly.cpp geomUtils.cpp polyUtils.cpp
HDR= cutPoly.h dPoly.h geomUtils.h polyUtils.h

all: test_dPoly testCutPoly polyPtsCmp

#start: the lines between start and end will be updated every time hdr is run.

OBJ1  =  $(XTRA_OBJ)


polyPtsCmp: polyPtsCmp.o $(OBJ1) $(HDR) 
	$(CPP)  -o $@ $@.o $(OBJ1)  $(LIBS)

test_dPoly: test_dPoly.o $(OBJ1) $(HDR) 
	$(CPP)  -o $@ $@.o $(OBJ1)  $(LIBS)

testCutPoly: testCutPoly.o $(OBJ1) $(HDR) 
	$(CPP)  -o $@ $@.o $(OBJ1)  $(LIBS)

# object files which contain headers must be recompiled when headers change
cutPoly.o: cutPoly.cpp cutPoly.h
	$(CPP)  -c  cutPoly.cpp

dPoint.o: dPoint.cpp dPoint.h
	$(CPP)  -c  dPoint.cpp

dPoly.o: dPoly.cpp dPoint.h dPoly.h
	$(CPP)  -c  dPoly.cpp

geomUtils.o: geomUtils.cpp geomUtils.h
	$(CPP)  -c  geomUtils.cpp

polyPtsCmp.o: polyPtsCmp.cpp dPoly.h dPoint.h
	$(CPP)  -c  polyPtsCmp.cpp

test_dPoly.o: test_dPoly.cpp dPoly.h
	$(CPP)  -c  test_dPoly.cpp

testCutPoly.o: testCutPoly.cpp dPoly.h cutPoly.h
	$(CPP)  -c  testCutPoly.cpp


#end

.o:    %.cpp
	$(CPP)  -c $<

%.o:    %.f
	$(FC)  -c $<
