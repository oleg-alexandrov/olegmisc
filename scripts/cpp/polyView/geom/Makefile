CPP = g++ -O3 -Wall
CC = gcc -O3
FC = g77 -O3
OBJ=cutPoly.o dPoly.o geomUtils.o polyUtils.o kdTree.o edgeUtils.o dTree.cpp
HDR= cutPoly.h dPoly.h geomUtils.h polyUtils.h kdTree.h edgeUtils.h dTree.h dTree.h

all: test_distBwPolys test_dPoly testCutPoly polyPtsCmp test_boxTree test_kdTree \
	test_edgesInBox 

test_distBwPolys: test_distBwPolys.o $(OBJ) $(HDR) 
	$(CPP)  -o $@ $@.o $(OBJ)  $(LIBS)

test_edgesInBox: test_edgesInBox.o $(OBJ) $(HDR) 
	$(CPP)  -o $@ $@.o $(OBJ)  $(LIBS)

test_boxTree: test_boxTree.o $(OBJ) $(HDR) 
	$(CPP)  -o $@ $@.o $(OBJ)  $(LIBS)

polyPtsCmp: polyPtsCmp.o $(OBJ) $(HDR) 
	$(CPP)  -o $@ $@.o $(OBJ)  $(LIBS)

test_dPoly: test_dPoly.o $(OBJ) $(HDR) 
	$(CPP)  -o $@ $@.o $(OBJ)  $(LIBS)

test_kdTree: test_kdTree.o $(OBJ) $(HDR) 
	$(CPP)  -o $@ $@.o $(OBJ)  $(LIBS)

testCutPoly: testCutPoly.o $(OBJ) $(HDR) 
	$(CPP)  -o $@ $@.o $(OBJ)  $(LIBS)

cutPoly.o: cutPoly.cpp cutPoly.h edgeUtils.h
	$(CPP)  -c  cutPoly.cpp

dPoly.o: dPoly.cpp dPoly.h
	$(CPP)  -c  dPoly.cpp

edgeUtils.o: edgeUtils.cpp edgeUtils.h
	$(CPP)  -c  edgeUtils.cpp

geomUtils.o: geomUtils.cpp geomUtils.h
	$(CPP)  -c  geomUtils.cpp

kdTree.o: kdTree.cpp kdTree.h geomUtils.h
	$(CPP)  -c  kdTree.cpp

polyPtsCmp.o: polyPtsCmp.cpp dPoly.h geomUtils.h polyUtils.h
	$(CPP)  -c  polyPtsCmp.cpp

polyUtils.o: polyUtils.cpp geomUtils.h polyUtils.h dPoly.h
	$(CPP)  -c  polyUtils.cpp

test_boxTree.o: test_boxTree.cpp dTree.h dPoly.h
	$(CPP)  -c  test_boxTree.cpp

test_dPoly.o: test_dPoly.cpp dPoly.h
	$(CPP)  -c  test_dPoly.cpp

test_kdTree.o: test_kdTree.cpp kdTree.h dPoly.h
	$(CPP)  -c  test_kdTree.cpp

testCutPoly.o: testCutPoly.cpp dPoly.h cutPoly.h
	$(CPP)  -c  testCutPoly.cpp

.o:    %.cpp
	$(CPP)  -c $<

%.o:    %.f
	$(FC)  -c $<
