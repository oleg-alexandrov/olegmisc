#include <iostream>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <cassert>
#include <limits>
#include <ctime>
#include "boxTree.h"
#include "dPoly.h"

using namespace std;
using namespace utils;

double rand_ab(double a, double b){
  assert(a <= b);
  return a + rand()%max(int(b - a), 1);
}


int main(int argc, char** argv){

 srand(time(NULL));

 int Repeat = 1; // How many times to repeat for timing each experiment
 int N      = 1000; // Num boxes
 int L      = 100, w = 10; // Box region params
 int Big    = 10000; // How many experiments

 vector<Box> Boxes;

 for (int q = 0; q < Big; q++){
   
    Boxes.clear();

   for (int s = 0; s < N; s++){
     double xl = rand_ab(-L, L), xh = xl + rand_ab(0, w);
     double yl = rand_ab(-L, L), yh = yl + rand_ab(0, w);
     Boxes.push_back(Box(xl, yl, xh, yh));
   }
 
   boxTree T;
   // Boxes will be reordered but otherwise unchanged inside of this function.
   // Do not modify this vector afterward.
   T.formTree(Boxes); 

   double L2 = 2*L;
   double xl = rand_ab(-L2, L2), xh = xl + rand_ab(0, L2);
   double yl = rand_ab(-L2, L2), yh = yl + rand_ab(0, L2);

   time_t Start_t, End_t;
   int time_task;
   
   vector<Box> outBoxes;
   Start_t = time(NULL);    //record time that task 1 begins
   for (int v = 0; v < Repeat; v++){
     T.getBoxesInBox(xl, yl, xh, yh, outBoxes);
   }
   End_t = time(NULL);  
   time_task = difftime(End_t, Start_t);
   if (Big == 1) cout << "Tree search time: " << time_task << endl;

   sort(outBoxes.begin(), outBoxes.end(), lexLessThan);
 
   vector<Box> outBoxes2; // Must be different than Boxes
   outBoxes2.resize(N);
   
   Start_t = time(NULL);
   for (int v = 0; v < Repeat; v++){
     outBoxes2.clear();
     for (int s = 0; s < (int)Boxes.size(); s++){
       const Box & B = Boxes[s];
       if (boxesIntersect(B.xl, B.yl, B.xh, B.yh, xl, yl, xh, yh)){
         outBoxes2.push_back(B);
       }
     }
   }
   End_t = time(NULL);
   time_task = difftime(End_t, Start_t);
   if (Big == 1) cout << "Brute force search time: " << time_task << endl;

   sort(outBoxes2.begin(), outBoxes2.end(), lexLessThan);

   bool haveProblem = false;
 
   int len = outBoxes.size();
   if ( len != (int)outBoxes2.size() ){
     haveProblem = true;
   }else{
     for (int s = 0; s < len; s++){
       if ( outBoxes[s] != outBoxes2[s] ){
         haveProblem = true;
       }
     }
   }

   if (haveProblem){
     cerr << "Have a problem!" << endl;

     cout << "In:   " << Boxes.size()     << endl;
     cout << "Out:  " << outBoxes.size()  << endl;
     cout << "Out2: " << outBoxes2.size() << endl;
     
     vector<Box> W;
     W.push_back(Box(xl, yl, xh, yh));
   
     saveBoxes( Boxes,    "all.xg",       "blue"   );
     saveBoxes( W,        "window.xg",    "green"  );
     saveBoxes( outBoxes, "outBoxes.xg",  "white"  );
     saveBoxes( outBoxes, "outBoxes2.xg", "yellow" );
     exit(1);
   }

   if (q%1000 == 0){
     cout << "In:   " << Boxes.size()     << endl;
     cout << "Out:  " << outBoxes.size()  << endl;
     cout << "Out2: " << outBoxes2.size() << endl;
   }
   
 }

 return 0;
}
