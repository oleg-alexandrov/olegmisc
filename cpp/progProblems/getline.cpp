#include <iostream>
#include <cmath>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>

using namespace std;

void getChars(ifstream & is, std::string & chars){

  // Get a number of characters from an input stream. Return zero
  // characters only if the end of the stream was reached.
  
  chars = "";
  int count = 0;
  char k = '\n';
  while (count < 10 && is >> k){
    count++;
    chars += k;
  }

  return;
}

bool getLine(ifstream & is, std::string & line){

  // Implement getLine using getChars

  static string prevLeftover = ""; // What is left from a previous getChars operation
  
  bool endOfFile = false;
  string chars;
  
  while(1){

    // Keep on reading characters until the end of file or a newline is reached
    getChars(is, chars);

    if (chars == ""){
      endOfFile = true;
      break; // reached the end of the stream
    }
    
    prevLeftover += chars;

    bool foundNewline = false;
    for (int s = 0; s < (int)chars.size(); s++){
      if (chars[s] == '\n'){
        foundNewline = true;
        break;
      }
    }
    if (foundNewline) break;
      
  }

  if (endOfFile && prevLeftover == ""){
    // End of file was reached, and all leftovers have been exhausted
    line = "";
    return false;
  }
  
  // We have a bunch of characters in prevLeftover. Return all
  // characters until (but not including) the first newline, or until
  // the end of prevLeftover is reached. The characters after the
  // first newline are stored in prevLeftover for a future call to
  // this function.

  string buffer = prevLeftover;
  line          = "";
  prevLeftover  = "";

  bool appToLine = true;

  for (int s = 0; s < (int)buffer.size(); s++){

    char k = buffer[s];
    if (appToLine){
      if (k != '\n') line += k;
      else appToLine = false;
    }else{
      prevLeftover += k;
    }
    
  }
  
  return true;
}
  
int main() {

  string file = "data.txt";
  ifstream is(file.c_str());
  is.unsetf(ios_base::skipws); // don't skip any whitespace

  ofstream os("out.txt");
  
  string line;
  while(getLine(is, line)){
    cout << line << endl;
    cout << "----" << endl;
    os << line << endl;
  }

  os.close();
  is.close();

  ofstream os2("out2.txt");
  os2 << "x" << endl;
  os2.close();
  
  return 0;
}
