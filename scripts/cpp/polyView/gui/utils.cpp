#include <iostream>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <limits>
#include <cstring>
#include "utils.h"

using namespace std;

void utils::printUsage(std::string progName){
  std::cout << "Usage: " << progName << " "
            << "[ -geo[metry] 1000x800 ] [ -c[olor] yellow ] [ -lw | -lineWidth 2 ] "
            << "[ -p[oints] ] [ -nc | -noClosedPolys ] "
            << "file_1.xg ... file_N.xg " << std::endl;
}

void utils::extractWindowDims(// inputs
                              int numArgs, char ** args,
                              // outputs
                              int & windowWidX, int & windowWidY
                              ){

  // Parse the command line arguments '-geo[metry] 500x600'

  windowWidX = 900; windowWidY = 700; // defaults
   
  for (int s = 1; s < numArgs; s++){

    if ( !strstr(args[s-1], "-geo") ) continue;

    string lineStr = args[s];
    char * line    = (char*) lineStr.c_str();
    
    // Blank the geometry settings once located
    // to not confuse other parsers.
    args[s-1][0] = '\0';
    args[s  ][0] = '\0';
    
    char * pch, * delimiter = "x";
    
    pch = strtok (line, delimiter);
    if (pch == NULL) continue;
    int windowWidX_tmp = atoi(pch);

    pch = strtok (NULL, delimiter);
    if (pch == NULL) continue;
    int windowWidY_tmp = atoi(pch);
    
    if (windowWidX_tmp > 0 && windowWidY_tmp > 0){
      windowWidX = windowWidX_tmp;
      windowWidY = windowWidY_tmp;
    }
    
  }
  
}

void utils::parseCmdOptions(//inputs
                            int argc, char** argv, std::string exeName,
                            // outputs
                            int & windowWidX, int & windowWidY, cmdLineOptions & options
                            ){

  // To do: Convert everywhere below from strncmp to strcmp.
  
  options.useCmdLineColors = false;
  options.noClosedPolys    = false;
  options.lineWidth        = 1;
  options.cmdLineColors.clear();
  options.polyFilesVec.clear();
  options.plotPointsOnlyVec.clear();

  // Skip argv[0] as that's the program name
  extractWindowDims(argc - 1, argv + 1, windowWidX, windowWidY);

  string color        = "yellow"; // default command line color
  bool plotPointsOnly = false; // plot the edges or just the vertices
  
  for (int argIter = 1; argIter < argc; argIter++){

    char * currArg = argv[argIter];

    if (currArg == NULL || strlen(currArg) == 0) continue;

    if (currArg[0] == '-'){
      // Transform -P into -p, etc.
      transform(currArg, currArg + strlen(currArg), currArg, ::tolower);
    }

    if (strncmp ( currArg, "-?",  strlen("-?")  ) == 0 ||
        strncmp ( currArg, "-h",  strlen("-h")  ) == 0 ||
        strncmp ( currArg, "--h", strlen("--h") ) == 0){
      printUsage(exeName);
      exit(0);
    }

    if ( strncmp (currArg, "-p",  strlen("-p")) == 0 ){
      plotPointsOnly = !plotPointsOnly;
      continue;
    }

    if ( (strcmp(currArg, "-lw") == 0 || strcmp(currArg, "-linewidth") == 0 )
         &&
         argIter < argc - 1
         ){
      int lw = atoi(argv[argIter + 1]);
      if (lw > 0) options.lineWidth = lw;
      argIter++;
      continue;
    }

    if ( strncmp (currArg, "-nc",             strlen("-nc"))            == 0 || 
         strncmp (currArg, "-noclosedpolys",  strlen("-noclosedpolys")) == 0
         ){
      options.noClosedPolys = true;
      continue;
    }

    if ( strncmp (currArg, "-c",  2) == 0 && argIter < argc - 1){
      options.useCmdLineColors = true;
      color = argv[argIter + 1];
      argIter++;
      continue;
    }

    // Other command line options are ignored
    if (currArg[0] == '-') continue;
    
    options.cmdLineColors.push_back(color);
    options.polyFilesVec.push_back(currArg);
    options.plotPointsOnlyVec.push_back(plotPointsOnly);
  }
  
  return;
}

std::string utils::inFileToOutFile(const std::string & inFile){

  string outFile = "";

  bool lastDot = true;
  for (int s = (int)inFile.length() - 1; s >= 0; s--){

    string currChar = inFile.substr(s, 1);
    if ( currChar == "/"){
      break; // strip path
    }else if (currChar == "." && lastDot){
      outFile = string("_out") + currChar + outFile;
      lastDot = false;
    }else{
      outFile = currChar + outFile;
    }
    
  }

  if (outFile.length() == 0){
    cerr << "Invalid filename" << endl;
  }
  
  return outFile;
  
}

std::string utils::getFilenameExtension(std::string filename){

  std::string::size_type idx;
  idx = filename.rfind('.');

  if(idx != std::string::npos) return filename.substr(idx+1);
  else                         return "";
}

std::string utils::replaceAll(std::string result, 
                              const std::string & replaceWhat, 
                              const std::string & replaceWithWhat){
  
  while(1){
    const int pos = result.find(replaceWhat);
    if (pos == -1) break;
    result.replace(pos,replaceWhat.size(),replaceWithWhat);
  }
  return result;
}

