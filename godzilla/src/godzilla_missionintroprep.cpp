#include "sms/SmsPattern.h"
#include "sms/GodzillaCmp.h"
#include "sms/OneBppCmp.h"
#include "util/TIfstream.h"
#include "util/TOfstream.h"
#include "util/TBufStream.h"
#include "util/TGraphic.h"
#include "util/TPngConversion.h"
#include "util/TStringConversion.h"
#include "util/TOpt.h"
#include <string>
#include <iostream>

using namespace std;
using namespace BlackT;
using namespace Sms;

const static int tilemapBasePointer = 0x9781;
const static int tilemapSize = 224;
const static int listStartPos = 0x41655;
const static int listEndPos = 0x416CD;

int main(int argc, char* argv[]) {
  if (argc < 3) {
    cout << "Godzilla (Game Gear) mission intro prep tool"
      << endl;
    cout << "Usage: " << argv[0] << " <inrom> <outrom>"
      << endl;
    return 0;
  }
  
  TBufStream ifs;
  ifs.open(argv[1]);
  
  // replace all pointers to mission intro tilemaps with indices.
  // FFFF terminates each list.
  ifs.seek(listStartPos);
  while (ifs.tell() < listEndPos) {
    int ptr = ifs.readu16le();
    if (ptr == 0xFFFF) continue;
    int offset = ptr - tilemapBasePointer;
    
    int index = offset / tilemapSize;
    if ((index * tilemapSize) != offset) {
      cerr << "error: unexpected pointer at " << hex << ifs.tell() - 1
        << endl;
      continue;
    }
    
    ifs.seekoff(-2);
    ifs.writeu16le(index);
  }
  
  ifs.save(argv[2]);
  
  return 0;
}
