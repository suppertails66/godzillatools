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

int mapsPerFile = 5;
int mapWidth = 256;
int mapHeight = 48;

void doGraphic(string inFile, string outPrefix) {
  TGraphic inGrp;
  TPngConversion::RGBAPngToGraphic(inFile, inGrp);

  for (int i = 0; i < mapsPerFile; i++) {
    TGraphic outGrp(mapWidth, mapHeight);
    outGrp.clearTransparent();
    outGrp.copy(inGrp,
                TRect(0, 0, 0, 0),
                TRect(i * mapWidth, 0, mapWidth, mapHeight));
    TPngConversion::graphicToRGBAPng(
      outPrefix + TStringConversion::intToString(i) + ".png", outGrp);
  }
}

int main(int argc, char* argv[]) {
  if (argc < 3) {
    cout << "Godzilla (Game Gear) intro prep tool"
      << endl;
    cout << "Usage: " << argv[0] << " <inprefix> <outprefix>"
      << endl;
    return 0;
  }
  
  string inPrefix = string(argv[1]);
  string outPrefix = string(argv[2]);
  
  for (int i = 1; i <= 5; i++) {
    doGraphic(inPrefix + "intro_scroll_"
                + TStringConversion::intToString(i)
                +".png",
              outPrefix + "intro_scroll_"
                + TStringConversion::intToString(i)
                + "_");
  }
  
  return 0;
}
