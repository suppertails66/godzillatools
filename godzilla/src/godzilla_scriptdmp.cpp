#include "util/TStringConversion.h"
#include "util/TBufStream.h"
#include "util/TIfstream.h"
#include "util/TOfstream.h"
#include "util/TThingyTable.h"
#include "exception/TGenericException.h"
#include <string>
#include <fstream>
#include <sstream>
#include <iostream>

using namespace std;
using namespace BlackT;

TThingyTable table;
TThingyTable table1bpp;
TThingyTable tablestd;
TThingyTable tablecompen;
TThingyTable tablefixed;

const static int op_1bpp_br         = 0xFE;
const static int op_1bpp_terminator = 0xFF;

void dump1bppString(TStream& ifs, std::ostream& ofs, int offset, int slot,
              string comment = "") {
  ifs.seek(offset);
  
  std::ostringstream oss;
  
  if (comment.size() > 0)
    oss << "// " << comment << endl;
  
  // comment out first line of original text
  oss << "// ";
  while (true) {
    
    TThingyTable::MatchResult result = table1bpp.matchId(ifs);
    if (result.id == -1) {
      throw TGenericException(T_SRCANDLINE,
                              "dumpString(TStream&, std::ostream&)",
                              string("At offset ")
                                + TStringConversion::intToString(
                                    ifs.tell(),
                                    TStringConversion::baseHex)
                                + ": unknown character '"
                                + TStringConversion::intToString(
                                    (unsigned char)ifs.peek(),
                                    TStringConversion::baseHex)
                                + "'");
    }
    
    string resultStr = table1bpp.getEntry(result.id);
    oss << resultStr;
    
    if (result.id == op_1bpp_terminator) {
//      oss << endl;
      oss << endl << endl;
      oss << resultStr;
      break;
    }
    else if (result.id == op_1bpp_br) {
      oss << endl;
      oss << "// ";
    }
  }
  
  ofs << "#STARTMSG("
      // offset
      << TStringConversion::intToString(
          offset, TStringConversion::baseHex)
      << ", "
      // size
      << TStringConversion::intToString(
          ifs.tell() - offset, TStringConversion::baseDec)
      << ", "
      // slot num
      << TStringConversion::intToString(
          slot, TStringConversion::baseDec)
      << ")" << endl << endl;
  
  ofs << oss.str();
  
//  oss << endl;
  ofs << endl << endl;
//  ofs << "//   end pos: "
//      << TStringConversion::intToString(
//          ifs.tell(), TStringConversion::baseHex)
//      << endl;
//  ofs << "//   size: " << ifs.tell() - offset << endl;
  ofs << endl;
  ofs << "#ENDMSG()";
  ofs << endl << endl;
}

void dump1bppStringSet(TStream& ifs, std::ostream& ofs, int startOffset, int slot,
               int numStrings,
               string comment = "") {
  if (comment.size() > 0) {
    ofs << "//=======================================" << endl;
    ofs << "// " << comment << endl;
    ofs << "//=======================================" << endl;
    ofs << endl;
  }
  
  ifs.seek(startOffset);
  for (int i = 0; i < numStrings; i++) {
    ofs << "// substring " << i << endl;
    dump1bppString(ifs, ofs, ifs.tell(), slot);
  }
}

void dumpTilemap(TStream& ifs, std::ostream& ofs, int offset, int slot,
              TThingyTable& tbl, int w, int h,
              bool isHalved = true,
              string comment = "") {
  ifs.seek(offset);
  
  std::ostringstream oss;
  
  if (comment.size() > 0)
    oss << "// " << comment << endl;
  
  // comment out first line of original text
  oss << "// ";
  for (int j = 0; j < h; j++) {
    for (int i = 0; i < w; i++) {
    
//      TThingyTable::MatchResult result = tbl.matchId(ifs);
      
      TByte next = ifs.get();
      if (!tbl.hasEntry(next)) {
        throw TGenericException(T_SRCANDLINE,
                                "dumpTilemap()",
                                string("At offset ")
                                  + TStringConversion::intToString(
                                      ifs.tell() - 1,
                                      TStringConversion::baseHex)
                                  + ": unknown character '"
                                  + TStringConversion::intToString(
                                      (unsigned char)next,
                                      TStringConversion::baseHex)
                                  + "'");
      }
      
//      string resultStr = tbl.getEntry(result.id);
      string resultStr = tbl.getEntry(next);
      oss << resultStr;
      
      if (!isHalved) ifs.get();
    }
    
    // end of line
    oss << endl;
    oss << "// ";
  }
  
//  oss << endl << endl << "[end]";
  
  ofs << "#STARTMSG("
      // offset
      << TStringConversion::intToString(
          offset, TStringConversion::baseHex)
      << ", "
      // size
      << TStringConversion::intToString(
          ifs.tell() - offset, TStringConversion::baseDec)
      << ", "
      // slot num
      << TStringConversion::intToString(
          slot, TStringConversion::baseDec)
      << ")" << endl << endl;
  
  ofs << oss.str();
  
//  oss << endl;
  ofs << endl << endl;
//  ofs << "//   end pos: "
//      << TStringConversion::intToString(
//          ifs.tell(), TStringConversion::baseHex)
//      << endl;
//  ofs << "//   size: " << ifs.tell() - offset << endl;
  ofs << endl;
  ofs << "#ENDMSG()";
  ofs << endl << endl;
}

void dumpTilemapSet(TStream& ifs, std::ostream& ofs, int startOffset, int slot,
               TThingyTable& tbl, int w, int h,
               int numTilemaps,
               bool isHalved = true,
               string comment = "") {
  if (comment.size() > 0) {
    ofs << "//=======================================" << endl;
    ofs << "// " << comment << endl;
    ofs << "//=======================================" << endl;
    ofs << endl;
  }
  
  ifs.seek(startOffset);
  for (int i = 0; i < numTilemaps; i++) {
    ofs << "// tilemap " << i << endl;
    dumpTilemap(ifs, ofs, ifs.tell(), slot, tbl, w, h, isHalved);
  }
}

//void dumpString(TStream& ifs, std::ostream& ofs, int offset) {
//  ifs.seek(offset);
//  dumpString(ifs, ofs);
//}

void dumpCredits(TStream& ifs, std::ostream& ofs, int offset, int slot,
              TThingyTable& tbl, string comment = "") {
  ifs.seek(offset);
  
  std::ostringstream oss;
  
  if (comment.size() > 0)
    oss << "// " << comment << endl;
  
  // comment out first line of original text
  oss << "// ";
  while (true) {
    
    TThingyTable::MatchResult result = tbl.matchId(ifs);
    if (result.id == -1) {
      throw TGenericException(T_SRCANDLINE,
                              "dumpString(TStream&, std::ostream&)",
                              string("At offset ")
                                + TStringConversion::intToString(
                                    ifs.tell(),
                                    TStringConversion::baseHex)
                                + ": unknown character '"
                                + TStringConversion::intToString(
                                    (unsigned char)ifs.peek(),
                                    TStringConversion::baseHex)
                                + "'");
    }
    
    string resultStr = tbl.getEntry(result.id);
    oss << resultStr;
    
    if (result.id == 0xFF) {
//      oss << endl;
      oss << endl << endl;
      oss << resultStr;
      break;
    }
    else if (result.id == 0xFE) {
      oss << endl;
      oss << "// ";
    }
  }
  
  ofs << "#STARTMSG("
      // offset
      << TStringConversion::intToString(
          offset, TStringConversion::baseHex)
      << ", "
      // size
      << TStringConversion::intToString(
          ifs.tell() - offset, TStringConversion::baseDec)
      << ", "
      // slot num
      << TStringConversion::intToString(
          slot, TStringConversion::baseDec)
      << ")" << endl << endl;
  
  ofs << oss.str();
  
//  oss << endl;
  ofs << endl << endl;
//  ofs << "//   end pos: "
//      << TStringConversion::intToString(
//          ifs.tell(), TStringConversion::baseHex)
//      << endl;
//  ofs << "//   size: " << ifs.tell() - offset << endl;
  ofs << endl;
  ofs << "#ENDMSG()";
  ofs << endl << endl;
}

int main(int argc, char* argv[]) {
  if (argc < 3) {
    cout << "Godzilla (Game Gear) script dumper" << endl;
    cout << "Usage: " << argv[0] << " [rom] [outprefix]" << endl;
    
    return 0;
  }
  
  string romName = string(argv[1]);
//  string tableName = string(argv[2]);
  string outPrefix = string(argv[2]);
  
  TBufStream ifs;
  ifs.open(romName.c_str());
  
  table1bpp.readSjis(string("table/godzilla_1bpp.tbl"));
  tablestd.readSjis(string("table/godzilla_std.tbl"));
  tablecompen.readSjis(string("table/godzilla_compendium.tbl"));
  tablefixed.readSjis(string("table/godzilla_fixed.tbl"));
  
  
  {     
    std::ofstream ofs((outPrefix + "misc.txt").c_str(),
                  ios_base::binary);
    dump1bppStringSet(ifs, ofs, 0xC5B8, 2, 1,
      "battle: usually-loaded charset (tile indices 0x30-0x5F)");
  }
  
  
  {     
    std::ofstream ofs((outPrefix + "unitnames.txt").c_str(),
                  ios_base::binary);
    
    // unit names require special handling because diacritics are stored
    // separately from the regular characters.
    // regular name table = 0xC5E9
    // diacritic table = 0x924E
    
    TBufStream diaifs;
    diaifs.open(romName.c_str());
    
    ifs.seek(0xC5E9);
    diaifs.seek(0x924E);
    
    TBufStream buf(0x10000);
    
    for (int j = 0; j < 55; j++) {
      for (int i = 0; i < 8; i++) {
        if ((unsigned char)diaifs.peek() != 0xFF) {
          char nextDia = diaifs.readu8();
          if (nextDia != 0) {
            // dakuten
            if (nextDia == 0x08) buf.put(0x9E);
            // handakuten
            if (nextDia == 0x09) buf.put(0x9F);
          }
        }
        
        buf.put(ifs.get());
      }
      
      // terminator
      buf.put(ifs.get());
      diaifs.get();
    }
    
    
//    dump1bppStringSet(ifs, ofs, 0xC5E9, 2, 55, "unit names");
    buf.seek(0);
    dump1bppStringSet(buf, ofs, 0x0, 2, 55, "unit names");
  }
  
  
  {     
    std::ofstream ofs((outPrefix + "unitnames_battle.txt").c_str(),
                  ios_base::binary);
    
    // unit names require special handling because diacritics are stored
    // separately from the regular characters.
    // regular name table = 0x74678
    // diacritic table = 0x7479D
    
    TBufStream diaifs;
    diaifs.open(romName.c_str());
    
    ifs.seek(0x74678);
    diaifs.seek(0x7479D);
    
    TBufStream buf(0x10000);
    
    for (int j = 0; j < 33; j++) {
      int base = diaifs.tell();
      for (int i = 0; i < 8; i++) {
        int nextDia = diaifs.readu8();
        // dakuten
        if (nextDia == 0xCE) buf.put(0x9E);
        // handakuten
        else if (nextDia == 0xCF) buf.put(0x9F);
        
        if ((unsigned char)ifs.peek() != 0xFF)
          buf.put(ifs.get());
      }
      
      // terminator
      buf.put(ifs.get());
      
      diaifs.seek(base + 0x10);
    }
    
    
//    dump1bppStringSet(ifs, ofs, 0xC5E9, 2, 55, "unit names");
    buf.seek(0);
    dump1bppStringSet(buf, ofs, 0x0, 2, 33, "unit names (battle)");
  }
  
  {     
    std::ofstream ofs((outPrefix + "dialogue.txt").c_str(),
                  ios_base::binary);
    dump1bppStringSet(ifs, ofs, 0xD143, 2, 93, "dialogue messages");
  }
  
  {     
    std::ofstream ofs((outPrefix + "dialogue_names.txt").c_str(),
                  ios_base::binary);
    dump1bppStringSet(ifs, ofs, 0xCBBD, 2, 3, "dialogue message names");
  }
  
  {     
    std::ofstream ofs((outPrefix + "tryagain.txt").c_str(),
                  ios_base::binary);
    dumpTilemapSet(ifs, ofs, 0x43EEA, 2,
                   tablestd, 0x10, 6,
                   2, true, "?");
  }
  
  {     
    std::ofstream ofs((outPrefix + "compendium.txt").c_str(),
                  ios_base::binary);
    dumpTilemapSet(ifs, ofs, 0x77486, 2,
                   tablecompen, 10, 6,
                   36, true, "compendium pages");
  }
  
  {     
    std::ofstream ofs((outPrefix + "destroy_spacemonsters.txt").c_str(),
                  ios_base::binary);
    dumpTilemapSet(ifs, ofs, 0x6948F, 2,
                   tablestd, 18, 8,
                   2, true, "must destroy space monsters message");
  }
  
  {     
    std::ofstream ofs((outPrefix + "missionintro_topwin.txt").c_str(),
                  ios_base::binary);
    dumpTilemapSet(ifs, ofs, 0x416CD, 2,
                   tablestd, 10, 3,
                   1, false, "stage clear");
    dumpTilemapSet(ifs, ofs, 0x41709, 2,
                   tablestd, 10, 3,
                   1, false, "hint");
    dumpTilemapSet(ifs, ofs, 0x41745, 2,
                   tablestd, 10, 3,
                   1, false, "game over");
  }
  
  {     
    std::ofstream ofs((outPrefix + "missionintro.txt").c_str(),
                  ios_base::binary);
//    for (int i = 0; i < 256; i++) {
//      std::cerr << std::hex << i << " " << tablestd.hasEntry(i) << std::endl;
//    }
    dumpTilemapSet(ifs, ofs, 0x41781, 2,
                   tablestd, 16, 7,
                   39, false, "mission description text");
  }
  
  {     
    std::ofstream ofs((outPrefix + "unitactions.txt").c_str(),
                  ios_base::binary);
    dumpTilemapSet(ifs, ofs, 0x8D47, 2,
                   tablefixed, 9, 5,
                   1, false, "unit actions list 1");
    dumpTilemapSet(ifs, ofs, 0x8D8F, 2,
                   tablefixed, 9, 5,
                   1, false, "unit actions list 2");
    dumpTilemapSet(ifs, ofs, 0x8DD7, 2,
                   tablefixed, 9, 5,
                   1, false, "unit actions list 3");
  }
  
  {     
    std::ofstream ofs((outPrefix + "turnstartwindows.txt").c_str(),
                  ios_base::binary);
    dumpTilemapSet(ifs, ofs, 0x90F9, 2,
                   tablefixed, 16, 3,
                   1, false, "turn start windows 1");
    dumpTilemapSet(ifs, ofs, 0x9159, 2,
                   tablefixed, 10, 3,
                   1, false, "turn start windows 2");
    dumpTilemapSet(ifs, ofs, 0x9195, 2,
                   tablefixed, 10, 3,
                   1, false, "turn start windows 3");
  }
  
  {     
    std::ofstream ofs((outPrefix + "credits.txt").c_str(),
                  ios_base::binary);
    dumpCredits(ifs, ofs, 0x2C947, 2,
                   tablestd, "credits");
  }
  
  
  return 0;
} 
