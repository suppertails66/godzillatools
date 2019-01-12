#include "util/TStringConversion.h"
#include "util/TBufStream.h"
#include "util/TIfstream.h"
#include "util/TOfstream.h"
#include "util/TThingyTable.h"
#include "godzilla/GodzillaScriptReader.h"
#include "godzilla/GodzillaLineWrapper.h"
#include "exception/TGenericException.h"
#include <string>
#include <map>
#include <fstream>
#include <iostream>

using namespace std;
using namespace BlackT;
using namespace Sms;

TThingyTable table;

const static int hashMask = 0x1FFF;

const static int op_tilebr     = 0xEF;
const static int op_br         = 0xFE;
const static int op_terminator = 0xFF;

string getStringName(GodzillaScriptReader::ResultString result) {
//  int bankNum = result.srcOffset / 0x4000;
  return string("string_")
    + TStringConversion::intToString(result.srcOffset,
          TStringConversion::baseHex);
}

void exportRawResults(GodzillaScriptReader::ResultCollection& results,
                      std::string filename) {
  TBufStream ofs(0x10000);
  for (int i = 0; i < results.size(); i++) {
    ofs.write(results[i].str.c_str(), results[i].str.size());
  }
  ofs.save((filename).c_str());
}

void exportRawResults(TStream& ifs,
                      std::string filename) {
  GodzillaScriptReader::ResultCollection results;
  GodzillaScriptReader(ifs, results, table)();
  exportRawResults(results, filename);
}

void exportTabledResults(TStream& ifs,
                         std::string binFilename,
                         GodzillaScriptReader::ResultCollection& results,
                         TBufStream& ofs) {
  int offset = 0;
  for (int i = 0; i < results.size(); i++) {
    ofs.writeu16le(offset + (results.size() * 2));
    offset += results[i].str.size();
  }
  
  for (int i = 0; i < results.size(); i++) {
    ofs.write(results[i].str.c_str(), results[i].str.size());
  }
  
  ofs.save((binFilename).c_str());
}

void exportTabledResults(TStream& ifs,
                         std::string binFilename) {
  GodzillaScriptReader::ResultCollection results;
  GodzillaScriptReader(ifs, results, table)();
  
//  std::ofstream incofs(incFilename.c_str());
  TBufStream ofs(0x10000);
  exportTabledResults(ifs, binFilename, results, ofs);
}

void exportSizeTabledResults(TStream& ifs,
                         std::string binFilename) {
  GodzillaScriptReader::ResultCollection results;
  GodzillaScriptReader(ifs, results, table)();
  
//  std::ofstream incofs(incFilename.c_str());
  TBufStream ofs(0x10000);
  ofs.writeu8(results.size());
  exportTabledResults(ifs, binFilename, results, ofs);
}

int main(int argc, char* argv[]) {
  if (argc < 4) {
    cout << "Godzilla script builder" << endl;
    cout << "Usage: " << argv[0] << " [inprefix] [thingy] [outprefix]"
      << endl;
    
    return 0;
  }
  
  string inPrefix = string(argv[1]);
  string tableName = string(argv[2]);
  string outPrefix = string(argv[3]);
  
  table.readSjis(tableName);
  
  // wrap script
  {
    // read size table
    GodzillaLineWrapper::CharSizeTable sizeTable;
    {
      TBufStream ifs;
      ifs.open("out/font/sizetable.bin");
      int pos = 0;
      while (!ifs.eof()) {
        sizeTable[pos++] = ifs.readu8();
      }
    }
    
    {
      TBufStream ifs;
      ifs.open((inPrefix + "dialogue.txt").c_str());
      
      TLineWrapper::ResultCollection results;
      GodzillaLineWrapper(ifs, results, table, sizeTable)();
      
      if (results.size() > 0) {
        TOfstream ofs((outPrefix + "dialogue_wrapped.txt").c_str());
        ofs.write(results[0].str.c_str(), results[0].str.size());
      }
    }
    
    {
      TBufStream ifs;
      ifs.open((inPrefix + "missionintro.txt").c_str());
      
      TLineWrapper::ResultCollection results;
      GodzillaLineWrapper(ifs, results, table, sizeTable)();
      
      if (results.size() > 0) {
        TOfstream ofs((outPrefix + "missionintro_wrapped.txt").c_str());
        ofs.write(results[0].str.c_str(), results[0].str.size());
      }
    }
    
    {
      TBufStream ifs;
      ifs.open((inPrefix + "compendium.txt").c_str());
      
      TLineWrapper::ResultCollection results;
      GodzillaLineWrapper(ifs, results, table, sizeTable)();
      
      if (results.size() > 0) {
        TOfstream ofs((outPrefix + "compendium_wrapped.txt").c_str());
        ofs.write(results[0].str.c_str(), results[0].str.size());
      }
    }
    
    {
      TBufStream ifs;
      ifs.open((inPrefix + "destroy_spacemonsters.txt").c_str());
      
      TLineWrapper::ResultCollection results;
      GodzillaLineWrapper(ifs, results, table, sizeTable)();
      
      if (results.size() > 0) {
        TOfstream ofs((outPrefix + "destroy_spacemonsters_wrapped.txt").c_str());
        ofs.write(results[0].str.c_str(), results[0].str.size());
      }
    }
    
    {
      TBufStream ifs;
      ifs.open((inPrefix + "tryagain.txt").c_str());
      
      TLineWrapper::ResultCollection results;
      GodzillaLineWrapper(ifs, results, table, sizeTable)();
      
      if (results.size() > 0) {
        TOfstream ofs((outPrefix + "tryagain_wrapped.txt").c_str());
        ofs.write(results[0].str.c_str(), results[0].str.size());
      }
    }
    
/*    {
      TBufStream ifs;
      ifs.open((inPrefix + "manual_ingame.txt").c_str());
      
      TLineWrapper::ResultCollection results;
      GodzillaLineWrapper(ifs, results, table, sizeTable)();
      
      if (results.size() > 0) {
        TOfstream ofs((outPrefix + "manual_ingame_wrapped.txt").c_str());
        ofs.write(results[0].str.c_str(), results[0].str.size());
      }
    } */
  }
  
  // remapped strings
/*  {
    TBufStream ifs;
//    ifs.open((inPrefix + "script.txt").c_str());
    ifs.open((outPrefix + "script_wrapped.txt").c_str());
    
    GodzillaScriptReader::ResultCollection results;
    GodzillaScriptReader(ifs, results, table)();
    
//    TBufStream ofs(0x20000);
//    for (unsigned int i = 0; i < results.size(); i++) {
//      ofs.write(results[i].str.c_str(), results[i].str.size());
//    }
//    ofs.save((outPrefix + "script.bin").c_str());
    
    // create:
    // * an individual .bin file for each compiled string
    // * a .inc containing, for each string, one superfree section with an
    //   incbin that includes the corresponding string's .bin
    // * a .inc containing the hash bucket arrays for the remapped strings.
    //   table keys are (orig_pointer & 0x1FFF).
    //   the generated bucket sets go in a single superfree section.
    //   each bucket set is an array of the following structure (terminate
    //   arrays with FF so we can detect missed entries):
    //       struct Bucket {
    //       u8 origBank
    //       u16 origPointer  // respects original slotting!
    //       u8 newBank
    //       u16 newPointer
    //     }
    // * a .inc containing the bucket array start pointers (keys are 16-bit
    //   and range from 0x0000-0x1FFF, so this gets its own bank)
    
    std::ofstream strIncOfs((outPrefix + "strings.inc").c_str());
    std::map<int, GodzillaScriptReader::ResultCollection>
      mappedStringBuckets;
    for (unsigned int i = 0; i < results.size(); i++) {
      std::string stringName = getStringName(results[i]);
      
      // write string to file
      TBufStream ofs(0x10000);
      ofs.write(results[i].str.c_str(), results[i].str.size());
      ofs.save((outPrefix + "strings/" + stringName + ".bin").c_str());
      
      // add string binary to generated includes
      strIncOfs << ".slot 1" << endl;
      strIncOfs << ".section \"string include " << i << "\" superfree"
        << endl;
      strIncOfs << "  " << stringName << ":" << endl;
      strIncOfs << "    " << ".incbin \""
        << outPrefix << "strings/" << stringName << ".bin"
        << "\"" << endl;
      strIncOfs << ".ends" << endl;
      
      // add to map
      mappedStringBuckets[results[i].srcOffset & hashMask]
        .push_back(results[i]);
    }
    
    // generate bucket arrays
    std::ofstream stringHashOfs(
      (outPrefix + "string_bucketarrays.inc").c_str());
    stringHashOfs << ".include \""
      << outPrefix + "strings.inc\""
      << endl;
    stringHashOfs << ".section \"string hash buckets\" superfree" << endl;
    stringHashOfs << "  stringHashBuckets:" << endl;
    for (std::map<int, GodzillaScriptReader::ResultCollection>::iterator it
           = mappedStringBuckets.begin();
         it != mappedStringBuckets.end();
         ++it) {
      int key = it->first;
      GodzillaScriptReader::ResultCollection& results = it->second;
      
      stringHashOfs << "  hashBucketArray_"
        << TStringConversion::intToString(key,
              TStringConversion::baseHex)
        << ":" << endl;
      
      for (unsigned int i = 0; i < results.size(); i++) {
        GodzillaScriptReader::ResultString result = results[i];
        string stringName = getStringName(result);
        
        // original bank
        stringHashOfs << "    .db " << result.srcOffset / 0x4000 << endl;
        // original pointer (respecting slotting)
        stringHashOfs << "    .dw "
          << (result.srcOffset & 0x3FFF) + (0x4000 * result.srcSlot)
          << endl;
        // new bank
        stringHashOfs << "    .db :" << stringName << endl;
        // new pointer
        stringHashOfs << "    .dw " << stringName << endl;
      }
      
      // array terminator
      stringHashOfs << "  .db $FF " << endl;
    }
    stringHashOfs << ".ends" << endl;
    
    // generate bucket array hash table
    std::ofstream bucketHashOfs(
      (outPrefix + "string_bucket_hashtable.inc").c_str());
    bucketHashOfs << ".include \""
      << outPrefix + "string_bucketarrays.inc\""
      << endl;
    bucketHashOfs
      << ".section \"bucket array hash table\" size $4000 align $4000 superfree"
      << endl;
    bucketHashOfs << "  bucketArrayHashTable:" << endl;
    for (int i = 0; i < hashMask; i++) {
      std::map<int, GodzillaScriptReader::ResultCollection>::iterator findIt
        = mappedStringBuckets.find(i);
      if (findIt != mappedStringBuckets.end()) {
        int key = findIt->first;
        bucketHashOfs << "    .dw hashBucketArray_"
        << TStringConversion::intToString(key,
              TStringConversion::baseHex)
        << endl;
      }
      else {
        // no array
        bucketHashOfs << "    .dw $FFFF" << endl;
      }
    }
    bucketHashOfs << ".ends" << endl;
  } */
  
  // dialogue
  {
    TBufStream ifs;
    ifs.open((outPrefix + "dialogue_wrapped.txt").c_str());
    
    exportTabledResults(ifs, outPrefix + "dialogue.bin");
  }
  
  // dialogue names
  {
    TBufStream ifs;
    ifs.open((inPrefix + "dialogue_names.txt").c_str());
    
    exportTabledResults(ifs, outPrefix + "dialogue_names.bin");
  }
  
  // mission intro
  {
    TBufStream ifs;
    ifs.open((outPrefix + "missionintro_wrapped.txt").c_str());
    
    exportTabledResults(ifs, outPrefix + "missionintro.bin");
  }
  
  // mission (in)complete
  {
    TBufStream ifs;
    ifs.open((outPrefix + "destroy_spacemonsters_wrapped.txt").c_str());
    
    exportRawResults(ifs, outPrefix + "destroy_spacemonsters_gforce.bin");
    exportRawResults(ifs, outPrefix + "destroy_spacemonsters_godzilla.bin");
  }
  
  // "congratulations" screen
  {
    TBufStream ifs;
    ifs.open((outPrefix + "tryagain_wrapped.txt").c_str());
    
    exportRawResults(ifs, outPrefix + "tryagain_gforce.bin");
    exportRawResults(ifs, outPrefix + "tryagain_godzilla.bin");
  }
  
  // mission intro top window
  {
    TBufStream ifs;
    ifs.open((inPrefix + "missionintro_topwin.txt").c_str());
    
    exportTabledResults(ifs, outPrefix + "missionintro_topwin.bin");
  }
  
  // unit names
  {
    TBufStream ifs;
    ifs.open((inPrefix + "unitnames.txt").c_str());
    
    exportTabledResults(ifs, outPrefix + "unitnames.bin");
  }
  
  // unit names (battle)
  {
    TBufStream ifs;
    ifs.open((inPrefix + "unitnames_battle.txt").c_str());
    
    exportTabledResults(ifs, outPrefix + "unitnames_battle.bin");
  }
  
  // compendium
  {
    TBufStream ifs;
    ifs.open((outPrefix + "compendium_wrapped.txt").c_str());
    
    for (int i = 0; i < 12; i++) {
      exportTabledResults(ifs, outPrefix + "compendium"
        + TStringConversion::intToString(i) + ".bin");
    }
  }
  
  // compendium height/weight
  {
    TBufStream ifs;
    ifs.open((inPrefix + "compendium_heightweight.txt").c_str());
    
    exportTabledResults(ifs, outPrefix + "compendium_height.bin");
    exportTabledResults(ifs, outPrefix + "compendium_weight.bin");
  }
  
  // credits
  {
    TBufStream ifs;
    ifs.open((inPrefix + "credits.txt").c_str());
    
    exportRawResults(ifs, outPrefix + "credits.bin");
  }
  
  // mission complete
  {
    TBufStream ifs;
    ifs.open((inPrefix + "missioncomplete.txt").c_str());
    
    exportRawResults(ifs, outPrefix + "missioncomplete_1.bin");
    exportRawResults(ifs, outPrefix + "missioncomplete_2.bin");
  }
  
  // new text
  {
    TBufStream ifs;
    ifs.open((inPrefix + "new.txt").c_str());
    
    // turn counter
    {
//      GodzillaScriptReader::ResultCollection results;
//      GodzillaScriptReader(ifs, results, table)();
      exportRawResults(ifs, outPrefix + "turn_counter.bin");
    }
    
    // slash (attack/defense separator)
    exportRawResults(ifs, outPrefix + "slash_atkdef.bin");
    
    // atk
    exportRawResults(ifs, outPrefix + "atk.bin");
    
    // def
    exportRawResults(ifs, outPrefix + "def.bin");
    
    // unit quantity
    exportRawResults(ifs, outPrefix + "unit_quantity.bin");
    
    // slash (current/max HP separator)
    exportRawResults(ifs, outPrefix + "slash_hp.bin");
    
    // start menu
    exportRawResults(ifs, outPrefix + "start_menu.bin");
    
    // y/n prompt
    exportRawResults(ifs, outPrefix + "yn_prompt.bin");
    
    // save prompt
    exportRawResults(ifs, outPrefix + "save_prompt.bin");
    
    // load prompt
    exportRawResults(ifs, outPrefix + "load_prompt.bin");
    
    // save confirmed
    exportRawResults(ifs, outPrefix + "save_done.bin");
    
    // load failed
    exportRawResults(ifs, outPrefix + "load_failed.bin");
    
    // merge prompt
    exportRawResults(ifs, outPrefix + "merge_prompt.bin");
    
    // unit menu 1
    exportRawResults(ifs, outPrefix + "unit_actions_1.bin");
    
    // unit menu 2
    exportRawResults(ifs, outPrefix + "unit_actions_2.bin");
    
    // unit menu 3
    exportRawResults(ifs, outPrefix + "unit_actions_3.bin");
    
    // turn change
    exportRawResults(ifs, outPrefix + "turn_change_cpu.bin");
    exportRawResults(ifs, outPrefix + "turn_change_player.bin");
    exportRawResults(ifs, outPrefix + "turn_change_spacemonsters.bin");
    
    // turns remaining (unit overview)
    exportRawResults(ifs, outPrefix + "turns_left_unitoverview.bin");
    
    // turns remaining (unit overview) (singular)
    exportRawResults(ifs, outPrefix + "turns_left_unitoverview_singular.bin");
    
    // monster arrival countdown message (unit overview)
    exportRawResults(ifs, outPrefix + "monster_arrival_unitoverview.bin");
    
    // monster arrival countdown message (unit overview) (singular)
    exportRawResults(ifs, outPrefix + "monster_arrival_unitoverview_singular.bin");
    
    // list of arrival monsters (unit overview)
    exportTabledResults(ifs, outPrefix + "monster_arrival_list_unitoverview.bin");
  }
  
  return 0;
}

