
echo "*******************************************************************************"
echo "Setting up environment..."
echo "*******************************************************************************"

set -o errexit

BASE_PWD=$PWD
PATH=".:./asm/bin/:$PATH"
INROM="godzilla.gg"
OUTROM="godzilla_en.gg"
WLADX="./wla-dx/binaries/wla-z80"
WLALINK="./wla-dx/binaries/wlalink"

cp "$INROM" "$OUTROM"

mkdir -p out

echo "*******************************************************************************"
echo "Building tools..."
echo "*******************************************************************************"

make blackt
make libsms
make

if [ ! -f $WLADX ]; then
  
  echo "********************************************************************************"
  echo "Building WLA-DX..."
  echo "********************************************************************************"
  
  cd wla-dx
    cmake -G "Unix Makefiles" .
    make
  cd $BASE_PWD
  
fi

# echo "*******************************************************************************"
# echo "Doing initial ROM prep..."
# echo "*******************************************************************************"
# 
# mkdir -p out
# mold_romprep "$OUTROM" "$OUTROM"

echo "*******************************************************************************"
echo "Prepping intro..."
echo "*******************************************************************************"

mkdir -p out/intro
godzilla_introprep rsrc/ out/intro/

echo "*******************************************************************************"
echo "Prepping mission intros..."
echo "*******************************************************************************"

godzilla_missionintroprep "$OUTROM" "$OUTROM"

echo "*******************************************************************************"
echo "Building font..."
echo "*******************************************************************************"

mkdir -p out/font
vwf_fontbuild rsrc/font_vwf/ out/font/

echo "*******************************************************************************"
echo "Building graphics..."
echo "*******************************************************************************"

mkdir -p out/precmp
mkdir -p out/grp
#grpundmp_gg rsrc/font.png out/precmp/font.bin 0x90
#grpundmp_gg rsrc/battle_font.png out/grp/battle_font.bin 0x17

grpundmp_gg rsrc/stageinfo.png out/grp/stageinfo.bin 8

grpundmp_gg rsrc/unit_moves_left.png out/grp/unit_moves_left.bin 8
filepatch "$OUTROM" 0x92CF out/grp/unit_moves_left.bin "$OUTROM"

grpundmp_gg rsrc/resupply_complete.png out/grp/resupply_complete.bin 8
filepatch "$OUTROM" 0x93CF out/grp/resupply_complete.bin "$OUTROM"

grpundmp_gg rsrc/completed_1.png out/grp/completed_1.bin 12 -r 4
grpundmp_gg rsrc/completed_2.png out/grp/completed_2.bin 12 -r 4
grpundmp_gg rsrc/completed_3.png out/grp/completed_3.bin 12 -r 4
grpundmp_gg rsrc/completed_4.png out/grp/completed_4.bin 12 -r 4
filepatch "$OUTROM" 0x3161E out/grp/completed_1.bin "$OUTROM"
filepatch "$OUTROM" 0x3179E out/grp/completed_2.bin "$OUTROM"
filepatch "$OUTROM" 0x3191E out/grp/completed_3.bin "$OUTROM"
filepatch "$OUTROM" 0x31A9E out/grp/completed_4.bin "$OUTROM"

grpundmp_gg rsrc/congratulations_continued_1.png out/grp/congratulations_continued_1.bin 6 -r 3
grpundmp_gg rsrc/congratulations_continued_2.png out/grp/congratulations_continued_2.bin 6 -r 3
filepatch "$OUTROM" 0x69AF3 out/grp/congratulations_continued_1.bin "$OUTROM"
filepatch "$OUTROM" 0x69BB3 out/grp/congratulations_continued_2.bin "$OUTROM"

grpundmp_gg rsrc/compendium_menulabel.png out/grp/compendium_menulabel.bin 9

grpundmp_gg rsrc/font_credits.png out/grp/font_credits.bin 0x50

echo "*******************************************************************************"
echo "Building tilemaps..."
echo "*******************************************************************************"
mkdir -p out/maps
mkdir -p out/grp
#tilemapper_gg tilemappers/title.txt
for file in tilemappers/*; do
  tilemapper_gg "$file"
done

filepatch "$OUTROM" 0x77DC0 out/maps/compendium_map_main.bin "$OUTROM"

filepatch "$OUTROM" 0x77D18 out/maps/compendium_map_name00.bin "$OUTROM"
filepatch "$OUTROM" 0x77D26 out/maps/compendium_map_name01.bin "$OUTROM"
filepatch "$OUTROM" 0x77D34 out/maps/compendium_map_name02.bin "$OUTROM"
filepatch "$OUTROM" 0x77D42 out/maps/compendium_map_name03.bin "$OUTROM"
filepatch "$OUTROM" 0x77D50 out/maps/compendium_map_name04.bin "$OUTROM"
filepatch "$OUTROM" 0x77D5E out/maps/compendium_map_name05.bin "$OUTROM"
filepatch "$OUTROM" 0x77D6C out/maps/compendium_map_name06.bin "$OUTROM"
filepatch "$OUTROM" 0x77D7A out/maps/compendium_map_name07.bin "$OUTROM"
filepatch "$OUTROM" 0x77D88 out/maps/compendium_map_name08.bin "$OUTROM"
filepatch "$OUTROM" 0x77D96 out/maps/compendium_map_name09.bin "$OUTROM"
filepatch "$OUTROM" 0x77DA4 out/maps/compendium_map_name0A.bin "$OUTROM"
filepatch "$OUTROM" 0x77DB2 out/maps/compendium_map_name0B.bin "$OUTROM"

filepatch "$OUTROM" 0x3F74F out/maps/title_godzilla_base.bin "$OUTROM"
filepatch "$OUTROM" 0x3F7F8 out/maps/title_godzilla_mouth.bin "$OUTROM"

#cat out/grp/title_grp.bin out/maps/title.bin rsrc_raw/title_pal.bin > out/precmp/title_data.bin

# echo "*******************************************************************************"
# echo "Compressing graphics..."
# echo "*******************************************************************************"
# 
# mkdir -p out/cmp
# for file in out/precmp/*; do
#   mold_grpcmp "$file" "out/cmp/$(basename $file)"
# done

# echo "*******************************************************************************"
# echo "Patching graphics..."
# echo "*******************************************************************************"
# 
# filepatch "$OUTROM" 0x5401C out/cmp/font.bin "$OUTROM" -l 2432
# filepatch "$OUTROM" 0x56D22 out/grp/battle_font.bin "$OUTROM"

echo "*******************************************************************************"
echo "Building script..."
echo "*******************************************************************************"

mkdir -p out/script
#mkdir -p out/script/strings
godzilla_scriptbuild script/ table/godzilla_vwf_en.tbl out/script/

echo "********************************************************************************"
echo "Applying ASM patches..."
echo "********************************************************************************"

mkdir -p "out/asm"
cp "$OUTROM" "asm/godzilla.gg"

cd asm
  # apply hacks
  ../$WLADX -I ".." -o "main.o" "main.s"
  ../$WLALINK -s -v linkfile godzilla_patched.gg
  
  mv -f "godzilla_patched.gg" "godzilla.gg"
  
  # update region code in header (WLA-DX forces it to 4,
  # for "export SMS", when the .smstag directive is used
  # -- we want 7, for "international GG")
  ../$WLADX -o "main2.o" "main2.s"
  ../$WLALINK -v linkfile2 godzilla_patched.gg
cd "$BASE_PWD"

mv -f "asm/godzilla_patched.gg" "$OUTROM"
mv -f "asm/godzilla_patched.sym" "$(basename $OUTROM .gg).sym"
rm "asm/godzilla.gg"
rm "asm/main.o"
rm "asm/main2.o"

# echo "*******************************************************************************"
# echo "Finalizing ROM..."
# echo "*******************************************************************************"
# 
# romfinalize "$OUTROM" "out/villgust_chr.bin" "$OUTROM"

echo "*******************************************************************************"
echo "Success!"
echo "Output file:" $OUTROM
echo "*******************************************************************************"
