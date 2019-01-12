mkdir -p rsrc
mkdir -p rsrc/orig
mkdir -p rsrc_raw

make libsms && make rawdmp
make libsms && make grpdmp_gg
make libsms && make godzilla_fontdmp
make libsms && make godzilla_decmp

# unit font
./godzilla_fontdmp godzilla.gg rsrc/orig/font_1bpp.png 0xC003 0xA4

# title screen

# 2020
./godzilla_decmp godzilla.gg 0x3DB5E rsrc_raw/title_godzilla_grp.bin
# 0200
./godzilla_decmp godzilla.gg 0x6971E rsrc_raw/title_font_grp.bin
# 0020
./godzilla_decmp godzilla.gg 0x32779 rsrc_raw/title_border_grp.bin

./tilemapdmp_gg godzilla.gg 0x3F74F half 0xD 0xD rsrc_raw/title_godzilla_grp.bin 0x0101 rsrc/orig/title_godzilla_base.png -h 0x01
./tilemapdmp_gg godzilla.gg 0x3F7F8 half 0x6 0x3 rsrc_raw/title_godzilla_grp.bin 0x0101 rsrc/orig/title_godzilla_mouth.png -h 0x01
./tilemapdmp_gg godzilla.gg 0x3F80A full 0xE 0x7 rsrc_raw/title_godzilla_grp.bin 0x0101 rsrc/orig/title_godzilla_logo.png

# resupply complete overlay

./grpdmp_gg godzilla.gg rsrc/orig/resupply_complete.png 0x93CF 8

# stage select
./godzilla_decmp godzilla.gg 0x30000 rsrc_raw/stage_select_grp.bin
#./grpdmp_gg rsrc_raw/stage_select_grp.bin test.png
./tilemapdmp_gg godzilla.gg 0x43D0E half 0x8 0x6 rsrc_raw/stage_select_grp.bin 0x001C rsrc/orig/stage_select_1.png -h 0x00
./tilemapdmp_gg godzilla.gg 0x43D3E half 0x8 0x6 rsrc_raw/stage_select_grp.bin 0x001C rsrc/orig/stage_select_2.png -h 0x00
./tilemapdmp_gg godzilla.gg 0x43D6E half 0x8 0x6 rsrc_raw/stage_select_grp.bin 0x001C rsrc/orig/stage_select_3.png -h 0x00
./tilemapdmp_gg godzilla.gg 0x43D9E half 0x8 0x6 rsrc_raw/stage_select_grp.bin 0x001C rsrc/orig/stage_select_4.png -h 0x00
./tilemapdmp_gg godzilla.gg 0x43DCE half 0x8 0x6 rsrc_raw/stage_select_grp.bin 0x001C rsrc/orig/stage_select_5.png -h 0x00

# completion kanji
#./grpdmp_gg godzilla.gg rsrc/orig/completed_1.png 0x3161E 16
./rawdmp godzilla.gg rsrc_raw/completed_grp_1.bin 0x3161E 0x180
./rawdmp godzilla.gg rsrc_raw/completed_grp_2.bin 0x3179E 0x180
./rawdmp godzilla.gg rsrc_raw/completed_grp_3.bin 0x3191E 0x180
./rawdmp godzilla.gg rsrc_raw/completed_grp_4.bin 0x31A9E 0x180
./tilemapdmp_gg godzilla.gg 0x43DFE half 0x4 0x3 rsrc_raw/completed_grp_1.bin 0x0160 rsrc/orig/completed_1.png -h 0x01
./tilemapdmp_gg godzilla.gg 0x43E0A half 0x4 0x3 rsrc_raw/completed_grp_2.bin 0x016C rsrc/orig/completed_2.png -h 0x01
./tilemapdmp_gg godzilla.gg 0x43E16 half 0x4 0x3 rsrc_raw/completed_grp_3.bin 0x0178 rsrc/orig/completed_3.png -h 0x01
./tilemapdmp_gg godzilla.gg 0x43E22 half 0x4 0x3 rsrc_raw/completed_grp_4.bin 0x0184 rsrc/orig/completed_4.png -h 0x01

# "congratulations" screen images
#./godzilla_decmp godzilla.gg 0x6987A rsrc_raw/congratulations_images.bin
./godzilla_decmp godzilla.gg 0x31F46 rsrc_raw/congratulations_images.bin
./tilemapdmp_gg godzilla.gg 0x43E71 half 0x8 0x6 rsrc_raw/congratulations_images.bin 0x0027 rsrc/orig/congratulations_1.png -h 0x00
./tilemapdmp_gg godzilla.gg 0x43EA1 half 0x8 0x6 rsrc_raw/congratulations_images.bin 0x0027 rsrc/orig/congratulations_2.png -h 0x00

# "congratulations" screen "to be continued" overlay
./rawdmp godzilla.gg rsrc_raw/congratulations_continued_1.bin 0x69AF3 0xC0
./rawdmp godzilla.gg rsrc_raw/congratulations_continued_2.bin 0x69BB3 0xC0
./tilemapdmp_gg godzilla.gg 0x43D86 half 0x3 0x2 rsrc_raw/congratulations_continued_1.bin 0x0081 rsrc/orig/congratulations_continued_1.png -h 0x00
./tilemapdmp_gg godzilla.gg 0x43D86 half 0x3 0x2 rsrc_raw/congratulations_continued_2.bin 0x0081 rsrc/orig/congratulations_continued_2.png -h 0x00

# compendium graphics
./godzilla_decmp godzilla.gg 0x332F7 rsrc_raw/compendium_grp.bin
./grpdmp_gg rsrc_raw/compendium_grp.bin rsrc/orig/compendium_grp.png

./tilemapdmp_gg godzilla.gg 0x77DC0 half 0x14 0x12 rsrc_raw/compendium_grp.bin 0x0001 rsrc/orig/compendium_map_main.png -h 0x08

./tilemapdmp_gg godzilla.gg 0x77D18 half 0x7 0x2 rsrc_raw/compendium_grp.bin 0x0001 rsrc/orig/compendium_map_name00.png -h 0x08
./tilemapdmp_gg godzilla.gg 0x77D26 half 0x7 0x2 rsrc_raw/compendium_grp.bin 0x0001 rsrc/orig/compendium_map_name01.png -h 0x08
./tilemapdmp_gg godzilla.gg 0x77D34 half 0x7 0x2 rsrc_raw/compendium_grp.bin 0x0001 rsrc/orig/compendium_map_name02.png -h 0x08
./tilemapdmp_gg godzilla.gg 0x77D42 half 0x7 0x2 rsrc_raw/compendium_grp.bin 0x0001 rsrc/orig/compendium_map_name03.png -h 0x08
./tilemapdmp_gg godzilla.gg 0x77D50 half 0x7 0x2 rsrc_raw/compendium_grp.bin 0x0001 rsrc/orig/compendium_map_name04.png -h 0x08
./tilemapdmp_gg godzilla.gg 0x77D5E half 0x7 0x2 rsrc_raw/compendium_grp.bin 0x0001 rsrc/orig/compendium_map_name05.png -h 0x08
./tilemapdmp_gg godzilla.gg 0x77D6C half 0x7 0x2 rsrc_raw/compendium_grp.bin 0x0001 rsrc/orig/compendium_map_name06.png -h 0x08
./tilemapdmp_gg godzilla.gg 0x77D7A half 0x7 0x2 rsrc_raw/compendium_grp.bin 0x0001 rsrc/orig/compendium_map_name07.png -h 0x08
./tilemapdmp_gg godzilla.gg 0x77D88 half 0x7 0x2 rsrc_raw/compendium_grp.bin 0x0001 rsrc/orig/compendium_map_name08.png -h 0x08
./tilemapdmp_gg godzilla.gg 0x77D96 half 0x7 0x2 rsrc_raw/compendium_grp.bin 0x0001 rsrc/orig/compendium_map_name09.png -h 0x08
./tilemapdmp_gg godzilla.gg 0x77DA4 half 0x7 0x2 rsrc_raw/compendium_grp.bin 0x0001 rsrc/orig/compendium_map_name0A.png -h 0x08
./tilemapdmp_gg godzilla.gg 0x77DB2 half 0x7 0x2 rsrc_raw/compendium_grp.bin 0x0001 rsrc/orig/compendium_map_name0B.png -h 0x08

# compendium kanji
./godzilla_decmp godzilla.gg 0x33EC7 rsrc_raw/compendium_kanji.bin
./grpdmp_gg rsrc_raw/compendium_kanji.bin rsrc/orig/compendium_kanji.png

# main game basics
#./godzilla_decmp godzilla.gg 0x35DDD rsrc_raw/main_base.bin
./godzilla_decmp godzilla.gg 0x3764D rsrc_raw/main_base.bin
./grpdmp_gg rsrc_raw/main_base.bin rsrc/orig/main_base.png

# unit "moves left" window (sprite overlay)
./grpdmp_gg godzilla.gg rsrc/orig/unit_moves_left.png 0x92CF 8

# intro

# 6020
./godzilla_decmp godzilla.gg 0x3CEAF rsrc_raw/intro_scroll_1a.bin
# 6780
./godzilla_decmp godzilla.gg 0x3D006 rsrc_raw/intro_scroll_1b.bin

# 6020
./godzilla_decmp godzilla.gg 0x3D006 rsrc_raw/intro_scroll_2a.bin
# 6560
./godzilla_decmp godzilla.gg 0x3D106 rsrc_raw/intro_scroll_2b.bin

# 6600
./godzilla_decmp godzilla.gg 0x3D404 rsrc_raw/intro_scroll_3a.bin
# 6FC0
./godzilla_decmp godzilla.gg 0x3D5BB rsrc_raw/intro_scroll_3b.bin
# 7660
./godzilla_decmp godzilla.gg 0x3D6EF rsrc_raw/intro_scroll_3c.bin

# 6020
./godzilla_decmp godzilla.gg 0x3D5BB rsrc_raw/intro_scroll_4a.bin
# 66C0
./godzilla_decmp godzilla.gg 0x3D6EF rsrc_raw/intro_scroll_4b.bin
# 6840
./godzilla_decmp godzilla.gg 0x3D73B rsrc_raw/intro_scroll_4c.bin
# 6C80
./godzilla_decmp godzilla.gg 0x3D85E rsrc_raw/intro_scroll_4d.bin

# 6020
./godzilla_decmp godzilla.gg 0x3D85E rsrc_raw/intro_scroll_5a.bin
# 6300
./godzilla_decmp godzilla.gg 0x3D8D0 rsrc_raw/intro_scroll_5b.bin
# 7140
./godzilla_decmp godzilla.gg 0x3D6EF rsrc_raw/intro_scroll_5c.bin

cat rsrc_raw/blank_tile.bin rsrc_raw/intro_scroll_1a.bin rsrc_raw/intro_scroll_1b.bin > rsrc_raw/intro_scroll_1.bin
cat rsrc_raw/blank_tile.bin rsrc_raw/intro_scroll_2a.bin rsrc_raw/intro_scroll_2b.bin > rsrc_raw/intro_scroll_2.bin
cat rsrc_raw/blank_tile.bin rsrc_raw/intro_scroll_3a.bin rsrc_raw/intro_scroll_3b.bin rsrc_raw/intro_scroll_3c.bin > rsrc_raw/intro_scroll_3.bin
cat rsrc_raw/blank_tile.bin rsrc_raw/intro_scroll_4a.bin rsrc_raw/intro_scroll_4b.bin rsrc_raw/intro_scroll_4c.bin rsrc_raw/intro_scroll_4d.bin > rsrc_raw/intro_scroll_4.bin
cat rsrc_raw/blank_tile.bin rsrc_raw/intro_scroll_5a.bin rsrc_raw/intro_scroll_5b.bin rsrc_raw/intro_scroll_5c.bin > rsrc_raw/intro_scroll_5.bin

./tilemapdmp_gg godzilla.gg 0x3E790 half 32 6 rsrc_raw/intro_scroll_1.bin 0x100 rsrc/orig/intro_scroll_1.png -h 0x01
./tilemapdmp_gg godzilla.gg 0x3E854 half 32 6 rsrc_raw/intro_scroll_2.bin 0x100 rsrc/orig/intro_scroll_2.png -h 0x01
./tilemapdmp_gg godzilla.gg 0x3E91A half 32 6 rsrc_raw/intro_scroll_3_corrected.bin 0x100 rsrc/orig/intro_scroll_3a.png -h 0x01
./tilemapdmp_gg godzilla.gg 0x3E9DA half 32 6 rsrc_raw/intro_scroll_3_corrected.bin 0x100 rsrc/orig/intro_scroll_3b.png -h 0x01
./tilemapdmp_gg godzilla.gg 0x3EA9E half 32 6 rsrc_raw/intro_scroll_4.bin 0x100 rsrc/orig/intro_scroll_4.png -h 0x01
./tilemapdmp_gg godzilla.gg 0x3EB64 half 32 6 rsrc_raw/intro_scroll_5.bin 0x100 rsrc/orig/intro_scroll_5a.png -h 0x01
./tilemapdmp_gg godzilla.gg 0x3EC24 half 32 6 rsrc_raw/intro_scroll_5.bin 0x100 rsrc/orig/intro_scroll_5b.png -h 0x01

#./grpdmp_gg rsrc_raw/intro_scroll_3.bin test.png
#./grpdmp_gg rsrc_raw/intro_scroll_5a.bin testa.png
#./grpdmp_gg rsrc_raw/intro_scroll_5b.bin testb.png
#./grpdmp_gg rsrc_raw/intro_scroll_5c.bin testc.png

# unit list

./godzilla_decmp godzilla.gg 0x6895B rsrc_raw/unitlist.bin
./grpdmp_gg rsrc_raw/unitlist.bin rsrc/orig/unitlist.png

# g-force ending

./tilemapdmp_gg godzilla.gg 0x2D58A full 20 18 rsrc_raw/ending_gforce_vram.bin 0x0 rsrc/orig/ending_gforce_newspaper.png


