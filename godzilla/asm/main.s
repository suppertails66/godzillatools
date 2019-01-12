
.include "sys/sms_arch.s"
  ;.include "base/ram.s"
;.include "base/macros.s"
  ;.include "res/defines.s"

.rombankmap
  bankstotal 64
  banksize $4000
  banks 64
.endro

.emptyfill $FF

.background "godzilla.gg"

.unbackground $80000 $FFFFF

; free unused space
.unbackground $7070 $7FEF

;===============================================
; Update header after building
;===============================================
.smstag

;===============================================
; constants
;===============================================

  ;=====
  ; vwf settings
  ;=====

  .define vwfWindowRBorderIndex $1A
;  .define controlCodeStartIndex $F0
  .define controlCodeStartIndex $F0
  .define controlCodeLimitIndex $FF
  .define vwfTileBrIndex $F0
  .define opNumIndex $F8
  .define opNameIndex $F9
  .define opInlineNumIndex $FA
  .define vwfBrIndex $FE
  .define terminatorIndex $FF
  
  .define maxVwfTiles $80
  
  .define vwfDigitStartOffset $07
  .define vwfDigitSpaceOffset $1F

  ;=====
  ; vwf settings for each screen type
  ;=====
  
  ; base tile at which vwf tiles are initially allocated
  .define vwfTileBase_main $0028
  ; one past the last index of assignable vwf tiles
  .define vwfTileSize_main $0060-vwfTileBase_main
  ; if nonzero, assume nametable scroll is zero regardless of actual value
  .define vwfScrollZeroFlag_main $00
  
  .define vwfTileBase_missionIntro $0140
  .define vwfTileSize_missionIntro $01B0-vwfTileBase_missionIntro
  .define vwfScrollZeroFlag_missionIntro $FF
  
  ; unit list text on unit overview screen
  .define vwfTileBase_unitOverview_main $0120
  .define vwfTileSize_unitOverview_main $01A0-vwfTileBase_unitOverview_main
  .define vwfScrollZeroFlag_unitOverview_main $FF
  
  ; non-unit list text on unit overview screen
  ; drawn to a different section of VRAM and then "abandoned" so that
  ; there's space for the unit list.
  .define vwfTileBase_unitOverview $00C0
  .define vwfTileSize_unitOverview $0100-vwfTileBase_unitOverview
  .define vwfScrollZeroFlag_unitOverview $FF
  
  .define vwfTileBase_compendium $0140
  .define vwfTileSize_compendium $01C0-vwfTileBase_compendium
  .define vwfScrollZeroFlag_compendium $FF
  
  .define vwfTileBase_battle $001E
  .define vwfTileSize_battle $0030-vwfTileBase_battle
  .define vwfScrollZeroFlag_battle $FF
  
  .define vwfTileBase_stageClear $0140
  .define vwfTileSize_stageClear $01B0-vwfTileBase_stageClear
  .define vwfScrollZeroFlag_stageClear $FF
  
  .define vwfTileBase_congratulations $0140
  .define vwfTileSize_congratulations $01B0-vwfTileBase_congratulations
  .define vwfScrollZeroFlag_congratulations $FF
  
  .define vwfTileBase_vsTest $0060
  .define vwfTileSize_vsTest $00D0-vwfTileBase_vsTest
  .define vwfScrollZeroFlag_vsTest $FF

  ;=====
  ; misc
  ;=====
  
;  .define screenVisibleW 20
;  .define screenVisibleH 18
;  .define screenVisibleX 3
;  .define screenVisibleY 2
  .define screenVisibleW 20
  .define screenVisibleH 18
  .define screenVisibleX 6
  ; oops, this should have been 3
  ; but i already defined all the new coordinates in this file in terms of
  ; this wrong value, so FIXME
  .define screenVisibleY 4
  
;===============================================
; memory
;===============================================

; not possible -- missions 3 and 4 overwrite this with some sort
; of terrain or unit data
;.enum $D000
; used through D23A+
;.enum $D234

.enum $D500
  vwfAllocationArray    ds maxVwfTiles  ; bytes in this array are nonzero if
                                        ; the corresponding tile has been
                                        ; allocated for display
  vwfBuffer             ds bytesPerTile ; tiles are composed here before going
                                        ; to the VDP
  
  vwfAllocationArraySize db             ; number of currently used VWF tiles
                                        ; out of the maximum
  vwfAllocationArrayPos db              ; most recently assigned index in
                                        ; the VWF alloc array
  vwfAllocationArrayBaseTile .dw         ; base tile index the vwf alloc array is
                                        ; targeting
    vwfAllocationArrayBaseTileLo db
    vwfAllocationArrayBaseTileHi db
  vwfFullDeallocFlag    db
  assumeScrollZeroFlag  db              ; when set, assume nametable scroll is
                                        ; zero for purpose of computing
                                        ; screen-local coordinates
  noInterruptDisableFlag db
  
  vwfBufferPending      db              ; nonzero if buffer not empty and
                                        ; not flushed
  vwfBufferAllocatedTile .dw             ; index of currently allocated
                                        ; buffer tile (zero if none)
    vwfBufferAllocatedTileLo db
    vwfBufferAllocatedTileHi db
  vwfPixelOffset        db              ; currently target pixel position in
                                        ; VWF buffer
  vwfTransferCharSize   db
  vwfTransferRight_leftShift    db
  
  vwfNametableHighMask      db          ; OR mask applied to high byte of
                                        ; VWF nametable output
  
  vwfLocalTargetFlag        db          ; if true, print nametable data to RAM
                                        ; rather than directly to VDP
    vwfLocalTargetBaseAddr      dw      ; base address of local nametable
                                        ; target (RAM address of nametable data
                                        ; for upper-left character in printable
                                        ; area)
    vwfLocalTargetW  db                 ; width in tiles of one line in the
                                        ; local nametable target, for
                                        ; linebreak calculations
    vwfLocalTargetH  db                 ; height in tiles of the
                                        ; local nametable target,
                                        ; for garbage collection checks
    vwfLocalTargetCurrLineAddr      dw    ; address of current target line
                                          ; during printing
  
  printBaseXY           .dw
    printBaseY            db
    printBaseX            db
  printOffsetXY         .dw
    printOffsetY          db
    printOffsetX          db
  
  scratch               .dw
    scratchLo             db
    scratchHi             db
  
  inlinePrintNum        dw      ; number printed when an inline_num op
                                ; is encountered
  inlinePrintDigitCount db      ; number of digits in inline print
                                ; (00 = don't care)
  inlinePrintShowLeadingZeroes db
  
  numberPrintBuffer     ds 8    ; buffer for printing script-inline numbers
  numberConvBuffer      ds 4    ; buffer for temp BCD conversion storage
  
;  windowCompositionBuffer ds 128        ; for fuck's sake why would you use
;                                        ; DE00 for something other than
;                                        ; window composition
  
.ende

; pixel x/y of nametable scrolling.
; these are only functional during primary gameplay;
; battles and other scenes override with their own values
; (mostly zero).
.define mainScreenScrollYLo $D401
.define mainScreenScrollXLo $D404

.define openWindowCount $C2D8
.define highestOpenWindowAddr $C2D9

;===============================================
; existing routines
;===============================================

.define sendRawTilesToVdp $A64
.define sendReversedRawTilesToVdp $A75
.define sendTilemapToAbsoluteVdpAddr $8BE
.define sendHalfTilemapToAbsoluteVdpAddr $8ED
.define bcdConv4Digit $C97
.define bcdConv3Digit $CB8
.define openWindow $5964
.define closeWindow $59E1

;===============================================
; macros
;===============================================

.macro callExternal
  ld a,(mapperSlot2Ctrl)
  push af
  
    ld a,:\1
    ld (mapperSlot2Ctrl),a
    call \1
  
  pop af
  ld (mapperSlot2Ctrl),a
.endm

.macro callExternalHardcoded
  ld a,(mapperSlot2Ctrl)
  push af
  
    ld a,\1
    ld (mapperSlot2Ctrl),a
    call \2
  
  pop af
  ld (mapperSlot2Ctrl),a
.endm

.macro doLongjmp
  ld a,:\1
  ld hl,\1
  call longjmp
.endm

.macro read8BitTable
  rst $20
.endm

.macro read16BitTable
  rst $28
.endm

.macro startLocalPrint ARGS baseAddr, nametableW, nametableH, x, y
  ld hl,baseAddr
  ld (vwfLocalTargetBaseAddr),hl
  
  ld hl,baseAddr+(nametableW*2*y)+(2*x)
  ld (vwfLocalTargetCurrLineAddr),hl
  
  ld a,nametableW
  ld (vwfLocalTargetW),a
  
  ld a,nametableH
  ld (vwfLocalTargetH),a
  
  ld a,$FF
  ld (vwfLocalTargetFlag),a
.endm

.macro startLocalPrintNonFixed ARGS nametableW, nametableH, x, y
  ld (vwfLocalTargetBaseAddr),hl
  
  ld de,(nametableW*2*y)+(2*x)
  add hl,de
  ld (vwfLocalTargetCurrLineAddr),hl
  
  ld a,nametableW
  ld (vwfLocalTargetW),a
  
  ld a,nametableH
  ld (vwfLocalTargetH),a
  
  ld a,$FF
  ld (vwfLocalTargetFlag),a
.endm

.macro moveLocalPrint ARGS baseAddr, nametableW, nametableH, x, y
  ld hl,baseAddr+(nametableW*2*y)+(2*x)
  ld (vwfLocalTargetCurrLineAddr),hl
.endm

.macro endLocalPrint
  xor a
  ld (vwfLocalTargetFlag),a
.endm

; set up a value for inline script printing
; HL = value
.macro setUpNumberPrint ARGS digits, showLeadingZeroes
;  ld hl,value
;  ld hl,(valueAddr)
  ld (inlinePrintNum),hl
  
  ld a,digits
  ld (inlinePrintDigitCount),a
  
  ld a,showLeadingZeroes
  ld (inlinePrintShowLeadingZeroes),a
.endm

.macro openTempBank
  ld a,(mapperSlot2Ctrl)
  push af
    ld a,\1
    ld (mapperSlot2Ctrl),a
.endm

.macro closeTempBank
  pop af
  ld (mapperSlot2Ctrl),a
.endm












;===============================================
; vwf tile allocation
;===============================================

.bank $01 slot 1
.section "vwf tile allocation" free
  ;========================================
  ; returns a free VWF tile index in HL
  ;========================================
  allocVwfTile:
;    push hl
    push de
    push bc
    
;      ld h,>vwfAllocationArray
;      ld a,(vwfAllocationArrayPos)
;      ld l,a
      ld a,(vwfAllocationArrayPos)
      ld l,a
      ld h,$00
      ld bc,vwfAllocationArray
      ; save starting search point to E
      ld e,a
      ld d,$00  ; flag: set after garbage collection
      @searchLoop:
        
        ; preincrement (saves a little time)
        inc l
        
        ; HACK: don't assign tile 003A in strategy mode.
        ; many tilemaps expect this to be a blank space.
        ; why not just use tile 0????
        ;
        ; technically will also detect erroneous cases such as
        ; tile base being 0128 instead of 0028, but it probably
        ; will never matter
        ld a,(vwfAllocationArrayBaseTileLo)
        cp <vwfTileBase_main
        jr nz,+
          ld a,l
          cp $3A-(<vwfTileBase_main)
          jr nz,+
            inc l
        +:
        
        ; wrap around at end of array
        ld a,(vwfAllocationArraySize)
        cp l
;        jr nz,++       ; for safety reasons, do the full check for now
        jr z,+
        jr nc,++
          +:
          ld l,$00
;          ld a,l
        ++:
        
        ; check if second loop done (D nonzero)
        ld a,d
        or a
        jr z,+
          ; check if current index == startindex
          ld a,e
          cp l
          jr nz,+
            @fullLoad:
            
            ; uh-oh: we ran garbage collection, but there are still no tiles
            ; available. there's nothing we can do to actually fix the problem
            ; at this point, so we just declare all tiles free and cause some
            ; visual corruption so the new stuff can print.
;            call freeAllVwfTiles
            
            ; actually, just overwrite the next tile in the sequence and
            ; re-run this whole procedure next time we print something.
            ; will cause considerable slowdown but less noticeable corruption
            jr @done
            
            ; TODO: possible last resort: search for blank/duplicate tiles
            ; or blank VWF tiles outside of current window
        +:
        
        ; if allocation array is totally full (we've looped to our starting
        ; point), run garbage collection and hope for the best
        ; (note: actually can run when array is one short of full. same deal.)
        ld a,e
        cp l    ; compare current pos to initial
        jr nz,+
          call collectVwfGarbage
          
          ; flag D so that, if no tiles are available even after
          ; garbage collection, we can detect a second loop
          inc d
        +:
        
        @checkCurrentTile:
        push hl
          ; add array base address to current check index
          add hl,bc
          ; if byte nonzero, slot is in use
          ld a,(hl)
          or a
          jr z,+
            pop hl
            jr @searchLoop
        +:
        pop de
      
      @done:
      
      ; mark tile as allocated (nonzero)
;      inc (hl)
      ld a,$FF
      ld (hl),a
      
      ; save search pos
      ld a,e
      ld (vwfAllocationArrayPos),a
      
      ; add offset to actual tile index
;      ld e,a
;      ld d,0
      ld hl,(vwfAllocationArrayBaseTile)
      add hl,de
    
    pop bc
    pop de
;    pop hl
    ret
  
  ;========================================
  ; marks all VWF tiles as free
  ;========================================
  freeAllVwfTiles:
    push hl
    push bc
      
      ld b,maxVwfTiles
      ld hl,vwfAllocationArray
      -:
        ld (hl),$00
        inc hl
        djnz -
    
    pop bc
    pop hl
    ret
  
  ;========================================
  ; initialize VWF tile allocation.
  ; resets and configures with new parameters
  ; 
  ; A  = number of tiles
  ; B  = assume nametable zero flag
  ; HL = base tile index
  ;========================================
  setUpVwfTileAlloc:
    ld (vwfAllocationArraySize),a
    ld (vwfAllocationArrayBaseTile),hl
    xor a
    ld (vwfAllocationArrayPos),a
    
    ld a,b
    ld (assumeScrollZeroFlag),a
    
    ; should this be a parameter?
    ld a,$18
    ld (vwfNametableHighMask),a
    
    call freeAllVwfTiles
    call resetVwf
    ret
  
  ;========================================
  ; marks a VWF tile as free
  ;
  ; DE  = tile index
  ;========================================
  freeVwfTile:
    push hl
    
      ; subtract base position from tile index
      ld hl,(vwfAllocationArrayBaseTile)
      ex de,hl
      or a
      sbc hl,de
      
      ; low byte = index into 0x100-aligned allocation array
;      ld h,>vwfAllocationArray
      ; low byte = index into allocation array
      ex de,hl
      ld hl,vwfAllocationArray
      add hl,de
      
      ; if full deallocation flag zero, zero reference counter
      ld a,(vwfFullDeallocFlag)
      or a
      jr nz,+
        ld (hl),$00
        jr @done
      +:
      
      ; if nonzero, decrement reference counter
      @decReferenceCounter:
        dec (hl)
    
    @done:
    pop hl
    ret
  
  ;========================================
  ; reads the nametable in the specified coordinates and deallocates all
  ; VWF tiles contained within
  ;
  ; HL = screen-local tile x/y
  ; BC = box w/h
  ;========================================
  deallocVwfTileArea:
    push hl
    push bc
      
      @yLoop:
        
        ; save W
        push hl
        push bc
          
          @xLoop:
            ; read tile using readTileFromNametable
            push bc
              push hl
                call readLocalTileFromNametable
              pop hl
              
              ; high bytes must match (i.e. in same table half)
              
              ; AND high byte to just bit 0 (bit 9 of pattern num)
              ld a,d
              and $01
              ld d,a
              ; compare to nametable target high byte
              ld a,(vwfAllocationArrayBaseTileHi)
              cp d
              jr nz,+
              
              ; ignore tiles < start index
              @checkLow:
              ld a,(vwfAllocationArrayBaseTileLo)
              cp e
              jr z,@checkHigh
              jr nc,+
                
                ; ignore tiles > end index
                @checkHigh:
                ld c,a  ; C = low byte of base VWF tile index
                ld a,(vwfAllocationArraySize)
                add a,c
                cp e
                jr z,+
                jr c,+
                
                  ; free the tile
                  @free:
                  call freeVwfTile
                  
              +:
            pop bc
            
            ; move to next X
            inc h
            dec b
            jr nz,@xLoop
            
          @xLoopDone:
        
        ; restore W
        pop bc
        pop hl
        
        ; move to next Y
        inc l
        dec c
        jr nz,@yLoop
    
    @done:
    pop bc
    pop hl
    ret
  
  ;========================================
  ; reads the nametable at the specified
  ; nametable-absolute address and frees
  ; all VWF tiles in the specified box
  ;
  ; HL = address
  ; BC = box w/h
  ;========================================
  deallocVwfTileAreaByAddr:
    push hl
    push bc
      
      call nametableAddrToLocalCoords
      call deallocVwfTileArea
      
    pop bc
    pop hl

    ret
  
  ;========================================
  ; HL = nametable address
  ;========================================
  nametableAddrToAbsoluteCoords:
    push de
      ; nametable addr -= $3800
      ld de,$3800
      or a
      sbc hl,de
      
      ; y-pos = amount / $40
      push hl
        .rept 6
          srl h
          rr l
        .endr
        ld e,l
      pop hl
      
      ; x-pos = (amount % $40) / 2
      ld a,l
      and $3F
      srl a
      
      ; X
      ld h,a
      ; Y
      ld l,e
    pop de
    ret
  
  nametableAddrToLocalCoords:
    call nametableAddrToAbsoluteCoords
    push de
      call absoluteToLocalCoords
    pop de
    ret
  
  ;========================================
  ; fully reset the VWF allocation buffer.
  ; clears existing buffer contents, then reads all visible tiles from VDP and
  ; marks those actually in use as allocated.
  ; obviously has considerable overhead, so this routine's use should be
  ; minimized as much as possible.
  ;========================================
  collectVwfGarbage:
    ; clear buffer
    call freeAllVwfTiles
    
    push hl
    push de
    push bc
      
      ;=====
      ; evaluate visible screen area and mark all used VWF tiles as
      ; allocated
      ;=====
      
;      ld h,screenVisibleX
;      ld l,screenVisibleY
      ld h,0
      ld l,0
      ld b,screenVisibleW
      ld c,screenVisibleH
      
      ; vwfFullDeallocFlag nonzero = decrement reference counter
      ld a,$01
      ld (vwfFullDeallocFlag),a
        ; allocate area
        call deallocVwfTileArea
      xor a
      ld (vwfFullDeallocFlag),a
    
      ;=====
      ; if VWF tiles have been temporarily hidden behind another tilemap,
      ; mark them as allocated
      ;=====
      
      call markHiddenVwfTilesAllocated
    
    @done:
    pop bc
    pop de
    pop hl
    
    ret
  
  markHiddenVwfTilesAllocated:
    
    ; if composing local tilemap, flag any VWF tiles used there
    ld a,(vwfLocalTargetFlag)
    or a
    jr z,+
      ld a,(vwfLocalTargetW)    ; window w
      ld d,a
      ld a,(vwfLocalTargetH)    ; window h
      ld e,a
      ld hl,(vwfLocalTargetBaseAddr)     ; tile data start
    
      call checkHiddenVwfTiles
    +:
    
    ; if any VWF tiles are hidden by an open window, flag them
    @windowHideCheck:
    ld a,(openWindowCount)
    or a
    jr z,+
      push de
      push bc
        ld b,a
        -:
          ld hl,(highestOpenWindowAddr)
          
          ; skip VDP addr
          inc l
          inc l
          
          ; height
          ld e,(hl)
          inc l
          ; width
          ld d,(hl)
          inc l
          
          push hl
          push bc
            call checkHiddenVwfTiles
          pop bc
          pop hl
          
          ; move to next-lowest window
          dec h
          djnz -
      
      pop bc
      pop de
    +:
      
    @done:
    ret
  
  ;========================================
  ; HL = data pointer
  ; DE = w/h
  ;========================================
  checkHiddenVwfTiles:
      
      @yLoop:
        
        ; save W
        push de
          
          @xLoop:
            push de
              ; get nametable entry
              ld a,(hl)
              ld e,a
              inc hl
              ld a,(hl)
;              ld d,a
              inc hl
            
              ; high bytes must match (i.e. in same table half)
              
              ; AND high byte to just bit 0 (bit 9 of pattern num)
;              ld a,d
              and $01
              ld d,a
              ; compare to nametable target high byte
              ld a,(vwfAllocationArrayBaseTileHi)
              cp d
              jr nz,+
              
              ; ignore tiles < start index
              @checkLow:
              ld a,(vwfAllocationArrayBaseTileLo)
              cp e
              jr z,@checkHigh
              jr nc,+
                
                ; ignore tiles > end index
                @checkHigh:
                ld c,a  ; C = low byte of base VWF tile index
                ld a,(vwfAllocationArraySize)
                add a,c
                cp e
                jr z,+
                jr c,+
                
                  ; mark the tile as allocated
  ;                    call freeVwfTile
                  @hiddenTileFound:
                  push hl
                    ; subtract base position from tile index
                    ld hl,(vwfAllocationArrayBaseTile)
                    ex de,hl
                    or a
                    sbc hl,de
                    
                    ; low byte = index into 0x100-aligned allocation array
;                    ld h,>vwfAllocationArray
                    ex de,hl
                    ld hl,vwfAllocationArray
                    add hl,de
                    ld a,$FF
                    ld (hl),a
                  pop hl
            
            +:
            pop de
;            inc hl
            dec d
            jr nz,@xLoop
            
          @xLoopDone:
        
        ; restore W
        pop de
        
        ; move to next Y
        dec e
        jr nz,@yLoop
    ret
  
  ;========================================
  ; convert local coordinates to absolute
  ;
  ; HL = screen-local X/Y
  ;========================================
  localToAbsoluteCoords:
    ; if force-zero flag is set, assume nametable scroll is zero on
    ; both axes
    ld a,(assumeScrollZeroFlag)
    or a
    jr nz,@assumeNametableZero
    
      ; convert screen-local coords to absolute nametable coords
      
      ;=====
      ; x
      ;=====
      
      ; get raw scrolling x-coord
      ld a,(mainScreenScrollXLo)
      ; divide by 8
      srl a
      srl a
      srl a
      ; add target X
      add a,h
      ; add visible-screen tile offset
      add a,screenVisibleX
      
      ; wrap to valid range (0-1F)
      and $1F
      
      ld h,a
      
      ;=====
      ; y
      ;=====
      
      ; get raw scrolling x-coord
      ld a,(mainScreenScrollYLo)
      ; divide by 8
      srl a
      srl a
      srl a
      ; add target Y
      add a,l
      ; add visible-screen tile offset
      add a,screenVisibleY
      
      ; wrap to 0-1F
;      and $1F
      ; remap 1C+ to 00-03
      cp $1C
      jr c,+
        sub $1C
      +:
      
      ld l,a
      
      jr @done
    
    @assumeNametableZero:
    ; x
    ld a,h
    add a,screenVisibleX
    ld h,a
    ; y
    ld a,l
    add a,screenVisibleY
    ld l,a
    
    @done:
    
    ret
  
  absoluteToLocalCoords:
    ; if force-zero flag is set, assume nametable scroll is zero on
    ; both axes
    ld a,(assumeScrollZeroFlag)
    or a
    jr nz,@assumeNametableZero
    
      ; convert absolute nametable coords to screen-local
      
      push bc

        ;=====
        ; x
        ;=====
        
        ; get raw scrolling x-coord
        ld a,(mainScreenScrollXLo)
        ; divide by 8
        srl a
        srl a
        srl a
        ; add visible-screen tile offset
        add a,screenVisibleX
        and $1F
        ; subtract from target X
        ld c,a
        ld a,h
        sub c
        ld h,a
        
        ;=====
        ; y
        ;=====
        
        ; get raw scrolling x-coord
        ld a,(mainScreenScrollYLo)
        ; divide by 8
        srl a
        srl a
        srl a
        ; add visible-screen tile offset
        add a,screenVisibleY
        ; wrap to 0-1F
        ; remap 1C+ to 00-03
        cp $1C
        jr c,+
          sub $1C
        +:
        ; subtract from target Y
        ld c,a
        ld a,l
        sub c
        ld l,a
      
      pop bc
      
      jr @done
    
    @assumeNametableZero:
    ; x
    ld a,h
    sub screenVisibleX
    ld h,a
    ; y
    ld a,l
    sub screenVisibleY
    ld l,a
    
    @done:
    
    ret
  
  ;========================================
  ; reads a screen-local tile from the
  ; nametable
  ;
  ; HL = screen-local X/Y
  ;
  ; returns result in DE
  ;========================================
  readLocalTileFromNametable:
    call localToAbsoluteCoords
;    jp readAbsoluteTileFromNametable
  ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ; !!!!!!! DROP THROUGH -- DO NOT PLACE NEW CODE HERE !!!!!!!!
  ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ;========================================
  ; reads a tile from the nametable
  ;
  ; HL = absolute nametable X/Y
  ;
  ; returns result in DE
  ;========================================
  readAbsoluteTileFromNametable:
    ; DE = X * 2
    ld a,h
    sla a
    ld e,a
    ; add $3800 to get read command + target address
    ld d,$38
    
    ; HL = Y * $40
    ld h,$00
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl
    
    ; add x-offset to base Y
    add hl,de
    
    ;=====
    ; do the read
    ;=====
    
    ; if no interrupt disable flag set, don't disable interrupts
    ld a,(noInterruptDisableFlag)
    or a
    jr nz,+
      di
        ; set address
        ld a,l
        out ($BF),a
        ld a,h
        out ($BF),a
        
        ; waste cycles
;        push iy
;        pop iy
        ; read low byte
        in a,($BE)
        ld e,a
        
        ; waste cycles
;        push iy
;        pop iy
        ; read high byte
        in a,($BE)
        ld d,a
      ei
      ret
    +:
    
    ; set address
    ld a,l
    out ($BF),a
    ld a,h
    out ($BF),a
        
    ; waste cycles
;    push iy
;    pop iy
    ; read low byte
    in a,($BE)
    ld e,a
    
    ; waste cycles
;    push iy
;    pop iy
    ; read high byte
    in a,($BE)
    ld d,a
    
    ret
  
  ;========================================
  ; writes a screen-local tile to the
  ; nametable
  ;
  ; DE = tile
  ; HL = screen-local X/Y
  ;========================================
  writeLocalTileToNametable:
    call localToAbsoluteCoords
;    jp readAbsoluteTileFromNametable
  ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ; !!!!!!! DROP THROUGH -- DO NOT PLACE NEW CODE HERE !!!!!!!!
  ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ;========================================
  ; writes a tile from the nametable
  ;
  ; DE = tile
  ; HL = absolute nametable X/Y
  ;========================================
  writeAbsoluteTileToNametable:
    ; BC = X * 2
    ld a,h
    sla a
    ld c,a
    ; add $7800 to get write command + target address
    ld b,$78
    
    ; HL = Y * $40
    ld h,$00
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl
    
    ; add x-offset to base Y
    add hl,bc
    
    ;=====
    ; do the write
    ;=====
    
    ; if no interrupt disable flag set, don't disable interrupts
    ld a,(noInterruptDisableFlag)
    or a
    jr nz,+
    
      di
        ; set address
        ld a,l
        out ($BF),a
        ld a,h
        out ($BF),a
        
        ld a,e
        ; waste cycles
;        push iy
;        pop iy
        ; write low byte
        out ($BE),a
        
        ld a,d
        ; waste cycles
;        push iy
;        pop iy
        ; write high byte
        out ($BE),a
      ei
      ret
    +:
    
    ; set address
    ld a,l
    out ($BF),a
    ld a,h
    out ($BF),a
    
    ld a,e
    ; waste cycles
;    push iy
;    pop iy
    ; write low byte
    out ($BE),a
    
    ld a,d
    ; waste cycles
;    push iy
;    pop iy
    ; write high byte
    out ($BE),a
    
    ret
  
.ends

;========================================
; vwf composition code
;========================================

.include "out/font/font.inc"

;.slot 2
;.section "font lookups" align $4000 superfree
.bank $01 slot 1
.section "font lookups" free
  ; must be $100-aligned but WLA-DX can't enforce this without a separate
  ; section which we can't make because then it couldn't be made to be part
  ; of the superfree section the code that accesses it is in.
  ; so we'll just hope we don't have to do this more than once!!
  fontSizeTable:
    .incbin "out/font/sizetable.bin" FSIZE fontCharLimit
    .define numFontChars fontCharLimit-1

  fontRightShiftBankTbl:
    .db :font_rshift_00
    .db :font_rshift_01
    .db :font_rshift_02
    .db :font_rshift_03
    .db :font_rshift_04
    .db :font_rshift_05
    .db :font_rshift_06
    .db :font_rshift_07
  fontRightShiftPtrTbl:
    .dw font_rshift_00
    .dw font_rshift_01
    .dw font_rshift_02
    .dw font_rshift_03
    .dw font_rshift_04
    .dw font_rshift_05
    .dw font_rshift_06
    .dw font_rshift_07
  fontLeftShiftBankTbl:
    .db :font_lshift_00
    .db :font_lshift_01
    .db :font_lshift_02
    .db :font_lshift_03
    .db :font_lshift_04
    .db :font_lshift_05
    .db :font_lshift_06
    .db :font_lshift_07
  fontLeftShiftPtrTbl:
    .dw font_lshift_00
    .dw font_lshift_01
    .dw font_lshift_02
    .dw font_lshift_03
    .dw font_lshift_04
    .dw font_lshift_05
    .dw font_lshift_06
    .dw font_lshift_07
  
  charANDMasks:
    .db $00,$80,$C0,$E0,$F0,$F8,$FC,$FE,$FF
  
  
  
  ; C = target char
  printVwfChar:
  
    ; handle tile break
    ld a,c
    cp vwfTileBrIndex
    jr nz,+
      call sendVwfBufferIfPending
      call resetVwf
      ld hl,printOffsetX
      inc (hl)
      jp @done
    +:
    
    ; vwf composition works like this:
    ; 1. OR left part of new character into composition buffer using
    ;    appropriate entry from right-shifted character tables.
    ;    (if vwfPixelOffset is zero, we can copy instead of ORing)
    ; 2. send composition buffer to VDP (allocating tile if not already done)
    ; 3. if composition buffer was filled, clear it.
    ; 4. if entire character has already been copied, we're done.
    ; 5. copy right part of new character directly to composition buffer using
    ;    appropriate entry from left-shifted character tables.
    ; 6. send composition buffer to VDP (allocating tile)
    
    ;=====
    ; look up size of target char
    ;=====
    
;    ld h,>fontSizeTable
;    ld a,c
;    ld l,a
    ld hl,fontSizeTable
    ld a,c
    ld e,a
    ld d,$00
    add hl,de
    
    ; get width
    ld a,(hl)
    ; if width is zero, we have nothing to do
    or a
    jp z,@done
    
    ld (vwfTransferCharSize),a
    
    ;=====
    ; transfer 1: XOR left part of target char with buffer
    ;=====
    
    @transfer1:
    
    ; if char is space, no transfer needed
    ; (or it wouldn't be, except what if nothing else has been printed
    ; to the buffer yet? then the part we skipped won't get the background
    ; color)
;    ld a,c
;    cp vwfSpaceCharIndex
;    jr z,@transfer1Done
    
      push bc
        
        ;=====
        ; look up character data
        ;=====
        
        ; B = bank
        ld a,(vwfPixelOffset)
        ld e,a
        ld d,$00
        ld hl,fontRightShiftBankTbl
        add hl,de
        ld b,(hl)
        
        ; HL = pointer to char table base
        ld hl,fontRightShiftPtrTbl
        ; pixel offset *= 2
        sla e
;      rl d     ; pointless, will never shift anything in
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
        ; add offset to actual char
        ld l,c
        ld h,$00
        ; * 32 for tile offset
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,hl
;        .rept 5
;          sla e
;          rl d
;        .endr
        add hl,de
        
        ; can copy to buffer instead of ORing if pixel offset is zero
        ld a,(vwfPixelOffset)
        or a
        jr nz,+
          ld de,vwfBuffer
          call copyToTileBuffer
          jr @dataTransferred
        +:
        
        ; look up AND mask to remove low bits
        push hl
          ld hl,charANDMasks
          ld a,(vwfPixelOffset)
          ld e,a
          ld d,$00
          add hl,de
          ld c,(hl)
        pop hl
        
        ;=====
        ; OR to buffer
        ;=====
        
        ld de,vwfBuffer
        call orToTileBuffer
        
        @dataTransferred:
        
      pop bc
      
      ; check if border needs to be added to tile
      call checkBorderTransfer
    
      ;=====
      ; send modified buffer
      ;=====
;       call sendVwfBuffer
    
    @transfer1CompositionDone:
    
    ; determine right transfer shift amount
    ld a,(vwfPixelOffset)
    ld b,a
    sub $08
    neg
    ld (vwfTransferRight_leftShift),a
    
    ; advance vwfPixelOffset by transfer size
;    ld a,b
;    ld b,a
    ld a,(vwfTransferCharSize)
    add a,b
    
    cp $08
    jr nc,+
      ; if position in VWF buffer < 8, no second transfer needed
      
      ; send modified buffer if print speed nonzero (printing character-
      ; by-character); if text printing is instant, this just wastes time.
      ; also send if only printing a single character.
      push af
;        sendVwfBufferIfNeeded
        ld a,$FF
        ld (vwfBufferPending),a
        
 /*       ; if printing independent character rather than entire string,
        ; do buffer send
        ld a,(stringIsPrinting)
        or a
        jr z,++
        ; if print speed is zero (instant), don't do buffer send
        ld a,(printSpeed)
        or a
        jr z,+++
          ++:
          call sendVwfBuffer
        +++: */
      pop af
      
      ld (vwfPixelOffset),a
      jr @done
    +:
    jr nz,+
      ; if we filled the VWF buffer exactly to capacity, then we need to
      ; send it, but don't need a right transfer or new tile allocation.
      ; instead, we reset the buffer in case more text is added.
      
      ; send modified buffer
      call sendVwfBuffer
      
      ; reset buffer
;      xor a
;      ld (vwfPixelOffset),a
      call resetVwf
      ; move to next x-pos
;      ld a,(printOffsetX)
;      inc a
;      ld (printOffsetX),a
      ld hl,printOffsetX
      inc (hl)
      jr @done
    +:
    
    ;=====
    ; buffer filled, and second transfer needed
    ;=====
    
    ; save updated pixel offset
    push af
      ; send modified buffer
      call sendVwfBuffer
      
      ; we'll add content for the second transfer, so set the
      ; buffer pending flag
      ld a,$FF
      ld (vwfBufferPending),a
    ; restore updated pixel offset
    pop af
    
    ; modulo by 8 to get new offset in next buffer (after second transfer)
    and $07
    ld (vwfPixelOffset),a
    ; new allocation needed
    xor a
    ld (vwfBufferAllocatedTile+0),a
    ld (vwfBufferAllocatedTile+1),a
    ; move to next x-pos
    ld hl,printOffsetX
    inc (hl)
    
    ;=====
    ; transfer 2: copy right part of character to buffer
    ;=====
    
    @transfer2:
    
    ; transfer size of zero = skip
;    ld a,(vwfTransferRightSize)
;    jr z,@transfer2Done
    
    ; if char is space, no transfer needed
    ; (or it wouldn't be, except... something, I've already forgotten
    ; what this breaks. but it definitely breaks something)
;    ld a,c
;    cp vwfSpaceCharIndex
;    jr z,@transfer2Done
    
      ;=====
      ; look up character data
      ;=====
      
      ; B = bank
      ld a,(vwfTransferRight_leftShift)
      ld e,a
      ld d,$00
      ld hl,fontLeftShiftBankTbl
      add hl,de
      ld b,(hl)
      
      ; HL = pointer to char table base
      ld hl,fontLeftShiftPtrTbl
      ; pixel offset *= 2
      sla e
;      rl d     ; pointless, will never shift anything in
      add hl,de
      ld e,(hl)
      inc hl
      ld d,(hl)
      ; add offset to actual char
      ld l,c
      ld h,$00
      ; * 32 for tile offset
      add hl,hl
      add hl,hl
      add hl,hl
      add hl,hl
      add hl,hl
;      .rept 5
;        sla e
;        rl d
;      .endr
      add hl,de
      
      ;=====
      ; copy to buffer
      ;=====
      
      ld de,vwfBuffer
;      ld a,b
      call copyToTileBuffer
      
      ; check if border needs to be added to tile
      call checkBorderTransfer
    
      ;=====
      ; send modified buffer
      ;=====
;      call sendVwfBuffer

      ; transfer only needed here for single-character print;
      ; string prints will handle terminating tile themselves
/*      ld a,(stringIsPrinting)
      or a
      jr nz,+
        call sendVwfBuffer
      +:*/
    
    @transfer2Done:
    
    ;=====
    ; finish up
    ;=====
    
    @done:
    
      ;=====
      ; update last-printed data
      ;=====
      
/*      ld a,(printOffsetX)
      ld (lastPrintOffsetX),a
      ld a,(printOffsetY)
      ld (lastPrintOffsetY),a
      
      ld a,(printBaseX)
      ld (lastPrintBaseX),a
      ld a,(printBaseY)
      ld (lastPrintBaseY),a */
    
    ret
  
  checkBorderTransfer:
    ;=====
    ; check if we printed into the tile containing the right border
    ; of the window. if so, we need to draw the border onto the
    ; tile.
    ; (done primarily to allow us to "cheat" so we can squeeze
    ; seven-character party member names into what was supposed to be
    ; a four-tile space)
    ;=====
    
    ; FIXME: oops non-nametable prints aren't setting up the width
    ld a,(vwfLocalTargetFlag)
    or a
    ret z
    
    ; FIXME: this only works for nametable transfers if the base x-offset
    ; is 1
    ld a,(printOffsetX)
    inc a
    inc a
;    ld hl,printAreaW
    ld hl,vwfLocalTargetW
    cp (hl)
    jr nz,+
      push bc
        ; border is 4px on right side of tile
        ld c,$F0
        ld b,:font_rshift_00
        ld de,vwfBuffer
        ld hl,font_rshift_00+(bytesPerTile*vwfWindowRBorderIndex)
        call orToTileBuffer
      pop bc
    +:
    
    ret
  
.ends

.bank $01 slot 1
.section "vwf composition 1" free
  
  ;========================================
  ; reset the VWF buffer
  ;========================================
  resetVwf:
    push hl
      xor a
      
      ; reset pixel x-pos
      ld (vwfPixelOffset),a
      ld (vwfBufferAllocatedTile+0),a
      ld (vwfBufferAllocatedTile+1),a
      ld (vwfBufferPending),a
      
      ; clear tile composition buffer
      ld hl,vwfBuffer
      ld b,bytesPerTile
      -:
        ld (hl),a
        inc hl
        djnz -
    pop hl
    ret
  
  ;========================================
  ; B = src data bank
  ; C = AND mask for each existing byte in buffer
  ; DE = dst pointer
  ; HL = src data pointer
  ;========================================
  orToTileBuffer:
    ld a,(mapperSlot2Ctrl)
    push af
      
      ld a,b
      ld (mapperSlot2Ctrl),a
      ld b,bytesPerTile
      -:
        ld a,(de)
        and c
        or (hl)
        ld (de),a
        
        inc hl
        inc de
        djnz -
      
    pop af
    ld (mapperSlot2Ctrl),a
    ret
  
  ;========================================
  ; B = src data bank
  ; DE = dst pointer
  ; HL = src data pointer
  ;========================================
  copyToTileBuffer:
    ld a,(mapperSlot2Ctrl)
    push af
      
      ld a,b
      ld (mapperSlot2Ctrl),a
      ld bc,bytesPerTile
      ldir
      
    pop af
    ld (mapperSlot2Ctrl),a
    ret
  
/*  doVwf:
    ld a,(mapperSlot1Ctrl)
    push af
      
      ; C = target char index
      ld a,:printVwfChar
      ld (mapperSlot1Ctrl),a
      call printVwfChar
      
    pop af
    ld (mapperSlot1Ctrl),a
    ret */
  
  sendVwfBuffer:
    push hl
    push bc
      
      ;=====
      ; allocate tile for buffer if unallocated
      ;=====
      ld hl,vwfBufferAllocatedTileHi
      ld a,(vwfBufferAllocatedTileLo)
      or (hl)
      ld c,a    ; C will be zero if tile is newly allocated,
                ; so we know to send it to the nametable later
;      or a
      jr nz,+
        call allocVwfTile
        ld (vwfBufferAllocatedTile),hl
      +:
      
      ; HL = dst tile index
      ld hl,(vwfBufferAllocatedTile)
      ; multiply by 32 and add $4000 to get VDP target command
      add hl,hl
      add hl,hl
      add hl,hl
      add hl,hl
      add hl,hl
      ld de,$4000
      add hl,de
      ; DE = src data pointer
      ld de,vwfBuffer
      ; B = tile count
      ld b,$01
      push bc
        ld a,(noInterruptDisableFlag)
        or a
        jr nz,+
          di
            call sendRawTilesToVdp
          ei
          jr ++
        +:
          call sendRawTilesToVdp
        ++:
      pop bc
      
      ;=====
      ; if tile newly allocated, send to nametable
      ;=====
      ld a,c    ; check if initial tile num was zero
      or a
      jr nz,+
      
        ;=====
        ; send to nametable
        ;=====
        
;        ; x/y pos
;        ld a,(printBaseX)
;;        inc a
;        ld h,a
;        ld a,(printBaseY)
;;        inc a
;        ld l,a
        ld hl,(printBaseXY)
        ld de,(printOffsetXY)
        add hl,de
        
        ; tile index
;        ld a,(vwfBufferAllocatedTile)
;        ld e,a
;        ld d,$00
        ld de,(vwfBufferAllocatedTile)
        
;        call sendCharToNametable
;        ld a,d
;        or $18
;        ld d,a
        ; apply OR mask to high byte of nametable data
        ld a,(vwfNametableHighMask)
        or d
        ld d,a
        call writeVwfCharToNametable
      +:
      
      ; reset buffer pending flag
      xor a
      ld (vwfBufferPending),a
    
    pop bc
    pop hl
    ret
  
  ; DE = nametable data
  writeVwfCharToNametable:
    ;=====
    ; if not targeting local nametable, send directly to VDP
    ;=====
    ld a,(vwfLocalTargetFlag)
    or a
    jp z,writeLocalTileToNametable
    
    ;=====
    ; write to local nametable
    ;=====
    
    @localNametable:
    
    ; get current line address
    ld hl,(vwfLocalTargetCurrLineAddr)
    
    ; add x-offset * 2
    ld a,(printOffsetX)
    sla a
    add a,l
    ld l,a
    ld a,$00
    adc a,h
    ld h,a
    
    ; write
    ld (hl),e
    inc hl
    ld (hl),d
    
    ret
    
  
  sendVwfBufferIfPending:
    ld a,(vwfBufferPending)
    or a
    jr z,+
;      callExternal sendVwfBuffer
      call sendVwfBuffer
    +:
    ret
  
  ;========================================
  ; DE = base x/y position
  ; HL = string pointer
  ;========================================
  startVwfString:
;    ld a,(mapperSlot2Ctrl)
;    push af
      
      ; load string bank
;      ld a,b
;      ld (mapperSlot2Ctrl),a
      
      ; set up print position
      ld a,d
      ld (printBaseX),a
      ld a,e
      ld (printBaseY),a
      xor a
      ld (printOffsetX),a
      ld (printOffsetY),a
      
      ; reset VWF
      call resetVwf
      
  ;========================================
  ; HL = string pointer
  ;========================================
  printVwfString:
      @printLoop:
        ld a,(hl)
        inc hl
        
        ; check for terminator
        cp terminatorIndex
        jr z,@printingDone
        
        ; check for linebreak
        cp vwfBrIndex
        jr nz,+
          call sendVwfBufferIfPending
          
          ; reset VWF
          call resetVwf
          
          @vdpLinebreak:
          ; reset X
          xor a
          ld (printOffsetX),a
          
          ; Y++
          ld a,(printOffsetY)
;          add a,$02
          inc a
          ld (printOffsetY),a
          
          ld a,(vwfLocalTargetFlag)
          or a
          jr z,++
            @localLinebreak:
            push hl
              ld hl,(vwfLocalTargetCurrLineAddr)
              
              ; add nametable tile width * 2 to current line address to
              ; get next line's address
              ld a,(vwfLocalTargetW)
              sla a
              ld e,a
              ld d,$00
              add hl,de
              
              ld (vwfLocalTargetCurrLineAddr),hl
            pop hl
          ++:
          
          jr @printLoop
        +:
        
        ; check for old number op
        cp opNumIndex
        jr nz,+
          @oldNumOp:
          push hl
            ; get target number
            ld hl,($C520)
            ; digit count = don't care, hide leading zeroes
            ld bc,$0000
            call prepNumberString
            
            ; print result
            ld hl,numberPrintBuffer
            call printVwfString
          pop hl
          jr @printLoop
        +:
        
        ; check for new inline number print op
        cp opInlineNumIndex
        jr nz,+
          @newNumOp:
          push hl
            call printScriptNum
          pop hl
          jr @printLoop
        +:
        
        ; check for name op
        cp opNameIndex
        jr nz,+
          push hl
            call printScriptName
          pop hl
          jr @printLoop
        +:
        
;        ; check for ops
;        cp controlCodeStartIndex
;        jr c,+
;          
;          controlCodeJumpTable:
;          
;        +:
        
        ; C = target char index
        ld c,a
        push hl
          call printVwfChar
        pop hl
;        inc hl
        jr @printLoop
      
      @printingDone:
      
      ; do possible final transfer
      call sendVwfBufferIfPending
      
;    pop af
;    ld (mapperSlot2Ctrl),a
    ret
    
  printScriptNum:
    ; get target number
    ld hl,(inlinePrintNum)
    ld a,(inlinePrintDigitCount)
    ld b,a
    ld a,(inlinePrintShowLeadingZeroes)
    ld c,a
    
    call prepNumberString
    
    ; print result
    ld hl,numberPrintBuffer
    jp printVwfString
    
  scriptNames:
    .incbin "out/script/dialogue_names.bin"
  
  printScriptName:
    ; index of name
    ld a,($C522)
    ; table of names
    ld hl,scriptNames
    
    call readOffsetTable
    jp printVwfString
.ends

;========================================
; dispose VWF tiles on window close
;========================================

.bank $01 slot 1
.org $19EF
.section "window close vwf dispose 1" overwrite
  call freeClosedWindowVwfTiles
  nop
.ends

.slot 1
.section "window close vwf dispose 2" free
  ;========================================
  ; HL = pointer to window data
  ;========================================
  freeClosedWindowVwfTiles:
    ; make up work
    ld ($C2D9),hl
    ld h,a
    
    push hl
      call freeWindowVwfTiles
    pop hl
    
    ret
.ends

.slot 1
.section "window close vwf dispose 3" free
  ;========================================
  ; HL = pointer to window data
  ;      (VDP dst addr, h, w)
  ;========================================
  freeWindowVwfTiles:
    
    ld e,(hl)
    inc hl
    ld d,(hl)
    inc hl
    ld c,(hl)
    inc hl
    ld b,(hl)
    
    ex de,hl
    
    jp deallocVwfTileAreaByAddr
.ends

;========================================
; number printing routines
;========================================

  ;========================================
  ; string used when we need to print a
  ; number as part of a script
  ;========================================
  .slot 1
  .section "number print string" free
    numberPrintString:
      .db opInlineNumIndex
      .db terminatorIndex
  .ends

  ;========================================
  ; prepare a number for inline printing
  ;========================================
  .slot 1
  .section "number prep 1" free
    ;========================================
    ; convert a number to string encoding
    ; and place in numberPrintBuffer
    ;
    ; HL = number
    ; B = number of digits
    ;     0 = don't care, no space padding
    ; C = nonzero if leading zeroes
    ;     should be shown
    ;     (will be replaced with spaces if
    ;     nonzero)
    ;========================================
    .define maxPrintingDigits 4
    prepNumberString:
      ; handle zero specially
      ld a,h
      or l
      jr nz,@numberIsNonzero
        @numberIsZero:
        
        ; if digit count iz zero, output string is "0"
        ld a,b
        or a
        ld a,$00+vwfDigitStartOffset
        jr nz,+
          ld (numberPrintBuffer+0),a
          ld a,terminatorIndex
          ld (numberPrintBuffer+1),a
          ret
        +:
        
        ; if digit count nonzero, fill with "0" or spaces (depending on C)
        ; to digit count
        
        ld a,c
        or a
        jr z,+
          ; C nonzero = show zeroes
          ld a,$00+vwfDigitStartOffset
          jr ++
        +:
          ; C zero = show spaces
          ld a,$00+vwfDigitSpaceOffset
        ++:
        
        ld de,numberPrintBuffer
        dec b
        jr z,+
        -:
          ld (de),a
          inc de
          djnz -
        +:
        ; final digit must be zero
        ld a,$00+vwfDigitStartOffset
        ld (de),a
        inc de
        ; write terminator
        ld a,terminatorIndex
        ld (de),a
        ret
      
      @numberIsNonzero:
      
      ;=====
      ; if number exceeds our capacity to display, show as a string of 9s
      ;=====
      
      ; >= 10000 is undisplayable
      push hl
        ld de,10000
        or a
        sbc hl,de
      pop hl
      jr c,+
        ld hl,9999
        jr @overflowChecksDone
      +:
      
      ld a,b
      
      ; 1000
      cp $03
      jr nz,+
      push hl
        ld de,1000
        or a
        sbc hl,de
      pop hl
      jr c,++
        ld hl,999
      ++:
      jr @overflowChecksDone
      +:
      
      ; 100
      cp $02
      jr nz,+
      push hl
        ld de,100
        or a
        sbc hl,de
      pop hl
      jr c,++
        ld hl,99
      ++:
      jr @overflowChecksDone
      +:
      
      ; 10
      cp $01
      jr nz,+
      push hl
        ld de,10
        or a
        sbc hl,de
      pop hl
      jr c,++
        ld hl,9
      ++:
;      jr @overflowChecksDone   ; not needed
      +:
      
      @overflowChecksDone:
      
      ;=====
      ; convert to BCD
      ;=====
      
      push bc
        call bcdConv4Digit
        ; B = thousands
        ; C = hundreds
        ; D = tens
        ; A = ones
        ld (numberConvBuffer+3),a
        ld a,d
        ld (numberConvBuffer+2),a
        ld a,c
        ld (numberConvBuffer+1),a
        ld a,b
        ld (numberConvBuffer+0),a
      pop bc
      
      ;=====
      ; convert raw BCD to VWF
      ;=====
      
      ; save digit setting
      push bc
        ; convert raw BCD digits to VWF encoding
        ld hl,numberConvBuffer
        ld de,numberPrintBuffer
        ld b,maxPrintingDigits
        -:
          ld a,(hl)
          add a,vwfDigitStartOffset
          ld (de),a
          inc hl
          inc de
          djnz -
      pop bc
      
      ; if digit count is zero, remove leading zeroes
      ; (since we handled zero specially, there must be at least one
      ; nonzero digit. unless the number exceeded 9999 in which case we
      ; have other problems anyway.)
      ld a,b
      or a
      jr nz,+
        ; locate first nonzero digit
        ld hl,numberPrintBuffer
        ld b,maxPrintingDigits
        -:
          ld a,(hl)
          cp $00+vwfDigitStartOffset
          jr nz,++
            inc hl
            djnz -
        ++:
        
        @removeLeadingDigits:
        
        ; copy backward
        ld de,numberPrintBuffer
        -:
          ld a,(hl)
          ld (de),a
          inc hl
          inc de
          djnz -
        
        ; add terminator
        ld a,terminatorIndex
        ld (de),a
        
        ; nothing left to do (no leading zeroes)
        ret
      +:
      
      @checkLeadingZeroes:
      ; if C zero, leading zeroes should be replaced with spaces
      ld a,c
      or a
      jr nz,+
        ld hl,numberPrintBuffer
        -:
          ld a,(hl)
          cp $00+vwfDigitStartOffset
          jr nz,++
            ld a,vwfDigitSpaceOffset
            ld (hl),a
            inc hl
            jr -
        ++:
      +:
      
      @checkDigitCount:
      
      ; if digit limit exists, shift to match
      ; if limit equal to max digit count, we're done
      ld a,b
      or a
      cp maxPrintingDigits
      jr nz,+
        ld a,terminatorIndex
        ld (numberPrintBuffer+maxPrintingDigits),a
        ret
      +:
      
      ; otherwise, get pointer to start of content we want to print
      ; in HL
      ; subtract target number of digits from max
      ld a,maxPrintingDigits
      sub b
      ; add to base buffer address
      ld hl,numberPrintBuffer
      ld e,a
      ld d,$00
      add hl,de
      jr @removeLeadingDigits
      
  .ends

  ;========================================
  ; [num] opcode
  ;========================================
  
/*  .bank $03 slot 2
  .org $0B19
  .section "update num opcode 1" overwrite
    push de
      ; get target number
      ld hl,($C520)
      ; digit count = don't care, hide leading zeroes
      ld bc,$0000
      call prepNumberString
      
      ; print result
      ld hl,numberPrintBuffer
      call printVwfString
    pop de
    ret
  .ends */

;========================================
; load numbers for unit moves left
; window
;========================================

.bank $00 slot 0
.org $34C8
.section "unit moves window 1" overwrite
  call prepUnitMovesWindow
.ends

.bank $01 slot 1
.section "unit moves window 2" free
  prepUnitMovesWindow:
    ; loads number graphics to 30...3A
    ; no other text is displayed on this screen, so we don't care
    ; if we overwrite anything
    openTempBank 3
      call $8A16
    closeTempBank
    
    ; make up work
    ld a,(iy+$17)
    ret
.ends

;========================================
; intro
;========================================


  ;========================================
  ; tilemaps
  ; note that these are accessed through slot 1
  ;========================================
  
  .bank $01 slot 1
  .section "intro tilemap tables" free
    introTilemapTable:
    introTilemap1Table:
      .dw introTilemap1_0,introTilemap1_1,introTilemap1_2,introTilemap1_3,introTilemap1_4
    introTilemap2Table:
      .dw introTilemap2_0,introTilemap2_1,introTilemap2_2,introTilemap2_3,introTilemap2_4
    introTilemap3Table:
      .dw introTilemap3_0,introTilemap3_1,introTilemap3_2,introTilemap3_3,introTilemap3_4
    introTilemap4Table:
      .dw introTilemap4_0,introTilemap4_1,introTilemap4_2,introTilemap4_3,introTilemap4_4
    introTilemap5Table:
      .dw introTilemap5_0,introTilemap5_1,introTilemap5_2,introTilemap5_3,introTilemap5_4
  .ends

  .slot 1
  .section "intro tilemaps" superfree
    introTilemaps:
    introTilemap1_0: .incbin "out/maps/intro_scroll_1_0.bin"
    introTilemap1_1: .incbin "out/maps/intro_scroll_1_1.bin"
    introTilemap1_2: .incbin "out/maps/intro_scroll_1_2.bin"
    introTilemap1_3: .incbin "out/maps/intro_scroll_1_3.bin"
    introTilemap1_4: .incbin "out/maps/intro_scroll_1_4.bin"
    introTilemap2_0: .incbin "out/maps/intro_scroll_2_0.bin"
    introTilemap2_1: .incbin "out/maps/intro_scroll_2_1.bin"
    introTilemap2_2: .incbin "out/maps/intro_scroll_2_2.bin"
    introTilemap2_3: .incbin "out/maps/intro_scroll_2_3.bin"
    introTilemap2_4: .incbin "out/maps/intro_scroll_2_4.bin"
    introTilemap3_0: .incbin "out/maps/intro_scroll_3_0.bin"
    introTilemap3_1: .incbin "out/maps/intro_scroll_3_1.bin"
    introTilemap3_2: .incbin "out/maps/intro_scroll_3_2.bin"
    introTilemap3_3: .incbin "out/maps/intro_scroll_3_3.bin"
    introTilemap3_4: .incbin "out/maps/intro_scroll_3_4.bin"
    introTilemap4_0: .incbin "out/maps/intro_scroll_4_0.bin"
    introTilemap4_1: .incbin "out/maps/intro_scroll_4_1.bin"
    introTilemap4_2: .incbin "out/maps/intro_scroll_4_2.bin"
    introTilemap4_3: .incbin "out/maps/intro_scroll_4_3.bin"
    introTilemap4_4: .incbin "out/maps/intro_scroll_4_4.bin"
    introTilemap5_0: .incbin "out/maps/intro_scroll_5_0.bin"
    introTilemap5_1: .incbin "out/maps/intro_scroll_5_1.bin"
    introTilemap5_2: .incbin "out/maps/intro_scroll_5_2.bin"
    introTilemap5_3: .incbin "out/maps/intro_scroll_5_3.bin"
    introTilemap5_4: .incbin "out/maps/intro_scroll_5_4.bin"
  .ends
  
  ;========================================
  ; graphics
  ; also accessed through slot 1
  ;========================================

  .slot 1
  .section "intro graphics 1" superfree
    introGraphics1: .incbin "out/grp/intro_1_grp.bin" fsize introGraphics1Size
  .ends

  .slot 1
  .section "intro graphics 2" superfree
    introGraphics2: .incbin "out/grp/intro_2_grp.bin" fsize introGraphics2Size
  .ends

  .slot 1
  .section "intro graphics 3" superfree
    introGraphics3: .incbin "out/grp/intro_3_grp.bin" fsize introGraphics3Size
  .ends

  .slot 1
  .section "intro graphics 4" superfree
    introGraphics4: .incbin "out/grp/intro_4_grp.bin" fsize introGraphics4Size
  .ends

  .slot 1
  .section "intro graphics 5" superfree
    introGraphics5: .incbin "out/grp/intro_5_grp.bin" fsize introGraphics5Size
  .ends
  
  .define introGraphics1TileCount introGraphics1Size/bytesPerTile
  .define introGraphics2TileCount introGraphics2Size/bytesPerTile
  .define introGraphics3TileCount introGraphics3Size/bytesPerTile
  .define introGraphics4TileCount introGraphics4Size/bytesPerTile
  .define introGraphics5TileCount introGraphics5Size/bytesPerTile
  
  ;========================================
  ; load new graphics
  ;========================================
  
  .unbackground $3FEF0 $3FFFF

  .bank $0F slot 2
  .section "load intro graphics set" free
    introGraphicsBankTable:
      .db :introGraphics1
      .db :introGraphics2
      .db :introGraphics3
      .db :introGraphics4
      .db :introGraphics5
    
    introGraphicsSizeTable:
      .dw introGraphics1TileCount
      .dw introGraphics2TileCount
      .dw introGraphics3TileCount
      .dw introGraphics4TileCount
      .dw introGraphics5TileCount
    
    introGraphicsPtrTable:
      .dw introGraphics1
      .dw introGraphics2
      .dw introGraphics3
      .dw introGraphics4
      .dw introGraphics5
  
    loadIntroGraphicsSet:
      push de
      push bc
        ; A = set index
        ld e,a
        ; save slot 1 bank
        ld a,(mapperSlot1Ctrl)
        push af
          ; look up bank
          ld d,0
          ld hl,introGraphicsBankTable
          add hl,de
          ld a,(hl)
          
          ; load bank
          ld (mapperSlot1Ctrl),a
          
          ; BC = tile count
          ld a,e
          ld hl,introGraphicsSizeTable
          read16BitTable
          ld c,l
          ld b,h
          
          ; get pointer
          ld a,e
          ld hl,introGraphicsPtrTable
          read16BitTable
          
          ; DE = src pointer
          ex de,hl
          
          ; HL = VDP write command
          ld hl,$4000
          -:
            push bc
              ; send one tile (routine can only send 8-bit count of tiles at
              ; once)
              ld b,$01
              
              ; push VDP write command
              push hl
;                di
                  call sendRawTilesToVdp
;                ei
                ; next tile srcptr to DE
                ex de,hl
              ; pop VDP write command
              pop hl
              
              ; move VDP target to next tile
              ld bc,$0020
              add hl,bc
            
            ; retrieve and decrement tile count
            pop bc
            dec bc
            
            ; check if tile count zero
            ld a,b
            or c
            jr nz,-
        pop af
        ld (mapperSlot1Ctrl),a
      pop bc
      pop de
      ret
  .ends

  .bank $0F slot 2
  .org $0176
  .section "load intro graphics 1" overwrite
    ld a,$00
    jp loadIntroGraphicsSet
  .ends

  .bank $0F slot 2
  .org $04AF
  .section "load intro graphics 2" overwrite
    ld a,$01
    jp loadIntroGraphicsSet
  .ends

  .bank $0F slot 2
  .org $04DE
  .section "load intro graphics 3" overwrite
    ld a,$02
    jp loadIntroGraphicsSet
  .ends

  .bank $0F slot 2
  .org $0712
  .section "load intro graphics 4" overwrite
    ld a,$03
    jp loadIntroGraphicsSet
  .ends

  .bank $0F slot 2
  .org $087B
  .section "load intro graphics 5" overwrite
    ld a,$04
    jp loadIntroGraphicsSet
  .ends
  
  ;========================================
  ; replace pointers to original tilemap
  ; lists with new ones
  ;========================================

  .bank $0F slot 2
  .org $0154
  .section "intro tilemap 1 pointer" overwrite
    ld hl,introTilemap1Table
  .ends

  .bank $0F slot 2
  .org $0492
  .section "intro tilemap 2 pointer" overwrite
    ld hl,introTilemap2Table
  .ends

  .bank $0F slot 2
  .org $04C7
  .section "intro tilemap 3 pointer" overwrite
    ld hl,introTilemap3Table
  .ends

  .bank $0F slot 2
  .org $06F5
  .section "intro tilemap 4 pointer" overwrite
    ld hl,introTilemap4Table
  .ends

  .bank $0F slot 2
  .org $085E
  .section "intro tilemap 5 pointer" overwrite
    ld hl,introTilemap5Table
  .ends
  
  ;========================================
  ; use new bank and format for tilemaps
  ;========================================

  .bank $0F slot 2
  .org $0E5E
  .section "new intro tilemap format 1" overwrite
    rrca
    ld c,a      ; offset from base nametable pos
    nop ; don't shift right
    ld hl,-$3E ; tilemap is now full-width
  .ends

  .bank $0F slot 2
  .org $0E78
  .section "new intro tilemap format 2a" overwrite
    ; load bank containing tilemaps
    call loadNewIntroTilemapBank
  .ends

  .bank $0F slot 2
  .section "new intro tilemap format 2b" free
    loadNewIntroTilemapBank:
      ld a,:introTilemaps
      ld (mapperSlot1Ctrl),a
    
      ; make up work
      ld a,($D220)
      ret
  .ends

  .bank $0F slot 2
  .org $0E9A
  .section "new intro tilemap format 3a" overwrite
    ; load normal slot 1 bank
    call loadOldIntroTilemapBank
  .ends

  .bank $0F slot 2
  .section "new intro tilemap format 3b" free
    loadOldIntroTilemapBank:
      ld a,$01
      ld (mapperSlot1Ctrl),a
    
      ; make up work
      ld a,($D114)
      ret
  .ends

  .bank $0F slot 2
  .org $0E7E
  .section "new intro tilemap format 4" overwrite
    introVdpColumnSendLoop:
      ; send next column (DE = VDP target)
      inc c
      out (c),e
      out (c),d
      dec c
      ld a,$40
      add a,e
      ld e,a
      jr nc,+
        inc d
      +:
      
      ; maps are now full-width
  ;    ld a,$1F
      ld a,$3E
      add a,l
      ld l,a
      jr nc,+
        inc h
      +:
      
      ; output first half of tile
      outi
      ; loop counter is half of what it should be
      inc b
      ; output second half of tile
      outi
      jr nz,introVdpColumnSendLoop
      ; pad to original size
      nop
    
  .ends
  
  ;========================================
  ; adjust intro timing
  ;========================================
  
  ; ~0x268 pixel width = 0x340 pixel scroll width
  ; add 0xD8
  .define introScrollWidthAddValue $D8
  
  .define introScroll1Width 616+introScrollWidthAddValue
  .define introScroll2Width 692+introScrollWidthAddValue
  .define introScroll3Width 672+introScrollWidthAddValue
  .define introScroll4Width 404+introScrollWidthAddValue
  .define introScroll5Width 732+introScrollWidthAddValue

  .bank $0F slot 2
  .org $018A
  .section "intro scroll 1 width" overwrite
    ld bc,introScroll1Width
  .ends

  .bank $0F slot 2
  .org $04B7
  .section "intro scroll 2 width" overwrite
    ld bc,introScroll2Width
  .ends

  .bank $0F slot 2
  .org $04E6
  .section "intro scroll 3 width" overwrite
    ld bc,introScroll3Width
  .ends

  .bank $0F slot 2
  .org $071A
  .section "intro scroll 4 width" overwrite
    ld bc,introScroll4Width
  .ends

  .bank $0F slot 2
  .org $0883
  .section "intro scroll 5 width" overwrite
    ld bc,introScroll5Width
  .ends

;========================================
; use vwf and new strings where needed
;========================================

  ;.define vwfTileBase_main $003A
  ;.define vwfTileSize_main $0060-vwfTileBase_main
  
  ;========================================
  ; main strategy mode
  ;========================================

  .bank $01 slot 1
  .org $23D3
  .section "set up vwf main game 1" overwrite
    call setUpVwf_main
  .ends

  .bank $01 slot 1
  .section "set up vwf main game 2" free
    setUpVwf_main:
      ; make up work
      call $0432
      
      ld a,vwfTileSize_main
      ld b,vwfScrollZeroFlag_main
      ld hl,vwfTileBase_main
      jp setUpVwfTileAlloc
  .ends
  
  ;========================================
  ; mission intros
  ;========================================

  .bank $10 slot 2
  .org $13EC
  .section "set up vwf mission intro 1" overwrite
    call setUpVwf_missionIntro
  .ends

  .bank $01 slot 1
  .section "set up vwf mission intro 2" free
    setUpVwf_missionIntro:
      ld a,vwfTileSize_missionIntro
      ld b,vwfScrollZeroFlag_missionIntro
      ld hl,vwfTileBase_missionIntro
      call setUpVwfTileAlloc
      
      ; make up work
      ld hl,$947A
      ret
  .ends
  
  ;========================================
  ; unit overview mode
  ;========================================

  .bank $1A slot 1
  .org $0011
  .section "set up vwf unit overview 1" overwrite
    call setUpVwf_unitOverview
    nop
  .ends

  .bank $01 slot 1
  .section "set up vwf unit overview 2" free
    setUpVwf_unitOverview:
      ld a,vwfTileSize_unitOverview
      ld b,vwfScrollZeroFlag_unitOverview
      ld hl,vwfTileBase_unitOverview
      call setUpVwfTileAlloc
      
      ; make up work
      xor a
      ld hl,$893F
      ret
  .ends

  .bank $1A slot 1
  .org $0116
  .section "set up vwf unit overview 3" overwrite
    call setUpVwf_unitOverview_main
  .ends

  .bank $01 slot 1
  .section "set up vwf unit overview 4" free
    setUpVwf_unitOverview_main:
      ld a,vwfTileSize_unitOverview_main
      ld b,vwfScrollZeroFlag_unitOverview_main
      ld hl,vwfTileBase_unitOverview_main
      call setUpVwfTileAlloc
      
      ; make up work
      ld a,($C46D)
      ret
  .ends
  
  ;========================================
  ; compendium
  ;========================================

  .bank $00 slot 0
  .org $2FDE
  .section "set up vwf compendium 1" overwrite
    call setUpVwf_compendium
  .ends

  .bank $01 slot 1
  .section "set up vwf compendium 2" free
    setUpVwf_compendium:
      ; make up work
      call $2FF6
      
      ; load new graphics
      callExternal loadCompendiumGraphics
      
      ld a,vwfTileSize_compendium
      ld b,vwfScrollZeroFlag_compendium
      ld hl,vwfTileBase_compendium
      jp setUpVwfTileAlloc
  .ends

  .slot 2
  .section "set up vwf compendium 3" superfree
    compendiumGraphics:
      .incbin "out/grp/compendium_grp.bin" fsize compendiumGrpSize
    .define numCompendiumTiles compendiumGrpSize/bytesPerTile
    
    loadCompendiumGraphics:
      ; load new graphics
      ld hl,$4000
      ld de,compendiumGraphics
      ld b,numCompendiumTiles
      jp sendRawTilesToVdp
  .ends
  
  ;========================================
  ; battle mode
  ;========================================

  .bank $01 slot 1
  .section "set up vwf battle 2" free
    setUpVwf_battle:
      ld a,vwfTileSize_battle
      ld b,vwfScrollZeroFlag_battle
      ld hl,vwfTileBase_battle
      jp setUpVwfTileAlloc
  .ends
  
  ;========================================
  ; stage clear screen
  ;========================================

  .bank $1A slot 2
  .org $1235
  .section "set up vwf stage clear 1" overwrite
    call setUpVwf_stageClear
  .ends

  .bank $01 slot 1
  .section "set up vwf stage clear 2" free
    setUpVwf_stageClear:
      ; make up work
      call $0227
      
      ld a,vwfTileSize_stageClear
      ld b,vwfScrollZeroFlag_stageClear
      ld hl,vwfTileBase_stageClear
      jp setUpVwfTileAlloc
  .ends
  
  ;========================================
  ; congratulations screen
  ;========================================

  .bank $10 slot 2
  .org $3C4F
  .section "set up vwf congratulations 1" overwrite
    call setUpVwf_congratulations
  .ends

  .bank $01 slot 1
  .section "set up vwf congratulations 2" free
    setUpVwf_congratulations:
      ; make up work
      call $021B
      
      ld a,vwfTileSize_congratulations
      ld b,vwfScrollZeroFlag_congratulations
      ld hl,vwfTileBase_congratulations
      jp setUpVwfTileAlloc
  .ends
  
  ;========================================
  ; utility routine for reading offset
  ; tables for e.g. new strings
  ;========================================

  .bank $01 slot 1
  .section "read offset table" free
    ;========================================
    ; A  = index
    ; HL = table pointer
    ;
    ; returns absolute pointer in HL
    ;========================================
    readOffsetTable:
      push de
        push hl
          read16BitTable
        pop de
        add hl,de
      pop de
      
      ret
  .ends

  ;=========================================================================
  ; standard dialogue messages
  ;=========================================================================
  
  .define stdDialogueWindowW 12
  .define stdDialogueWindowH 7
  .define stdDialogueWindowNametableBase $D800

  .bank $03 slot 2
  .org $0872
  .section "vwf dialogue messages 1" size $26 overwrite
    ; set up dialogue message
    ld a,($C524)
    ld hl,$9089
    rst $28
;    call $8A4E
    call $8C23
    
    ; show empty window
/*    ld bc,$0C07
    ld de,$D800
    ld hl,$7068
    call openWindow
    
    ; print new string
    ld a,($C524)
    call printNewDialogueString
    jp $8898 */
    
    ; print new string
    call printNewDialogueString
    
    ; show window
    ld bc,$0C07
    ld de,$D800
    ld hl,$7068
    call openWindow
    
    ; resume
    jp $8898
  .ends

  .bank $01 slot 1
  .section "vwf dialogue messages 2" free
    ;========================================
    ; A = message index
    ;========================================
    printNewDialogueString:
      ; save message index
      ld a,($C524)
      ld b,a
    
      ; set up local nametable print
;      ld hl,stdDialogueWindowNametableBase
;      ld (vwfLocalTargetBaseAddr),hl
;      ld hl,stdDialogueWindowNametableBase+(stdDialogueWindowW*2*1)+(2*1)
;      ld (vwfLocalTargetCurrLineAddr),hl
;      ld a,stdDialogueWindowW
;      ld (vwfLocalTargetW),a
;      ld a,stdDialogueWindowH
;      ld (vwfLocalTargetH),a
;      ld a,$FF
;      ld (vwfLocalTargetFlag),a
      startLocalPrint stdDialogueWindowNametableBase stdDialogueWindowW stdDialogueWindowH 1 1
      
        ld a,(mapperSlot2Ctrl)
        push af
        
          ; load string bank
          ld a,:newDialogueStrings
          ld (mapperSlot2Ctrl),a
          
          ld a,b
          ld hl,newDialogueStrings
          call readOffsetTable
          
          ; printing x/y pos (screen-local)
          ld de,$080B
          
          call startVwfString
        
        pop af
        ld (mapperSlot2Ctrl),a
      
;      xor a
;      ld (vwfLocalTargetFlag),a
      endLocalPrint
      
      ret
      
  .ends

  .slot 2
  .section "vwf dialogue messages 3" superfree
    newDialogueStrings:
      .incbin "out/script/dialogue.bin"
      
  .ends

  ;=========================================================================
  ; unit status window
  ;=========================================================================
  
  .define unitStatusWindowW 12
  .define unitStatusWindowH 6   ; note: height is 5 for kaijuu,
                                ; but this only means garbage collection
                                ; will check one line unnecessarily
  .define unitStatusWindowNametableBase $DE00

  ;========================================
  ; blank out the parts of the tilemap that would normally
  ; contain tiles 28-2F, which in the original game always
  ; hold the current unit name.
  ; we'll print it ourself instead
  ; (window starts at ROM 9001 and is 12x6)
  ;========================================
  .bank $02 slot 2
  .org $102D
  .section "unit status window 1a" overwrite
    ; line 1
    .rept 8
      .dw $083A
    .endr
  .ends
  
  .bank $02 slot 2
  .org $102D+((unitStatusWindowW*2)*1)
  .section "unit status window 1b" overwrite
    ; line 2
    .rept 8
      .dw $083A
    .endr
  .ends
  
  .bank $02 slot 2
  .org $102D+((unitStatusWindowW*2)*2)-2
  .section "unit status window 1c" overwrite
    ; line 3 (including "attack" kanji)
    .rept 9
      .dw $083A
    .endr
  .ends
  
  .bank $02 slot 2
  .org $102D+((unitStatusWindowW*2)*3)
  .section "unit status window 1d" overwrite
    ; line 4
    .rept 8
      .dw $083A
    .endr
  .ends

  ;========================================
  ; do not send unit name to hardcoded VDP position
  ;========================================
  .bank $01 slot 1
  .org $20C9
  .section "unit status window 2" overwrite
    jp $60E5
  .ends

  ;========================================
  ; set up status window nametable printing
  ;========================================
  
  .bank $01 slot 1
  .org $2236
  .section "unit status window 3a" overwrite
    call startUnitStatusPrint
  .ends

  .bank $01 slot 1
  .section "unit status window 3b" free
    startUnitStatusPrint:
      startLocalPrint unitStatusWindowNametableBase unitStatusWindowW unitStatusWindowH 2 1
    
      ; make up work
      ld bc,$0060
      ret
  .ends
  
  ;========================================
  ; unit quantity
  ;========================================
  
  .bank $01 slot 1
  .org $2263
  .section "unit status window quantity 1" overwrite
    call doUnitStatusWinQuantity
    nop
    nop
  .ends
  
  .bank $01 slot 1
  .section "unit status window quantity 2" free
    unitQuantityString:
      .incbin "out/script/unit_quantity.bin"
    
    doUnitStatusWinQuantity:
      ; A = number of units
      ld l,a
      ld h,0
      setUpNumberPrint 2 1
      
      moveLocalPrint unitStatusWindowNametableBase unitStatusWindowW unitStatusWindowH 1 4
      ld hl,unitQuantityString
      call startVwfString
      
      ret
  .ends
  
  ;========================================
  ; attack/defense/current/max hp
  ;========================================
  
  .bank $01 slot 1
  .org $2277
  .section "unit status window stats 1" overwrite
    openTempBank :doUnitStatusWinStats
      call doUnitStatusWinStats
    closeTempBank
    jp $62B0
  .ends
  
  .slot 2
  .section "unit status window stats 2" superfree
    unitAttackString:
      .incbin "out/script/atk.bin"
    unitDefenseString:
      .incbin "out/script/def.bin"
    unitAtkDefSlashString:
      .incbin "out/script/slash_atkdef.bin"
    unitCurrMaxHpSlashString:
      .incbin "out/script/slash_hp.bin"
    
    doUnitStatusWinStats:
      moveLocalPrint unitStatusWindowNametableBase unitStatusWindowW unitStatusWindowH 1 3
      
      ; attack
      ld l,(iy+$1C)
      ld h,0
      setUpNumberPrint 3 0
      ld hl,unitAttackString
      call startVwfString
      
      ; separator
      ld hl,unitAtkDefSlashString
      call printVwfString
      
      ; defense
      ld l,(iy+$1D)
      ld h,0
      setUpNumberPrint 3 0
      ld hl,unitDefenseString
      call printVwfString
      
      ; current HP / slash / max HP
      moveLocalPrint unitStatusWindowNametableBase unitStatusWindowW unitStatusWindowH 3 2
      
      ; current hp
      ld l,(iy+$18)
      ld h,(iy+$19)
      setUpNumberPrint 4 0
      ld hl,numberPrintString
      call startVwfString
      
      ; separator
      ld hl,unitCurrMaxHpSlashString
      call printVwfString
      
      ; max hp
      ld l,(iy+$1A)
      ld h,(iy+$1B)
      setUpNumberPrint 4 0
      ld hl,numberPrintString
      call printVwfString
      
      ret
  .ends
  
  ;========================================
  ; instead of drawing unit name diacritics,
  ; draw the unit name itself (from our
  ; new table of names)
  ;========================================
  
  .bank $01 slot 1
  .org $22B0
  .section "unit status window 4" overwrite
    call drawNewUnitName_statusWindow
    
    ; we're done printing
    endLocalPrint
    jp $62C5
  .ends

  .bank $01 slot 1
  .section "unit status window 5" free
    tileBrString:
      .db vwfTileBrIndex
      .db terminatorIndex
  
    drawNewUnitName_statusWindow:
      moveLocalPrint unitStatusWindowNametableBase unitStatusWindowW unitStatusWindowH 1 1
      
      ld hl,tileBrString
      call startVwfString
      
      ld a,(mapperSlot2Ctrl)
      push af
        ld a,:printNewUnitName_ext
        ld (mapperSlot2Ctrl),a
        
        ; get unit ID - 1
        ld a,(iy+$0D)
        dec a
        
        ; "base x/y" (not used)
        ld de,$0000
        
        call printNewUnitName_ext
      pop af
      ld (mapperSlot2Ctrl),a
      
      ret
  .ends

  .slot 2
  .section "unit status window 6" superfree
    newUnitNames:
      .incbin "out/script/unitnames.bin"
    
    ;========================================
    ; A  = string num
    ; DE = base x/y
    ;========================================
    printNewUnitName_ext:
      ld hl,newUnitNames
;      read16BitTable
      call readOffsetTable
      jp printVwfString
    
    ;========================================
    ; A  = string num
    ; DE = base x/y
    ;========================================
    startNewUnitName_ext:
      ld hl,newUnitNames
;      read16BitTable
      call readOffsetTable
      jp startVwfString
    
    newUnitNames_battle:
      .incbin "out/script/unitnames_battle.bin"
    
    ;========================================
    ; A  = string num
    ; DE = base x/y
    ;========================================
    startNewUnitName_battle_ext:
      ld hl,newUnitNames_battle
;      read16BitTable
      call readOffsetTable
      jp startVwfString
  .ends

  ;=========================================================================
  ; turns left window
  ;=========================================================================
  
  .define turnsLeftWindowW 8
  .define turnsLeftWindowH 3+1  ; we're making the window one line taller
  .define turnsLeftWindowNametableBase $DE00

  .bank $01 slot 1
  .org $22D6
  .section "turns left window 1" size $2A overwrite
    ; make up work
    exx
      
      openTempBank :printNewTurnsLeftWindow
        ; print new string
        call printNewTurnsLeftWindow
      closeTempBank
    
    ; make up work
    exx
    
    ; show window
    ld bc,(turnsLeftWindowW<<8)|turnsLeftWindowH
    ld de,turnsLeftWindowNametableBase
    jp openWindow
  .ends

  .slot 2
  .section "turns left window 2" superfree
    printNewTurnsLeftWindow:
      ; load base tilemap
      ld bc,newTurnsLeftWindowTilemapEnd-newTurnsLeftWindowTilemap
      ld hl,newTurnsLeftWindowTilemap
      ld de,turnsLeftWindowNametableBase
      ldir
    
      ; set up
      startLocalPrint turnsLeftWindowNametableBase turnsLeftWindowW turnsLeftWindowH 1 1
        
        ; set up count of turns remaining for printing
        ld a,($C46F)
        dec a
        ld l,a
        ld h,$00
        setUpNumberPrint 2 0
      
        ld hl,turnsLeftString
        call startVwfString
      
      endLocalPrint
      
      
      ret
  
    newTurnsLeftWindowTilemap:
      ; expanded from 12x3 to 12x4 and hardcoded content blanked
      .dw $080A,$080B,$080B,$080B,$080B,$080B,$080B,$0A0A
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $0C0A,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0E0A
    newTurnsLeftWindowTilemapEnd:
  
    turnsLeftString:
      .incbin "out/script/turn_counter.bin"
    
  .ends
  
  .bank $01 slot 1
  .org $20FC
  .section "turns left window 3" overwrite
    ; when remaining turn window is on bottom of screen, move it up a tile
    ; since we made it a tile taller
    ld h,$98-8
  .ends

  ;=========================================================================
  ; start menu
  ;=========================================================================
  
  .define startMenuWindowW 6+2  ; widened
  .define startMenuWindowH 6
  .define startMenuWindowNametableBase $DE00
  
  .bank $01 slot 1
  .org $1DCA
  .section "start menu 1" size 12 overwrite
    call doStartMenu
    jp $5DD6
  .ends
  
  .bank $01 slot 1
  .section "start menu 2" free
;    unitAttackString:
;      .incbin "out/script/atk.bin"
    
    doStartMenu:
      openTempBank :doStartMenu_ext
        call doStartMenu_ext
      closeTempBank
      ret
  .ends
  
  .slot 2
  .section "start menu 3" superfree
;    unitAttackString:
;      .incbin "out/script/atk.bin"

    newStartMenuString:
      .incbin "out/script/start_menu.bin"
  
    newStartMenuWindowTilemap:
      ; expanded from 6x6 to 8x6 and hardcoded content blanked
      .dw $080A,$080B,$080B,$080B,$080B,$080B,$080B,$0A0A
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $0C0A,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0E0A
    newStartMenuWindowTilemapEnd:
    
    doStartMenu_ext:
      ; load tilemap
      ld hl,newStartMenuWindowTilemap
      ld de,startMenuWindowNametableBase
      ld bc,newStartMenuWindowTilemapEnd-newStartMenuWindowTilemap
      ldir
      
      startLocalPrint startMenuWindowNametableBase startMenuWindowW startMenuWindowH 2 1
        
        ld hl,newStartMenuString
        call startVwfString
        
      endLocalPrint
      
      ; open window
;      ld hl,$4038
      ld hl,$4030       ; shift left a tile to match turn window alignment
      ld bc,(startMenuWindowW<<8)|startMenuWindowH
      ld de,startMenuWindowNametableBase
      jp openWindow
  .ends
  
  .bank $02 slot 2
  .org $11D1
  .section "start menu 4" overwrite
    ; move cursor positions left 8 pixels to match new window position,
    ; and move up 1 pixel to match new font baseline
;    .dw $4040
;    .dw $4048
;    .dw $4050
;    .dw $4058
    .dw $383F
    .dw $3847
    .dw $384F
    .dw $3857
  .ends

  ;=========================================================================
  ; change turn yes/no prompt
  ;=========================================================================
  
  ; common relative jump target for the three prompt-based start menus
  ; (change, save, load)
  .bank $01 slot 1
  .org $1E76
  .section "start menu commands jump target" overwrite
    startMenuCommandOpenedJumpTarget:
      ld hl,$C467
  .ends
  
  .bank $02 slot 2
  .org $11D9
  .section "start menu commands cursor pos" overwrite
    ; move up a pixel to match new font baseline
;    .dw $6060
;    .dw $8060
    .dw $605F
    .dw $805F
  .ends
  
  .define ynPromptWindowW 10
  .define ynPromptWindowH 4
  .define ynPromptWindowNametableBase $DE00
  
  .bank $01 slot 1
  .org $1E3E
  .section "yn prompt 1" overwrite
    call doYnPrompt
    jr startMenuCommandOpenedJumpTarget
  .ends
  
  .bank $01 slot 1
  .section "yn prompt 2" free
    doYnPrompt:
      openTempBank :doYnPrompt_ext
        call doYnPrompt_ext
      closeTempBank
      ret
  .ends
  
  .slot 2
  .section "yn prompt 3" superfree
    ynPromptString:
      .incbin "out/script/yn_prompt.bin"
  
    ynPromptWindowTilemap:
      .dw $080A,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$0A0A
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $0C0A,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0E0A
    ynPromptWindowTilemapEnd:
    
    doYnPrompt_ext:
      ; load tilemap
      ld hl,ynPromptWindowTilemap
      ld de,ynPromptWindowNametableBase
      ld bc,ynPromptWindowTilemapEnd-ynPromptWindowTilemap
      ldir
      
      startLocalPrint ynPromptWindowNametableBase ynPromptWindowW ynPromptWindowH 1 1
        
        ld hl,ynPromptString
        call startVwfString
        
      endLocalPrint
      
      ; open window
      ld hl,$5858
      ld bc,(ynPromptWindowW<<8)|ynPromptWindowH
      ld de,ynPromptWindowNametableBase
      jp openWindow
  .ends

  ;=========================================================================
  ; save message
  ;=========================================================================
  
  .define savePromptWindowW 10
  .define savePromptWindowH 4
  .define savePromptWindowNametableBase $DE00
  
  .bank $01 slot 1
  .org $1E54
  .section "save prompt 1" overwrite
    call doSavePrompt
    jr startMenuCommandOpenedJumpTarget
  .ends
  
  .bank $01 slot 1
  .section "save prompt 2" free
    doSavePrompt:
      openTempBank :doSavePrompt_ext
        call doSavePrompt_ext
      closeTempBank
      ret
  .ends
  
  .slot 2
  .section "save prompt 3" superfree
    savePromptString:
      .incbin "out/script/save_prompt.bin"
  
    savePromptWindowTilemap:
      .dw $080A,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$0A0A
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $0C0A,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0E0A
    savePromptWindowTilemapEnd:
    
    doSavePrompt_ext:
      ; load tilemap
      ld hl,savePromptWindowTilemap
      ld de,savePromptWindowNametableBase
      ld bc,savePromptWindowTilemapEnd-savePromptWindowTilemap
      ldir
      
      startLocalPrint savePromptWindowNametableBase savePromptWindowW savePromptWindowH 1 1
        
        ld hl,savePromptString
        call startVwfString
        
      endLocalPrint
      
      ; open window
      ld hl,$5858
      ld bc,(savePromptWindowW<<8)|savePromptWindowH
      ld de,savePromptWindowNametableBase
      jp openWindow
  .ends
  
  ;=========================================================================
  ; load message
  ;=========================================================================
  
  .define loadPromptWindowW 10
  .define loadPromptWindowH 4
  .define loadPromptWindowNametableBase $DE00
  
  .bank $01 slot 1
  .org $1E6A
  .section "load prompt 1" overwrite
    call doLoadPrompt
    ; drop through
    jr startMenuCommandOpenedJumpTarget
  .ends
  
  .bank $01 slot 1
  .section "load prompt 2" free
    doLoadPrompt:
      openTempBank :doLoadPrompt_ext
        call doLoadPrompt_ext
      closeTempBank
      ret
  .ends
  
  .slot 2
  .section "load prompt 3" superfree
    loadPromptString:
      .incbin "out/script/load_prompt.bin"
  
    loadPromptWindowTilemap:
      .dw $080A,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$0A0A
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $0C0A,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0E0A
    loadPromptWindowTilemapEnd:
    
    doLoadPrompt_ext:
      ; load tilemap
      ld hl,loadPromptWindowTilemap
      ld de,loadPromptWindowNametableBase
      ld bc,loadPromptWindowTilemapEnd-loadPromptWindowTilemap
      ldir
      
      startLocalPrint loadPromptWindowNametableBase loadPromptWindowW loadPromptWindowH 1 1
        
        ld hl,loadPromptString
        call startVwfString
        
      endLocalPrint
      
      ; open window
      ld hl,$5858
      ld bc,(loadPromptWindowW<<8)|loadPromptWindowH
      ld de,loadPromptWindowNametableBase
      jp openWindow
  .ends

  ;=========================================================================
  ; save confirmed message
  ;=========================================================================
  
  .define saveConfirmedWindowW 9-1
  .define saveConfirmedWindowH 3
  .define saveConfirmedWindowNametableBase $DE00
  
  .bank $01 slot 1
  .org $1EE0
  .section "save confirmed 1" overwrite
    call doSaveConfirmed
    jp $5EEC
  .ends
  
  .bank $01 slot 1
  .section "save confirmed 2" free
    doSaveConfirmed:
      openTempBank :doSaveConfirmed_ext
        call doSaveConfirmed_ext
      closeTempBank
      ret
  .ends
  
  .slot 2
  .section "save confirmed 3" superfree
    saveConfirmedString:
      .incbin "out/script/save_done.bin"
  
    saveConfirmedWindowTilemap:
      .dw $080A,$080B,$080B,$080B,$080B,$080B,$080B,$0A0A
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $0C0A,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0E0A
    saveConfirmedWindowTilemapEnd:
    
    doSaveConfirmed_ext:
      ; load tilemap
      ld hl,saveConfirmedWindowTilemap
      ld de,saveConfirmedWindowNametableBase
      ld bc,saveConfirmedWindowTilemapEnd-saveConfirmedWindowTilemap
      ldir
      
      startLocalPrint saveConfirmedWindowNametableBase saveConfirmedWindowW saveConfirmedWindowH 1 1
        
        ld hl,saveConfirmedString
        call startVwfString
        
      endLocalPrint
      
      ; open window
;      ld hl,$6058
      ld hl,$6060       ; move right a tile
      ld bc,(saveConfirmedWindowW<<8)|saveConfirmedWindowH
      ld de,saveConfirmedWindowNametableBase
      jp openWindow
  .ends

  ;=========================================================================
  ; load failed message
  ;=========================================================================
  
  .define loadFailedWindowW 11+5
  .define loadFailedWindowH 3
  .define loadFailedWindowNametableBase $DE00
  
  .bank $01 slot 1
  .org $1F09
  .section "load failed 1" overwrite
    call doLoadFailed
    jp $5F15
  .ends
  
  .bank $01 slot 1
  .section "load failed 2" free
    doLoadFailed:
      openTempBank :doLoadFailed_ext
        call doLoadFailed_ext
      closeTempBank
      ret
  .ends
  
  .slot 2
  .section "load failed 3" superfree
    loadFailedString:
      .incbin "out/script/load_failed.bin"
  
    loadFailedWindowTilemap:
      .dw $080A,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$0A0A
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $0C0A,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0E0A
    loadFailedWindowTilemapEnd:
    
    doLoadFailed_ext:
      ; load tilemap
      ld hl,loadFailedWindowTilemap
      ld de,loadFailedWindowNametableBase
      ld bc,loadFailedWindowTilemapEnd-loadFailedWindowTilemap
      ldir
      
      startLocalPrint loadFailedWindowNametableBase loadFailedWindowW loadFailedWindowH 1 1
        
        ld hl,loadFailedString
        call startVwfString
        
      endLocalPrint
      
      ; open window
;      ld hl,$6058
      ld hl,$6040
      ld bc,(loadFailedWindowW<<8)|loadFailedWindowH
      ld de,loadFailedWindowNametableBase
      jp openWindow
  .ends

  ;=========================================================================
  ; merge garuda/mechagodzilla prompt
  ;=========================================================================
  
  .define mergePromptWindowW 10
  .define mergePromptWindowH 4
  .define mergePromptWindowNametableBase $DE00
  
  .bank $01 slot 1
  .org $1FBA
  .section "merge prompt 1" overwrite
    call doMergePrompt
    jp $5FC6
  .ends
  
  .bank $01 slot 1
  .section "merge prompt 2" free
    doMergePrompt:
      openTempBank :doMergePrompt_ext
        call doMergePrompt_ext
      closeTempBank
      ret
  .ends
  
  .slot 2
  .section "merge prompt 3" superfree
    mergePromptString:
      .incbin "out/script/merge_prompt.bin"
  
    mergePromptWindowTilemap:
      .dw $080A,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$0A0A
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $0C0A,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0E0A
    mergePromptWindowTilemapEnd:
    
    doMergePrompt_ext:
      ; load tilemap
      ld hl,mergePromptWindowTilemap
      ld de,mergePromptWindowNametableBase
      ld bc,mergePromptWindowTilemapEnd-mergePromptWindowTilemap
      ldir
      
      startLocalPrint mergePromptWindowNametableBase mergePromptWindowW mergePromptWindowH 1 1
        
        ld hl,mergePromptString
        call startVwfString
        
      endLocalPrint
      
      ; open window
      ld hl,$5858
      ld bc,(mergePromptWindowW<<8)|mergePromptWindowH
      ld de,mergePromptWindowNametableBase
      jp openWindow
  .ends

  ;=========================================================================
  ; unit actions menu
  ;=========================================================================
  
  .define unitActionsWindowW 10
  ; max height, may be smaller if fewer options
  .define unitActionsWindowH 5
;  .define unitActionsWindowNametableBase $DE00
  ; nope! DE00 is being used for completely unrelated calculations
  ; such as menu type!
;  .define unitActionsWindowNametableBase windowCompositionBuffer
  .define unitActionsWindowNametableBase $D800
  
  
  .bank $01 slot 1
  .org $1B29
  .section "unit actions 1" size 19 overwrite
    openTempBank :doUnitActions_ext
      call doUnitActions_ext
    closeTempBank
    jp $5B3C
  .ends
  
;  .bank $01 slot 1
;  .section "unit actions 2" free
;    doUnitActions:
;  .ends
  
  .slot 2
  .section "unit actions 3" superfree
    unitActionsString1:
      .incbin "out/script/unit_actions_1.bin"
    unitActionsString2:
      .incbin "out/script/unit_actions_2.bin"
    unitActionsString3:
      .incbin "out/script/unit_actions_3.bin"
    
    unitActionsMenuTable:
      .dw unitActionsString1
      .dw unitActionsString2
      .dw unitActionsString3
  
    ;  4 tiles high
    unitActionsWindowTilemapSmall:
      .dw $080A,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$0A0A
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $0C0A,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0E0A
    unitActionsWindowTilemapSmallEnd:
  
    ;  5 tiles high
    unitActionsWindowTilemapLarge:
      .dw $080A,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$0A0A
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $0C0A,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0E0A
    unitActionsWindowTilemapLargeEnd:
    
    doUnitActions_ext:
      ; save dstpos
      push de
        
        ; get unit action menu type
        ; 00 = attack/done
        ; 01 = long-range/done
        ; 02 = attack/long-range/done
        ld a,($DE0B)
        
        ; load tilemap
        
        ; size/pointer
        ld de,unitActionsWindowNametableBase
        ld hl,unitActionsWindowTilemapSmall
        ld bc,unitActionsWindowTilemapSmallEnd-unitActionsWindowTilemapSmall
        ; if bit 1 set, this is a 3-option window
        bit 1,a
        jr z,+
          ld hl,unitActionsWindowTilemapLarge
          ld bc,unitActionsWindowTilemapLargeEnd-unitActionsWindowTilemapLarge
        +:
        
        ldir
      
        startLocalPrint unitActionsWindowNametableBase unitActionsWindowW unitActionsWindowH 2 1
        
          ; look up target string
          ld a,($DE0B)
          ld hl,unitActionsMenuTable
          read16BitTable
          call startVwfString
          
        endLocalPrint
      
      ; retrieve dstpos
      pop hl
      
      ; open window
      ld bc,(unitActionsWindowW<<8)|unitActionsWindowH
      
      ; decrement height if small
      ld a,($DE0B)
      bit 1,a
      jr nz,+
        dec c
      +:
      
      ld de,unitActionsWindowNametableBase
      jp openWindow
      
  .ends
  
  ; move cursor positions up to match new font baseline
  .bank $02 slot 2
  .org $11DD
  .section "unit actions 4" overwrite
;    .db $80,$88,$90
    .db $7F,$87,$8F
  .ends

  ;=========================================================================
  ; turn change messages
  ;=========================================================================
  
  .define turnChangeWindowW 16
  .define turnChangeWindowH 3
  .define turnChangeWindowNametableBase $DE00
  .define turnChangeWindowSmallW 10
  .define turnChangeWindowSmallH 3
  
  .bank $01 slot 1
  .org $234E
  .section "turn change 1" size $22 overwrite
    openTempBank :doTurnChange_ext
      call doTurnChange_ext
    closeTempBank
    ret
  .ends
  
  .slot 2
  .section "turn change 2" superfree
;    turnChangeString:
;      .incbin "out/script/load_failed.bin"
    
    turnChangeCpuString:
      .incbin "out/script/turn_change_cpu.bin"
    turnChangePlayerString:
      .incbin "out/script/turn_change_player.bin"
    turnChangeSpaceMonstersString:
      .incbin "out/script/turn_change_spacemonsters.bin"
  
    turnChangeWindowTilemapLarge:
      .dw $080A,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$0A0A
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $0C0A,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0E0A
    turnChangeWindowTilemapLargeEnd:
  
    turnChangeWindowTilemapSmall:
      .dw $080A,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$080B,$0A0A
      .dw $080C,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$083A,$0A0C
      .dw $0C0A,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0C0B,$0E0A
    turnChangeWindowTilemapSmallEnd:
    
    doTurnChange_ext:
      ; decide whose turn it is
      ld a,($C46D)      ; current side's ID
      ; id of 02 is always space monsters
      cp $02
      jr z,@spaceMonsters
      
      ; check if current side is controlled by player or CPU
      ld l,a
      ld a,($C46E)      ; player's side ID
      xor l
      jr z,@player
      
      @cpu:
      
        ; load tilemap
        ld hl,turnChangeWindowTilemapSmall
        ld de,turnChangeWindowNametableBase
        ld bc,turnChangeWindowTilemapSmallEnd-turnChangeWindowTilemapSmall
        ldir
        
        startLocalPrint turnChangeWindowNametableBase turnChangeWindowSmallW turnChangeWindowH 1 1
          
          ld hl,turnChangeCpuString
          call startVwfString
          
        endLocalPrint
        
        ; open window
  ;      ld hl,$6058
        ld hl,$5858
        ld bc,(turnChangeWindowSmallW<<8)|turnChangeWindowH
        ld de,turnChangeWindowNametableBase
        jp openWindow
      
      @player:
      
        ; load tilemap
        ld hl,turnChangeWindowTilemapSmall
        ld de,turnChangeWindowNametableBase
        ld bc,turnChangeWindowTilemapSmallEnd-turnChangeWindowTilemapSmall
        ldir
        
        startLocalPrint turnChangeWindowNametableBase turnChangeWindowSmallW turnChangeWindowH 1 1
          
          ld hl,turnChangePlayerString
          call startVwfString
          
        endLocalPrint
        
        ; open window
  ;      ld hl,$6058
        ld hl,$5858
        ld bc,(turnChangeWindowSmallW<<8)|turnChangeWindowH
        ld de,turnChangeWindowNametableBase
        jp openWindow
      
      @spaceMonsters:
      
        ; load tilemap
        ld hl,turnChangeWindowTilemapLarge
        ld de,turnChangeWindowNametableBase
        ld bc,turnChangeWindowTilemapLargeEnd-turnChangeWindowTilemapLarge
        ldir
        
        startLocalPrint turnChangeWindowNametableBase turnChangeWindowW turnChangeWindowH 1 1
          
          ld hl,turnChangeSpaceMonstersString
          call startVwfString
          
        endLocalPrint
        
        ; open window
  ;      ld hl,$6058
        ld hl,$5840
        ld bc,(turnChangeWindowW<<8)|turnChangeWindowH
        ld de,turnChangeWindowNametableBase
        jp openWindow
      
  .ends

  ;=========================================================================
  ; mission intro
  ;=========================================================================
  
  .unbackground $43FAC $43FFF
  
  .bank $01 slot 1
  .section "number print setup routine" free
    ; HL = number
    ; B = digit count (0 for don't care)
    ; C = nonzero to show leading zeroes
    setUpNumberPrint_func:
      ld (inlinePrintNum),hl
      
      ld a,b
      ld (inlinePrintDigitCount),a
      
      ld a,c
      ld (inlinePrintShowLeadingZeroes),a
      ret
  .ends
  
  ;========================================
  ; main window
  ;========================================
  
  .define missionIntroMainWindowW 16
  .define missionIntroMainWindowH 7
;  .define stdDialogueWindowNametableBase $D800

  .bank $10 slot 2
  .org $1040
  .section "vwf mission intro main 1" overwrite
    ex de,hl
    ld hl,$D216
    inc (hl)
    ex de,hl
    jp printMissionIntroMessage
  .ends

  .bank $10 slot 2
  .org $104E
  .section "vwf mission intro deallocate before blank 1" overwrite
    ld bc,$1007
    ld hl,$3B90
    call deallocVwfTileAreaByAddr
    jp $905A
  .ends

  .bank $1 slot 1
  .section "vwf mission intro main 2" free
    ; HL = message index
    printMissionIntroMessage:
      ; draw blank window
      push hl
        ; deallocate tiles
        ld bc,$1007
        ld hl,$3B90
        call deallocVwfTileAreaByAddr
        
        ld bc,$1007
        ld de,$B8C1
        ld hl,$7B90
        call sendTilemapToAbsoluteVdpAddr
      pop hl
      
      ; print message
      callExternal printMissionIntroMessage_ext
      
      ret
  .ends

  .slot 2
  .section "vwf mission intro main 3" superfree
    missionIntroMessages:
      .incbin "out/script/missionintro.bin"
    
    ; HL = message index
    printMissionIntroMessage_ext:
      ld a,l
      ld hl,missionIntroMessages
      call readOffsetTable
      
      ld de,$030B
      jp startVwfString
      
  .ends

  .bank $10 slot 2
  .org $0EC9
  .section "vwf mission intro play stage confirmation" overwrite
    ; tilemap 37 == stage confirmation message
    ld hl,37
    call printMissionIntroMessage
    jp $8ED5
  .ends

  .bank $10 slot 2
  .org $1358
  .section "vwf mission intro destruction rate g-force" size $1F overwrite
    ; ?
    ld a,$01
    ld ($D229),a
    call $9063
    
    ; get destruction rate
    ld a,($C461)
    ld hl,$94C9
    read8BitTable
    
    ; set up number print
    ld l,a
    xor a
    ld h,a
    ld b,a
    ld c,a
    call setUpNumberPrint_func
    
    ; print mission intro page
    ld hl,($D217)
    jp $9032
  .ends

  .bank $10 slot 2
  .org $1384
  .section "vwf mission intro destruction rate godzilla" size $1F overwrite
    ; ?
    ld a,$01
    ld ($D229),a
    call $9063
    
    ; get destruction rate
    ld a,($C461)
    ld hl,$94CD
    read8BitTable
    
    ; set up number print
    ld l,a
    xor a
    ld h,a
    ld b,a
    ld c,a
    call setUpNumberPrint_func
    
    ; print mission intro page
    ld hl,($D217)
    jp $9032
  .ends
  
  ;========================================
  ; top window
  ;========================================
  
  .define missionIntroTopWindowW 9
  .define missionIntroTopWindowH 3

  .bank $10 slot 2
  .org $101C
  .section "vwf mission intro top 1" overwrite
    jp printMissionIntroTopMessage
  .ends

  .bank $1 slot 1
  .section "vwf mission intro top 2" free
    ; A = message index
    printMissionIntroTopMessage:
      ld l,a
      
      ; print message
      callExternal printMissionIntroTopMessage_ext
      
      ret
  .ends

  .slot 2
  .section "vwf mission intro top 3" superfree
    missionIntroTopMessages:
      .incbin "out/script/missionintro_topwin.bin"
    
    missionIntroEmptyTopWindow:
      .dw $0901,$0903,$0903,$0903,$0903,$0903,$0903,$0903,$0B01
      .dw $0902,$0900,$0900,$0900,$0900,$0900,$0900,$0900,$0B02
      .dw $0D01,$0D03,$0D03,$0D03,$0D03,$0D03,$0D03,$0D03,$0F01
    
    ; L = message index
    printMissionIntroTopMessage_ext:
      ; draw blank window
      push hl
        ; deallocate tiles
        ld bc,$0A03
        ld hl,$38D6
        call deallocVwfTileAreaByAddr
        
        ld bc,(missionIntroTopWindowW<<8)|missionIntroTopWindowH
        ld de,missionIntroEmptyTopWindow
        ld hl,$78D6
        call sendTilemapToAbsoluteVdpAddr
      pop hl
      
      ld a,l
      ld hl,missionIntroTopMessages
      call readOffsetTable
      
      ld de,$0600
      jp startVwfString
      
  .ends

  ;=========================================================================
  ; unit overview
  ;=========================================================================
  
  ;========================================
  ; unit list
  ;========================================
  
  .define unitListMainWindowW 18
  .define unitListMainWindowH 3
  .define unitListMainWindowNametableBase $D800

  .bank $1A slot 2
  .org $0308
  .section "vwf unit overview unit list 1" overwrite
      ; BC = DE = base dst pointer
      ld l,e
      ld h,d
      
      ; set up printing
      push de
      startLocalPrintNonFixed unitListMainWindowW unitListMainWindowH 0 1
      
        call startNewUnitName
      
      endLocalPrint
      pop de
      
      jp $833C
  .ends

  .bank $01 slot 1
  .section "vwf unit overview unit list 2" free
    startNewUnitName:
      ld a,(mapperSlot2Ctrl)
      push af
        ld a,:startNewUnitName_ext
        ld (mapperSlot2Ctrl),a
        
        ; get unit ID
        ld a,(ix+$0D)
        dec a
        
        ; "base x/y" (not used)
        ld de,$0000
        
        call startNewUnitName_ext
      pop af
      ld (mapperSlot2Ctrl),a
      ret
  .ends

  ; change attack/defense kanji to "A" and "D", respectively
  
  .bank $1A slot 2
  .org $068F
  .section "vwf unit overview unit list 3" overwrite
    .dw $0101
  .ends
  
  .bank $1A slot 2
  .org $0699
  .section "vwf unit overview unit list 4" overwrite
    .dw $0104
  .ends
  
  ; clear visible tilemap before redrawing new names (since vwf may
  ; reallocate visible content)
  
  .bank $1A slot 2
  .org $01CE
  .section "vwf unit overview unit list 5" overwrite
    call hideVisibleUnitList
  .ends
  
  .bank $1A slot 2
  .org $02CD
  .section "vwf unit overview unit list deallocate init 1" overwrite
    call deallocOnUnitListDraw
  .ends
  
  .bank $1A slot 2
  .section "vwf unit overview unit list deallocate init 2" free
    deallocOnUnitListDraw:
      ; garbage collection must not run while we're drawing the unit names.
      ; since nothing else is drawn on this screen, we can get away with
      ; deallocating the vwf tiles before preparing the unit list.
      call freeAllVwfTiles
      call resetVwf
      
      ; make up work
      ld a,($C46C)
      ret
  .ends
  
  .bank $1A slot 2
  .section "vwf unit overview unit list 6" free
    blankUnitListTilemap:
      .rept 18
        .dw $008C
      .endr
      .rept 18*5*2
        .db $00
      .endr
  
    hideVisibleUnitList:
      push hl
/*        ld hl,$7BCE
        ld de,$8635
        ld bc,$1203
        call $0212
        
        ld hl,$7C8E
        ld de,$8635
        ld bc,$1203
        call $0212 */
        
        ld hl,$7BCE
        ld de,blankUnitListTilemap
        ld bc,$1206
        call $0212
      pop hl
      
      ; make up work
      ld a,l
      jp $82BF
  .ends

  ;=====
  ; label text
  ;=====
  
  ; blank out "turns remaining"
  .bank $1A slot 2
  .org $07C9
  .section "vwf unit overview label text 1" overwrite
    .db $00,$00,$00,$00,$00,$00,$00,$00,$00
  .ends
  
  ; draw turns remaining text
  .bank $1A slot 2
  .org $0098
  .section "vwf unit overview label text 2" overwrite
    
    push de
      call drawRemainingTurns_unitOverview
    pop de
    
    jp $80AD
  .ends
  
  ; draw monster arrival text
  .bank $1A slot 2
  .org $04AC
  .section "vwf unit overview label text 3" overwrite
    
    call drawMonsterArrival_unitOverview
    
;    jp $84CA
    ret
  .ends
  
  .unbackground $69E80 $6BFFF
  
  ; draw turns remaining text
  .define turnsRemaining_unitOverview_intermediatePrint $DA00
  .define turnsRemaining_unitOverview_intermediatePrint_length 9
  .bank $1A slot 2
  .section "vwf unit overview label text 4" free
    newStageExplanationTilemap:
      .incbin "out/grp/stageinfo.bin"
    
    remainingTurnsString_unitOverview:
      .incbin "out/script/turns_left_unitoverview.bin"
    
    remainingTurnsString_unitOverview_singular:
      .incbin "out/script/turns_left_unitoverview_singular.bin"
    
    arrivalMonsterMessage:
      .incbin "out/script/monster_arrival_unitoverview.bin"
    
    arrivalMonsterMessageSingular:
      .incbin "out/script/monster_arrival_unitoverview_singular.bin"
    
    arrivalMonsterList:
      .incbin "out/script/monster_arrival_list_unitoverview.bin"
  
    drawRemainingTurns_unitOverview:
      ; get turn count
      ld a,($C46F)
      dec a
      
      ; set up for printing
      ld l,a
      ld h,$00
      setUpNumberPrint 2 0
      
      ; clear memory we're going to write to
      xor a
      ld hl,turnsRemaining_unitOverview_intermediatePrint
      ld bc,(turnsRemaining_unitOverview_intermediatePrint_length<<8)|$01
      call clearFullTilemap
      
      ; don't disable interrupts (they're already off)
      dec a
      ld (noInterruptDisableFlag),a
      
      ; print full tilemap to DA00
      startLocalPrint turnsRemaining_unitOverview_intermediatePrint 11 1 0 0
        ; get remaining turn count
        ld a,($C46F)
        cp 2
        jr nz,+
          ld hl,remainingTurnsString_unitOverview_singular
          jr ++
        +:
          ld hl,remainingTurnsString_unitOverview
        ++:
        call startVwfString
      endLocalPrint
      
      ; copy in halfwidth form to $D832
      ld hl,turnsRemaining_unitOverview_intermediatePrint
      ld de,$D832
      ld bc,(turnsRemaining_unitOverview_intermediatePrint_length<<8)|$01
      call halveTilemap
      
      ; clear vwf interrupt disable flag
      xor a
      ld (noInterruptDisableFlag),a
      
      ret
    
;  .define monsterArrivalName_unitOverview_intermediatePrint $DA00
;  .define monsterArrivalName_unitOverview_intermediatePrint_length 9
    .define monsterArrivalNameXY $0A05
    .define monsterArrivalMessageXY $0A06
    drawMonsterArrival_unitOverview:
      
      exx
        ; A = monster index
        push af
          ; don't disable interrupts (they're already off)
          ld a,$FF
          ld (noInterruptDisableFlag),a
        pop af
        
        ; print monster name string
        ld hl,arrivalMonsterList
        call readOffsetTable
        ld de,monsterArrivalNameXY
        call startVwfString
      exx
      
      ; HL = turn count pointer
      ld a,(hl)
        
      ; prep turn count string
      push af
        ld l,a
        ld h,$00
        setUpNumberPrint 0 0
      pop af
      
      ; print arrival message
      ld de,monsterArrivalMessageXY
      
      ; if only one turn left, use singular message ("1 turn left")
      dec a
      jr nz,+
        ld hl,arrivalMonsterMessageSingular
        jr ++
      +:
        ld hl,arrivalMonsterMessage
      ++:
      call startVwfString
      
      ; clear vwf interrupt disable flag
      xor a
      ld (noInterruptDisableFlag),a
      
      ret
    
    ; BC = dimensions
    ; HL = dst
    clearFullTilemap:
      xor a
      --:
        push bc
          -:
            ld (hl),a
            inc hl
            ld (hl),a
            inc hl
            djnz -
        pop bc
        dec c
        jr nz,--
      ret
    
    ; HL = src
    ; DE = dst
    ; B = w
    ; C = h
    halveTilemap:
      --:
        push bc
          -:
            ld a,(hl)
            ld (de),a
            inc hl
            inc hl
            inc de
            djnz -
        pop bc
        dec c
        jr nz,--
      ret
    
    loadNewStageExplanationGraphics:
      ; make up work
      call $0233
      
      ; load graphics
      ld de,newStageExplanationTilemap
      ld hl,$4F20
      ld b,8
      jp sendRawTilesToVdp
  .ends
  
  ; patch "stage explanation" graphics
  .bank $1A slot 2
  .org $0020
  .section "vwf unit overview label text 5" overwrite
    call loadNewStageExplanationGraphics
  .ends
  
  ;========================================
  ; compendium
  ;========================================
  
  .define numCompendiumMonsters 12
  
  .bank $00 slot 0
  .org $3088
  .section "compendium 1" overwrite
    ; don't draw "man"
    jp $3096
  .ends
  
  .bank $1D slot 2
  .org $3426
  .section "compendium 2" overwrite
    ; convert page table pointers to indices
    .rept numCompendiumMonsters index count
      .dw count
    .endr
  .ends
  
  .bank $00 slot 0
  .org $2F8F
  .section "compendium 3" overwrite
    call doNewCompendiumPage
    ret
  .ends
  
  .bank $01 slot 1
  .section "compendium 4" free
    doNewCompendiumPage:
      callExternal doNewCompendiumPage_ext
      ret
  .ends
  
  .bank $00 slot 0
  .org $2FB5
  .section "compendium 5" overwrite
    call doNewCompendiumHeightWeight
    ret
  .ends
  
  .bank $01 slot 1
  .section "compendium 6" free
    doNewCompendiumHeightWeight:
      callExternal doNewCompendiumHeightWeight_ext
      ret
  .ends
  
  .slot 2
  .section "compendium pages" superfree
    compendiumSet00: .incbin "out/script/compendium0.bin"
    compendiumSet01: .incbin "out/script/compendium1.bin"
    compendiumSet02: .incbin "out/script/compendium2.bin"
    compendiumSet03: .incbin "out/script/compendium3.bin"
    compendiumSet04: .incbin "out/script/compendium4.bin"
    compendiumSet05: .incbin "out/script/compendium5.bin"
    compendiumSet06: .incbin "out/script/compendium6.bin"
    compendiumSet07: .incbin "out/script/compendium7.bin"
    compendiumSet08: .incbin "out/script/compendium8.bin"
    compendiumSet09: .incbin "out/script/compendium9.bin"
    compendiumSet0A: .incbin "out/script/compendium10.bin"
    compendiumSet0B: .incbin "out/script/compendium11.bin"
    
    compendiumSetTable:
      .dw compendiumSet00
      .dw compendiumSet01
      .dw compendiumSet02
      .dw compendiumSet03
      .dw compendiumSet04
      .dw compendiumSet05
      .dw compendiumSet06
      .dw compendiumSet07
      .dw compendiumSet08
      .dw compendiumSet09
      .dw compendiumSet0A
      .dw compendiumSet0B
    
    blankCompendiumPageMap:
      .rept 6
        .rept 10
          .dw $011A
        .endr
      .endr
    
    doNewCompendiumPage_ext:
      ; clear existing content
      
      ; deallocate VWF
      ld bc,$0A06
      ld hl,$3B9E
      call deallocVwfTileAreaByAddr
      
      ld bc,$0A06
      ld de,blankCompendiumPageMap
      ld hl,$7B9E
      call sendTilemapToAbsoluteVdpAddr
    
      ; monster index (originally pointer to page pointer table
      ; but changed for translation)
;      ld hl,($D20C)
      ld a,($D20C)
      ld hl,compendiumSetTable
      read16BitTable
      
      ; page index
      ld a,($D20B)
      call readOffsetTable
      
      ld de,$0A0B
      call startVwfString
      
      ret
      
    compendiumHeightStrings:
      .incbin "out/script/compendium_height.bin"
    compendiumWeightStrings:
      .incbin "out/script/compendium_weight.bin"
    
    blankCompendiumHeightWeightMap:
      .rept 1
        .rept 5
          .dw $0827
        .endr
      .endr
    
    doNewCompendiumHeightWeight_ext:
      ; clear existing content
      
      ; deallocate VWF
      ld bc,$0503
      ld hl,$3C12
      call deallocVwfTileAreaByAddr
      
      ; height
      ld bc,$0501
      ld de,blankCompendiumHeightWeightMap
      ld hl,$7C12
      call sendTilemapToAbsoluteVdpAddr
      ; weight
      ld bc,$0501
      ld de,blankCompendiumHeightWeightMap
      ld hl,$7C92
      call sendTilemapToAbsoluteVdpAddr
      
      ; height
      
      ; monster index
      ld a,($D20A)
      ld hl,compendiumHeightStrings
      call readOffsetTable
      ld de,$060C
      call startVwfString
      
      ; weight
      
      ; monster index
      ld a,($D20A)
      ld hl,compendiumWeightStrings
      call readOffsetTable
      ld de,$030E
      call startVwfString
      
      ret
    
  .ends
  
  ;========================================
  ; names on battle screen
  ;========================================
  
  .bank $00 slot 0
  .org $182A
  .section "battle screen 1" overwrite
    ld a,(mapperSlot1Ctrl)
    push af
      ld a,1
      ld (mapperSlot1Ctrl),a
      ; init VWF
      call setUpVwf_battle
    pop af
    ld (mapperSlot1Ctrl),a
    
    ; don't load names/diacritics
    jp $1854
  .ends
  
  ; draw name tilemap
  .bank $00 slot 0
  .org $1F37
  .section "battle screen 2" overwrite
    ; A = name index
    dec a
    ld c,a
    
    ld a,(mapperSlot1Ctrl)
    push af
      ld a,1
      ld (mapperSlot1Ctrl),a
      
      call printCombatantName
    pop af
    ld (mapperSlot1Ctrl),a
    ret
  .ends
  
  .define combatantLocalBuffer $DE00
  .define combatantNameLen 8
  
  .bank $01 slot 1
  .section "battle screen 3" free
    printCombatantName:
      ; DE = dst
      ; C = combatant ID
      
      ; add 8 to dst to skip diacritic row
      push de
        ex de,hl
        ld e,combatantNameLen
        ld d,$00
        add hl,de
      pop de
      
      ex de,hl
      
      push de
        
        ; clear name buffer
        ld de,combatantLocalBuffer
        ld b,combatantNameLen
        xor a
        -:
          ld (de),a
          inc de
          ld (de),a
          inc de
          djnz -
          
      
        ld a,$FF
        ld (noInterruptDisableFlag),a
          
;          startLocalPrintNonFixed 10 1 0 0
          startLocalPrint combatantLocalBuffer 10 1 0 0
            
            ld a,(mapperSlot2Ctrl)
            push af
              ld a,:startNewUnitName_battle_ext
              ld (mapperSlot2Ctrl),a
              
              ; get unit ID - 1
              ld a,c
              
              ; "base x/y" (not used)
              ld de,$0000
              
              call startNewUnitName_battle_ext
            pop af
            ld (mapperSlot2Ctrl),a
            
          endLocalPrint
        
        xor a
        ld (noInterruptDisableFlag),a
      
      ; halve tilemap
      ; DE = target dst
      pop de
      ld hl,combatantLocalBuffer
      
      ld b,$08
      -:
        ld a,(hl)
        ld (de),a
        
        inc hl
        inc hl
        inc de
        djnz -
      
      ret
  .ends
  
  ;========================================
  ; must defeat space monsters message
  ;========================================
  
  .bank $1A slot 2
  .org $11E0
  .section "must defeat space monsters 1" overwrite
    call printDefeatSpaceMonstersMessage    
    jp $91F7
  .ends
  
  .bank $1A slot 2
  .section "must defeat space monsters 2" free
    mustDefeatSpaceMonstersMessage_gForce:
      .incbin "out/script/destroy_spacemonsters_gforce.bin"
    mustDefeatSpaceMonstersMessage_godzilla:
      .incbin "out/script/destroy_spacemonsters_godzilla.bin"
    
    spaceMonstersClearTilemap:
      .rept 8
        .rept 18
          .db $00
        .endr
      .endr
      
    printDefeatSpaceMonstersMessage:
      ; clear existing content
      ld a,$09
      ld bc,$1208
      ld de,spaceMonstersClearTilemap
      ld hl,$7B4E
      call sendHalfTilemapToAbsoluteVdpAddr
    
      ; D213 = zero if player is g-force, nonzero if godzilla
      ld a,($D213)
      or a
      
      jr z,+
        ld hl,mustDefeatSpaceMonstersMessage_godzilla
        jr ++
      +:
        ld hl,mustDefeatSpaceMonstersMessage_gForce
      ++:
      
      ld de,$010A
      jp startVwfString
  .ends
  
  ;========================================
  ; congratulations messages
  ;========================================
  
  .bank $10 slot 2
  .org $3C73
  .section "congratulations message 1" overwrite
    jp printCongratulationsMessage
;    ret
  .ends
  
  .bank $01 slot 1
  .section "congratulations message 2" free
    printCongratulationsMessage:
      callExternal printCongratulationsMessage_ext
      ret
  .ends
  
  .slot 2
  .section "congratulations message 3" superfree
    congratulationMessage_gGorce:
      .incbin "out/script/tryagain_gforce.bin"
    congratulationMessage_godzilla:
      .incbin "out/script/tryagain_godzilla.bin"
    
    congratulationsClearTilemap:
      .rept 6
        .rept 16
          .db $00
        .endr
      .endr
      
    printCongratulationsMessage_ext:
      ; clear existing content
      ld a,$01
      ld bc,$1006
      ld de,congratulationsClearTilemap
      ld hl,$7BD2
      call sendHalfTilemapToAbsoluteVdpAddr
    
      ; C46E = zero if player is g-force, nonzero if godzilla
      ld a,($C46E)
      or a
      
      jr z,+
        ld hl,congratulationMessage_godzilla
        jr ++
      +:
        ld hl,congratulationMessage_gGorce
      ++:
      
      ld de,$030B
      jp startVwfString
  .ends

  ;========================================
  ; vs test mode menu
  ;========================================
  
  .bank $00 slot 0
  .org $29A9
  .section "vs test mode 1" overwrite
    call initVsTestMode
  .ends
  
  .bank $01 slot 1
  .section "vs test mode 2" free
    initVsTestMode:
      ld a,vwfTileSize_vsTest
      ld b,vwfScrollZeroFlag_vsTest
      ld hl,vwfTileBase_vsTest
      call setUpVwfTileAlloc
      
      ; make up work
      jp $2B3A
  .ends
  
  .bank $00 slot 0
  .org $2AAB
  .section "vs test mode 3a" overwrite
    ld a,($D201)
    ld b,a
    ld a,(hl)
    ld c,a
    jp drawVsTestName
  .ends
  
  ; draw initial enemy names properly
  .bank $00 slot 0
  .org $2BA9
  .section "vs test mode 3b" overwrite
    ; left name
;    ld bc,$0000
    ld a,($D234)
    ld c,a
    ld b,$00
    call drawVsTestName
    
    ; right name
    ld a,($D23A)
    ld c,a
    ld b,$01
    call drawVsTestName
    
    jp $2BCA
  .ends
  
  .bank $01 slot 1
  .section "vs test mode 4" free
    blankEnemyNameTilemap:
      .rept 2
        .rept 8
          .db $00
        .endr
      .endr
      
    drawVsTestName:
      ; B = 0 if left, 1 if right
      ; C = unit ID
      
      ld a,(mapperSlot2Ctrl)
      push af
        ld a,:startNewUnitName_battle_ext
        ld (mapperSlot2Ctrl),a
        
        ; blank existing content
        push hl
        push bc
          ld a,b
          or a
          jr nz,@right
          @left:
            ld hl,$799E
            jr +
          @right:
            ld hl,$7A1E
          +:
          
          push hl
            ; deallocate VWF
            ld bc,$4000
            or a
            sbc hl,bc
            ld bc,$0802
            call deallocVwfTileAreaByAddr
          pop hl
          
          ld a,$00
          ld bc,$0802
          ld de,blankEnemyNameTilemap
          call sendHalfTilemapToAbsoluteVdpAddr
        pop bc
        pop hl
        
        ; base x/y
        ld a,b
        or a
        jr nz,@right2
        @left2:
          ld de,$0903
          jr +
        @right2:
          ld de,$0905
        +:
        
        ; enemy ID
        ld a,c
        
        call startNewUnitName_battle_ext
      pop af
      ld (mapperSlot2Ctrl),a
      ret
  .ends
  
;========================================
; title screen
;========================================

  ;=====
  ; load new graphics
  ;=====

  .bank $0F slot 2
  .org $0A1A
  .section "title screen 1" overwrite
    jp initNewTitleScreen
  .ends

  .bank $01 slot 1
  .section "title screen 2" free
    initNewTitleScreen:
      ; make up work
      call $0233
      
      callExternal initNewTitleScreen_ext
      ret
  .ends

  .slot 2
  .section "title screen 3" superfree
    newTitleGodzillaGrp:
      .incbin "out/grp/title_godzilla_grp.bin" FSIZE newTitleGodzillaGrpSize
      .define numNewTitleGodzillaGrpTiles newTitleGodzillaGrpSize/bytesPerTile
    
    newTitleGodzillaLogoTilemap:
      .incbin "out/maps/title_godzilla_logo.bin"
    
    newCompendiumMenuLabel:
      .incbin "out/grp/compendium_menulabel.bin" FSIZE newCompendiumMenuLabelSize
      .define numCompendiumMenuLabelTiles newCompendiumMenuLabelSize/bytesPerTile
      
    initNewTitleScreen_ext:
      ; load new graphics
      ld b,numNewTitleGodzillaGrpTiles
      ld de,newTitleGodzillaGrp
      ld hl,$6020
      call sendRawTilesToVdp
      
      ld b,numCompendiumMenuLabelTiles
      ld de,newCompendiumMenuLabel
      ld hl,$4C00
      jp sendRawTilesToVdp
    
    sendNewTitleLogo_ext:
      ld bc,$1407
      ld de,newTitleGodzillaLogoTilemap
      ld hl,$7A4C
      jp $0212
  .ends

  ;=====
  ; use new tilemaps
  ;=====

  .bank $0F slot 2
  .org $008E
  .section "title screen logo tilemap 1" overwrite
    call sendNewTitleLogo
    jp $809A
  .ends

  .bank $0F slot 2
  .org $0B01
  .section "title screen logo tilemap 2" overwrite
    call sendNewTitleLogo
    jp $8B0D
  .ends

  .bank $01 slot 1
  .section "title screen logo tilemap 3" free
    sendNewTitleLogo:
      callExternal sendNewTitleLogo_ext
      ret
  .ends

  ;=====
  ; use new menus and fix mistakes in original text
  ;=====

  ; use new compendium menu label
  .bank $0F slot 2
  .org $3986
  .section "compendium menu label 1" overwrite
    .dw $0860,$0861,$0862,$0863,$0864,$0865,$0866,$0867,$0868
  .ends

  ; "GAME PLAY" -> "PLAY GAME"
  .bank $0F slot 2
  .org $3936
  .section "menu labels 1" overwrite
    .dw $0850,$084C,$0841,$0859,$0000,$0847,$0841,$084D,$0845
  .ends

  ; "OPTION" -> "OPTIONS"
  .bank $0F slot 2
  .org $396A
  .section "menu labels 2" overwrite
    .dw $0853
  .ends

  ; "DATA LOAD" -> "LOAD DATA"
  .bank $0F slot 2
  .org $3A26
  .section "menu labels 3" overwrite
    .dw $084C,$084F,$0841,$0844,$0000,$0844,$0841,$0854,$0841
  .ends

  ; options menu: "OPTION MODE" -> "OPTIONS MODE"
  .bank $10 slot 2
  .org $0303
  .section "menu labels 4" overwrite
    .db $53
  .ends
  
;========================================
; credits
;========================================

.bank $0B slot 2
.org $04B9
.section "credits 1a" overwrite
  call readNewCreditsChar
  nop
  nop
.ends

.bank $0B slot 2
.org $04D2
.section "credits 1b" overwrite
  call printNewCreditsString
  jp $84E2
.ends

.bank $01 slot 1
.section "credits 2" free
  readNewCreditsChar:
    push hl
      openTempBank :newCreditsList
        ; read character
        ld de,($D244)
        ld a,(de)
        ld l,a
      closeTempBank
      ld a,l
    pop hl
    ret
  
  printNewCreditsString:
    push hl
      openTempBank :newCreditsList
        ; send string to VDP
        -:
          ld a,(de)
          inc de
          out (c),a
          push af
          pop af
          ld a,($D246)
          out (c),a
          ld a,(de)
          cp $FE
          jr c,-
        ld l,a
      closeTempBank
      ld a,l
    pop hl
    ret
  
  initNewCredits:
    ; load graphics
    openTempBank :newCreditsFont
      ld b,newCreditsFontNumTiles
      ld de,newCreditsFont
      ld hl,$6800
      call sendRawTilesToVdp
    closeTempBank
    
    ; make up work: load pointer to new credits string list
    ld hl,newCreditsList
    ret
.ends

.slot 2
.org $008E
.section "credits 3" superfree
  newCreditsList:
    .incbin "out/script/credits.bin"
  
  newCreditsFont:
    .incbin "out/grp/font_credits.bin" fsize newCreditsFontSize
    .define newCreditsFontNumTiles newCreditsFontSize/bytesPerTile
.ends

.bank $0B slot 2
.org $01F1
.section "credits 4" overwrite
  call initNewCredits
.ends
  
;========================================
; "the end" tilemap
;========================================

.bank $0B slot 2
.org $0227
.section "the end 1" overwrite
  call showTheEnd
  jp $8233
.ends

.bank $01 slot 1
.section "the end 2" free
  showTheEnd:
    callExternal showTheEnd_ext
    ret
.ends

.slot 2
.section "the end 3" superfree
  theEndGrp:
    .incbin "out/grp/end_grp.bin" fsize theEndGrpSize
    .define theEndGrpNumTiles theEndGrpSize/bytesPerTile
  theEndMap:
    .incbin "out/maps/end.bin"

  showTheEnd_ext:
    ; load graphics
    ld b,theEndGrpNumTiles
    ld de,theEndGrp
    ld hl,$5A40
    call sendRawTilesToVdp
    
    ; send tilemap
    xor a
    ld bc,$0806
    ld de,theEndMap
    ld hl,$7A58
    jp sendHalfTilemapToAbsoluteVdpAddr
.ends

;========================================
; g-force newspaper
;========================================

.bank $0B slot 2
.org $0343
.section "newspaper 1" overwrite
  call showNewspaper
  jp $834F
.ends

.bank $01 slot 1
.section "newspaper 2" free
  showNewspaper:
    callExternal showNewspaper_ext
    ret
.ends

.slot 2
.section "newspaper 3" superfree
  newspaperGrp:
    .incbin "out/grp/newspaper_grp.bin" fsize newspaperGrpSize
    .define newspaperGrpNumTiles newspaperGrpSize/bytesPerTile
  newspaperMap:
    .incbin "out/maps/newspaper.bin"

  showNewspaper_ext:
    di
      ; load graphics
      ld b,newspaperGrpNumTiles
      ld de,newspaperGrp
      ld hl,$5800
      call sendRawTilesToVdp
      
      ; send tilemap
      ld bc,$1412
      ld de,newspaperMap
      ld hl,$78CC
      call sendTilemapToAbsoluteVdpAddr
    ei
    ret
.ends

;========================================
; g-force mission complete
;========================================

.bank $0B slot 2
.org $0254
.section "mission complete 1" overwrite
  call showMissionComplete
  jp $8275
.ends

.bank $01 slot 1
.section "mission complete 2" free
  showMissionComplete:
    callExternal showMissionComplete_ext
    ret
.ends

.slot 2
.section "mission complete 3" superfree
  missionCompleteMap_1:
    .incbin "out/script/missioncomplete_1.bin"
  missionCompleteMap_2:
    .incbin "out/script/missioncomplete_2.bin"

  showMissionComplete_ext:
    
    ld hl,missionCompleteMap_1
    ld de,$7B56
    call showMissionCompleteString
    
    ld hl,missionCompleteMap_2
    ld de,$7B96
    jp showMissionCompleteString
  
  showMissionCompleteString:
    ; HL = src
    ; DE = vdp dst
    
    ld b,10
    ex de,hl
    -:
      push bc
        push hl
        push de
          ld a,$09
          ld bc,$0101
          call sendHalfTilemapToAbsoluteVdpAddr
        
        ; wait frames
          ld b,$1E/4
          call $02AA
        pop de
        pop hl
        
        inc hl
        inc hl
        inc de
      pop bc
      djnz -
    ret
.ends
