; notes about the TS2068

; 48K of RAM
; 24K of ROM
; Color
; Sound: AY-3-8912
; twin joystick ports

CodeOrg equ $6000
CodeLen equ $2000

Zeus_PC=CodeOrg
zeusemulate "48K"
output_bin "forth2068.bin",CodeOrg,CodeLen
;output_szx "forth2068.szx",CodeOrg,CodeOrg
;output_tzx "forth2068.tzx","forth2068","go forth and codify!",CodeOrg,CodeLen,0,CodeOrg

SpectrumClearScreen     equ $0daf
SpectrumShowNumber      equ $1a1b
SpectrumShowString      equ $203c
SpectrumSetBorder       equ $229b
SpectrumLastKey         equ $5c08
SpectrumScreenColour    equ $5c8d

BS              equ 12
NL              equ 13

; notes on this forth

; link ptr: 2 bytes
; lenflags: 1 byte (len up to 0x1f)
; name: N bytes
; code pointer: 2 bytes (or N bytes if defined in asm)
; definition: N bytes

; sp is the normal stack
; ix is the current instruction pointer

FORTHVER        equ 0
F_IMMED         equ 0x80
F_HIDDEN        equ 0x40
F_LENMASK       equ 0x1f

stack_top       equ 0xE000
rstack_top      equ 0xF000

jNEXT   macro()
        jp Next
mend

pushRSP macro(r1,r2)
        ld hl,(rstack)
        dec hl
        ld (hl),r1
        dec hl
        ld (hl),r2
        ld (rstack),hl
mend

popRSP  macro(r1,r2)
        ld hl,(rstack)
        ld r2,(hl)
        inc hl
        ld r1,(hl)
        inc hl
        ld (rstack),hl
mend

HEADER  macro(name,previous,flags)
        dw previous
        db length(name)+flags
        db name
mend

defCODE macro(name,previous)
        HEADER(name,previous,0)
        dw . + 2
mend

defIMMCODE macro(name,previous)
        HEADER(name,previous,F_IMMED)
        dw . + 2
mend

defWORD macro(name,previous)
        HEADER(name,previous,0)
mend

defIMMWORD macro(name,previous)
        HEADER(name,previous,F_IMMED)
mend

push16  macro(name)
        ld hl,name
        push hl
mend

pushAndGo macro(name)
        push16(name)
        jNEXT()
mend

lods    macro()
        ld hl,(ix)
        inc ix
        inc ix
mend

; ok, let's get started!
org CodeOrg
Start:  ;di
        ld hl,(var_S0)
        ld sp,hl
        ld ix,cold_start
        jNEXT()

Next:
        lods()
        ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        ex de,hl
        jp (hl)

Colon   ld bc,ix
        pushRSP(b,c)
        ld ix,de
        jNEXT()

rstack dw rstack_top
interp_lit db 0
parse_error db "PARSE ERROR",NL
parse_error_len equ . - parse_error
hello_msg db "FORTH2068 v"
hello_msg_len equ . - hello_msg
ok_msg db " ok",NL
ok_msg_len equ . - ok_msg
var_STATE dw 0
var_HERE dw end_of_builtins
var_LATEST dw last_word
var_BASE dw 10
var_S0 dw stack_top
var_ECHO dw 0

; this means that the start of the buffer will be 00 when in a 16-bit reg
align $100
word_buffer dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; DROP ( x -- )
pDROP   defCODE("DROP",0)
_DROP   pop af
        jNEXT()

; SWAP ( x1 x2 -- x2 x1 )
pSWAP   defCODE("SWAP",pDROP)
_SWAP   pop af
        pop bc
        push af
        push bc
        jNEXT()

; DUP ( x -- x x )
pDUP    defCODE("DUP",pSWAP)
_DUP    pop af
        push af
        push af
        jNEXT()

; OVER ( x1 x2 -- x1 x2 x1 )
pOVER   defCODE("OVER",pDUP)
_OVER   pop af
        pop bc
        push bc
        push af
        push bc
        jNEXT()

; ROT ( x1 x2 x3 -- x2 x3 x1 )
pROT    defCODE("ROT",pOVER)
_ROT    pop af
        pop bc
        pop de
        push bc
        push af
        push de
        jNEXT()

; -ROT ( x1 x2 x3 -- x1 x3 x2 )
pNROT   defCODE("-ROT",pROT)
_NROT   pop af
        pop bc
        pop de
        push af
        push de
        push bc
        jNEXT()

 ; 2DROP ( x1 x2 -- )
p2DROP  defCODE("2DROP",pNROT)
_2DROP  pop af
        pop af
        jNEXT()

; 2SWAP ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
p2SWAP  defCODE("2SWAP",p2DROP)
_2SWAP  pop af
        pop bc
        pop de
        pop hl
        push bc
        push af
        push hl
        push de
        jNEXT()

; ?DUP ( x -- x x | 0 )
pQDUP   defCODE("?DUP",p2SWAP)
_QDUP   pop bc
        push bc
        ld a,b
        or c
        jr z,_QDUPx
        push bc
_QDUPx  jNEXT()

; 1+ ( x -- x+1 )
pINCR   defCODE("1+",pQDUP)
_INCR   pop bc
        inc bc
        push bc
        jNEXT()

; 1- ( x -- x-1 )
pDECR   defCODE("1-",pINCR)
_DECR   pop bc
        dec bc
        push bc
        jNEXT()

; CELL+ ( x -- x+2 )
pCELLP  defCODE("CELL+",pDECR)
_CELLP  pop bc
        inc bc
        inc bc
        push bc
        jNEXT()

; CELL- ( x -- x-2 )
pCELLM  defCODE("CELL-",pCELLP)
_CELLM  pop bc
        dec bc
        dec bc
        push bc
        jNEXT()

; + ( x1 x2 -- x1+x2 )
pADD    defCODE("+",pCELLM)
_ADD    pop hl
        pop bc
        add hl,bc
        push hl
        jNEXT()

; - ( x1 x2 -- x1-x2 )
pSUB    defCODE("-",pADD)
_SUB    pop de
        pop hl
        or a ; clear carry
        sbc hl,de
        push hl
        jNEXT()

; thanks to http://map.grauw.nl/articles/mult_div_shifts.php
; * ( x1 x2 -- x1*x2 )
pMUL    defCODE("*",pSUB)
_MUL    pop bc
        pop de
        ld hl,0
        ld a,b
        ld b,16
_MUL_L  add hl,hl
        sla c
        rla
        jr nc, _MUL_NA
        add hl,de
_MUL_NA djnz _MUL_L
        push hl
        jNEXT()

; thanks to https://wikiti.brandonw.net/index.php?title=Z80_Routines:Math:Division
; /MOD ( x1 x2 -- x1%x2 x1/x2 )
pDIVMOD defCODE("/MOD",pMUL)
_DIVMOD pop de          ; divisor in de
        pop bc
        ld a,b          ; dividend in ac
        ld hl,0
        ld b,16
_DIVM_L:sli c
        rla
        adc hl,hl
        sbc hl,de
        jr nc,_DIVM_J
        add hl,de
        dec c
_DIVM_J:djnz _DIVM_L
        push hl         ; remainder
        ld b,a
        push bc         ; quotient
        jNEXT()

; thanks to http://z80-heaven.wikidot.com/optimization
; = ( x1 x2 -- x1=x2 )
pEQ     defCODE("=",pDIVMOD)
_EQ     pop bc
        pop hl
        ld de,0
        or a ; clear carry
        sbc hl,bc
        add hl,bc
        jr nz,_EQx
        dec de ; -1 = true
_EQx    push de
        jNEXT()

; <> ( x1 x2 -- x1!=x2 )
pNEQ    defCODE("<>",pEQ)
_NEQ    pop bc
        pop hl
        ld de,0
        or a ; clear carry
        sbc hl,bc
        add hl,bc
        jr z,_NEQx
        dec de ; -1 = true
_NEQx   push de
        jNEXT()

; < ( x1 x2 -- x1<x2 )
pLT     defCODE("<",pNEQ)
_LT     jNEXT() ; TODO

; > ( x1 x2 -- x1>x2 )
pGT     defCODE(">",pLT)
_GT     jNEXT() ; TODO

; <= ( x1 x2 -- x1<=x2 )
pLTE    defCODE("<=",pGT)
_LTE    jNEXT() ; TODO

; >= ( x1 x2 -- x1>=x2 )
pGTE    defCODE(">=",pLTE)
_GTE    jNEXT() ; TODO

; 0= ( x -- flag )
pZEQ    defCODE("0=",pGTE)
_ZEQ    pop hl
        ld de,0
        ld a,h
        cp l
        jr nz,_ZEQx
        dec de ; -1 = true
_ZEQx   push de
        jNEXT()

; 0<> ( x -- flag )
pZNEQ   defCODE("0=",pZEQ)
_ZNEQ   pop hl
        ld de,0
        ld a,h
        cp l
        jr z,_ZNEQx
        dec de ; -1 = true
_ZNEQx  push de
        jNEXT()

; 0< ( x -- flag )
pZLT    defCODE("0<",pZNEQ)
_ZLT    jNEXT() ; TODO

; 0> ( x -- flag )
pZGT    defCODE("0>",pZLT)
_ZGT    jNEXT() ; TODO

; 0<= ( x -- flag )
pZLTE   defCODE("0<=",pZGT)
_ZLTE   jNEXT() ; TODO

; 0>= ( x -- flag )
pZGTE   defCODE("0>=",pZLTE)
_ZGTE   jNEXT() ; TODO

; AND ( x1 x2 -- x1&x2 )
pAND    defCODE("AND",pZGTE)
_AND    jNEXT() ; TODO

; OR ( x1 x2 -- x1|x2 )
pOR     defCODE("OR",pAND)
_OR     jNEXT() ; TODO

; XOR ( x1 x2 -- x1^x2 )
pXOR    defCODE("XOR",pOR)
_XOR    jNEXT() ; TODO

; INVERT ( x -- ~x )
pINVERT defCODE("INVERT",pXOR)
_INVERT pop bc
        ld a,b
        cpl
        ld b,a
        ld a,c
        cpl
        ld c,a
        push bc
        jNEXT()

pEXIT   defCODE("EXIT",pINVERT)
_EXIT   popRSP(b,c)
        ld ix,bc
        jNEXT()

pLIT    defCODE("LIT",pEXIT)
_LIT    lods()
        push hl
        jNEXT()

; ! ( adr x -- )
pSTORE  defCODE("!",pLIT)
_STORE  pop bc
        pop hl
        ld (hl),c
        inc hl
        ld (hl),b
        jNEXT()

; @ ( adr -- x )
pFETCH  defCODE("@",pSTORE)
_FETCH  pop hl
        ld c,(hl)
        inc hl
        ld b,(hl)
        push bc
        jNEXT()

; +! ( adr x -- )
pADDSTO defCODE("+!",pFETCH)
_ADDSTO pop bc
        pop hl
        ld e,(hl)
        inc hl
        ld d,(hl)
        ld a,d
        add a,b
        ld (hl),a
        ld a,e
        adc a,c
        dec hl
        ld (hl),a
        jNEXT()

; -! ( adr x -- )
pSUBSTO defCODE("-!",pADDSTO)
_SUBSTO pop bc
        pop hl
        ld e,(hl)
        inc hl
        ld d,(hl)
        ld a,d
        sub a,b
        ld (hl),a
        ld a,e
        sbc a,c
        dec hl
        ld (hl),a
        jNEXT()

; C! ( adr x -- )
pCSTORE defCODE("C!",pSUBSTO)
_CSTORE pop bc
        pop hl
        ld (hl),c
        jNEXT()

; C@ ( adr -- x )
pCFETCH defCODE("C@",pCSTORE)
_CFETCH pop hl
        xor b
        ld c,(hl)
        push bc
        jNEXT()

; C@C! ( src dst -- src+1 dst+1 )
pCPMV   defCODE("C@C!",pCFETCH)
_CPMV   jNEXT() ; TODO

; CMOVE ( src dst len -- )
pCMOVE  defCODE("CMOVE",pCPMV)
_CMOV   jNEXT(); TODO

pSTATE  defCODE("STATE",pCMOVE)
_STATE  pushAndGo(var_STATE)

pHERE   defCODE("HERE",pSTATE)
_HERE   pushAndGo(var_HERE)

pLATEST defCODE("LATEST",pHERE)
_LATEST pushAndGo(var_LATEST)

pS0     defCODE("S0",pLATEST)
_S0     pushAndGo(var_S0)

pBASE   defCODE("BASE",pS0)
_BASE   pushAndGo(var_BASE)

pECHO   defCODE("ECHO",pBASE)
_ECHO   pushAndGo(var_ECHO)

pVER    defCODE("VERSION",pECHO)
_VER    pushAndGo(FORTHVER)

pR0     defCODE("R0",pVER)
_R0     pushAndGo(rstack_top)

pDOCOL  defCODE("DOCOL",pR0)
_DOCOL  pushAndGo(Colon)

pFIMM   defCODE("F_IMMED",pDOCOL)
_FIMM   pushAndGo(F_IMMED)

pFHID   defCODE("F_HIDDEN",pFIMM)
_FHID   pushAndGo(F_HIDDEN)

pFLEN   defCODE("F_LENMASK",pFHID)
_FLEN   pushAndGo(F_LENMASK)

; >R ( x -- ) ( R: -- x )
pTOR    defCODE(">R",pFLEN)
_TOR    pop bc
        pushRSP(b,c)
        jNEXT()

; R> ( -- x ) ( R: x -- )
pFROMR  defCODE("R>",pTOR)
_FROMR  popRSP(b,c)
        push bc
        jNEXT()

; RSP@ ( -- x )
pRSPFET defCODE("RSP@",pFROMR)
_RSPFET ld hl,(rstack)
        push hl
        jNEXT()

; RSP! ( x -- )
pRSPSTO defCODE("RSP!",pRSPFET)
_RSPSTO pop hl
        ld (rstack),hl
        jNEXT()

; RDROP ( R: x -- )
pRDROP  defCODE("RDROP",pRSPSTO)
_RDROP  ld hl,(rstack)
        inc hl
        inc hl
        ld (rstack),hl
        jNEXT()

; DSP@ ( -- x )
pDSPFET defCODE("DSP@",pRDROP)
_DSPFET ld hl,0
        add hl,sp
        push hl
        jNEXT()

; DSP! ( x -- )
pDSPSTO defCODE("DSP!",pDSPFET)
_DSPSTO pop hl
        ld sp,hl
        jNEXT()

; WARN: uses spectrum internals
;   not checked on TS2068
; KEY ( -- x )
pKEY    defCODE("KEY",pDSPSTO)
_KEY    call do_key
        push af
        jNEXT()
; puts hit key in a, trashes hl, echoes if var_ECHO is nonzero
do_key:
        push bc
        ld hl,SpectrumLastKey
        ld (hl),0       ; clear sys var
_gk_l   ld a,(hl)
        cp 0
        jr z, _gk_l
        ld b,a
        ld hl,(var_ECHO)
        ld a,h
        or l
        ld a,b
        jr z,_gk_x
        ; todo: avoid nonprintables?
        ; todo: backspace???
        rst $10 ; SPECTRUM!
        ld a,b
        pop bc
_gk_x   ret

; WORD ( -- adr len )
pWORD   defCODE("WORD",pKEY)
_WORD   call do_word
        push de
        push hl
        jNEXT()
do_word:
        call do_key
        cp '\'
        jr z,_skip_comment
        cp ' '
        jr z,do_word
        cp BS
        jr z,do_word
        cp NL
        jr z,do_word
        ld de,word_buffer
_main:
        ld (de),a
        inc de
        call do_key
        cp BS
        jr z,_bs
        cp ' '+1
        jr nc,_main
        ld hl,word_buffer
        or a; ccf
        ex de,hl
        sbc hl,de       ; hl = length
        jr z,do_word    ; got nothing? do it again
        ret
_skip_comment:
        call do_key
        cp NL
        jr nz,_skip_comment
        jr do_word
_bs:
        dec de
        jr z,do_word    ; this only works because word_buffer is XX00
        jr _main

; NUMBER ( adr len -- x result )
pNUMBER defCODE("NUMBER",pWORD)
_NUMBER pop bc ; len
        pop de ; addr
        call do_number
        push hl ; number
        push bc ; result
        jNEXT()
do_number:
        ld hl,0         ; hl = number
        ; if zero-length, return 0
        xor a
        add a,c
        jr z,_number_exit
        ; if it starts with -:
        push hl ; zero
        ld a,(de)
        inc de
        cp '-'
        jr nz,_number_convert
        ;   save this fact
        pop hl  ; remove old zero
        push af ; guaranteed to not be zero
        ;   if len is 1: go to end (string only has a minus in it)
        dec c
        jr nz,_number_read
        pop af
        ld bc,1
        ret
        ; loop:
_number_loop:
        push de
        ;   multiply accumulator by BASE
        ld a,(var_BASE)
        ld de,hl

        ; this block of code performs AHL = A*DE
        ld h,0
        ld l,h
        add a,a
        jr nc,.+4
        ld h,d
        ld l,e
        ld b,7
_number_mul:
        add hl,hl
        rla
        jr nc,.+4
        add hl,de
        adc a,0
        djnz _number_mul

        pop de
_number_read:
        ;   read next char
        ld a,(de)
        inc de
_number_convert:
        ;   take off '0'
        sub '0'
        ;   if that caused a carry: go to end
        jr c,_number_error
        ;   if < 10: go to digit
        cp 10
        jr c,_number_digit
        ;   take off 17 (distance from '0' to 'A'?)
        sub 17 ; check
        ;   if that caused a carry: go to end
        jr c,_number_error
        ;   add 10, go to digit
        add 10
        ;   digit:
_number_digit:
        ;     if value >= BASE: go to error
        push af
        ld a,(var_BASE)
        ld b,a
        pop af
        cp b
        jr nc,_number_error
        ;     add value to accumulator
        add l
        ld l,a
        jr nc,.+3
        inc h
        dec c
        jr nz,_number_loop
        ; end:
_number_done:
        pop de
        ld a,d
        or e
        ;   if negative flag was set: negate accumulator
        jr z,_number_exit
        ld a,h
        cpl
        ld h,a
        ld a,l
        neg
        ld l,a
        jr _number_exit
_number_error:
        pop de
_number_exit:
        ;   make sure HL is the number and BC is the result/error field
        ld b,0
        ret

; FIND ( adr len -- x )
pFIND   defCODE("FIND",pNUMBER)
_FIND:  pop hl  ; length
        pop de  ; address
        call do_find
        push hl
        jNEXT()
do_find:
        ld bc,hl        ; bc = length
        ld hl,(var_LATEST)
_fmain: ld a,h
        or l
        jr z,_not_found
        push hl
        ; compare string length
        inc hl
        inc hl  ; skip link field
        ld a,(hl)
        and a, F_HIDDEN|F_LENMASK
        cp c
        jr nz,_next
        ; compare string content
        inc hl
        push de
        push bc
_loop:
        ; compare (de) and (hl) up to bc chars
        ld a,(de)
        cpi
        jr nz,_nextp
        ld a,b
        or c
        jr z,_found
        inc de
        jp _loop
_found:
        pop bc
        pop de
        pop hl
        or a ; ccf
        ret
_nextp: pop bc
        pop de
_next:  pop hl
        push de
        ld e,(hl)
        inc hl
        ld d,(hl)
        ld hl,de
        pop de
        jr _fmain
_not_found:
        ; ld hl,0
        scf
        ret

; >CFA ( x -- x )
pTCFA   defCODE(">CFA",pFIND)
_TCFA   pop hl
        call do_tcfa
        push hl
        jNEXT()
do_tcfa:
        inc hl
        inc hl          ; skip link field
        ld a,(hl)
        inc hl
        and a,F_LENMASK ; get length only
        ld b,0
        ld c,a
        add hl,bc       ; skip name
        ret

; >DFA ( x -- x )
pTDFA   defWORD(">CFA",pTCFA)
cTDFA   dw Colon
        dw _TCFA-2
        dw _CELLP-2
        dw _EXIT-2

; CREATE ( adr len -- )
pCREATE defCODE("CREATE",pTDFA)
_CREATE ld de,(var_HERE)        ; de = HERE
        push de
        ld hl,var_LATEST        ; hl = LATEST
        ldi                     ; (de++) = (hl++)
        ldi                     ; (de++) = (hl++)
        pop hl                  ; hl = HERE
        ld (var_LATEST),hl      ; LATEST = hl
        pop bc                  ; c = length
        ld a,c
        ld (de),a
        inc de
        pop hl
        ldir
        ld (var_HERE),de
        jNEXT()

; , ( x -- )
pCOMMA  defCODE(",",pCREATE)
_COMMA  pop bc
        call do_comma
        jNEXT()
do_comma:
        ld hl,(var_HERE)
        ld (hl),c
        inc hl
        ld (hl),b
        inc hl
        ld (var_HERE),hl
        ret

; [ ( -- )
pLBRAC  defIMMCODE("[",pCOMMA)
_LBRAC  xor a
        ld (var_STATE),a
        jNEXT()

; ] ( -- )
pRBRAC  defCODE("]",pLBRAC)
_RBRAC  ld a,1
        ld (var_STATE),a
        jNEXT()

; : ( C: "<spaces>name" -- )
lCOLON  defWORD(":",pRBRAC)
cCOLON  dw Colon
        dw _WORD-2
        dw _CREATE-2
        dw _LIT-2, Colon, _COMMA-2
        dw _LATEST-2, _FETCH-2, _HIDDEN-2
        dw _RBRAC-2
        dw _EXIT-2

; ; ( C: -- )
pSEMIC  defIMMWORD(";",lCOLON)
cSEMIC  dw Colon
        dw _LIT-2, _EXIT-2, _COMMA-2
        dw _LATEST-2, _FETCH-2, _HIDDEN-2
        dw _LBRAC-2
        dw _EXIT-2

; IMMEDIATE ( -- ) \ make latest word immediate
pIMMED  defIMMCODE("IMMEDIATE",pSEMIC)
_IMMED  ld hl,(var_LATEST)
        inc hl
        inc hl
        ld a,(hl)
        or F_IMMED
        ld (hl),a
        jNEXT()

; HIDDEN ( adr -- ) \ toggle hidden bit
pHIDDEN defCODE("HIDDEN",pIMMED)
_HIDDEN pop hl
        inc hl
        inc hl  ; 2 bytes for link
        ld a,(hl)
        xor F_HIDDEN
        ld (hl),a
        jNEXT()

pHIDE   defWORD("HIDE",pHIDDEN)
cHIDE   dw Colon
        dw _WORD-2
        dw _FIND-2
        dw _HIDDEN-2
        dw _EXIT-2

; ' ( -- x )
pTICK   defCODE("'",pHIDE)
_TICK   lods()
        push hl
        jNEXT()

; BRANCH ( -- )
pBRANCH defCODE("BRANCH",pTICK)
_BRANCH lods()
        ld de,hl
        add ix,de
        jNEXT()

; 0BRANCH ( flag -- )
pZBRAN  defCODE("0BRANCH",pBRANCH)
_ZBRAN  pop af
        cp 0
        jr z,_BRANCH
        inc ix
        inc ix
        jNEXT()

; LITSTRING ( -- adr len )
pLITSTR defCODE("LITSTRING",pZBRAN)
_LITSTR jNEXT() ; todo

; WARN: uses spectrum internal function
;   not checked on TS2068
; TELL ( adr len -- )
pTELL   defCODE("TELL",pLITSTR)
_TELL   pop bc
        pop de
        call SpectrumShowString
        jNEXT()

; QUIT ( -- )
pQUIT   defWORD("QUIT",pTELL)
cQUIT   dw Colon
        dw _R0-2, _RSPSTO-2             ; R0 RSP!
        dw _ECHO-2, _LIT-2, 1, _STORE-2 ; ECHO 1 !
        dw _INTERP-2                    ; DO INTERPRET
        dw _BRANCH-2, -6                ; LOOP
        ; don't need NEXT

; CHAR ( -- x )
pCHAR   defWORD("CHAR",pQUIT)
_CHAR   jNEXT() ; todo

; EXECUTE ( xt -- )
pEXEC   defCODE("EXECUTE",pCHAR)
_EXEC   pop hl
        jp (hl)

; INTERPRET ( -- )
pINTERP defCODE("INTERPRET",pEXEC)
_INTERP call do_word
        xor a
        ld (interp_lit),a
        call do_find ; hl = pointer
        jr c,_literal
        ; we have a word
        push hl
        inc hl
        inc hl  ; skip link field
        ld d,(hl)
        pop hl
        call do_tcfa ; hl = CFA
        ld a,d
        and a,F_IMMED
        jr nz,_execute
        jr _checkstate
_literal:
        ld a,1
        ld (interp_lit),a
        call do_number ; de = number, bc = error
        ld a,b
        or c
        jr nz,_error
        ld bc,hl
        ld hl, _LIT-2
_checkstate:
        ex de,hl
        ld hl,var_STATE
        ld a,(hl)
        inc hl
        or a,(hl)
        ex de,hl
        jr z,_execute
        ; compile then
        push bc
        ld bc,hl
        call do_comma   ; compile BC
        pop bc
        ld a,(interp_lit)
        cp 0
        jr z,_no_lit
        ; compile LIT operand
        call do_comma
_no_lit jNEXT()
_execute:
        ld a,(interp_lit)
        cp 0
        jr nz,_do_lit
        ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        ex de,hl
        jp (hl)
_do_lit:
        push bc
        jNEXT()
_error:
        ; WARN: spectrum function
        ld de,parse_error
        ld bc,parse_error_len
        call SpectrumShowString
        jNEXT()

; WARN: uses spectrum internal function
;   not checked on TS2068
;   also doesn't obey BASE
; . ( x -- )
pNUMSH  defCODE(".",pINTERP)
_NUMSH  pop bc
        call SpectrumShowNumber
        jNEXT()

; WARN: uses spectrum internal function
;   not checked on TS2068
; WORDS ( -- )
pWORDS  defCODE("WORDS",pNUMSH)
_WORDS  ld hl,(var_LATEST)
_wloop: push hl
        inc hl
        inc hl  ; skip link for now
        ld b,0
        ld a,(hl)
        and F_LENMASK
        ld c,a
        inc hl
        ld de,hl
        call SpectrumShowString ; print it out
        pop hl
        ld e,(hl)
        ld a,e
        inc hl
        ld d,(hl)
        or d
        jr z,_wexit
        ld hl,de
        ld a,' '
        rst $10 ; SPECTRUM
        jr _wloop
_wexit: jNEXT()

; WARN: not checked on TS2068
; SETCOLOUR ( x -- )
pSETCOL defCODE("COLOUR",pWORDS)
_SETCOL pop bc
        ld a,c
        ld (SpectrumScreenColour),a
        jNEXT()

; WARN: uses spectrum internal function
;   not checked on TS2068
; SETBORDER ( x -- )
pSETBOR defCODE("BORDER",pSETCOL)
_SETBOR pop bc
        ld a,c
        call SpectrumSetBorder
        jNEXT()

; WARN: uses spectrum internal function
;   not checked on TS2068
; CLS ( -- )
pCLS    defCODE("CLS",pSETBOR)
_CLS    call SpectrumClearScreen
        jNEXT()

cold_start:
        dw _LIT-2, 0
        dw _SETBOR-2    ; 0 SETBORDER
        dw _LIT-2, 7
        dw _SETCOL-2    ; 7 SETCOLOUR
        dw _CLS-2       ; CLS
        dw _LIT-2, hello_msg
        dw _LIT-2, hello_msg_len
        dw _TELL-2      ; .( FORTH2068 v)
        dw _VER-2
        dw _NUMSH-2     ; VERSION .
        dw _LIT-2, ok_msg
        dw _LIT-2, ok_msg_len
        dw _TELL-2      ; .(  ok)
        dw cQUIT        ; QUIT

last_word equ pCLS
end_of_builtins equ .
