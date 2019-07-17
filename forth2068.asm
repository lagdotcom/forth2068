; notes about the TS2068

; 48K of RAM
; 24K of ROM
; Color
; Sound: AY-3-8912
; twin joystick ports

CodeOrg equ $6000
CodeLen equ $2000

Zeus_PC = CodeOrg
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

chBS            equ 12
chNL            equ 13
chAT            equ 22

; notes on this forth

; link ptr: 2 bytes
; lenflags: 1 byte (len up to 0x1f)
; name: N bytes
; code pointer: 2 bytes (or N bytes if defined in asm)
; data field: N bytes
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

clearCarry macro()
        or a
mend

clearA  macro()
        xor a
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

link = 0
HEADER  macro(name,flags)
        dw link
        ::link = . -2
        db length(name)+flags
        db name
mend

defCode macro(name)
        HEADER(name,0)
        dw . + 2
mend

defCodeFl macro(name,flags)
        HEADER(name,flags)
        dw . + 2
mend

defWord macro(name)
        HEADER(name,0)
mend

defWordFl macro(name,flags)
        HEADER(name,flags)
mend

push16  macro(name)
        ld hl,name
        push hl
mend

pushAndGo macro(name)
        push16(name)
        jNEXT()
mend

lods    macro(reg)
        ld reg,(ix)
        inc ix
        inc ix
mend

lit     macro(n)
        dw _LIT-2, n
mend

setVar  macro(var, val)
        dw _LIT-2, val, var-2, _STORE-2
mend

postpone macro(n)
        lit(n)
        dw _COMMA-2
mend

postponeByte macro(n)
        lit(n)
        dw _CCOMMA-2
mend

; ok, let's get started!
org CodeOrg
Start:  ;di
        ld hl,(var_S0)
        ld sp,hl
        ld ix,cold_start
        jNEXT()

Next:
        lods(hl)
        ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        ex de,hl
        jp (hl)

DOCOLON ld bc,ix
        pushRSP(b,c)
        ld ix,de
        jNEXT()

rstack dw rstack_top
interp_lit db 0
parse_error db "PARSE ERROR",chNL
parse_error_len equ . - parse_error
hello_msg db "FORTH2068 v"
hello_msg_len equ . - hello_msg
bytes_msg db "B used;"
bytes_msg_len equ . - bytes_msg
ok_msg db " ok",chNL
ok_msg_len equ . - ok_msg
var_STATE dw 0
var_DP dw end_of_builtins
var_LATEST dw last_word
var_BASE dw 10
var_S0 dw stack_top
var_SRC dw 0
var_SRCID dw 0
var_SRCLEN dw 0
var_SRCIN dw 0

; buffer region
align $100

word_buffer_count db 0
word_buffer equ .
org . + 31

terminal_input_buffer equ .
org . + 80

parse_buffer equ .
org . + 80

; DROP ( x -- )
defCode("DROP")
_DROP   pop af
        jNEXT()

; SWAP ( x1 x2 -- x2 x1 )
defCode("SWAP")
_SWAP   pop af
        pop bc
        push af
        push bc
        jNEXT()

; DUP ( x -- x x )
defCode("DUP")
_DUP    pop af
        push af
        push af
        jNEXT()

; OVER ( x1 x2 -- x1 x2 x1 )
defCode("OVER")
_OVER   pop af
        pop bc
        push bc
        push af
        push bc
        jNEXT()

; ROT ( x1 x2 x3 -- x2 x3 x1 )
defCode("ROT")
_ROT    pop af
        pop bc
        pop de
        push bc
        push af
        push de
        jNEXT()

; -ROT ( x1 x2 x3 -- x3 x1 x2 )
defCode("-ROT")
_NROT   pop af
        pop bc
        pop de
        push af
        push de
        push bc
        jNEXT()

 ; 2DROP ( x1 x2 -- )
defCode("2DROP")
_2DROP  pop af
        pop af
        jNEXT()

; 2SWAP ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
defCode("2SWAP")
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
defCode("?DUP")
_QDUP   pop bc
        push bc
        ld a,b
        or c
        jr z,_QDUPx
        push bc
_QDUPx  jNEXT()

; 1+ ( x -- x+1 )
defCode("1+")
_INCR   pop bc
        inc bc
        push bc
        jNEXT()

; 1- ( x -- x-1 )
defCode("1-")
_DECR   pop bc
        dec bc
        push bc
        jNEXT()

; CELL+ ( x -- x+2 )
defCode("CELL+")
_CELLP  pop bc
        inc bc
        inc bc
        push bc
        jNEXT()

; CELL- ( x -- x-2 )
defCode("CELL-")
_CELLM  pop bc
        dec bc
        dec bc
        push bc
        jNEXT()

; 2* ( x -- x<<1 )
defCode("2*")
_SHIFTL pop hl
        sla l
        rl h
        push hl
        jNEXT()

; 2/ ( x -- x>>1 )
defCode("2/")
_SHIFTR pop hl
        sra h
        rr l
        push hl
        jNEXT()

; + ( x1 x2 -- x1+x2 )
defCode("+")
_ADD    pop hl
        pop bc
        add hl,bc
        push hl
        jNEXT()

; - ( x1 x2 -- x1-x2 )
defCode("-")
_SUB    pop de
        pop hl
        clearCarry()
        sbc hl,de
        push hl
        jNEXT()

; thanks to http://map.grauw.nl/articles/mult_div_shifts.php
; * ( x1 x2 -- x1*x2 )
defCode("*")
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
defCode("/MOD")
_DIVMOD pop de          ; divisor in de
        pop bc
        ld a,b          ; dividend in ac
        ld hl,0
        ld b,16
_DIVM_L sli c
        rla
        adc hl,hl
        sbc hl,de
        jr nc,_DIVM_J
        add hl,de
        dec c
_DIVM_J djnz _DIVM_L
        push hl         ; remainder
        ld b,a
        push bc         ; quotient
        jNEXT()

; thanks to http://z80-heaven.wikidot.com/optimization
; = ( x1 x2 -- x1=x2 )
defCode("=")
_EQ     pop bc
        pop hl
        ld de,0
        clearCarry()
        sbc hl,bc
        add hl,bc
        jr nz,_EQx
        dec de ; -1 = true
_EQx    push de
        jNEXT()

; <> ( x1 x2 -- x1!=x2 )
defCode("<>")
_NEQ    pop bc
        pop hl
        ld de,0
        clearCarry()
        sbc hl,bc
        add hl,bc
        jr z,_NEQx
        dec de ; -1 = true
_NEQx   push de
        jNEXT()

; < ( x1 x2 -- x1<x2 )
defCode("<")
_LT     pop bc
        pop hl
        ld de,0
        clearCarry()
        sbc hl,bc
        jr nc,_LTx
        dec de ; -1 = true
_LTx    push de
        jNEXT()

; > ( x1 x2 -- x1>x2 )
defCode(">")
_GT     pop hl
        pop bc
        ld de,0
        clearCarry()
        sbc hl,bc
        jr nc,_GTx
        dec de ; -1 = true
_GTx    push de
        jNEXT()

; <= ( x1 x2 -- x1<=x2 )
defCode("<=")
_LTE    pop hl
        pop bc
        ld de,0
        clearCarry()
        sbc hl,bc
        jr c,_LTEx
        dec de ; -1 = true
_LTEx   push de
        jNEXT()

; >= ( x1 x2 -- x1>=x2 )
defCode(">=")
_GTE    pop bc
        pop hl
        ld de,0
        clearCarry()
        sbc hl,bc
        jr c,_GTEx
        dec de ; -1 = true
_GTEx   push de
        jNEXT()

; 0= ( x -- flag )
defCode("0=")
_ZEQ    pop hl
        ld de,0
        ld a,h
        cp l
        jr nz,_ZEQx
        dec de ; -1 = true
_ZEQx   push de
        jNEXT()

; 0<> ( x -- flag )
defCode("0<>")
_ZNEQ   pop hl
        ld de,0
        ld a,h
        cp l
        jr z,_ZNEQx
        dec de ; -1 = true
_ZNEQx  push de
        jNEXT()

; AND ( x1 x2 -- x1&x2 )
defCode("AND")
_AND    pop bc
        pop de
        ld a,b
        and d
        ld d,a
        ld a,c
        and e
        ld e,a
        push de
        jNEXT()

; OR ( x1 x2 -- x1|x2 )
defCode("OR")
_OR     pop bc
        pop de
        ld a,b
        or d
        ld d,a
        ld a,c
        or e
        ld e,a
        push de
        jNEXT()

; XOR ( x1 x2 -- x1^x2 )
defCode("XOR")
_XOR    pop bc
        pop de
        ld a,b
        xor d
        ld d,a
        ld a,c
        xor e
        ld e,a
        push de
        jNEXT()

; INVERT ( x -- ~x )
defCode("INVERT")
_INVERT pop bc
        ld a,b
        cpl
        ld b,a
        ld a,c
        cpl
        ld c,a
        push bc
        jNEXT()

; EXIT ( R: return-addr -- )
defCode("EXIT")
_EXIT   popRSP(b,c)
        ld ix,bc
        jNEXT()

; LIT ( -- x )
defCode("LIT")
_LIT    lods(hl)
        push hl
        jNEXT()

; LITSTRING ( -- c-addr u )
defCode("LITSTRING")
_LITST  lods(de)
        push ix
        push de
        add ix,de
        jNEXT()

; ! ( x adr -- )
defCode("!")
_STORE  pop hl
        pop bc
        ld (hl),c
        inc hl
        ld (hl),b
        jNEXT()

; @ ( adr -- x )
defCode("@")
_FETCH  pop hl
        ld c,(hl)
        inc hl
        ld b,(hl)
        push bc
        jNEXT()

; +! ( x adr -- )
defCode("+!")
_ADDSTO pop hl
        pop bc
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
defCode("-!")
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
defCode("C!")
_CSTORE pop bc
        pop hl
        ld (hl),c
        jNEXT()

; C@ ( adr -- x )
defCode("C@")
_CFETCH pop hl
        ld b,0
        ld c,(hl)
        push bc
        jNEXT()

; C, ( x -- )
defCode("C,")
_CCOMMA pop bc
        ld hl,(var_DP)
        ld (hl),c
        inc hl
        ld (var_DP),hl
        jNEXT()

; CMOVE ( src dst len -- )
defCode("CMOVE")
_CMOVE  pop bc
        pop de
        pop hl
        ldir
        jNEXT()

defCode("STATE")
_STATE  pushAndGo(var_STATE)

defCode("DP")
_DP     pushAndGo(var_DP)

defWord("HERE")
cHERE   dw DOCOLON
        dw _DP-2, _FETCH-2      ; DP @
        dw _EXIT-2

defWord("SOURCE-ID")
cSRCID  dw DOCOLON
        dw _SRCID-2, _FETCH-2   ; SRC-ID @
        dw _EXIT-2

defCode("LATEST")
_LATEST pushAndGo(var_LATEST)

defCode("S0")
_S0     pushAndGo(var_S0)

defCode("BASE")
_BASE   pushAndGo(var_BASE)

defCodeFl("SRC", F_HIDDEN)
_SRC    pushAndGo(var_SRC)

defCodeFl("SRC-ID", F_HIDDEN)
_SRCID  pushAndGo(var_SRCID)

defCodeFl("SRC-LEN", F_HIDDEN)
_SRCLEN pushAndGo(var_SRCLEN)

defCode(">IN")
_SRCIN  pushAndGo(var_SRCIN)

defCode("VERSION")
_VER    pushAndGo(FORTHVER)

defCode("R0")
_R0     pushAndGo(rstack_top)

defCode("BL")
_BL     pushAndGo(' ')

defCodeFl("F_IMMED", F_HIDDEN)
_FIMM   pushAndGo(F_IMMED)

defCodeFl("F_HIDDEN", F_HIDDEN)
_FHID   pushAndGo(F_HIDDEN)

defCodeFl("F_LENMASK", F_HIDDEN)
_FLEN   pushAndGo(F_LENMASK)

; >R ( x -- ) ( R: -- x )
defCode(">R")
_TOR    pop bc
        pushRSP(b,c)
        jNEXT()

; R> ( -- x ) ( R: x -- )
defCode("R>")
_FROMR  popRSP(b,c)
        push bc
        jNEXT()

; R@ ( -- x ) ( R: x -- x )
defCode("R@")
_FETCHR ld hl,(rstack)
        ld c,(hl)
        inc hl
        ld b,(hl)
        push bc
        jNEXT()

; RSP@ ( -- x )
defCodeFl("RSP@", F_HIDDEN)
_RSPFET ld hl,(rstack)
        push hl
        jNEXT()

; RSP! ( x -- )
defCodeFl("RSP!", F_HIDDEN)
_RSPSTO pop hl
        ld (rstack),hl
        jNEXT()

; RDROP ( R: x -- )
defCode("RDROP")
_RDROP  ld hl,(rstack)
        inc hl
        inc hl
        ld (rstack),hl
        jNEXT()

; DSP@ ( -- x )
defCodeFl("DSP@", F_HIDDEN)
_DSPFET ld hl,0
        add hl,sp
        push hl
        jNEXT()

; DSP! ( x -- )
defCodeFl("DSP!", F_HIDDEN)
_DSPSTO pop hl
        ld sp,hl
        jNEXT()

; WARN: uses spectrum internals
;   not checked on TS2068
; EMIT ( x -- )
defCode("EMIT")
_EMIT   pop bc
        ld a,c
_EMITa  rst $10 ; SPECTRUM!
        jNEXT()

; SPACE ( -- )
defCode("SPACE")
_SPACE  ld a,' '
        jr _EMITa

; WARN: uses spectrum internals
;   not checked on TS2068
; KEY ( -- x )
defCode("KEY")
_KEY    call do_key
        push af
        jNEXT()
; puts hit key in a, trashes hl
do_key:
        ld hl,SpectrumLastKey
        ld (hl),0       ; clear sys var
_gk_l   ld a,(hl)
        cp 0
        jr z, _gk_l
_gk_x   ret

; WARN: uses spectrum internals
;   not checked on TS2068
; KEY? ( -- x | 0 )
defCode("KEY?")
_KEYQ   ld hl,SpectrumLastKey
        ld c,(hl)       ; get key (if any)
        ld (hl),0       ; clear last key
        ld b,0
        push bc
        jNEXT()

; WORD ( -- adr len )
defCode("WORD")
_WORD   call do_word
        push de
        push bc
        jNEXT()
; gets the next word in the input stream
; de = word
; bc = length
; trashes af and hl
do_word:
        ld hl,(var_SRC)
        ld bc,(var_SRCIN)
        clearCarry()
        adc hl,bc
        ld bc,0
_word_start:
        ld de,hl
        ld a,(hl)
        inc hl
        cp '\'
        jr z,_word_refill
        cp chNL
        jr z,_word_refill
        cp ' '
        jr z,_word_start
_word_main:
        ld a,(hl)
        inc hl
        inc bc
        cp ' '+1
        jr nc,_word_main
        ld hl,(var_SRCIN)
        clearCarry()
        adc hl,bc
        inc hl
        ld (var_SRCIN),hl
        dec hl
        ret
_word_refill:
        call do_refill
        jr do_word

; NUMBER ( adr len -- x result )
defCode("NUMBER")
_NUMBER pop bc ; len
        pop de ; addr
        call do_number
        push hl ; number
        push bc ; result
        jNEXT()
do_number:
        ld hl,0         ; hl = number
        ; if zero-length, return 0
        clearA()
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
defCode("FIND")
_FIND:  pop bc  ; length
        pop de  ; address
        call do_find
        push hl
        jNEXT()
do_find:
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
        clearCarry()
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

; >CFA ( xt -- x )
defCode(">CFA")
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

; >BODY ( xt -- x )
defWord(">BODY")
cTBODY  dw DOCOLON
        dw _TCFA-2
        dw _CELLP-2
        dw _EXIT-2

; write string into data area
; hl = source, bc = length
write_bytes:
        ld de,(var_DP)
        ldir
        ld (var_DP),de
        ret

; S, ( adr len -- )
defCode("S,")
_SCOMMA pop bc
        pop hl
        call write_bytes
        jNEXT()

; HEADER, ( adr len -- )
defCodeFl("HEADER,", F_HIDDEN)
_HEADERCOMMA:
        ld de,(var_DP)          ; de = HERE
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
        ld (var_DP),de
        jNEXT()

DOCREATE:
        ; at this point, DE is already pointing at the dfa!
        push de
        jNEXT()

; CREATE ( "<spaces>name" -- )
defWord("CREATE")
cCREATE dw DOCOLON
        dw _WORD-2                              ; WORD ( adr len -- )
        dw _HEADERCOMMA-2                       ; HEADER ( -- )
        postpone(DOCREATE)                      ; POSTPONE (CREATE)
        dw _EXIT-2

defWordFl("DOES>", F_IMMED)
cDOES   dw DOCOLON
        ; fix the CFA of the last defined word
        postpone(_LIT-2)                        ; POSTPONE LIT
        dw cHERE, _LIT-2, 12, _ADD-2, _COMMA-2  ; HERE 12 + ,
        postpone(_LATEST-2)                     ; POSTPONE LATEST
        postpone(_FETCH-2)                      ; POSTPONE @
        postpone(_TCFA-2)                       ; POSTPONE >CFA
        postpone(_STORE-2)                      ; POSTPONE !
        postpone(_EXIT-2)                       ; POSTPONE EXIT
        ; DOES> must set up the stack and call any remaining forth code
        postponeByte(0xD5)                      ; POSTPONE [push de]
        postponeByte(0x11)                      ; POSTPONE [ld de,XXXX]
        dw cHERE, _LIT-2, 5, _ADD-2, _COMMA-2   ; HERE 5 + ,
        postponeByte(0xC3)                      ; POSTPONE [jp]
        postpone(DOCOLON)                       ; POSTPONE DOCOLON
        dw _EXIT-2

; , ( x -- )
defCode(",")
_COMMA  pop bc
        call do_comma
        jNEXT()
do_comma:
        ld hl,(var_DP)
        ld (hl),c
        inc hl
        ld (hl),b
        inc hl
        ld (var_DP),hl
        ret

; [ ( -- )
defCodeFl("[", F_IMMED)
_LBRAC  clearA()
        ld (var_STATE),a
        jNEXT()

; ] ( -- )
defCode("]")
_RBRAC  ld a,1
        ld (var_STATE),a
        jNEXT()

; : ( C: "<spaces>name" -- )
defWord(":")
cCOLON  dw DOCOLON
        dw _WORD-2                              ; WORD
        dw _HEADERCOMMA-2                       ; HEADER,
        postpone(DOCOLON)                       ; POSTPONE DOCOLON
        dw _LATEST-2, _FETCH-2, _HIDDEN-2       ; LATEST @ HIDDEN
        dw _RBRAC-2                             ; ]
        dw _EXIT-2

; ; ( C: -- )
defWordFl(";", F_IMMED)
cSEMIC  dw DOCOLON
        postpone(_EXIT-2)                       ; POSTPONE EXIT
        dw _LATEST-2, _FETCH-2, _HIDDEN-2       ; LATEST @ HIDDEN
        dw _LBRAC-2                             ; [
        dw _EXIT-2

; IMMEDIATE ( -- ) \ make latest word immediate
defCodeFl("IMMEDIATE", F_IMMED)
_IMMED  ld hl,(var_LATEST)
        inc hl
        inc hl
        ld a,(hl)
        or F_IMMED
        ld (hl),a
        jNEXT()

; HIDDEN ( adr -- ) \ toggle hidden bit
defCode("HIDDEN")
_HIDDEN pop hl
        inc hl
        inc hl  ; 2 bytes for link
        ld a,(hl)
        xor F_HIDDEN
        ld (hl),a
        jNEXT()

; HIDE ( "<spaces>name" -- )
defWord("HIDE")
cHIDE   dw DOCOLON
        dw _WORD-2
        dw _FIND-2
        dw _HIDDEN-2
        dw _EXIT-2

; ' ( -- x )
defCode("'")
_TICK   lods(hl)
        push hl
        jNEXT()

; BRANCH ( -- )
defCode("BRANCH")
_BRANCH lods(de)
        add ix,de
        jNEXT()

; 0BRANCH ( flag -- )
defCode("0BRANCH")
_ZBRAN  pop bc
        ld a,b
        or c
        jr z,_BRANCH
        inc ix
        inc ix
        jNEXT()

; WARN: uses spectrum internal function
;   not checked on TS2068
; TYPE ( adr len -- )
defCode("TYPE")
_TYPE   pop bc
        pop de
        call SpectrumShowString
        jNEXT()

; EXECUTE ( xt -- )
defCode("EXECUTE")
_EXEC   pop hl
        ; TODO: shouldn't this affect RSP?
        jp (hl)

; WARN: uses spectrum internal function
;   not checked on TS2068
; REFILL ( -- flag )
defCode("REFILL")
_REFILL:
        call do_refill
        push de
        jNEXT()
; trashes everything
; de = flag
do_refill:
        ld hl,(var_SRCID)
        ld de,0
        ld a,h
        or l
        ret nz
_REFILL_console:
        ld hl,var_SRC
        ld (hl),terminal_input_buffer & $ff
        inc hl
        ld (hl),(terminal_input_buffer >> 8) & $ff
        ld hl,var_SRCIN
        ld (hl),0
        inc hl
        ld (hl),0
        ; grab input until NL is read
        ld bc,0
        ld de,terminal_input_buffer
_REFILL_loop:
        call do_key
        cp chNL
        jr z,_REFILL_loopend
        ; TODO: backspace
        push af
        rst $10 ; SPECTRUM
        pop af
        inc bc
        ld (de),a
        inc de
        jr _REFILL_loop
_REFILL_loopend:
        ld a,' '
        rst $10 ; SPECTRUM
        ld a,c
        or b
        jr z,do_refill ; do it again if we got nothing
        clearA()
        ld (de),a
        ld hl,var_SRCLEN
        ld (hl),c
        inc hl
        ld (hl),b
        ; return TRUE
        ld de,-1
        ret

; INTERPRET ( -- )
defCode("INTERPRET")
_INTERP call do_word
        clearA()
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
        call do_number ; hl = number, bc = error
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
defCode(".")
_NUMSH  pop bc
        call SpectrumShowNumber
        ld a,' '
        rst $10 ; SPECTRUM
        jNEXT()

; PARSE ( char "ccc<char>" -- c-addr u )
defCode("PARSE")
_PARSE: pop bc
        ld a,c  ; a = char
        ld hl,(var_SRC)
        ld bc,(var_SRCIN)
        clearCarry()
        adc hl,bc
        ld bc,0
        cpir
        ld a,c
        cpl
        ld c,a
        ld b,0
        ld de,parse_buffer
        push de
        push bc
        sbc hl,bc
        dec hl
        ldir
        ex de,hl
        ld hl,(var_SRC)
        ex de,hl
        sbc hl,de
        inc hl
        ld (var_SRCIN),hl
        jNEXT()

; .( ( "ccc<paren>" -- )
defWord(".(")
cDOTPAR dw DOCOLON
        lit(')')                                ; [ CHAR ) ]
        dw _PARSE-2, _TYPE-2                    ; PARSE TYPE
        dw _EXIT-2

; we manually write the header here because zeus doesn't like
; escaping the quote!
dw link
link = . - 2
db 2 + F_IMMED
db '.', '"'
; ." ( "ccc<quote>" -- )
cDOTQUO dw DOCOLON
        lit('"')                                ; [ CHAR " ]
        dw _PARSE-2                             ; PARSE
        postpone(_LITST-2)                      ; POSTPONE LITSTRING
        dw _DUP-2, _COMMA-2, _SCOMMA-2          ; DUP , S,
        postpone(_TYPE-2)                       ; POSTPONE TYPE
        dw _EXIT-2

dw link
link = . - 2
db 2 + F_IMMED
db 'S', '"'
; S" ( "ccc<quote>" -- c-addr len )
cSQUOTE dw DOCOLON
        lit('"')
        dw _PARSE-2
        dw _EXIT-2

; DEPTH ( -- x )
defCode("DEPTH")
_DEPTH  clearCarry()
        ld hl,(var_S0)
        sbc hl,sp
        srl h
        srl l   ; hl >>= 1
        push hl
        jNEXT()

; SAVE-INPUT ( -- input-spec )
defWord("SAVE-INPUT")
cSAVEIN dw DOCOLON
        dw _SRCLEN-2, _FETCH-2                  ; SRC-LEN @
        dw _SRC-2, _FETCH-2                     ; SRC @
        dw cSRCID                               ; SOURCE-ID
        dw _SRCIN-2, _FETCH-2                   ; >IN @
        dw _EXIT-2

; RESTORE-INPUT ( input-spec -- )
defWord("RESTORE-INPUT")
cRESTIN dw DOCOLON
        dw _SRCIN-2, _STORE-2                   ; >IN !
        dw _SRCID-2, _STORE-2                   ; SRC-ID !
        dw _SRC-2, _STORE-2                     ; SRC !
        dw _SRCLEN-2, _STORE-2                  ; SRC-LEN !
        dw _EXIT-2

defWordFl("INTERPRET-LOOP", F_HIDDEN)
cINLOOP dw DOCOLON
                                        ; BEGIN
        dw _INTERP-2                    ; INTERPRET
        dw _SRCLEN-2, _FETCH-2          ;   SRC-LEN @
        dw _SRCIN-2, _FETCH-2           ;   >IN @
        dw _LTE-2, _ZBRAN-2, -16        ; > WHILE
        dw _EXIT-2

; QUIT ( -- )
defWord("QUIT")
cQUIT   dw DOCOLON
        dw _R0-2, _RSPSTO-2             ; R0 RSP!
        setVar(_SRCID, 0)               ; 0 SRC-ID !
                                        ; BEGIN
        dw _REFILL-2, _DROP-2           ;   REFILL DROP
        dw cINLOOP                      ;   INTERPRET-LOOP
        lit(ok_msg)
        lit(ok_msg_len)
        dw _TYPE-2                      ;   .(  ok\n)
        dw _BRANCH-2, -20               ; AGAIN
        ; don't need EXIT

; EVALUATE ( * c-addr u -- * )
defWord("EVALUATE")
cEVAL   dw DOCOLON

        dw _SRCLEN-2, _FETCH-2, _TOR-2          ; SRC-LEN @ >R
        dw _SRC-2, _FETCH-2, _TOR-2             ; SRC @ >R
        dw cSRCID, _TOR-2                       ; SOURCE-ID >R
        dw _SRCIN-2, _FETCH-2, _TOR-2           ; >IN @ >R

        dw _SRCLEN-2, _STORE-2                  ; SRC-LEN !
        dw _SRC-2, _STORE-2                     ; SRC !
        setVar(_SRCIN, 0)                       ; 0 >IN !
        setVar(_SRCID, -1)                      ; -1 SRC-ID !
        dw cINLOOP                              ; INTERPRET-LOOP

        dw _FROMR-2, _SRCIN-2, _STORE-2         ; R> >IN !
        dw _FROMR-2, _SRCID-2, _STORE-2         ; R> SRC-ID !
        dw _FROMR-2, _SRC-2, _STORE-2           ; R> SRC !
        dw _FROMR-2, _SRCLEN-2, _STORE-2        ; R> SRC-LEN !

        dw _EXIT-2

; WARN: uses spectrum internal function
;   not checked on TS2068
; WORDS ( -- )
defCode("WORDS")
_WORDS  ld hl,(var_LATEST)
_words_loop:
        push hl
        inc hl
        inc hl  ; skip link for now
        ld b,0
        ld a,(hl)
        and F_HIDDEN
        jr nz,_words_skip
        ld a,(hl)
        and F_LENMASK
        ld c,a
        inc hl
        ld de,hl
        call SpectrumShowString ; print it out
        ld a,' '
        rst $10 ; SPECTRUM
_words_skip:
        pop hl
        ld e,(hl)
        ld a,e
        inc hl
        or (hl)
        jr z,_words_exit
        ld d,(hl)
        ld hl,de
        jr _words_loop
_words_exit:
        jNEXT()

; RAND ( -- x )
defCode("RAND")
_RAND   ld a,r
        push af ; pretty random :)
        jNEXT()

; WARN: uses spectrum internal function
;   not checked on TS2068
; AT ( y x -- )
defCode("AT")
_AT     ld a,chAT
        rst $10
        pop de
        ld a,e  ; x
        rst $10
        pop de
        ld a,e  ; y
        rst $10
        jNEXT()

; WARN: not checked on TS2068
; SETCOLOUR ( x -- )
defCode("COLOUR")
_SETCOL pop bc
        ld a,c
        ld (SpectrumScreenColour),a
        jNEXT()

; WARN: uses spectrum internal function
;   not checked on TS2068
; SETBORDER ( x -- )
defCode("BORDER")
_SETBOR pop bc
        ld a,c
        call SpectrumSetBorder
        jNEXT()

; WARN: uses spectrum internal function
;   not checked on TS2068
; CLS ( -- )
defCode("CLS")
_CLS    call SpectrumClearScreen
        jNEXT()

cold_start:
        lit(0)
        dw _SETBOR-2    ; 0 SETBORDER
        lit(7)
        dw _SETCOL-2    ; 7 SETCOLOUR
        dw _CLS-2       ; CLS
        lit(hello_msg)
        lit(hello_msg_len)
        dw _TYPE-2      ; .( FORTH2068 v)
        dw _VER-2
        dw _NUMSH-2     ; VERSION .
        dw _SPACE-2     ; SPACE
        lit(end_of_builtins - CodeOrg)
        dw _NUMSH-2     ; HERE &6000 - .
        lit(bytes_msg)
        lit(bytes_msg_len)
        dw _TYPE-2      ; .( bytes used;)
        lit(ok_msg)
        lit(ok_msg_len)
        dw _TYPE-2      ; .(  ok)
        dw cQUIT        ; QUIT

last_word equ link
end_of_builtins equ .
