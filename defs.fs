(
: 2>R SWAP >R >R ;
: 2R> R> R> SWAP ;
: 2R@ R> R> 2DUP >R >R SWAP ;
)

: / /MOD SWAP DROP ;
: 0< 0 < ;
: 0<= 0 <= ;
: 0> 0 > ;
: 0>= 0 >= ;
: 2! SWAP OVER ! CELL+ ! ;
: 2@ DUP CELL+ @ SWAP @ ;
: 2DUP OVER OVER ;
: ? @ . ;
: ALIGN HERE 1 AND IF 1 DP +! THEN ;
: ALIGNED DUP 1 AND IF 1+ THEN ;
: ALLOT HERE + DP ! ;
: CELLS 2 * ;
: CHAR WORD DROP C@ ;
: CHAR+ 1 + ;
: CONSTANT CREATE , DOES> @ ;
: CR 13 EMIT ;
: DECIMAL 10 BASE ! ;
: MAX 2DUP - 0> IF SWAP THEN DROP ;
: MIN 2DUP - 0< IF SWAP THEN DROP ;
: MOD /MOD DROP ;
: MOVE 2 * CMOVE ;
: POSTPONE WORD FIND , ;
: RECURSE LATEST @ , ;
: SPACE BL EMIT ;
: VARIABLE CREATE 0 , ;

: ACTION-OF ( "<name>" -- xt )
	STATE @ IF
		POSTPONE ['] POSTPONE DEFER@
	ELSE
		' DEFER@
	THEN
; IMMEDIATE

: BUFFER: ( u "<name>" -- )
	CREATE ALLOT
;

: COMPILE, , ;

: DEFER ( "<name"> -- )
	CREATE ['] ABORT ,
	DOES> @ EXECUTE
;

: DEFER! ( xt defer -- )
	>BODY !
;

: DEFER@ ( defer -- xt )
	>BODY @
;

: FALSE 1 0= ;
: HEX 16 BASE ! ;

: IS
	STATE @ IF
		POSTPONE ['] POSTPONE DEFER!
	ELSE
		' DEFER!
	THEN
; IMMEDIATE

\ TODO: not sure this is correct
: MARKER ( "<spaces>name" -- )
	CREATE HERE , LATEST @ ,
	DOES> DUP @ DP ! CELL+ LATEST !
;

: NIP SWAP DROP ;
: TRUE 0 0= ;
: TUCK SWAP OVER ;
