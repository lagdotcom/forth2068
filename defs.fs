: / /MOD SWAP DROP ;
: MOD /MOD DROP ;

: 0< 0 < ;
: 0<= 0 <= ;
: 0> 0 > ;
: 0>= 0 >= ;

(
: 2>R SWAP >R >R ;
: 2R> R> R> SWAP ;
: 2R@ R> R> 2DUP >R >R SWAP ;
)

: 2! SWAP OVER ! CELL+ ! ;
: 2@ DUP CELL+ @ SWAP @ ;
: 2DUP OVER OVER ;

: ? @ . ;
: CHAR WORD DROP C@ ;
: SPACE BL EMIT ;

: POSTPONE WORD FIND , ;

: CONSTANT CREATE , DOES> @ ;
: VARIABLE CREATE 0 , ;
