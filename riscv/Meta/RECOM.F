
WARNING @ WARNING 0!

\ : READ-FILE 2>R DUP THERE? IF 2R> TREAD-FILE BREAK 2R> READ-FILE ;

: ALLOT
 DP M@ THERE? SWAP
 ALLOT
 DP M@ THERE? <> ABORT" memory out"
 ;


: L, ( x -- )
  DP M@ 4 ALLOT L! ;

: I, L, ;

: , ( x -- ) \ 94
\ 
  DP M@  THERE? IF L, BREAK
  ,
 ;

: C, ( char -- ) \ 94
\ 
  DP M@ 1 ALLOT C!
 ;


: W, ( word -- )
\ 
  DP M@  2 ALLOT W!
 ;

: +! ( N ADDR -- )	DUP @ ROT + SWAP ! ;

: 1+! ( ADDR -- )	1 SWAP +! ;
[IFNDEF] R.0
: R.0
  >R 0 <# #S #> R> OVER - 0 MAX 
    BEGIN  DUP 0>
    WHILE 1- [CHAR] 0 EMIT
    REPEAT DROP
    TYPE 
;
[THEN]

: PTYPE
  0 DO COUNT >PRT EMIT LOOP DROP
;

: TPTYPE
  0 DO DUP C@ >PRT EMIT
 1+ LOOP DROP
;

: MDUMP ( addr u -- ) DUMP ;

: TDUMP ( addr u -- ) \ 94 TOOLS
  DUP 0= IF 2DROP EXIT THEN
  BASE M@ >R HEX
  15 + 16 U/ 0 DO
    CR DUP 4 R.0 SPACE
    SPACE DUP 16 0
      DO I 4 MOD 0= IF SPACE THEN
        DUP
        C@
        2 R.0 SPACE 1+
      LOOP SWAP 16  TPTYPE
  LOOP DROP R> BASE M!
;

: DUMP TDUMP ;

: MCOUNT COUNT ;
: COUNT DUP 1+ SWAP C@ ;

WARNING !

