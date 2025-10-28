
: B, C, ; : B@ C@ ; : B! C! ; : /CHAR 1 ;
0xf0000 ALLOCATE THROW DUP  YDP0 ! YDP !
: // POSTPONE \ ; IMMEDIATE
\- VECT :  VECT DEFER ;
\- DEFER : DEFER VECT ;

\ VARIABLE VLOOPTST : +LOOPTST 44 0 DO CR  I H.  VLOOPTST I + H. VLOOPTST @ .  1 +LOOP ;

REQUIRE CASE-INS lib\ext\caseins.f
REQUIRE CASE-INS lib\ext\caseins.f
REQUIRE [IF] ~mak\CompIF3.f 
REQUIRE [IFNDEF] ~nn\lib\ifdef.f
REQUIRE #define ~af/lib/c/define.f 
REQUIRE FLC> ~mak\fl.f 
0 VALUE T-STDOUT


\ REQUIRE DBG_INCLUDED lib\include\spf_navigator.f 

REQUIRE EDIT ~mak\Edit.4

REQUIRE <DBG> ~mak\lib\DBGWC.F

0
[IF]
: RRR\  ; IMMEDIATE
[ELSE]
: RRR\  POSTPONE \  ; IMMEDIATE
[THEN]

0 VALUE CPU2BR
0 VALUE CM_BR
<DBG>
REQUIRE #UNTIL ~mak\lib\mpass.f
riscv\src.f
 pROMRAM.4


TSAVE forth.elf

.( TSAVE OK ) MCR
\ RSTAT
 BYE

