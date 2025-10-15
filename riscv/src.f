\ NEEDUNIQUE ON
: \+    POSTPONE [DEFINED] 0= IF POSTPONE \ THEN ; IMMEDIATE
: \-    POSTPONE [DEFINED]    IF POSTPONE \ THEN ; IMMEDIATE

 ~mak/lib/fpc.f 

REQUIRE YDP><DP ~mak/lib/ydp.f

 REQUIRE (*  ~af/lib/comments.f 

\- U>= : U>= U< 0= ;

[IFNDEF] :-SET
 0 VALUE :-SET
: HERE DP @ DUP TO  :-SET  ;
[THEN]

\- CS-SWAP : CS-SWAP 2SWAP ;

\- AHEAD : AHEAD  HERE BRANCH, >MARK 2 ; IMMEDIATE

[IFNDEF] #HEADER
: #HEADER ( cfa -- )
\ F7_ED
  HERE >R
  DP ! HEADER
 R> DP ! ;
[THEN]


[IFNDEF] #define
: #define
  HEADER CONSTANT-CODE COMPILE, 0 PARSE EVALUATE ,
  LAST @ CURRENT @ ! ;
[THEN]

\- H. : H.  BASE M@ HEX SWAP U. BASE M!  ;

\- BOUNDS : BOUNDS OVER + SWAP ;

[IFNDEF]  REPLACE

: REPLACE ( by-xt what-xt -- )
\ by Day (Yakimov D.A.)
\ Example: ' NewSPACES ' SPACES REPLACE
  0xE9 OVER C!  \ JMP ...
  1+ DUP >R
  4 + -
  R> L!
;
[THEN]

[IFNDEF] OR!	:  OR! ( N ADDR -- )	DUP @ ROT OR	SWAP ! ; [THEN]
[IFNDEF] AND!	: AND! ( N ADDR -- )	DUP @ ROT AND	SWAP ! ; [THEN]
\- ANDC : ANDC INVERT AND ;

\+ ->DEFER REQUIRE <DBG> ~mak\lib\DBGWC.F

\- <\DBG> : <\DBG> ;
\- <DBG> : <DBG> ;
\- F7_ED : F7_ED ;
VECT MINST
REQUIRE INCLUDED_AL	riscv\Meta\mlist.f 

VARIABLE D-DP

VOCABULARY CM_VOC

CREATE SEG_DTCM
    0 ,  0 ,
   -1 ,  0 ,
    0 ,  0 ,   0 ,  0 ,   0 ,  0 ,   0 ,  0 ,     0 ,  0 ,   0 ,  0 ,   0 ,  0 ,   0 ,  0 , 
    0 ,  0 ,   0 ,  0 ,   0 ,  0 ,   0 ,  0 ,     0 ,  0 ,   0 ,  0 ,   0 ,  0 ,   0 ,  0 , 
    0 ,  0 ,   0 ,  0 ,   0 ,  0 ,   0 ,  0 ,     0 ,  0 ,   0 ,  0 ,   0 ,  0 ,   0 ,  0 , 
    0 ,  0 ,   0 ,  0 ,   0 ,  0 ,   0 ,  0 ,     0 ,  0 ,   0 ,  0 ,   0 ,  0 ,   0 ,  0 , 
HERE
  0 , 0 ,     
CONSTANT SEG_DTCM_END

SEG_DTCM VALUE SEG_DT
SEG_DTCM_END VALUE SEG_DT_END

0 VALUE MSTR_IMG
0 VALUE RAM_CPU1_IMG

\- 4+ : 4+ 4 + ;
: SEG-DT.
 BEGIN DUP  CELL+ CELL+ DUP H.  DUP 2@ OR
 WHILE DUP 2@ H. ."  " H.  CR
 REPEAT DROP
;

: SEG.
 MSTR_IMG DUP L@ H.  4+
 BEGIN DUP @ 
 WHILE CR DUP L@ H.
 DUP 4+ L@ DUP H.
 OVER 8 + L@  H.   KEY DROP
	2* + $C +
 REPEAT DROP
;

: SEG..
 MSTR_IMG DUP L@ H.  4+
 BEGIN DUP @ 
 WHILE

 CR
 DUP L@ H.
 DUP 4+ L@  H.
 DUP 8 + L@  H.
\ DUP 8 + L@ 0 ?DO DUP I +  $C + C@ H.  LOOP

 DUP 4+ L@

   KEY DROP
	2* + $C +
 REPEAT DROP
;

\- L+! : L+! ( N ADDR -- )	DUP L@ ROT + SWAP L! ;

0 VALUE COUNT_SEG

: COUNT_SEG_CLOSE
 COUNT_SEG
 IF
	HERE COUNT_SEG L@ -
        COUNT_SEG $C + L@ IF 1 OR 2/ THEN  \ for  1 bite cells \ mark of CM
	COUNT_SEG 4 + @ 8 + L+! \ size

	HERE COUNT_SEG L!

 THEN
;

: SEG_SET
 COUNT_SEG_CLOSE
 DUP TO COUNT_SEG
 L@ DP !
;

0 VALUE SEG_SAVE


REQUIRE T_@ ~mak\LIB\THERE\there.f 

\-  [F] : [F] ALSO FORTH ; IMMEDIATE
\-  [P] : [P]  PREVIOUS ; IMMEDIATE


\- L, : L, , ;
\- L! : L! ! ;
\- L@ : L@ @ ;


VARIABLE DP_CM

riscv\Meta\mhead0.f

: CELL DP @ THERE? IF 2 BREAK CELL ;

: ?.NAME>S      ( CFA -- )
\ ELIMINATE " 0x"
	DUP H.  \ 1 H.R>S SSPACE
\ DROP EXIT
	DUP 0xFFFF0000 AND 0x10000000 <> IF DROP BREAK
	NEAR_NFA 
	>R DUP
	IF ."  ( " DUP COUNT TYPE 
	     NAME> R> - DUP
	     IF   DUP ." +" NEGATE H. \ >S
	     THEN DROP        ."  ) "
	ELSE RDROP DROP
	THEN ;
: MLIT, ( W -> )
\+ 'DUP_V 'DUP_V
\- 'DUP_V ['] DUP
 COMPILE, \ INLINE,
   0x48 C,  0xB8 C,  , \ OPT  \ MOVABS RAX, #
;

VECT T-LIT,

' MLIT, TO T-LIT,

: TLIT, 
 DP M@
 THERE?
 IF  T-LIT, BREAK
 MLIT, ;

' TLIT,  ' LIT, REPLACE

: THERE?A  0x3fffff  U< ;
: THERE?B   DUP  0x1FFFC000 U>=
  OVER $7FFFF ANDC $200000 = OR ;

VARIABLE CS-ORG


REQUIRE TCCM-INTERPRET	riscv\Meta\tc.f 

\ <DBG> FLOAD riscv\define.4th <\DBG>
: CM_SET
 ONLY FORTH ALSO TCCM
 ALSO CM_VOC DEFINITIONS
SEG_DTCM TO SEG_DT
SEG_DTCM_END TO SEG_DT_END

 {{ TCCM ['] TC-LIT, }} TO T-LIT,
 ['] TCCM-INTERPRET  &INTERPRET M!
 {{ TCCM  MEM_MODE }}
 {{ TCCM ['] THERE?T }} TO THERE?
 {{ TCCM ['] AMINST }} TO MINST
 ;

[IFNDEF] W>S
 : W>S ( w -- n )  \ 
  0xFFFF AND    \ 
 0x8000 XOR 0x8000 - ;
[THEN]
\ eof

\+ DOS-LINES DOS-LINES

0 VALUE ROM-HERE

CASE-INS ON


 ' NOOP TO <PRE> 

\ <DBG> TEST.4 SSSS <\DBG>

 

