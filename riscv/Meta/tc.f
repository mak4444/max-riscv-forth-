
REQUIRE 'DOVALUE riscv\Meta\forward.f
[IFNDEF] #HEADER
: #HEADER ( cfa -- )
  HERE >R
  DP M! HEADER
 R> DP M! ;
[THEN]

<DBG>

: >SEG_DT ( madr tadr -- )
 SEG_DT_END @ ABORT" descriptor table is full"
 SEG_DT
 BEGIN  CELL+ CELL+  2DUP @ U<
 UNTIL  \  madr tadr dtadr
 DUP CELL+ @
  IF ." conflict with $" CELL- CELL- @ h. ." seg" ABORT
  THEN
 DUP
 DUP CELL+ CELL+ SEG_DT_END OVER - CMOVE>
 >R SWAP OVER - SWAP
 R> 2!
;

: T> ( tadr -- madr )
  SEG_DT
 BEGIN CELL+ CELL+ 2DUP @  U<
 UNTIL 
  CELL+ @ + 
;

riscv\Meta\mhead.f

REQUIRE RISCV_MOD riscv\Meta\riscv32.4th
riscv\Meta\LEX.F
riscv\Meta\prefix.f

MODULE: TCCM

: M_CMOVE ( c-addr1 c-addr2 u --- )
\ Copy u bytes starting at c-addr1 to c-addr2, proceeding in ascending
\ order.

   DUP IF  >R
   BEGIN
    OVER 
 MC@
 SWAP DUP >R
 C!
 R> 1+ SWAP 1+ SWAP
    R>  1- DUP  >R 0=
   UNTIL
   R>
   THEN
   2DROP DROP
;

REQUIRE DISARM	riscv\Meta\dist2.f

: [RISCV] ALSO RISCV_MOD ; IMMEDIATE

[IFNDEF] OR!	:  OR! ( N ADDR -- )	DUP @ ROT OR	SWAP ! ; [THEN]
[IFNDEF] AND!	: AND! ( N ADDR -- )	DUP @ ROT AND	SWAP ! ; [THEN]


: CMOVE ( c-addr1 c-addr2 u --- )
\ Copy u bytes starting at c-addr1 to c-addr2, proceeding in ascending
\ order.

   DUP IF  >R
   BEGIN
    OVER 
\ ." C=" DUP H.
 C@
 SWAP DUP >R
 C!
 R> 1+ SWAP 1+ SWAP
    R>  1- DUP  >R 0=
   UNTIL
   R>
   THEN
   2DROP DROP
;

: M_CMOVE ( c-addr1 c-addr2 u --- )
\ Copy u bytes starting at c-addr1 to c-addr2, proceeding in ascending
\ order.

   DUP IF  >R
   BEGIN
    OVER 
 MC@
 SWAP DUP >R
 C!
 R> 1+ SWAP 1+ SWAP
    R>  1- DUP  >R 0=
   UNTIL
   R>
   THEN
   2DROP DROP
;

: M\ ( POSTPONE \) ; IMMEDIATE
\ REQUIRE TINLINE? riscv\Meta\macroopt.4


: ><DP DP M@ T-DP M@
       DP M! T-DP M! ;

: DUP, 
	$1471	W, \  	addi	s0,s0,-4
	$c008	W, \  	sw	a0,0(s0)
;

: DROP, 
	$4008	W, \	lw	a0,(s0)
	$0411	W, \	s0,s0,4
;

: EXIT,
	$4082 W,	\	lw	ra,0(sp)
	$0111 W,	\	addi	sp,sp,4
	$8082 W,	\	ret
;
	   
: TC-LIT, ( n -- )
\ compile code for a literal
	DUP,  [RISCV] a0 SWAP li, [P]
;

: ALIT, ( n -- )
\ compile code for a literal
	DUP,  [RISCV] a0 SWAP la, [P]
;

	   
: S_COMPILE, [RISCV]  CALL, [P] ;

: T_COMPILE,
   S_COMPILE,

;

: TCOMPILE, ( tcfa -- )  
  DUP THERE? 0=
  IF  ?CONST IF EXECUTE TC-LIT, BREAK
      -9 THROW
  THEN

\	TINLINE? IF  TINLINE, BREAK
	T_COMPILE,
 ;
 

:  ?CONST ( cfa -- cfa flag )
 DUP C@ $E8 <> IF DROP 0 BREAK
  DUP 1+  REL@ 4 +
\- _QCONSTANT-CODE CONSTANT-CODE  =
\+ _QCONSTANT-CODE DUP CONSTANT-CODE  =
\+ _QCONSTANT-CODE SWAP ['] _CONSTANT-CODE  =  OR
 ;

: COMPILE,  
 DP @
 THERE?
 IF
 TCOMPILE,
 EXIT THEN
 COMPILE, ;
	   
: [COMPILE] '  COMPILE,  ; IMMEDIATE
	   
: #define  HERE THERE? IF ><DP #define ><DP BREAK #define ;
: ?#define >IN @  POSTPONE [DEFINED]  IF DROP POSTPONE \ BREAK >IN ! #define  ;

[IFDEF] TEXEC_BUF
: EXEC_BUF_SET
	>IN M@ >R
	PARSE-NAME
 SFIND

	IF
			TEXEC_BUF TEXEC_BUF CELL+ TEB_SIZE MOVE
			TEXEC_KEY TEXEC_KEY CELL+ TEB_SIZE MOVE
			TEXEC_BUF M!
		HERE	TEXEC_KEY M!
	ELSE	2DROP
	THEN

	R> >IN M!
;
[ELSE]
: EXEC_BUF_SET ;
[THEN]


4 CONSTANT TCELL
: TCELLS 2 << ;
: TCELL+ TCELL + ;

: IALLOT ALLOT ;

: TCONSTANT ( n -- ) 	CON:THS
 ><DP CONSTANT   ><DP ;

: CONSTANT HERE THERE? IF TCONSTANT BREAK CONSTANT ;

 VARIABLE TLAST ( --- addr)

: MCREATE CREATE  ;
: ICREATE HERE TCONSTANT ;
: CREATE ICREATE  ;

: ALLOT  D-DP +! ;
: IHERE  HERE ;
: DHERE  D-DP @ ;

: IVARIABLE ICREATE 0 L, ;

: DCREATE  DHERE TCONSTANT ;

: VARIABLE DCREATE TCELL ALLOT ;

: VALUE 
   DP M@ THERE? 0= IF  VALUE   BREAK
 :#THS
	HEADER
\  [RISCV] a1 'DOVALUE jal, [P]
  [RISCV] a1 'DOVALUE $6F J-TYPE [P]
 L,
;  

: ->DEFER
 :#THS
  EXEC_BUF_SET
	HEADER 
\  [RISCV] a1 'DOVECT jal, [P]
  [RISCV] a1 'DOVECT $6F J-TYPE [P]
 L,
;  

: $, ( addr u -- ) \ 
 DP M@ SWAP DUP IALLOT
   M_CMOVE ;

: >SEG_DT_H    ( len segRUN -- )
  DUP DUP >SEG_DT SWAP \ 2*
  +  HERE SWAP >SEG_DT
;

: CM_SEG2: ( len segLD segRUN  -- )
\  HERE $C + OVER >SEG_DT
  HERE OVER ><DP MCREATE  ML,  M, 1 ML, ><DP \ len segLD segRUN
  SWAP ML,	 \ len segRUN
  OVER 2/ ML, 0 ML,
  OVER IALLOT \ len segRUN
\  SWAP 2/ SWAP
  >SEG_DT_H
 ; 

: SEG2: ( len segLD segRUN  -- ) CM_SEG2: ;

: CM_SEG: ( len seg -- )  DUP  CM_SEG2: ;

: SEG: ( len seg -- )  DUP  CM_SEG2: ;

: SEGINFO
  DUP  ."  LAST ADR="  L@ MH. CR
\  DUP  ."  LAST ="  4+ CELL+  L@ MH. CR
    4+ @
  DUP  ." LOAD ADR =" L@ MH. CR
  DUP  ." MAX SIZE =" 4+ L@ MH. CR
  DUP  ." CUR SIZE =" 4+ 4+ L@ MH. CR

  DROP
; 

: THERE?T DUP T> SWAP <> ;

: ?COMP_ STATE M@ =
	IF  COMPILE,
	ELSE EXECUTE
	THEN ;

: ?OLD
  S"  DP M@ THERE? 0= IF" EVALUATE
  POSTPONE POSTPONE
  S" EXIT THEN" EVALUATE ; IMMEDIATE

: [']  \ 94
  ?COMP	?OLD [']
  ' ALIT,
; IMMEDIATE

: S", ( addr u -- ) \ 
  DUP  C, 
 DP M@ SWAP DUP IALLOT
\  TT_? IF  THEN
   M_CMOVE ;

: ALIGNED  1+ -2 AND ;

: CLITERAL,
  	DUP,
	DUP 2+ 1 ANDC
	HERE + 4 +
	[RISCV]	a0 SWAP jal, [P]
	S",
	HERE 1 AND IF 0 C, THEN

;

: CLITERAL
  HERE THERE? DROP
  HERE THERE?  STATE M@ AND
  IF CLITERAL,

  ELSE  POSTPONE CLITERAL
  THEN   ; IMMEDIATE

: SLITERAL
  HERE THERE? DROP
  HERE THERE?  STATE M@ AND
  IF CLITERAL, S" COUNT" EVALUATE
  ELSE  POSTPONE SLITERAL
  THEN   ; IMMEDIATE

\ 0 VALUE S"PTC?

: S"  ( "ccc<quote>" --- )
\ Parse a string delimited by " and compile the following runtime semantics.
\ Runtime: ( --- c-addr u) Return start address and length of that string.
  [CHAR] " PARSE
  POSTPONE SLITERAL
 ; IMMEDIATE

: Z"  ( "ccc<quote>" --- )
\ Parse a string delimited by " and compile the following runtime semantics.
\ Runtime: ( --- z-addr ) Return start 0 terminated address of that string.
  [CHAR] " PARSE
    	DUP,
	DUP 2+ 1 ANDC
	HERE + 4 +
	[RISCV]	a0 SWAP jal, [P]
	DP M@ SWAP DUP IALLOT M_CMOVE 0 C,
	HERE 1 AND IF 0 C, THEN

 ; IMMEDIATE
 
: ."
	?COMP
	HERE THERE? 0= IF  POSTPONE ." BREAK
	POSTPONE Z"
	S" ZTYPE" EVALUATE
; IMMEDIATE

: ABORT"
	?COMP
	HERE THERE? 0= IF  POSTPONE ABORT" EXIT THEN
	POSTPONE Z"
	S" (ABORT'')" EVALUATE
; IMMEDIATE

: TLINK, ( -> ) 
\ cr ." tl=" HERE TC_IMG - h. 
  TLAST   @ \ DUP H.
 L,
  HERE TLAST !
 ;

: TIMMED  1 TLAST M@ 8 -  W! ;

: #THEADER (  cfa "<spaces>name" -- )
	T-ALIGN
	 L,	\ cfa
	0 L,	\ flags
	TLINK,
	PARSE-NAME S",
\ 	TLAST @ MH.	TLAST @ T> COUNT TYPE  CR
;

: THEADER: >IN M@ ' SWAP >IN M! #THEADER ;

: TCONSTANT:
	T-ALIGN
	HERE SWAP \ hex f7_ed
	[RISCV] a1 'DOVALUE jal, [P]
	L,
	#THEADER
	;



1	CONSTANT IF_FLAG
11	CONSTANT IF.F_FLAG
13	CONSTANT HEAD_FLAG
3	CONSTANT BEGIN_FLAG
7	CONSTANT DO_FLAG1


: AHEAD ?OLD AHEAD	?COMP HERE 0 W, HEAD_FLAG	; IMMEDIATE

: IF
	?OLD IF	
	?COMP
	 $85aa W, \	mv	a1,a0
	 DROP,
	HERE 0 L, IF_FLAG
\	HEX f7_ed
; IMMEDIATE

: THEN	?OLD THEN	?COMP 
DUP IF_FLAG = IF DROP
\ HEX f7_ed
 HERE OVER  -
 DUP  $1E AND 7 <<
 OVER  $7E0 AND $14 << OR
 SWAP  $800 AND 4 >> OR
\  $50063 OR  \ beq	a0,zero,
  $58063 OR  \ beq	a1,zero,
  SWAP L!
  BREAK
 HEAD_FLAG = IF \ f7_ed
 HERE OVER -
 $A001  \ c.j
 OVER  $E AND 2 << OR
 OVER $10 AND 7 << OR 
 OVER $20 AND 3 >> OR 	\
 OVER $80 AND 1 >> OR
 OVER $400 AND 2 >> OR
 SWAP $B40 AND 1 << OR SWAP W! BREAK
  
   -314 THROW 
  ; IMMEDIATE

: ELSE   ( BO BI ADDR ? -- 0 0 ADDR1 ?1 )

  HERE THERE? 0= IF POSTPONE ELSE BREAK
\ HEX f7_ed  
  POSTPONE  AHEAD CS-SWAP
  POSTPONE  THEN
;  IMMEDIATE

: BEGIN
  HERE THERE? 0= IF POSTPONE BEGIN EXIT THEN
  ?COMP HERE
  BEGIN_FLAG
 ; IMMEDIATE

: UNTIL \ 94
  DP @ THERE? 0= IF POSTPONE UNTIL EXIT THEN
  BEGIN_FLAG <> IF -2004 THROW THEN \ ABORT" UNTIL 
  ?COMP
	[RISCV]
	a1 a0 mv,
	a0 0 [[ s0 ]] lw, \ drop
	s0 s0 4 addi,
	a1 SWAP
	beqz, [P]  
\  ?BRANCH,
; IMMEDIATE

: WHILE \ 94
  DP @ THERE? 0= IF POSTPONE WHILE EXIT THEN
  ?COMP [COMPILE] IF
  CS-SWAP
; IMMEDIATE

: AGAIN  HERE THERE? 0= IF  POSTPONE AGAIN EXIT THEN
  ?COMP
  ?COMP BEGIN_FLAG <> IF -2006 THROW THEN \ ABORT" AGAIN 
	[RISCV]	j, [P]
; IMMEDIATE

: REPEAT HERE THERE? 0= IF  POSTPONE REPEAT EXIT THEN
  POSTPONE AGAIN
  POSTPONE THEN 
; IMMEDIATE

: BREAK	?OLD BREAK	?COMP 
  EXIT,
  POSTPONE THEN 
; IMMEDIATE

: DO            \ Run: n1|u1 n2|u2 -- ; R: -- loop-sys           6.1.1240
\ *G Begin a *\fo{DO ... LOOP} construct. Takes the end-value and
\ ** start-value from the stack.
  '(DO) S_COMPILE, HERE 0 L, HERE DO_FLAG1
; IMMEDIATE

: ?DO           \ Run: n1|u1 n2|u2 -- ; R: -- | loop-sys ; 6.2.0620
\ *G Compile a *\fo{DO} which will only begin loop execution if
\ ** the loop parameters do not specify an interation count of 0.
  '(?DO) S_COMPILE, HERE 0 L, HERE DO_FLAG1
; IMMEDIATE


: LOOP          \ Run: -- ; R: loop-sys1 -- | loop-sys2         6.1.1800
\ *G The closing statement of a *\fo{DO ... LOOP} construct.
\ ** Increments the index and terminates when the index crosses
\ ** the limit.
  DO_FLAG1 ?PAIRS
  
	$0485 W, \	addi	s1,s1,1
	$0d85 W, \	addi	s11,s11,1
   
   	[RISCV]	s11 SWAP	bltz, [P]  

	$4d92 W, \	lw	s11,4(sp)
	$4482 W, \	lw	s1,0(sp)
	$0131 W, \	addi	sp,sp,12
   
   
  HERE SWAP L!
 ; IMMEDIATE


: +LOOP         \ Run: n -- ; R: loop-sys1 -- | loop-sys2       6.1.0140
\ *G As *\fo{LOOP} except that you specify the increment on the
\ ** stack. The action of *\fo{n +LOOP} is peculiar when n is
\ ** negative:
\ *C   -1 0 ?DO  i .  -1 +LOOP
\ *P prints *\fo{0 -1}, whereas:
\ *C   0 0 ?DO  i .  -1 +LOOP
\ *P prints nothing. This a result of the mathematical trick used
\ ** to detect the terminating condition. To prevent confusion
\ ** avoid using *\fo{n +LOOP} with negative *\i{n}.
  DO_FLAG1 ?PAIRS


	$94aa W, \	add	s1,s1,a0
	$9daa W, \	add	s11,s11,a0
    DROP,
   	[RISCV]	s11 SWAP	bltz, [P]  

	$4d92 W, \	lw	s11,4(sp)
	$4482 W, \	lw	s1,0(sp)
	$0131 W, \	addi	sp,sp,12

  HERE SWAP L!
 ; IMMEDIATE

: DOES> 
	S" (DOES>)" EVALUATE
 	DUP,
	$852E W, \	c.mv	a0,a1
; IMMEDIATE


: EXIT
	?OLD EXIT	EXIT,
; IMMEDIATE

: ; 
	?OLD ;
	EXIT,
	SMUDGE	[COMPILE] [
; IMMEDIATE

: T:
    HERE THERE? 0= IF  :  BREAK
	  HERE 1 AND IF $FF C, THEN
	  :#THS EXEC_BUF_SET
	:
	$1171 W, \     	addi	sp,sp,-4
	$c006 W, \     	sw	ra,0(sp)
;

: :
    HERE THERE? 0= IF  :  BREAK
	  HERE 1 AND IF $FF C, THEN
	  :#THS
	:
	$1171 W, \     	addi	sp,sp,-4
	$c006 W, \     	sw	ra,0(sp)
;

: IMMEDIATE  IMMEDIATE  1 TO TLASTFLG ;

EXPORT

0 value ?SOURCETYPE

: TCCM-INTERPRET ( -> )
    SAVEERR? ON
 ?SOURCETYPE IF  CR SOURCE TYPE key drop THEN
\   SOURCE-ID 0= IF  MAIN_S THEN
  BEGIN
    PARSE-NAME
 DUP
  WHILE
    SFIND ?DUP
    IF [']  ?COMP_  CATCH 
        THROW
    ELSE  ?SLITERAL
    THEN
    ?STACK
  REPEAT 2DROP 
\   SOURCE-ID 0= IF  MAIN_S THEN
;

: 8000:
    BASE M@ >R HEX
   PARSE-NAME

	  NUMBER?
	0= THROW  THROW
	>IN @ 
\ DUP H. CR SOURCE DUMP KEY DROP
 $C >
	IF L, ELSE  W,
		PARSE-NAME >IN @ $10 =
		if 	  NUMBER?	0= THROW  THROW W,
			PARSE-NAME >IN @ $15 =
			if 	  NUMBER?	0= THROW  THROW W,
				PARSE-NAME >IN @ $1A =
				if 	  NUMBER?	0= THROW  THROW W,
				else 2drop
				then
			else 2drop
			then
		else 2drop
		then
	 THEN \  7 c,  >in @ c,
(
	 PeekChar BL <>
	 IF
	   PARSE-NAME
	  NUMBER?
	0= THROW  THROW
	  W,
 THEN        )

   R> BASE M!
	POSTPONE \
;
: ####: 8000: ;
: ____: DEPTH IF ABORT THEN $12 >IN M!  ;

: QQ DP M! ;

\ ' TCCM-INTERPRET  &INTERPRET !
: TSAVE ( <filename> -- )
 PARSE-NAME W/O CREATE-FILE THROW  TO T-STDOUT
 MSTR_IMG DUP L@ H.  4+
 BEGIN DUP @ 
 WHILE

 CR
 DUP L@ H.
 DUP 4+ L@  H.
 DUP 8 + L@  H.
\ DUP 8 + L@ 0 ?DO DUP I +  $C + C@ H.  LOOP
 DUP L@ T> OVER 8 + L@ 2* DUP H. T-STDOUT WRITE-FILE THROW

 DUP 4+ L@

	2* + $C +
 REPEAT DROP
;
/*
: J-TYPE ( r2 ofset cod -- ) 
 BASE M@ >R
\ HEX F7_ED
 SWAP HERE -
 SWAP
 OVER  $7FE AND $14 << OR
 OVER  $800 AND 9 << OR
 OVER  $FF000 AND OR
 SWAP  $100000 AND $B << OR
 SWAP	7  << OR
 L,
  R> BASE M!
 0 TO PARM_HESH ;
*/
: XCALL,
 HERE -
 $EF
 OVER  $7FE AND $14 << OR
 OVER  $800 AND 9 << OR
 OVER  $FF000 AND OR
 SWAP  $100000 AND $B << OR L,
;

;MODULE

