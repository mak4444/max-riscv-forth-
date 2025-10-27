\ Assembler for riscv

\ REQUIRE HERE-TAB-CUR ~mak\lib\THERE\mlist.f 
\- ROMBIG  : ROMBIG ;
\- ROMEND  : ROMEND ;

\ REQUIRE REQUIRE_AL ~mak\lib\THERE\mlist.f 
REQUIRE [IF] ~mak/CompIF3.f
REQUIRE [IFNDEF] ~nn/lib/ifdef.f
REQUIRE NUMBER? ~mak/lib/fpcnum.f

\- DCREATE : DCREATE CREATE ;
\- TCREATE : TCREATE CREATE ;
\- TDOES>  : TDOES> POSTPONE DOES> ; IMMEDIATE

\- <<	: << LSHIFT ;
\- >>	: >> RSHIFT ;
\- A>>	: A>> ARSHIFT ;
\- BREAK	: BREAK  POSTPONE EXIT POSTPONE THEN ; IMMEDIATE
\- CELL/	: CELL/  CELL / ;
\- OR!	:  OR! ( N ADDR -- )	DUP @ ROT OR	SWAP ! ;
\- AND!	: AND! ( N ADDR -- )	DUP @ ROT AND	SWAP ! ;
\- USHORT? : USHORT? ( n -- -129<n<256 )  0x80 +  0x180 U< ;
\- NOT	: NOT INVERT ;
\- M@ : M@ @ ;
\- M! : M! ! ;

[IFNDEF] FF_ALIGN
: FF_ALIGN
	HERE 1 AND 
	IF 0xFF C,
	THEN ;
[THEN]
-1 VALUE TEXEC_?
: TEXEC_SET  -1 TO TEXEC_? ;
: TEXEC_DIS  0 TO TEXEC_? ;
\- .S  : .S ( -- )    DEPTH .SN ;

\- 3DUP : 3DUP DUP 2OVER ROT ;

DECIMAL

MODULE: RISCV_MOD

FLOAD ~mak/lib/multipass.4

0 VALUE MOREPASS
0 VALUE NPASS

0 VALUE ITERNUM

4 CONSTANT MAXLL

: :LTABLE      ( addr --- )
   TCREATE HERE  MAXLL 0 DO 0 , LOOP  TDOES> \ F7_ED
   DUP
   DUP
 CELL+  MAXLL 1- CELLS CMOVE>
   HERE SWAP ! ;

: BLTABLE      ( --- addr )               \ 
  TCREATE  , TDOES>  \  F7_ED
  @ @
 DUP HERE U> ABORT" backward ref error"
;

: FLTABLE      ( --- addr )               \ 
  TCREATE  , TDOES> \  HEX  F7_ED
  @ -1  SWAP  MAXLL CELLS BOUNDS

  DO  HERE  I @ U<
	IF I @ UMIN
	THEN
  MCELL
 +LOOP
 DUP -1 =
 IF
   NPASS
	IF 1  ABORT" forward ref error or MAXLL not enough" 
	ELSE DROP HERE 4 +
	THEN
	-1 TO MOREPASS
 THEN
;

: :::L  :LTABLE DUP BLTABLE  FLTABLE  ;

\ ::L  LL0 LL0:
\ LTABLE@ LL0 LTABLE! LL0:
:::L  0: 0B 0F
:::L  1: 1B 1F
:::L  2: 2B 2F
:::L  3: 3B 3F
:::L  4: 4B 4F
:::L  5: 5B 5F
:::L  6: 6B 6F
:::L  7: 7B 7F
:::L  8: 8B 8F
:::L  9: 9B 9F


0 VALUE BBF
0 VALUE HHF
0 VALUE !!F
0 VALUE SHF
0 VALUE ?{{

0 VALUE {{}}

0 VALUE COMCOD


: !!   \ writ-back bit
   -1
 TO !!F
 ;


0 VALUE PARM_HESH

: >PARM_HESH
  PARM_HESH 2 << + TO PARM_HESH
;

: RX 1 >PARM_HESH ;

0 VALUE R-DEPTH

: R: CREATE DUP   , 1+ DOES> @
 ?{{
 IF  1 SWAP <<  {{}} OR TO {{}}
 ELSE  RX
 THEN DEPTH TO R-DEPTH
 ;

: [[ 2 >PARM_HESH ;

: 0[[ \ F7_ED
 DEPTH R-DEPTH - 0= IF 0 THEN
 [[ ;

: ]]
 3 >PARM_HESH ;

: .{{ 6 >PARM_HESH -1 TO ?{{ 0 TO {{}} ;		: .}}  0 TO ?{{ ;

\ ROMEND
0 R: X0  R: X1  R: X2  R: X3  R: X4
  R: X5  R: X6  R: X7
  R: X8  R: X9 
  R: X10 R: X11 R: X12 R: X13 R: X14 R: X15 R: X16 R: X17
  R: X18 R: X19 R: X20 R: X21 R: X22 R: X23 R: X24 R: X25 R: X26 R: X27
  R: X28 R: X29 R: X30 R: X31
    DROP

0
  R: ZERO  R: RA  R: SP R: GP  R: TP
  R: T0  R: T1  R: 	T2
  R: S0 R: S1
  R: A0 R: A1  R: A2  R: A3  R:	A4  R: 	A5  R: 	A6  R: 	A7
  R: S2 R: S3  R: S4  R: S5  R:	S6  R: 	S7  R: 	S8  R: 	S9  R: S10  R: S11
  R: T3 R: T4  R: T5  R: T6
   DROP


0 R: F0  R: F1  R: F2  R: F3  R: F4
  R: F5  R: F6  R: F7
  R: F8  R: F9 
  R: F10 R: F11 R: F12 R: F13 R: F14 R: F15 R: F16 R: F17
  R: F18 R: F19 R: F20 R: F21 R: F22 R: F23 R: F24 R: F25 R: F26 R: F27 R: F28
  R: F29 R: F30 R: F31
    DROP

0 R: FT0  R: FT1  R: FT2  R: FT3  R: FT4
  R: FT5  R: FT6  R: FT7
  R: FS0  R: FS1
  R: FA0 R: FA1  R: FA2  R: FA3  R:	FA4  R: 	FA5  R: 	FA6  R: 	FA7
  R: FS2 R: FS3  R: FS4  R: FS5  R:	FS6  R: 	FS7  R: 	FS8  R: 	FS9  R: FS10  R: FS11
  R: FT8 R: FT9 R: FT10 R: FT11
    DROP

: PARAM:
 CREATE PARM_HESH ,  0 TO PARM_HESH
 DOES>  @ PARM_HESH =
 ;

 RX 		PARAM: LONER
 RX RX		PARAM: R,R
 [[ RX ]] PARAM: (R)
 RX [[ RX ]] PARAM: R(R)
 RX RX RX	PARAM: R,R,R

: C.MV,	2 << SWAP 7 << OR $8002 OR W, 0 TO PARM_HESH ;
: C.ADD, 2 << SWAP 7 << OR $9002 OR W, 0 TO PARM_HESH ;
: MV, C.MV, ;

:  >C.LI ( r imm --  )
  DUP $1F AND
  SWAP 
  $20 AND
  5 <<
  OR
  2 << 
  SWAP
  7 <<
  OR ;
: C.LI, >C.LI $4001 OR W, 0 TO PARM_HESH ;
: C.LUI, 
\ hex f7_ed
 >C.LI
 $6001 OR
 W,
 0 TO PARM_HESH ;

: C.ADDI, >C.LI 1 OR W, 0 TO PARM_HESH ;
: C.SLLI, >C.LI 2 OR W, 0 TO PARM_HESH ;
: C.SRLI, SWAP 7 AND SWAP >C.LI $8001 OR W, 0 TO PARM_HESH ;
: C.SRAI, SWAP 7 AND SWAP >C.LI $8401 OR W, 0 TO PARM_HESH ;
: C.ANDI, SWAP 7 AND SWAP >C.LI $8801 OR W, 0 TO PARM_HESH ;


: C.SLLI64, 0 C.SLLI, ;
: SLLI, NIP  C.SLLI, ; \ !!!!!
: C.NOP, 0 SWAP C.ADDI, ;
: C.NOP#  DEPTH R-DEPTH - 0= IF 0 THEN  C.NOP, ;

: >C.OR,
    SWAP $7 AND 2 << OR
    SWAP $7 AND 7 << OR W, 0 TO PARM_HESH ;

: C.SUB, $8c01 >C.OR, ;
: C.XOR, $8c21 >C.OR, ;
: C.OR,  $8c41 >C.OR, ;
: C.AND, $8c61 >C.OR, ;


\ rotate a bits left
: U-TYPE SWAP $C << OR SWAP $7 << OR L, 0 TO PARM_HESH ;
: R-TYPE SWAP $14 << OR SWAP $F << OR SWAP $7 << OR L, 0 TO PARM_HESH ;
: I-TYPE  R-TYPE ;
: S-TYPE ( 	 r1 ofset r2 cod -- )
\ BRAKEPOINT \ f7_ed
 SWAP $F << OR  \ r1 ofset cod+
 OVER $1f  AND $7 << OR
\ SWAP $7E0 AND $E << OR  \ r1 cod++
 SWAP $FE0 AND $14 << OR  \ r1 cod++
 SWAP $14 << OR  \ cod'
 L, 0 TO PARM_HESH ;

: B-TYPE ( r1 r2 ofset cod -- ) 
 BASE M@ >R
\ HEX F7_ED
 SWAP HERE -
 SWAP
 OVER  $1E AND 7 << OR
 OVER  $7E0 AND $14 << OR
 OVER  $800 AND 4 >> OR
 SWAP  $1000 AND $13 <<  OR
 
 SWAP	$14  << OR
 SWAP	$F  << OR L,
  R> BASE M!
 0 TO PARM_HESH ;

: LUI,
\ HEX F7_ED
 DUP $80000 XOR
 $80000 - 
 $20 +
 $3F ANDC
 IF $37 U-TYPE BREAK
   C.LUI,
 ;

\  mm[20|10:1|11|19:12] rd opcode J-type
 
: J-TYPE ( r2 ofset cod -- ) 
 BASE M@ >R
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


\ m[5:4|9:6|2|3]
: C.ADDI4SPN, ( r sp imm )
   SWAP 2 <> IF -333 THROW THEN
   SWAP $7 AND 2 <<
   OVER 8 AND 2 << OR
   OVER 4 AND 4 << OR
   OVER $3C0 AND 1 << OR
   SWAP $30 AND 7 << OR
   W,
   0 TO PARM_HESH 
;

: C.ADDI16SP, ( sp imm )
   DUP 0= IF -333 THROW THEN
   $6101
   OVER $10 AND 2 << OR
   OVER $20 AND 3 >> OR
   OVER $40 AND 1 >> OR
   OVER $180 AND 4 >> OR
   SWAP $200 AND 3 << OR
   W,
   0 TO PARM_HESH 
   2 <> IF -333 THROW THEN

;

: ADDI, ( r1 r2 imm -- )
\ HEX F7_ED
	DUP 0= IF DROP C.MV, BREAK
	3DUP \ 
	DUP 0=
	SWAP $20 + $3F ANDC	OR  \ r1 r2 imm r1 r2 flg
	-ROT XOR OR
	0= IF NIP C.ADDI, BREAK

	3DUP \ 
	DUP 0=
	SWAP $3FF ANDC OR
	SWAP 2 XOR OR   \ ?SP
	SWAP $18 AND 8 XOR OR \ ?S0..A5
	0= IF C.ADDI4SPN, BREAK

	3DUP \ 
	DUP 0=
	SWAP $200 + $3FF ANDC	OR
	SWAP 2 XOR OR   \ ?SP
	SWAP 2 XOR OR   \ ?SP
	0= IF NIP C.ADDI16SP, BREAK

	$00000013 I-TYPE
	 ;
: NCSRI? ( r1 r2 sift --  r1 r2 sift flg )
\	HEX  F7_ED
	3DUP
	DROP
	\ r1 r2
	DUP 8 XOR  3 ANDC  \ r1 r2 imm r1 r2 flg
	-ROT XOR OR
	;
	 
: SLTI,		$00002013 I-TYPE ;
: SLTIU,	$00003013 I-TYPE ;
: XORI,		$00004013 I-TYPE ;
: ORI,		$00006013 I-TYPE ;
: SLLI, NCSRI? IF $00001013 I-TYPE BREAK NIP C.SLLI, ;
: SRLI, NCSRI? IF $00005013 I-TYPE BREAK NIP C.SRLI, ;
: SRAI, NCSRI? IF $40005013 I-TYPE BREAK NIP C.SRAI, ;
: ANDI, NCSRI? IF $00007013 I-TYPE BREAK NIP C.ANDI, ;
: SEQZ, 1 SLTIU, ;


: NOT,	-1 XORI, ;

: CSRRWI, SWAP	$00005073 I-TYPE ;
: CSRRSI, SWAP	$00006073 I-TYPE ;
: CSRRCI, SWAP	$00007073 I-TYPE ;

: ADD,
	  DEPTH R-DEPTH - IF   ADDI, BREAK
	ROT 2DUP = IF DROP SWAP C.ADD, BREAK
	ROT 2DUP = IF NIP SWAP C.ADD, BREAK	
	ROT	$00000033 R-TYPE ;
: SUB,	$40000033 R-TYPE ;
: SLL,	$00001033 R-TYPE ;
: SLT,	$00002033 R-TYPE ;
: SLTU,	$00003033 R-TYPE ;
: XOR,	$00004033 R-TYPE ;
: SRL,	$00005033 R-TYPE ;
: SRA,	$40005033 R-TYPE ;
: OR, 	$00006033 R-TYPE ;
: AND,	$00007033 R-TYPE ;

: NEG,	ZERO SWAP SUB, ;

: MUL,	$02000033 R-TYPE ;
: MULH,	$02001033 R-TYPE ;
: MULHSU,	$02002033 R-TYPE ;
: MULHU,	$02003033 R-TYPE ;
: DIV,	$02004033 R-TYPE ;
: DIVU,	$02005033 R-TYPE ;
: REM,	$02006033 R-TYPE ;
: REMU,	$02007033 R-TYPE ;


: >C.LWSP, (  r ofset sp  --  )
 BASE M@ >R
\ HEX F7_ED
  >R
   DROP
  DUP 3 AND IF -333 THROW THEN
  DUP $1C AND  2 << 
  OVER $20 AND 7 << OR
  SWAP $C0 AND 4 >> OR  \ r cod+ 
  SWAP 7 << OR \ cod'
  R> OR \ $4002 OR
  W,
  0 TO PARM_HESH
  R> BASE M!
;

: C.LWSP, $4002 >C.LWSP, ;
: C.FLWSP, $6002 >C.LWSP, ;


: >C.SWSP, (  r ofset sp  --  )
 BASE M@ >R
\ HEX F7_ED
  >R
   DROP
  DUP 3 AND IF -333 THROW THEN
  DUP $3C AND  7 << 
  SWAP $C0 AND 1 << OR  \ r cod+ 
  SWAP 2 << OR \ cod'
  R> OR \ $C002 OR
  W,
  0 TO PARM_HESH
  R> BASE M!
;

: C.SWSP, $C002 >C.SWSP, ;

: >C.LW, (  r1 ofset r2 cod --  )
 BASE M@ >R
  >R 
\ HEX F7_ED
   DUP $18 AND 8 <> ( x8..x15 ) IF -333 THROW THEN 
  OVER 3 AND IF -333 THROW THEN
  $7 AND 7 <<
  OVER $4 AND 4 << OR
  OVER $38 AND 7 << OR  \ r cod+ 
  SWAP $40 AND 1 >> OR  \ r cod+ 
  SWAP $7 AND 2 << OR \ cod'
   R>  OR
  W,
  0 TO PARM_HESH

  R> BASE M!
;

: C.LW,  $4000 >C.LW, ;
: C.FLW, $6000 >C.LW, ;
: C.SW,  $C000 >C.LW, ;

: FLW,
 DUP 2 = IF C.FLWSP, BREAK
 C.FLW, ;


: LB,		SWAP $0003 I-TYPE ;
: LH,		SWAP $1003 I-TYPE ;
: LBU,		SWAP $4003 I-TYPE ;
: LHU,		SWAP $5003 I-TYPE ;

: LW,   (  r1 ofset r2  --  )
\ HEX F7_ED
	DUP 2OVER \ r1 ofset r2 r2 r1 ofset
	$FC ANDC   		\ r1 ofset r2 r2 r1 flg
	SWAP $18 AND 8 XOR OR	\ r1 ofset r2 r2 s0..a5
	SWAP $18 AND 8 XOR OR	\ r1 ofset r2 flg'
	0= IF C.LW, BREAK
	DUP 2 = IF C.LWSP, BREAK \ SP
		SWAP $2003 I-TYPE ;

: LI,
\ HEX F7_ED
 dup $20 +
 $3F ANDC
 0=
 IF C.LI,
 BREAK
 DUP ABS
 $7FF ANDC
 IF	2DUP
	DUP $C >> $FFFFF AND
	SWAP $800 AND $B >> + \ for borrow
	 LUI,
	$FFF AND
	DUP 0= IF 2DROP BREAK
	OVER SWAP ADDI,
 BREAK
 DUP 0= IF 2DROP BREAK
 $800 - $800 XOR
   0 SWAP ADDI,
  ;


: AUIPC, $17 U-TYPE ;


: LA, ( r adr -- )
 BASE M@ >R
\ HEX F7_ED
  HERE -
  2DUP
  $800 +
  $C >>
  AUIPC,
  $FFF AND $800 XOR $800 -
  ?DUP 
  IF  OVER SWAP $00000013 I-TYPE \ ADDI, \ !!!!
  ELSE 2DROP 
  THEN
  R> BASE M!
  ;


: SB,		$0023 S-TYPE ;
: SH,		$1023 S-TYPE ;
: SW,
	DUP 2OVER \ r1 ofset r2 r2 r1 ofset
	$FC ANDC   		\ r1 ofset r2 r2 r1 flg
	SWAP $18 AND 8 XOR OR	\ r1 ofset r2 r2 s0..a5
	SWAP $18 AND 8 XOR OR	\ r1 ofset r2 flg'
	0= IF C.SW, BREAK
	DUP 2 = IF C.SWSP, BREAK	$2023 S-TYPE ;

: BEQ,  $0063 B-TYPE ;
: BNE,	$1063 B-TYPE ;
: BLT,	$4063 B-TYPE ;
: BLE,	$5063 B-TYPE ;
: BLTU,	$6063 B-TYPE ;
: BGEU,	$7063 B-TYPE ;

: BGT, ROT SWAP $4063 B-TYPE ;

: BLTZ, 0 SWAP	BLT, ;
: BLEZ, 0 SWAP	BLE, ;

: BGTZ, 0 -ROT	BLT, ;
: BGEZ, 0 -ROT	BLE, ;


: >C.BEQZ,
 BASE M@ >R
\ HEX F7_ED
  SWAP HERE -
  SWAP
  OVER $6 AND  2 << OR  \ r ofset cod+
  OVER $18 AND  7 << OR
  OVER $20 AND  3 >> OR
  OVER $C0 AND  1 >> OR
  SWAP $100 AND  4 <<  OR \ cod'
  OVER $18 AND 8 <> ( x8..x15 ) IF -333 THROW THEN 
  SWAP $7 AND
  7 <<
  OR  W,
  0 TO PARM_HESH
  R> BASE M!
;

: C.BEQZ, ( r addr -- )  $C001 >C.BEQZ, ;
: C.BNEZ, ( r addr -- )  $E001 >C.BEQZ, ;

: BEQZ,  ( r addr -- )
\	HEX F7_ED
  OVER $18 AND  8 XOR	\ r addr RFlg
  OVER HERE - $100 + $1FF ANDC   \ r addr RFlg addrFlg
  OR IF  0 SWAP	BEQ, BREAK
  C.BEQZ,
  ;

: BNEZ,  ( r addr -- )
\	HEX F7_ED
  OVER $18 AND  8 XOR	\ r addr RFlg
  OVER HERE - $100 + $1FF ANDC   \ r addr RFlg addrFlg
  OR IF  0 SWAP	BNE, BREAK
  C.BNEZ,
  ;

: UNIMP, 0 W, ;
: RET, $8082 W, ;
: NOP,	1 W, ;
: ECALL, $73 L, ;


: >C.J, ( ofset cod -- )
 BASE M@ >R
\ HEX F7_ED
  
 OVER  $E AND
 2 << OR
 OVER $10 AND
 7 << OR 
 OVER $20 AND
 3 >> OR 	\
 OVER $80 AND
 1 >> OR
 OVER $400 AND
 2 >> OR
 SWAP $B40 AND
 1 << OR W,
  R> BASE M!
 ;

: C.JAL, HERE - $2001 >C.J, ;
: C.J, HERE - $A001 >C.J, ;

: JAL,
\	DUP HERE - $800 $FFF ANDC 	
\ HEX F7_ED
 PARM_HESH 
 OVER HERE -
 $800 +
 $FFF ANDC
 OR
 IF  PARM_HESH 0= IF 1 SWAP THEN  $6F J-TYPE 
 BREAK
  C.JAL, 
 ;
 
: CALL,  JAL, ;

: J,
\ HEX F7_ED
 DUP HERE -
 $800 +
 $FFF ANDC
  IF 0 SWAP $6F J-TYPE BREAK
   C.J, ;

: JALR,
\ HEX F7_ED
  R(R)  IF SWAP 0x67 I-TYPE BREAK
  (R)  IF
  1 -ROT
  SWAP
  0x67 I-TYPE BREAK
  R,R  IF
  DEPTH R-DEPTH - 0= IF 0 THEN
  0x67  I-TYPE BREAK
  LONER IF
  1 SWAP 0
  $67 I-TYPE  BREAK
  1 ABORT" JALR ?"
  ;

: JR, ( r -- )
  0 SWAP 0 0x67 I-TYPE
;

: .WORD, L, ;
: .SHORT, W, ;

0 VALUE TEB_CLEAN

: CODL
\+ :#THS   :#THS
	TEXEC_?
	IF
[IFDEF] TEXEC_BUF
	>IN M@ >R
	PARSE-NAME
\ 	ALSO FORTH
 SFIND
\ PREVIOUS

	IF
			TEB_CLEAN CELL+ TO TEB_CLEAN
			TEXEC_BUF TEXEC_BUF CELL+ TEB_SIZE MOVE
			TEXEC_KEY TEXEC_KEY CELL+ TEB_SIZE MOVE
			TEXEC_BUF M!
		HERE	TEXEC_KEY M!
	ELSE	2DROP
	THEN

	R> >IN M!
[THEN]  THEN
	HEADER
;

VECT A?HEAD

' NOOP TO A?HEAD

(
[IFNDEF] HERE-TAB-CUR VARIABLE HERE-TAB-CUR
[THEN]

[IFNDEF] SHERE-TAB-CUR VARIABLE SHERE-TAB-CUR
[THEN]
)
\+ HERE-TAB-CUR VARIABLE HERE-TAB-CUR-SAVE

\+ SHERE-TAB-CUR VARIABLE SHERE-TAB-CUR-SAVE

0 VALUE MOREPASSFLG
0 VALUE SAVELAST

: ASM_END

 MOREPASS TO MOREPASSFLG
 MOREPASS IF
\ CR ." MOREPASS"
\+ HERE-TAB-CUR 	HERE-TAB-CUR-SAVE @ HERE-TAB-CUR !
\+ SHERE-TAB-CUR 	SHERE-TAB-CUR-SAVE @ SHERE-TAB-CUR !
  ITERNUM 2+ TO   ITERNUM
   NPASS 1+ TO NPASS
\  redundant items delete
\+ HERE-TAB-CUR 	TEXEC_BUF TEB_CLEAN + TEXEC_BUF TEB_SIZE  MOVE
\+ HERE-TAB-CUR 	TEXEC_KEY TEB_CLEAN + TEXEC_KEY TEB_SIZE  MOVE
	0 TO TEB_CLEAN
  0 TO MOREPASS
  SAVELAST CURRENT M@ M!
\   XN 1- TO XN
   X[AGAIN]
   EXIT
 THEN
	PREVIOUS
;

: >BXX.H   4 << 0xD000 OR ;

: IF,        >BXX.H W, HERE 2- ; \ !!!!!
: AHEAD,     0xE000 W, HERE 2- ;
: THEN,      HERE OVER - 4 - 2/ SWAP C! ;
: ELSE,      AHEAD,  SWAP THEN, ;
: BEGIN,     HERE ;
: UNTIL,  ; \    >BXX.H SWAP  BXX.N OR W, ;
: AGAIN,  ; \   BXX.N 0xE000 OR W,  ;
: WHILE,     IF, SWAP ;
: REPEAT,    AGAIN,  THEN, ;


EXPORT

\- T-ALIGN : T-ALIGN  BEGIN  HERE 1 AND   WHILE 0xFF C, REPEAT ;

: 2ALIGN
    HERE 1 AND
  IF 0xFF C, THEN ;

: RV_ASM_BIG

	FF_ALIGN
	ALSO RISCV_MOD

  0 TO MOREPASS
  0 TO NPASS
  0 TO TEB_CLEAN
  CURRENT M@ M@ TO SAVELAST

  ITERNUM 2+ TO   ITERNUM

\+ HERE-TAB-CUR 	HERE-TAB-CUR @ HERE-TAB-CUR-SAVE !
\+ SHERE-TAB-CUR 	SHERE-TAB-CUR @ SHERE-TAB-CUR-SAVE !
 X[BEGIN]

 ;

: CODE_T2
   2ALIGN
 0 TO PARM_HESH
 0 TO !!F
    CODL
    RV_ASM_BIG \    (CODE)
  ;

0x10000000 VALUE EXEPTION_TABLE

: INTERR       ( addr -- )      
	T-ALIGN
    EXEPTION_TABLE +		                \ 
    HERE  1+    
    SWAP !                                \ 
    CODL
    RV_ASM_BIG \    (CODE)
;

: 0:INIT   MAXLL 0 DO 0: LOOP ;
: 1:INIT   MAXLL 0 DO 1: LOOP ;
: 2:INIT   MAXLL 0 DO 2: LOOP ;
: 3:INIT   MAXLL 0 DO 3: LOOP ;
: 4:INIT   MAXLL 0 DO 4: LOOP ;
: 5:INIT   MAXLL 0 DO 5: LOOP ;
: 6:INIT   MAXLL 0 DO 6: LOOP ;
: 7:INIT   MAXLL 0 DO 7: LOOP ;
: 8:INIT   MAXLL 0 DO 8: LOOP ;
: 9:INIT   MAXLL 0 DO 9: LOOP ;

;MODULE

\ ' RISCV_MOD H.

\ RRX
