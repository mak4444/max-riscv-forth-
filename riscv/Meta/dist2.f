REQUIRE [IF] ~mak/CompIF3.f
REQUIRE [IFNDEF] ~nn/lib/ifdef.f
REQUIRE PLACE  ~mak/place.f
REQUIRE CASE lib/ext/case.f
\- H. : H.  BASE @ HEX SWAP U. BASE !  ;


\- BOUNDS : BOUNDS OVER + SWAP ;
\- (D.) : (D.)          ( d -- addr len )       TUCK DABS  <# #S ROT SIGN #> ;

[IFNDEF] H.R
: H.R    ( n1 n2 -- )    \ display n1 as a hex number right
                        \ justified in a field of n2 characters
          BASE @ >R HEX >R
          0 <# #S #> R> OVER - 0 MAX SPACES TYPE
          R> BASE ! ;
[THEN]

\- TAB  : TAB 9 EMIT ;

[IFNDEF] H.N
: H.N           ( n1 n2 -- )    \ display n1 as a HEX number of n2 digits
                BASE @ >R HEX >R
                0 <# R> 0 ?DO # LOOP #> TYPE
                R> BASE ! ;
[THEN]

\- BREAK	: BREAK  POSTPONE EXIT POSTPONE THEN ; IMMEDIATE
\- <<	: << LSHIFT ;
\- >>	: >> RSHIFT ;

\- ANDC : ANDC INVERT AND ;

0 VALUE ADDR_OFF

: ?NAME>S      ( CFA -- )
    NEAR_NFA 
	>R DUP
	IF ."  ( " DUP MCOUNT TYPE 
	     NAME> R> - DUP
	     IF   DUP ." +" NEGATE H. \ >S
	     THEN DROP        ."  ) "
	ELSE RDROP DROP
	THEN ;

: ?.NAME>S      ( CFA -- )
\  h. exit
	DUP  ADDR_OFF +	 H. ?NAME>S ;

0 VALUE DIS_JMP_ADDR

MODULE: DISARM

0 VALUE OP_NAME
0 VALUE OPCODE
VECT OP.

:  [|;] (  cod  cod1 cod2  <NAME>  -- )
   TO OP_NAME
   OPCODE = 
   IF OP. RDROP RDROP
   THEN ;

: |;
     LAST M@  LIT,
     POSTPONE [|;]
     POSTPONE  ;
; IMMEDIATE

: .-  S>D (D.) TYPE ;
: U.-  0 (D.) TYPE ;
: H.-   BASE M@ HEX SWAP U>D (D.) TYPE BASE M! ;

0 VALUE IT_V
0 VALUE IT_N

: OP_TYPE 
	TAB OP_NAME   MCOUNT 1- TYPE ;

: NOP_TP TAB OP_TYPE DROP ;

: REG. ( reg -- )
	DUP 0= IF DROP ." zero" BREAK
	DUP 25 < IF 1- 2*
		S" raspgptpt0t1t2s0s1a0a1a2a3a4a5a6a7s2s3s4s5s6s7s8s9" 
		DROP + 2 TYPE
		BREAK
	DUP 26 = IF DROP ." s10" BREAK
	DUP 27 = IF DROP ." s11" BREAK
  
  	 28 - 2* S" t3t4t5t6" DROP + 2 TYPE
;

: REG,. REG. ',' EMIT ;

: SL64_TP
 TAB OP_TYPE TAB
	$7 >> $1F AND REG.
;

: CLUI_TP
 TAB OP_TYPE TAB
 DUP 	7 >> $1F AND REG,.
 DUP	2 >> $1F AND
 SWAP	7 >> $20 AND OR
 $20 XOR $20 -  .-
;

: C.SRI_TP
 TAB OP_TYPE TAB
 DUP 	7 >> $7 AND 8 OR REG,.
 DUP	2 >> $1F AND
 SWAP	7 >> $20 AND OR
 $20 XOR $20 -  .-
;

: C.OR_TP
 TAB OP_TYPE TAB
 DUP 7 >> $7 AND 8 OR REG,.
  2 >> $7 AND  8 OR REG. 
;

: C.SWSP_TP
 TAB OP_TYPE TAB
 DUP 2 >> $1F AND REG,.
 DUP 7 >> $3C AND
 SWAP 1 >> $C0 AND OR .- ." (sp)"
;

: C.LWSP_TP
 TAB OP_TYPE TAB
 DUP 7 >> $1f AND REG,.
 DUP $1C  2 << AND 2 >>
 OVER $20 7 << AND 7 >> OR
 SWAP $C0 4 >> AND 4 << OR  .- ." (sp)"
;

: C.LW_TP
 TAB OP_TYPE TAB
 DUP $7 2 << AND 2 >>  8 OR REG,.
  DUP  $4  4 << AND 4 >>
  OVER $38 7 << AND 7 >> OR
  OVER $40 1 >> AND 1 << OR .- 
    7 >> $7 AND ." (" 8 OR REG. ." )"
;

: C.BZ_TP
 OP_TYPE TAB
 DUP 7 >> 7 AND 8 OR  REG,.

  DUP  $6   2 << AND 2 >>
  OVER $18  7 << AND 7 >> OR
  OVER $20  3 >> AND 3 << OR
  OVER $C0  1 >> AND 1 << OR
  SWAP $100 4 << AND 4 >> OR \ cod'

 $100 XOR $100 -
 OVER + ." 0x" H.-
;

: C.J_TP
 TAB OP_TYPE TAB
 DUP  $E  2 << AND 2 >>
 OVER $10 7 << AND 7 >>  OR  
 OVER $20 3 >> AND 3 <<  OR 
 OVER $80 1 >> AND 1 <<  OR
 OVER $400 2 >> AND 2 << OR
 SWAP $B40 1 << AND 1 >> OR
 $800 XOR $800 -
 OVER + ." 0x" DUP H.- ?NAME>S

;

: U-TYPE.
 BL EMIT DUP 16 >> 4 H.N
   OP_TYPE TAB
	DUP 7 >> $1F AND REG,. 
	DUP $C >> H.-
	DROP
;

: J-TYPE.
 BL EMIT DUP 16 >> 4 H.N
   OP_TYPE TAB
	DUP 7 >> $1F AND REG,. 
 DUP   $7FE $14 << AND $14 >>
 OVER  $800   9 << AND   9 >> OR
 OVER  $FF000 AND OR 
 SWAP  $80000000 AND $B >> OR
 $100000 XOR $100000 - OVER + 2- ." 0x" DUP H.- ?NAME>S
;

: R-TYPE.
 BL EMIT DUP 16 >> 4 H.N
   OP_TYPE TAB
	DUP 7 >> $1F AND REG,. 
	DUP $F >> $1F AND REG,. 
	DUP $14 >> $1F AND REG.
	DROP
;

: I-TYPE.
 BL EMIT DUP 16 >> 4 H.N
   OP_TYPE TAB
	DUP 7 >> $1F AND REG,. 
	DUP $F >> $1F AND REG,. 
	DUP $14 >>  $800 XOR $800 - .-
	DROP
;

: L-TYPE.
 BL EMIT DUP 16 >> 4 H.N
   OP_TYPE TAB
	DUP 7 >> $1F AND REG,. 
	DUP $14 >>  $800 XOR $800 - .-
	DUP $F >> $1F AND ." (" REG. ." )"
	DROP
;

: JR-TYPE.
 BL EMIT DUP 16 >> 4 H.N
   OP_TYPE TAB
	DUP 7 >> $1F AND REG,. 
	DUP $F >> $1F AND REG,. 
	 $14 >>  $800 XOR $800 - OVER + ." 0x" H.-
;

: S-TYPE.
 BL EMIT DUP 16 >> 4 H.N
   OP_TYPE TAB
	DUP $14 >> $1F AND REG,. 
	DUP  $1f   $7 << AND $7 >>
	OVER $FE0  $14 << AND $14 >> OR $800 XOR $800 - .-
	DUP $F >> $1F AND ." (" REG. ." )"
	DROP
;

: B-TYPE.
 BL EMIT DUP 16 >> 4 H.N
   OP_TYPE TAB
	DUP $F >> $1F AND REG,. 
 	DUP $14 >> $1F AND REG,. 

 DUP   $1E   7 <<   AND 7 >>
 OVER  $7E0  $14 << AND $14 >> OR
 OVER  $800  4 >>   AND 4 <<   OR
 SWAP  $1000 $13 << AND $13 >> OR $1000 XOR $1000 - OVER + 2- ." 0x" H.-
;

: nop,	1   |;
: unimp, 0  |;
: ret, $8082  |;
: ebreak, $9002  |;

: nop,	1   |;
: unimp 0  |;
: ret, $8082  |;
: ebreak, $9002  |;

: nop,		0xBf00 |;
: yield,	0xBF10 |;
: wfe,		0xBF20 |;
: wfi,		0xBF30 |;
: sev,		0xBF40 |;


: c.slli64,	2 |;
: c.addi,	$0001 |;
: c.li,		$4001 |;
: c.lui,	$6001 |;
: c.slli,	2 |;


: add,	$00000033 |;
: sub,	$40000033 |;
: sll,	$00001033 |;
: slt,	$00002033 |;
: sltu,	$00003033 |;
: xor,	$00004033 |;
: srl,	$00005033 |;
: sra,	$40005033 |;
: or, 	$00006033 |;
: and,	$00007033 |;
: mul,	$02000033 |;
: mulh,	$02001033 |;
: mulhsu, $02002033 |;
: mulhu, $02003033 |;
: div,	$02004033 |;
: divu,	$02005033 |;
: rem,	$02006033 |;
: remu,	$02007033 |;

: addi,  	$00000013 |;  
: slli,  	$00001013 |;
: slti,		$00002013 |;
: sltiu,	$00003013 |;
: xori,		$00004013 |;
: srli,		$00005013 |;
: ori,		$00006013 |;
: andi,		$00007013 |;

: lb,	$0003 |;
: lh,	$1003 |;
: lw,	$2003 |;
: lbu,	$4003 |;
: lhu,	$5003 |;

: sb,	$0023 |;
: sh,	$1023 |;
: sw,	$2023 |;

: beq,  $0063 |;
: bne,	$1063 |;
: blt,	$4063 |;
: ble,	$5063 |;
: bltu,	$6063 |;
: bgeu,	$7063 |;


: auipc,	$00000017 |;
: lui,		$00000037 |;

: c.jal,	$2001 |;
: c.j,		$A001 |;

 
: c.sub, $8c01 |;
: c.xor, $8c21 |;
: c.or,  $8c41 |;
: c.and, $8c61 |;
: c.swsp, $c002 |;
: c.lwsp, $4002 |;

: c.lw,  $4000 |;
: c.flw, $6000 |;
: c.sw,  $c000 |;

: c.srli, $8001 |;
: c.srai, $8401 |;
: c.andi, $8801 |;


: c.beqz,  $C001 |;
: c.bnez,  $E001 |;

: jalr; $67 |;
: jal; $6F |;


EXPORT


: MINST_L
  SWAP 2+ TUCK W@ 16 << OR

  DUP TO OPCODE

  DUP 0xFE00707F AND TO OPCODE
   
  ['] R-TYPE. TO OP. mul,
  
 add,	 sub,	 sll,	 slt,	 sltu,	 xor,	 srl,	 sra,	 or, 	 and,	
 mul,	 mulh,	 mulhsu, mulhu, div,	 divu,	 rem,	 remu,	

  DUP 0x0000707F AND TO OPCODE
 ['] I-TYPE. TO OP.  addi,  slli,   slti,	 sltiu,	 xori,	 srli,	 ori,	 andi,
 ['] L-TYPE. TO OP.  LB, LH, LW, LBU, LHU,
 ['] S-TYPE. TO OP.  SB, SH, SW,
 ['] B-TYPE. TO OP.  beq, bne, blt, ble, bltu, bgeu,
 ['] JR-TYPE. TO OP.  jalr;


  DUP 0x0000007F AND TO OPCODE
 ['] U-TYPE. TO OP.  auipc, lui,		
 ['] J-TYPE. TO OP.  jal;
 
 ."  " 16 >> H. TAB ." L???" 
;

: MINST_ ( [INST] [INST] -- [INST'] )
  W@
\
  DUP TO OPCODE
\  DUP ." Q" H.
  DUP 3 AND 3 = IF MINST_L BREAK


    ['] NOP_TP TO OP.	NOP, UNIMP, RET, EBREAK,
     

  DUP $FC63 AND TO OPCODE
\  DROP EXIT

    ['] C.OR_TP TO OP.	C.SUB, C.XOR, C.OR, C.AND,

  
  DUP $F07F AND TO OPCODE
     ['] SL64_TP TO OP. c.slli64,
     

  DUP 0xEF83 AND TO OPCODE
  
  OPCODE $6001 = IF TAB ." c.lui	zero,0x"
 DUP 2 >> $1F AND
 SWAP 5 >>  $20 AND OR $20 - $FFFFF AND  H.-
 BREAK

  DUP 0xEC03 AND TO OPCODE
  ['] C.SRI_TP TO OP. C.SRLI, C.SRAI, C.ANDI,

  DUP 0xE003 AND TO OPCODE

  OPCODE $8002  \ ." CAD=" 2DUP H. H.
  = IF  TAB TAB
	DUP $1000 AND	IF ." c.add	" ELSE ." c.mv	" THEN
 DUP 7 >> $1F AND REG,.
  2 >> $1F AND REG.

  BREAK
 ['] C.SWSP_TP TO OP. C.SWSP,
 ['] C.LWSP_TP TO OP. C.LWSP,
 ['] C.LW_TP TO OP. c.lw, c.flw, c.sw, 

 ['] CLUI_TP TO OP. c.addi,  c.li, c.lui, c.andi, c.slli,

 ['] C.J_TP TO OP. c.j, c.jal,
 
  ['] C.BZ_TP TO OP.  c.beqz, c.bnez,



 DROP TAB TAB ." ???"

 ;

: H.H C@ 2 H.N SPACE ;

: AMINST ( [INST] -- [INST+2] )

  1 INVERT AND
  BASE M@ >R DECIMAL
  DUP >R

  R@ ADDR_OFF + 8 H.R TAB
 
	R@	 W@ 4 H.N
\	R@	 H.H	R@ 1+	H.H

  R@  MINST_

  RDROP

 2+
  R> BASE M!
 ;

: DISA
 BEGIN CR
 AMINST
 KEY 0x20 OR 'q' =
 UNTIL
;

;MODULE


\eof

