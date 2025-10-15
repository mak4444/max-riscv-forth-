
MODULE: RISCV_MOD

[IFNDEF] M! : M! ! ; [THEN]

ALSO FORTH

' NOOP VALUE INCT_V

: A; 
 INCT_V
 ['] NOOP TO INCT_V
 EXECUTE
 ;

[UNDEFINED] ASM_INTERPRET
[IF]  : ASM_INTERPRET INTERPRET ;
[THEN]

: P:_INTERPRET ( cfa -- ... )
    >R  A;  R> TO INCT_V
       >IN M@ >R 
\ + TRACE  ." >ASM_INT"
\ + TRACE TRGO TRACE
    ASM_INTERPRET
               R> >IN M@ >R >IN M!  \ монипул€ции с >IN ради обработчика
       A;      R> >IN M!           \ ошибок
;

: P: >IN M@ '
  SWAP  >IN M! PARSE-NAME 1- CREATED , POSTPONE \
 DOES>
\ SOURCE TYPE CR \ HERE H. CR
\ F7_ED
DEPTH 1-
 TO R-DEPTH
 M@ P:_INTERPRET
\ .S CR
 ;

PREVIOUS
P: AUIPC,
P: LUI,

P: C.MV,
P: MV,

P: C.LI,
P: C.LUI,
P: LI,

P: C.ADDI,

P: UNIMP,
P: RET,
P: NOP,
P: ECALL,

P: ADD,
P: SUB,
P: SLL,
P: SLT,
P: SLTU,
P: XOR,
P: SRL,
P: SRA,
P: OR, 
P: AND,
P: AND,
P: MUL,
P:	MULH,
P:	MULHSU,
P:	MULHU,
P:	DIV,
P:	DIVU,
P:	REM,
P:	REMU,

P: NEG,

P: C.ADD,

P: C.SUB,
P: C.XOR,
P: C.OR,
P: C.AND,


P: ADDI,
P: SLTI,
P: SLTIU,
P: XORI,
P: ORI,
P: ANDI,

P: SEQZ,

P: NOT,

P: CSRRWI,
P: CSRRSI,
P: CSRRCI,

P: C.ADDI4SPN,
P: C.ADDI16SP,

P: C.LWSP,
P: C.SWSP,

P: C.SLLI,
P: C.SRLI,
P: C.SRAI,
P: C.ANDI,


P: C.SLLI64,
P: SLLI,
P: SRLI,
P: SRAI,

P: LB,
P: LH,
P: LBU,
P: LHU,
P: LW,
P: C.LW,
P: C.SW,
P: FLW,
P: C.FLW,
P: C.FLWSP,
P: LA,

P: SB,
P: SH,
P: SW,

P: C.JAL,
P: JAL,
P: CALL,
P: JALR,
P: C.J,
P: J,
P: JR,

P: BEQ,
P: BNE,	
P: BLT,	
P: BLE,	
P: BLTU,
P: BGEU,
P: BLTU,

P: BGT,

P: BEQZ,
P: BNEZ,
P: BLTZ,
P: BLEZ,
P: BGTZ,
P: BGEZ,

P: C.BEQZ,
P: C.BNEZ,
P: C.NOP#

P: .WORD,
P: .SHORT,

;MODULE
