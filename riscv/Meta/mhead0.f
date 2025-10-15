REQUIRE WORDS tools/spf_words.f

: MC! C! ;
: MC@ C@ ;
: MW! W! ;
: MW@ W@ ;
: ML@ L@ ;
: ML! L! ;
:  M!  ! ;
:  M@  @ ; 
: M2@ 2@ ; 
: M0! 0! ;
: M_0! 0! ;
: M+! +! ;
:  M+  + ;
: M2+  2+ ;
:  M-  - ;
: M1+! 1+! ;
: M-/ / ;
: M, , ;
: ML, L, ;
: MDP! DP ! ;

: MNEGATE NEGATE ; 
: MOVER OVER ;
: MAND AND ;
: MCR CR ;
\- MH.
 : MH. H. ;
: MHERER H. ;
: M_. . ;
: M.S .S ;
: MDEPTH DEPTH ;
: MHEX HEX ;
: MKEY KEY ;
: MKEY? KEY? ;
: MEXECUTE EXECUTE ;
: MWDS WORDS ;
: MCMOVE CMOVE ;
: MFILL FILL ;
: \ZEOF ;
: MCELLS CELLS ;
: MCELL+ CELL+ ;
: MCELL CELL ;
: MALLOT ALLOT ;

VARIABLE T-DP


[IFNDEF] /*
: /*  ( -- )
  BEGIN
    PARSE-NAME DUP 0=
    IF  NIP  REFILL   0= IF DROP TRUE THEN
    ELSE  S" */" COMPARE 0=  THEN
  UNTIL
; IMMEDIATE
[THEN]


: DUPCOUTSET DUP TO H-STDOUT ;
: COUTRESTORE [ H-STDOUT LIT, ] TO H-STDOUT ;

0 VALUE TLASTFLG
0 VALUE THS_ID
: THS_CREATE
    R/W CREATE-FILE THROW TO THS_ID
   0 TO TLASTFLG
;

: THS_CLOSE
  THS_ID 0= IF BREAK
  THS_ID   CLOSE-FILE THROW 
 0 TO THS_ID
;

: THS>
  THS_ID DUPCOUTSET DROP INTERPRET
 COUTRESTORE
;

: DO_THS ( cfa -- )
   THS_ID 0= IF DROP BREAK
  THS_ID DUPCOUTSET DROP  EXECUTE
 COUTRESTORE
;
[IFNDEF] CHAR-UPPERCASE
: CHAR-UPPERCASE ( c -- c1 )
  DUP [CHAR] a [CHAR] z 1+ WITHIN IF 32 - EXIT THEN
;
[THEN]

: UP_TYPE ( adr len  -- )
  0 ?DO COUNT
\  DUP  '"' = IF '\'  EMIT THEN
\  DUP  '\' = IF '\'  EMIT THEN
 CHAR-UPPERCASE EMIT
	 LOOP DROP ;

: $THS (  adr len -- )
   THS_ID 0= IF 2DROP BREAK
 
 ['] TYPE DO_THS
;

: $ETHS (  c -- )
   THS_ID 0= IF 2DROP BREAK
 ['] EMIT DO_THS
;

: $UPTHS (  adr len -- )
   THS_ID 0= IF 2DROP BREAK
 ['] UP_TYPE DO_THS
;

VECT VGTYPE

' UP_TYPE TO VGTYPE

: $:THS ( adr len -- )
   THS_ID 0= IF 2DROP BREAK
   BL $ETHS 
  TLASTFLG  IF S"  TIMMED" $THS THEN  0 TO TLASTFLG 
  ['] CR DO_THS

\ '$' $ETHS  CURSTR @ ['] H. DO_THS \ 0 (D.)  ['] TYPE DO_THS 

 $THS

 >IN M@ PARSE-NAME $UPTHS
 >IN M!
 ;



: C$:THS ( n adr len -- n )
   THS_ID 0= IF 2DROP BREAK

   BL $ETHS 
  TLASTFLG  IF S"  TIMMED" $THS THEN  0 TO TLASTFLG 

  ['] CR DO_THS

\ '$' $ETHS  CURSTR @ ['] H. DO_THS \ 0 (D.)  ['] TYPE DO_THS 

 2 PICK '$' $ETHS ['] H. DO_THS \ 0 (D.)  ['] TYPE DO_THS 

 $THS

 >IN M@ PARSE-NAME $UPTHS
 >IN M!
;

: :#THS		S" THEADER: "	$:THS ;
: VAR:THS	S" TVARIABLE: "	C$:THS ;
: CON:THS	S" TCONSTANT: "	C$:THS ;
