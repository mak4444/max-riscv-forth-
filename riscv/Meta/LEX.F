
REQUIRE NUMBER? ~mak\LIB\fpcnum.f 
\ REQUIRE MTOKEN ~mak/CinF/MTOKEN.F

[IFNDEF]   SCAN
: SCAN (  addr len char -- addr' len')
    >R                  \ put char on return stack
    BEGIN
      DUP
    WHILE ( len<>0)    
      OVER C@ R@ <>     \ fetch char, compare to return stack  
    WHILE ( R@<>char)   
      1 /STRING         \ cut off the first character. (does addr+1, len-1)
    REPEAT
    THEN
    R> DROP 
;
[THEN]

[IFNDEF] MTOKEN

: PASS\N
  BEGIN  SkipDelimiters  EndOfChunk   
  WHILE REFILL 0= IF TRUE  EXIT THEN
  REPEAT      FALSE ;

: MTOKEN ( TABL -- ADDR N )
  PASS\N

  IF DROP CharAddr  0  EXIT THEN
  DUP >R COUNT PeekChar SCAN NIP
  IF RDROP CharAddr 1 DUP >IN +! EXIT THEN
  CharAddr
  BEGIN 1 >IN +!
     EndOfChunk
     IF  TRUE
     ELSE   R@ COUNT PeekChar 
   SCAN NIP
     THEN 
  UNTIL   CharAddr OVER -

  RDROP
;
[THEN]

\- MW@ : MW@ W@ ;

MODULE: RISCV_MOD


\ VECT LEX_EXECUTE ' EXECUTE TO LEX_EXECUTE
 : LEX_EXECUTE EXECUTE ;

0 VALUE ?LEX
0 VALUE ?,

CREATE ASMARM_TABL HERE
  S"  ,#(){}"  S",
  0x9 C, CHAR " C,
HERE OVER - 1- SWAP C!

[IFNDEF]  M@ :  M@  @ ; [THEN]
[IFNDEF] MC@ : MC@ C@ ; [THEN]

: ASM_INTERPRET ( -> )
  TRUE  TO ?LEX
  BEGIN \ PARSE-NAME
      SkipDelimiters  EndOfChunk
     IF 0 0 
     ELSE  ASMARM_TABL MTOKEN
     THEN  DUP
  WHILE
	SFIND ?DUP
    IF
         STATE M@ =
        IF COMPILE, ELSE LEX_EXECUTE THEN
    ELSE ?SLITERAL
    THEN
    ?STACK
  REPEAT 2DROP
  FALSE  TO ?LEX
 0 TO ?, ;

CREATE ITBUF.W 11 ALLOT

: tail_INTERPRET ( -> )
\ CR ." S=" SOURCE TYPE
  BEGIN  PARSE-NAME DUP
  WHILE
	SFIND  ?DUP
    IF
         STATE M@ =
        IF COMPILE, ELSE LEX_EXECUTE THEN
    ELSE
         S" NOTFOUND" SFIND 
         IF EXECUTE
         ELSE 2DROP ?SLITERAL THEN
    THEN
    ?STACK
  REPEAT 2DROP
;

0 VALUE ?DOADSP

: DOADSP ( -- )
  TRUE TO ?DOADSP
  tail_INTERPRET
  ?DOADSP 0= IF BREAK
  BEGIN  REFILL ?DOADSP AND
  WHILE  tail_INTERPRET
  REPEAT ;

: ASM_LEX
   ['] DOADSP  CATCH
 FALSE TO ?LEX ?DUP
 IF THROW THEN ;

0 VALUE ?MCR

: (  ?LEX 0=  IF POSTPONE	(	BREAK 0[[ ; IMMEDIATE
: )  ]] ;
: ,  ?LEX 0=  IF		,	THEN -1 TO ?, ;
\ : @  ?LEX 0=  IF		@ 	BREAK POSTPONE	\ ;
: //  POSTPONE	\ ; IMMEDIATE
: #   POSTPONE	\ ; IMMEDIATE
: "   POSTPONE	S" ;

: ASM_END
  ASM_END
 MOREPASSFLG 0= IF FALSE TO ?DOADSP THEN
;

: END-CODE
 ASM_END ;


EXPORT


: RV_ASM_BIG RV_ASM_BIG
 ?DOADSP 0= IF ASM_LEX THEN
 ;

: CODE 
    CODE_T2
 ?DOADSP 0= IF ASM_LEX THEN
	;

;MODULE
