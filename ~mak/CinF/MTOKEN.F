REQUIRE { ~mak\locals4.f

C" SCAN" FIND NIP 0=
[IF]
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

: PASS\N
  BEGIN  SkipDelimiters  EndOfChunk   
  WHILE REFILL 0= IF TRUE  EXIT THEN
  REPEAT      FALSE ;

: MTOKEN { CTABL -- ADDR LEN }
  PASS\N
  IF CharAddr  0  EXIT THEN
  CTABL COUNT PeekChar SCAN NIP 
  IF CharAddr 1 DUP >IN +! EXIT THEN
  CharAddr
  BEGIN 1 >IN +!
     EndOfChunk 
     IF  TRUE
     ELSE   CTABL COUNT  PeekChar SCAN NIP 
     THEN
  UNTIL   CharAddr OVER - \ ." MT=<" 2DUP TYPE ." >"
;

: SMTOKEN { TABL Tlen -- ADDR alen }
  PASS\N
  IF CharAddr  0  EXIT THEN
  TABL Tlen PeekChar SCAN NIP 
  IF CharAddr 1 DUP >IN +! EXIT THEN
  CharAddr
  BEGIN 1 >IN +!
     EndOfChunk 
     IF  TRUE
     ELSE   TABL Tlen  PeekChar SCAN NIP 
     THEN
  UNTIL   CharAddr OVER - \ ." MT=<" 2DUP TYPE ." >"
;

\EOF TEST
CREATE ZZZ HERE
  S"  ~!@#$%^&*()+|{}:<>?`-=\[];',./"  S",
  0x9 C, CHAR " C,
HERE OVER - SWAP C!

\ CREATE ZZZ HERE 0 C, BL C, 0x9 C, CHAR , C,
\ HERE OVER - SWAP C!
: TEST BEGIN ZZZ MTOKEN DUP 0= IF EXIT THEN DUP .
 ." {" TYPE KEY DROP ." }" AGAIN ;
TEST &+ QQQQ,WWWW EEEE
RRR$	TTTTTT
YYYYYYYYY UUUUU
IIII OOOOOO
