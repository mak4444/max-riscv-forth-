
: T_C@?  ( -- n )
       COM-CNT ;

VECT T_C@>  ( -- c )
' COM-GET TO T_C@>


: TWRITE
 TO_COM
 ;

:  COM? \ F7_ED
\  COM-LAST 0x7 AND 0= IF CR THEN
  ." Com=" COM-LAST DUP  BL < IF ." *"  ELSE DUP EMIT THEN  ."  " H. FJB
 ;

CREATE EXE_TAB
0x100 
 ' COM? ,  1- DUP [IF]  >IN 0! [THEN]
DROP

CREATE C!_BUFF $FF C, #C! C, 6 ALLOT 
CREATE C@_BUFF $FF C, #C@ C, 4 ALLOT 

CREATE W!_BUFF $FF C, #W! C, 7 ALLOT 
CREATE W@_BUFF $FF C, #W@ C, 4 ALLOT 

CREATE  !_BUFF $FF C, #! C, 9 ALLOT 
CREATE  @_BUFF $FF C, #@ C, 4 ALLOT 

CREATE  Q_BUFF $FF C, #Q C, 4 ALLOT 

CREATE  EXEC_BUFF $FF C, #EXEC C, 8 ALLOT 

VARIABLE KEY?_BUFF
VARIABLE KEY_BUFF

0xF00  VALUE #WAIT
VARIABLE WAIT_COUNT

VECT KEY_ABORT

: KEY_ABORT_ ( c -- )
 [CHAR] Q = IF -28 THROW THEN ;

' KEY_ABORT_ TO KEY_ABORT

: T-C@?
\ F7_ED
\ EXIT
	#WAIT WAIT_COUNT !
	BEGIN  T_C@? 0=
	WHILE	WAIT_COUNT @ 0= 0 AND ABORT" port is't answered" \ !!!!!!
		-1  WAIT_COUNT +!
		WAIT_COUNT @ 0x1FF AND 0=
		   IF
		   FJB KEY? IF KEY DUP KEY_BUFF C!	KEY_ABORT
		   -1 KEY?_BUFF !
		   THEN   1 SLEEP
		THEN
	REPEAT ;

: T-C@  ( -- c )
  T-C@?  T_C@> ;

: PWAIT ( cfa -- )
    >R \ F7_ED
    BEGIN  EXE_TAB 
 BEGIN  T-C@  DUP $FF <>
 WHILE   EMIT 
 REPEAT  DROP
  T-C@  \ ." ^" DUP H.


CELLS + @ DUP
\ ." PW=" DUP H. 
  R@ <> \ FJB
    WHILE  EXECUTE 
    REPEAT
    EXECUTE RDROP ;
