REQUIRE WAPI2: lib\WAPI.4

KERNEL32DLL WAPI2: GetConsoleScreenBufferInfo GetConsoleScreenBufferInfo
KERNEL32DLL WAPI2: SetConsoleCursorPosition SetConsoleCursorPosition

CREATE CONSOLE_SCREEN_BUFFER_INFO 22 ALLOT
: SBI CONSOLE_SCREEN_BUFFER_INFO 20 DUMP ;


: COORD ( x y -- COORD )
  16 LSHIFT OR 
;

: AT-XY-G ( x y -- )
\ 
  COORD H-STDOUT SWAP
  SetConsoleCursorPosition DROP \ ERR THROW
;

: AT-XY ( x y -- )
  H-STDOUT CONSOLE_SCREEN_BUFFER_INFO
  GetConsoleScreenBufferInfo  DROP
  CONSOLE_SCREEN_BUFFER_INFO $C + W@ +

 AT-XY-G
;

: SETXY AT-XY ;


: AT-XY? ( -- x y )
\ 
  H-STDOUT CONSOLE_SCREEN_BUFFER_INFO
  GetConsoleScreenBufferInfo  DROP
  CONSOLE_SCREEN_BUFFER_INFO 4 +
  DUP W@
  OVER 2 + W@ 
 ROT 8 + W@ - ;

: GETXY AT-XY? ;
