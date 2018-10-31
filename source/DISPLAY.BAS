'********** Display.Bas - slide show display program for QuickScreens

'by Don Malin and Ethan Winer

'Copyright (c) 1987 - 1989 Crescent Software Inc.

DEFINT A-Z

DECLARE SUB Lib2Scrn (NameInLib$, ScrnLib%(), MonoCode%, ReqLines%, ErrorCode%)
DECLARE SUB LoadScreen (ScrName$, MonoCode%, Attribute%, ErrorCode%)
DECLARE SUB QBLoad (FileName$, SEG Element AS ANY)
DECLARE FUNCTION Exist% (FileName$)
DECLARE FUNCTION FileSize& (FileName$)

Tmr! = 1

CLS
DIM Cmd$(1000)                          'will hold the list of commands
REDIM ScrnLib(0)                        'holds the screen library, and will
                                        '  be redimensioned sufficiently later

DEF SEG = 0                             'look in low memory
IF PEEK(&H463) = &HB4 THEN              'check the monitor type
   LOCATE , , 0, 12, 13                 'the mono cursor size if Pause is used
   MonoCode = 3
ELSE
   LOCATE , , 0, 6, 7                   'color cursor size
   MonoCode = 0
END IF

Temp$ = COMMAND$                        'Look for "/B" in Command line Arg.
X = INSTR(Temp$, "/B")
IF X THEN
   MID$(Temp$, X) = "  "
   MonoCode = 3
END IF


FileName$ = LTRIM$(RTRIM$(Temp$))       'see if a file was already specified
IF FileName$ = "" THEN FileName$ = "Display.Cmd"   'no, use the default name


OPEN FileName$ FOR INPUT AS #1          'read in the commands
WHILE NOT EOF(1) AND X < 1000           'until 1000 lines or the file ends
    X = X + 1
    LINE INPUT #1, Cmd$(X)
    IF RIGHT$(Cmd$(X), 1) = ":" THEN Cmd$(X) = UCASE$(Cmd$(X))
WEND
CLOSE
Number.Of.Commands = X                  'keep the number of commands we read


FOR X = 1 TO Number.Of.Commands         'step through each command one by one
    Cmd$(X) = LTRIM$(RTRIM$(Cmd$(X)))   'strip "B"oth left and right blanks
    S = INSTR(Cmd$(X), "'")
    IF S = 0 THEN S = INSTR(UCASE$(Cmd$(X)), "REM")
    IF S THEN Cmd$(X) = LEFT$(Cmd$(X), S - 1)

    S = INSTR(Cmd$(X), " ")
    IF S THEN
       Cmd$ = LEFT$(Cmd$(X), S - 1)
    ELSE
       Cmd$ = Cmd$(X)
    END IF

    Cmd$ = UCASE$(Cmd$)

   
    SELECT CASE Cmd$                    'brute force method
    
       CASE "LOADFILE"
       
          '----- Load and Display a screen file
          ScrName$ = LTRIM$(MID$(Cmd$(X), 9))   'isolate File name from command
          IF INSTR(ScrName$, ".") = 0 THEN ScrName$ = ScrName$ + ".Scr"
   
          LoadScreen ScrName$, MonoCode, 0, Flag

          IF Flag THEN
             PLAY "L32EC"
             CLS
          END IF
          SELECT CASE Flag              'Check for errors displaying screen
             CASE 1
                PRINT "Can't find " + ScrName$ + ".Scr"
             CASE 2
                PRINT "Screen cannot fit on this monitor."
             CASE 3
                PRINT ScrName$ + " is not a compatible screen."
             CASE ELSE
          END SELECT
          

       CASE "LOADLIB"
       
          LibName$ = LTRIM$(MID$(Cmd$(X), 8))   'isolate lib name from command
          IF INSTR(LibName$, ".") = 0 THEN LibName$ = LibName$ + ".QSL"
          Flag = 0
          IF Exist(LibName$) THEN
             DimSize& = (FileSize&(LibName$) - 7) \ 2

             OPEN LibName$ FOR BINARY AS #1
             GET #1, 10, Code%
             CLOSE #1

             IF DimSize& <= 32766 AND Code = 26 THEN
                REDIM ScrnLib(DimSize&)

                QBLoad LibName$, ScrnLib(0)
             ELSE
                Flag = -1
             END IF
          ELSE
             Flag = -1
          END IF

          IF Flag THEN
             PLAY "L32EC"
             CLS
             PRINT "Library "; LibName$; " not found - press any key to end."
             WHILE INKEY$ = "": WEND
             END
          END IF
          
          
       CASE "DISPLAY"
       
          ScrName$ = LTRIM$(MID$(Cmd$(X), 8))   'isolate the screen name

          S = INSTR(ScrName$, " ")
          IF S THEN
             Attribute = VAL(MID$(ScrName$, S + 1))
             ScrName$ = LEFT$(ScrName$, S - 1)
          END IF

          Lib2Scrn ScrName$, ScrnLib(), MonoCode, Attribute, Flag
          IF Flag THEN
             PLAY "L32EC"
             CLS
             PRINT "Screen "; ScrnName$; " not found - press any key to continue."
             WHILE INKEY$ = "": WEND
          END IF

          
       CASE "PAUSE"
       
          Delay! = VAL(MID$(Cmd$(X), 6)) * Tmr!
          X! = TIMER                            'pause for time specified
          WHILE TIMER < Delay! * Tmr! + X!
             GOSUB CheckKeys
          WEND

          
       CASE "CLS"

          CLS

          
       CASE "MESSAGE"

          Start = INSTR(Cmd$(X), CHR$(34)) + 1  'find the leading quote
          MsgLen = LEN(Cmd$(X)) - Start         'get the message length
          PRINT MID$(Cmd$(X), Start, MsgLen);   'print just the good stuff

          
       CASE "KEYPRESS"

          LOCATE , , 1                          'turn on the cursor
          WHILE INKEY$ = "": WEND               'wait for a key press
          LOCATE , , 0                          'turn the cursor off again


       CASE "LOCATE"

          V = VAL(MID$(Cmd$(X), 7))             'get the line number
          Comma = INSTR(Cmd$(X), ",")           'find the comma
          H = VAL(MID$(Cmd$(X), Comma + 1))     'get the column number
          LOCATE V, H                           'do the locate

          
       CASE "COLOR"

          FG = VAL(MID$(Cmd$(X), 6))            'get the foreground
          Comma = INSTR(Cmd$(X), ",")           'find the comma
          BG = VAL(MID$(Cmd$(X), Comma + 1))    'get the background
          COLOR FG, BG                          'set the color

          
       CASE "GOTO"

          Search$ = MID$(Cmd$(X), 5)            'isolate label from command
          Search$ = UCASE$(Search$)             'capitalize to ensure a match
          Search$ = LTRIM$(RTRIM$(Search$))
          Search$ = Search$ + ":"               'add colon to ensure a match
          FOR Y = 1 TO Number.Of.Commands       'search for the line label
              IF Search$ = Cmd$(Y) THEN         'we found it
                 X = Y - 1                      'force command counter there
                 EXIT FOR
              END IF
          NEXT

          
       CASE ELSE

          IF LEN(Cmd$) AND INSTR(Cmd$, ":") = 0 THEN
             PLAY "L32EC"                          'oops, not recognized
             CLS
             PRINT CHR$(34); Cmd$(X); CHR$(34); " is an invalid command - press any key.";
             WHILE INKEY$ = "": WEND
          END IF
          
    END SELECT

    GOSUB CheckKeys

NEXT

LOCATE 23                                       'don't let the screen scroll
END


CheckKeys:

    Ky$ = INKEY$
    IF LEN(Ky$) THEN
       IF Ky$ = CHR$(27) THEN
          X = Number.Of.Commands
          Delay! = 0
       ELSEIF Ky$ = "+" THEN
          IF Tmr! > 0 THEN Tmr! = Tmr! - .1

       ELSEIF Ky$ = "-" THEN
          IF Tmr! < 2 THEN Tmr! = Tmr! + .1

       ELSEIF Ky$ = CHR$(13) THEN
          Tmr! = 1

       ELSE
          DO
          LOOP UNTIL LEN(INKEY$)
       END IF
    END IF

RETURN
