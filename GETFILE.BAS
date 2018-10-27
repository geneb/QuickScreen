'*********** GetFile.Bas
'$SubTitle:'File Name Input Dialog Box and File Name Utilities'
'Copyright  (c) 1989 Crescent Software
'written by Don Malin

DEFINT A-Z

'$INCLUDE: 'DialType.Bi'

DIM SHARED N, X, Temp, Work$


DECLARE SUB Box0 (UlRow, UlCol, LrRow, LrCol, Char, Colr)
DECLARE SUB ButtonPress (Button, Status, Count, X, Y)
DECLARE SUB CDir (Path$)
DECLARE SUB ConcaveFrame (UlR%, UlC%, BrR%, BrC%)
DECLARE SUB DialogBox (Title$, PrmtAns$(), Dl() AS Dialog, Mult$(), ExitCode%)
DECLARE SUB LoadDir (FileSpec$, DirCnt, Count)
DECLARE SUB EditLine (Edit$, Bg%, En%, Code%, SCode%, MRow%, CurCol%)
DECLARE SUB ErrMsg (Text$)
DECLARE SUB Explode (UlR%, UlC%, BrR%, BrC%)
DECLARE SUB FCreate (FileName$)
DECLARE SUB FullNameB (FileName$)
DECLARE SUB GetCursor (X, Y, Button)
DECLARE SUB GetEquip (Floppies%, Parallels%, Serials%)
DECLARE SUB GetVMode (Mode, Page, PageSize, Rows, Columns)
DECLARE SUB HideCursor ()
DECLARE SUB KillFile (FileName$)
DECLARE SUB ListBox (A$(), Start%, Rows%, Cols%, Action%, SCode%, Row%, Col%, Choice%)
DECLARE SUB MScrnSave (UlRow, UlCol, LrRow, LrCol, SEG BufAdr)
DECLARE SUB MScrnRest (UlRow, UlCol, LrRow, LrCol, SEG BufAdr)
DECLARE SUB MPaintBox (UlRow, UlCol, LrRow, LrCol, Colr)
DECLARE SUB MQPrint (Text$, Colr)
DECLARE SUB Pause (NumTicks)
DECLARE SUB ReadFile (BYVAL Address)
DECLARE SUB ReadDir (BYVAL Address)
DECLARE SUB SetDrive (Drive$)
DECLARE SUB ShowCursor ()
DECLARE SUB SortStr (BYVAL Address, NumEls, Direction)

DECLARE FUNCTION AlertUser% (Message$, Prmt1$, Prmt2$, Prmt3$)
DECLARE FUNCTION BaseName$ (FileName$)
DECLARE FUNCTION DCount% (DirSpec$)
DECLARE FUNCTION DosError% ()
DECLARE FUNCTION ErrorMsg$ (ErrorNum)
DECLARE FUNCTION Exist% (FileSpec$)
DECLARE FUNCTION ErrorPrompt% ()
DECLARE FUNCTION FCount% (FileSpec$)
DECLARE FUNCTION FileNameOk% (FileName$)
DECLARE FUNCTION GetDir$ (Drive$)
DECLARE FUNCTION GetDrive% ()
DECLARE FUNCTION GoodDrive% (Drive$)
DECLARE FUNCTION LastDrive% ()
DECLARE FUNCTION MaxInt% (Arg1, Arg2)
DECLARE FUNCTION MGetKey% (Row%, Col%)
DECLARE FUNCTION MinInt% (Arg1, Arg2)
DECLARE FUNCTION NoPath$ (FileName$)
DECLARE FUNCTION ParsPath$ (FileSpec$)
DECLARE FUNCTION QInstrB% (Start%, Temp$, Search$)
DECLARE FUNCTION StripPath$ (FileName$)
DECLARE FUNCTION WhichError% ()

REDIM Array$(0)                             'Directory/File Name array
'CALL GetFile("*.*", FileSpec$)

FUNCTION AlertUser (Message$, Prmt1$, Prmt2$, Prmt3$) STATIC

    V = CSRLIN
    ShowCursor

    REDIM Temp$(10)

    Start = 1
    El = 0
    TotLen = LEN(Message$)
    MesLen = 0
    DO
       X = INSTR(Start, Message$, CHR$(20))
       IF X = 0 THEN X = TotLen + 1

       El = El + 1
       Temp$(El) = MID$(Message$, Start, X - Start)

       IF X - Start > MesLen THEN MesLen = X - Start

       Start = X + 1
    LOOP WHILE Start < TotLen

   
    IF LEN(Prmt3$) THEN
       Flds = 4
    ELSEIF LEN(Prmt2$) THEN
       Flds = 3
    ELSE
       Flds = 2
    END IF

    REDIM Msg1(Flds + El - 1) AS Dialog
    REDIM Fd$(Flds + El - 1, 1)
    REDIM Mult$(0)

    ButLen = (Flds - 1) * 4 + LEN(Prmt1$) + LEN(Prmt2$) + LEN(Prmt3$)
    IF (Flds - 2) * 2 + ButLen > MesLen THEN
       TotLen = (Flds - 2) * 2 + ButLen
       Start = 0
       X = 2
    ELSE
       TotLen = MesLen
       X = (MesLen - ButLen) \ Flds
       Start = X
    END IF

    FOR N = 1 TO El
       Msg1(N).DRow = N
       Msg1(N).DCol = (TotLen - MesLen) \ 2 + 2
       Msg1(N).DTyp = 6
       Fd$(N, 0) = Temp$(N)
       Temp$(N) = ""
    NEXT
    ERASE Temp$

    Y = El
    FOR N = 2 TO Flds
       El = N + Y - 1
       Msg1(El).DRow = Y + 1
       Msg1(El).DTyp = 5

       SELECT CASE N
          CASE 2
             Fd$(El, 0) = Prmt1$
             Msg1(El).DCol = Start + 2
          CASE 3
             Fd$(El, 0) = Prmt2$
             Msg1(El).DCol = Msg1(El - 1).DCol + LEN(Prmt1$) + 5 + X
          CASE ELSE
             Fd$(El, 0) = Prmt3$
             Msg1(El).DCol = Msg1(El - 1).DCol + LEN(Prmt2$) + 5 + X
       END SELECT
    NEXT

    WHILE INKEY$ <> "": WEND
    DialogBox "", Fd$(), Msg1(), Mult$(), ExitCode

    ERASE Fd$, Msg1, Mult$
    LOCATE V

    ButtonPress 1, 1, 1, 1, 1


    AlertUser = ExitCode

END FUNCTION

FUNCTION BaseName$ (FileName$) STATIC

    BaseName$ = ""

    IF LEN(FileName$) = 0 THEN EXIT FUNCTION

    N = MaxInt(INSTR(FileName$, ":"), QInstrB%(-1, FileName$, "\"))
    Temp = INSTR(N + 1, FileName$, ".") - 1
    IF Temp = -1 THEN Temp = LEN(FileName$)
    
    BaseName$ = MID$(FileName$, N + 1, Temp - N)

END FUNCTION

SUB ErrMsg (Text$) STATIC

    GetVMode 0, 0, 0, Temp, 0
    LOCATE Temp \ 2 - 1

    Temp = AlertUser(Text$, "Ok", "", "")

END SUB

FUNCTION ErrorPrompt
    ErrorPrompt = 0

    GetVMode 0, 0, 0, Rows, 0
    LOCATE Rows \ 2 - 2

    ErrNum = WhichError%

    SELECT CASE ErrNum

       '----- ReTry Errors
       CASE 24, 25, 27, 57, 71
          ExitCode = AlertUser(ErrorMsg$(ErrNum), "Retry", "Cancel", "")

       CASE ELSE'51, 52, 53, 54, 55, 58, 61, 63, 64, 67, 68, 70, 72, 74, 75, 76
          ExitCode = AlertUser(ErrorMsg$(ErrNum), "Ok", "", "") - 1

    END SELECT

    IF ExitCode = 2 THEN ExitCode = 0

    ErrorPrompt = ExitCode

END FUNCTION

FUNCTION FileNameOk (FileName$)

    Temp = 0
    IF NOT Exist(FileName$) THEN        'see if the file name is valid
       DO
          Again = 0
          FCreate FileName$
          Temp = DosError
          IF Temp THEN
             Again = ErrorPrompt
          ELSE
             KillFile FileName$
          END IF
       LOOP WHILE Again
    END IF

    FileNameOk = Temp = 0

END FUNCTION

SUB FullNameB (FileName$)

   Col = INSTR(FileName$, ":")

   bs = QInstrB(-1, FileName$, "\")

   IF bs = 0 THEN
      Path$ = GetDir$(LEFT$(FileName$, Col))
      IF RIGHT$(Path$, 1) <> "\" THEN Path$ = Path$ + "\"

      FileName$ = LEFT$(FileName$, Col) + Path$ + MID$(FileName$, Col + 1)
   END IF

   IF Col = 0 THEN FileName$ = CHR$(GetDrive%) + ":" + FileName$

   Path$ = ""

END SUB

SUB GetFile (SearchSpec$, FileSpec$)

    SHARED Array$()
    SHARED Drives

    DO
       CurPath$ = CHR$(GetDrive%) + ":"
       IF DosError THEN
          IF ErrorPrompt <> 1 THEN EXIT DO
       END IF
    LOOP WHILE DosError
    IF DosError THEN EXIT SUB
    CurPath$ = CurPath$ + GetDir$("")

    FileSpec$ = SearchSpec$

    GetVMode 0, 0, 0, Rows, 0                   'Get the number of screen rows

    LOCATE , , 0

    UlR = Rows \ 2 - 9
    CONST UlC = 5
    BrR = UlR + 19
    CONST BrC = 76
                                                'Save the underlying Screen
    REDIM ScrBuf(2000)'(BrR - UlR + 2) * (BrC - UlC + 3))

    MScrnSave UlR, UlC, BrR + 1, BrC + 2, ScrBuf(0)
    
    HideCursor

    '----- Draw the box frame
    Explode UlR, UlC, BrR, BrC

    LOCATE UlR, UlC + ((BrC - UlC + 1) \ 2) - 6
    MQPrint "  Open File  ", -1
    MPaintBox UlR + 1, BrC + 1, BrR, BrC + 2, 8
    MPaintBox BrR + 1, UlC + 2, BrR + 1, BrC + 2, 8

    LOCATE UlR + 2, UlC + 2
    MQPrint "File Name: ", BoxClr
    ConcaveFrame UlR + 1, UlC + 13, UlR + 3, BrC - 2
    'Box0 UlR + 1, UlC + 13, UlR + 3, BrC - 2, 1, -1

    Prmt$ = SPACE$(55)
    LSET Prmt$ = FileSpec$
    LOCATE UlR + 2, UlC + 14
    MQPrint Prmt$, BoxClr

    
    LOCATE BrR - 1, UlC + 20
    MQPrint "Þ", 127
    LOCATE , UlC + 21
    MQPrint " Open ", HiLite
    LOCATE , UlC + 27
    MQPrint "Ý", 120

    LOCATE , UlC + 42
    MQPrint "Þ", 127
    LOCATE , UlC + 43
    MQPrint " Cancel ", BoxClr
    LOCATE , UlC + 51
    MQPrint "Ý", 120

    'MQPrint "( Open )              ( Cancel )", BoxClr
    'MPaintBox BrR - 1, UlC + 21, BrR - 1, UlC + 26, HiLite

    Start = 1
    Choice = 2
    Rows = 10
    Cols = 5
    REDIM Array$(1)
    Array$(1) = SPACE$(12)
    LOCATE UlR + 5, UlC + 2
    ListBox Array$(), Start, Rows, Cols, 1, SCode, Row, Col, Choice

    GOSUB GetFiles

    ShowCursor

    Fld = 1


    '----- Main Loop
    DO

        SELECT CASE Fld
           CASE 1
              LOCATE UlR + 2, 1
              Temp$ = RTRIM$(Prmt$)
              SCode = 0
              EditLine Temp$, UlC + 14, BrC - 3, 0, SCode, Row, Col

              Edited = Temp$ <> RTRIM$(Prmt$)

              SELECT CASE SCode
                 CASE 27, 1001                  'Escape
                    FileSpec$ = ""
                    EXIT DO
                 CASE 9, -80                    'Tab, Down
                    Fld = 2
                 CASE -15, -72                  '^Tab, Up
                    Fld = 4
                 CASE 1000                      'Button press
                    GOSUB MousePress
                 CASE ELSE
              END SELECT

              LOCATE , , 0

              IF Edited THEN
                 LSET Prmt$ = Temp$

                 FileSpec$ = RTRIM$(LTRIM$(Prmt$))   'Strip both sides of answer
                 IF RIGHT$(FileSpec$, 1) = ":" OR RIGHT$(FileSpec$, 1) = "\" THEN
                    FileSpec$ = FileSpec$ + SearchSpec$
                 END IF

                 IF INSTR(FileSpec$, "*") = 0 AND INSTR(FileSpec$, "?") = 0 THEN

                    IF QInstrB(-1, FileSpec$, ".") <= QInstrB(-1, FileSpec$, "\") THEN
                       Temp = INSTR(QInstrB(-1, SearchSpec$, "\") + 1, SearchSpec$, ".")
                       IF Temp THEN
                          FileSpec$ = FileSpec$ + MID$(SearchSpec$, Temp)
                       END IF
                    END IF

                    Temp = 0
                    DO
                       IF Exist(FileSpec$) THEN
                          Temp = -1
                          FullNameB FileSpec$
                          Path$ = ParsPath$(FileSpec$)
                          EXIT DO
                       ELSE
                          FileSpec$ = SearchSpec$
                          IF NOT DosError THEN
                             ErrMsg "File Not Found!"
                          ELSE
                             IF ErrorPrompt <> 1 THEN EXIT DO
                          END IF
                       END IF
                    LOOP WHILE DosError
                    IF Temp THEN EXIT DO
                 END IF

                 GOSUB GetFiles
              END IF


           CASE 2

              LOCATE UlR + 5, UlC + 2, 0
              SCode = 0
              ListBox Array$(), Start, Rows, Cols, 3, SCode, Row, Col, Choice

              SELECT CASE SCode
                 CASE 27, 1001                  'Escape
                    FileSpec$ = ""
                    EXIT DO
                 CASE 13
                    IF Choice > DirCnt THEN
                       GOSUB ListChoice
                       IF INSTR(QInstrB(-1, FileSpec$, "\") + 1, FileSpec$, ".") = 0 THEN
                          FileSpec$ = FileSpec$ + "."
                       END IF
                       EXIT DO
                    ELSE
                       GOSUB ListChoice
                    END IF
                 CASE 9                         'Tab
                    Fld = 3
                 CASE -15                       '^Tab
                    Fld = 1
                 CASE 1000                      'Button press
                    GOSUB MousePress
                 CASE ELSE
              END SELECT


           CASE 3
              LOCATE BrR - 1, UlC + 22, 1
              IF SCode = 1000 THEN
                 SCode = 13
                 Pause 2
                 DO
                    GetCursor 0, 0, Buttons
                 LOOP WHILE Buttons
              ELSE
                 SCode = MGetKey(Row, Col)
              END IF
              SELECT CASE SCode
                 CASE 27, 1001                  'Escape
                    FileSpec$ = ""
                    EXIT DO
                 CASE 13, 32
                    GOSUB ListChoice
                    IF Choice > DirCnt THEN EXIT DO

                 CASE 9, -80, -77               'Tab, Down
                    Fld = 4
                 CASE -15, -72, -75             '^Tab, Up
                    Fld = 2
                 CASE 1000                      'Button press
                    GOSUB MousePress
                 CASE ELSE
              END SELECT

           CASE 4
              MPaintBox BrR - 1, UlC + 21, BrR - 1, UlC + 26, BoxClr
              MPaintBox BrR - 1, UlC + 43, BrR - 1, UlC + 50, HiLite
              LOCATE BrR - 1, UlC + 44, 1
              IF SCode = 1000 THEN
                 SCode = 13
                 Pause 2
                 DO
                    GetCursor 0, 0, Buttons
                 LOOP WHILE Buttons
              ELSE
                 SCode = MGetKey(Row, Col)
              END IF
              SELECT CASE SCode
                 CASE 13, 27, 32, 1001              'Escape
                    FileSpec$ = ""
                    EXIT DO
                 CASE 9, -80, -77               'Tab, Down
                    Fld = 1
                 CASE -15, -72, -75             '^Tab, Up
                    Fld = 3
                 CASE 1000                      'Button press
                    GOSUB MousePress
                 CASE ELSE
              END SELECT
              MPaintBox BrR - 1, UlC + 21, BrR - 1, UlC + 26, HiLite
              MPaintBox BrR - 1, UlC + 43, BrR - 1, UlC + 50, BoxClr

        END SELECT

    LOOP


    MScrnRest UlR, UlC, BrR + 1, BrC + 2, ScrBuf(0)

    ERASE ScrBuf, Array$

    SetDrive CurPath$
    CDir CurPath$

    SearchSpec$ = Path$ + SearchSpec$


    Prmt$ = ""
    Path$ = ""
    Work$ = ""
    CurPath$ = ""
    Temp$ = ""

    X& = FRE(Temp$)

    EXIT SUB



GetFiles:

    DO

       '----- Count matching files and load Array$() with Directory info
       LoadDir FileSpec$, DirCnt, Temp

       IF DosError THEN
          IF ErrorPrompt <> 1 THEN EXIT DO
       END IF

    LOOP WHILE DosError

    IF DosError THEN
       FileSpec$ = ""
    ELSE
       LSET Prmt$ = FileSpec$
       LOCATE UlR + 2, UlC + 14
       MQPrint Prmt$, BoxClr
       Path$ = ParsPath$(FileSpec$)         'Parse out the Path Spec.
       SearchSpec$ = StripPath$(FileSpec$)
    END IF

    Choice = Drives - (LEN(Path$) <= 3 AND (Temp > 0 OR DirCnt > Drives))
    LOCATE UlR + 5, UlC + 2
    ListBox Array$(), Start, Rows, Cols, 1, SCode, Row, Col, Choice

RETURN


MousePress:
    SELECT CASE Row
       CASE UlR + 1 TO UlR + 3
          IF Col >= UlC + 13 AND Col <= BrC - 2 THEN Fld = 1
       CASE UlR + 6 TO UlR + 15
          IF Col > UlC + 2 AND Col < BrC - 2 THEN
             Fld = 2
             Edited = 0
             Choice = MinInt(((Col - UlC - 3) \ 13) * Rows + (Row - UlR - 5), UBOUND(Array$))
          END IF

       CASE BrR - 1
          ButtonPress 1, 0, 0, 0, 0
          SELECT CASE Col
             CASE UlC + 20 TO UlC + 27
                Fld = 3
             CASE UlC + 42 TO UlC + 51
                Fld = 4
             CASE ELSE
          END SELECT

       CASE ELSE
    END SELECT
RETURN



ListChoice:
    IF Choice > DirCnt THEN     'File selection?

       FileSpec$ = UCASE$(RTRIM$(Path$ + Array$(Choice)))

    ELSE                        'Drive/Directory Selection
       Work$ = RTRIM$(Array$(Choice))

       IF INSTR(Work$, "[-") THEN 'Look for a Drive icon
          FileSpec$ = MID$(Work$, 3, 1) + ":" + SearchSpec$
       ELSE
          FileSpec$ = Work$ + "\" + SearchSpec$
       END IF

       GOSUB GetFiles
    END IF
RETURN


END SUB

SUB LoadDir (FileSpec$, DirCnt, FilCnt)
    SHARED Array$()
    SHARED Drives
    
    SrchSpec$ = StripPath$(FileSpec$)
    IF LEN(SrchSpec$) = 0 THEN SrchSpec$ = "*.*"

    FullNameB FileSpec$

    SetDrive FileSpec$
    IF DosError THEN
       EXIT SUB
    ELSE
       D = GetDrive%
    END IF

    Path$ = ParsPath$(FileSpec$)
    IF LEN(Path$) > 3 THEN Path$ = LEFT$(Path$, LEN(Path$) - 1)
    CDir Path$
    IF DosError THEN EXIT SUB

    Path$ = CHR$(D) + ":" + GetDir$("")
    IF LEN(Path$) > 3 THEN Path$ = Path$ + "\"
    FileSpec$ = Path$ + SrchSpec$

'
    REDIM Drv(1 TO 26) AS STRING * 1
    Drives = 0
    FOR N = 65 TO 65 + 25
       IF N <> D THEN
          IF GoodDrive(CHR$(N)) THEN
             Drives = Drives + 1
             Drv$(Drives) = CHR$(N)
          END IF
       END IF
    NEXT

    IF LEN(Path$) > 3 THEN Drives = Drives + 1

    DirSpec$ = Path$ + "*.*"
    DirCnt = DCount(DirSpec$) + Drives

    FilCnt = FCount(FileSpec$)
    REDIM Array$(FilCnt + DirCnt)

    FOR N = 1 TO DirCnt + FilCnt
        Array$(N) = SPACE$(12)
    NEXT

    IF FilCnt THEN
       Array$(DirCnt) = SrchSpec$
       ReadFile VARPTR(Array$(DirCnt))                'get the file names
       Array$(DirCnt) = SPACE$(12)

       FOR N = DirCnt + 1 TO DirCnt + FilCnt
           LSET Array$(N) = LCASE$(Array$(N))
       NEXT
       SortStr VARPTR(Array$(DirCnt + 1)), FilCnt, 0
    END IF

    IF DirCnt > Drives THEN
       Array$(Drives) = DirSpec$
       ReadDir BYVAL VARPTR(Array$(Drives))
       Array$(Drives) = SPACE$(12)

       SortStr VARPTR(Array$(Drives + 1)), DirCnt - Drives, 0
    END IF

    IF LEN(Path$) > 3 THEN
       LSET Array$(Drives) = ".."
       Last = Drives - 1
    ELSE
       Last = Drives
    END IF
    FOR N = 1 TO Last
       LSET Array$(N) = "[-" + Drv$(N) + "-]"
    NEXT

    Path$ = ""
    DirSpec$ = ""
    SrchSpec$ = ""

END SUB

FUNCTION NoPath$ (FileName$) STATIC

    N = MaxInt(INSTR(FileName$, ":"), QInstrB%(-1, FileName$, "\"))

    NoPath$ = MID$(FileName$, N + 1)

END FUNCTION

FUNCTION ParsPath$ (FileSpec$) STATIC

    N = MaxInt(INSTR(FileSpec$, ":"), QInstrB%(-1, FileSpec$, "\"))

    ParsPath$ = LEFT$(FileSpec$, N)

END FUNCTION

FUNCTION StripPath$ (FileName$) STATIC

    N = MaxInt(INSTR(FileName$, ":"), QInstrB%(-1, FileName$, "\"))

    StripPath$ = MID$(FileName$, N + 1)

END FUNCTION

