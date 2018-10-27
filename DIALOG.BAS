DECLARE SUB ErrMsg (Text$)
'********* Dialog.Bas
'$SubTitle:'Dialog Box Input Routines'
'Copyright (c) 1989 Crescent Software
'Written by Don Malin


DEFINT A-Z

'$INCLUDE: 'DialType.BI'

DECLARE FUNCTION AltKey% ()
DECLARE FUNCTION ASCII% (Char$)
DECLARE FUNCTION DosError% ()
DECLARE FUNCTION DOSVer% ()
DECLARE FUNCTION FindLast% (BYVAL Address, Size)
DECLARE FUNCTION FudgeFactor& ()
DECLARE FUNCTION MaxInt% (Num1, Num2)
DECLARE FUNCTION MGetKey% (Row, Col)
DECLARE FUNCTION MinInt% (Num1, Num2)
DECLARE FUNCTION Monitor% ()
DECLARE FUNCTION Peek1% (Segment%, Address%)
DECLARE FUNCTION PeekBuf% ()
DECLARE FUNCTION QPValI% (X$)
DECLARE FUNCTION WhichError% ()

DECLARE SUB ButtonPress (Button, Status, Count, X, Y)
DECLARE SUB Box0 (Top, Lc, Bot, Rc, LType, Clr)
DECLARE SUB ClearScr0 (UlRow, UlCol, LrRow, LrCol, Colr)
DECLARE SUB ConcaveFrame (UlR%, UlC%, BrR%, BrC%)
DECLARE SUB ConvexFrame (UlR%, UlC%, BrR%, BrC%)
DECLARE SUB EditLine (Edit$, Bg%, En%, Code%, SCode%, MRow%, CurCol%)
DECLARE SUB Explode (UlR%, UlC%, BrR%, BrC%)
DECLARE SUB GetCursor (X, Y, Button)
DECLARE SUB GetVMode (Mode, Page, PageSize, Rows, Columns)
DECLARE SUB HideCursor ()
DECLARE SUB ListBox (A$(), Start%, Rows%, Cols%, Action%, SCode%, Row%, Col%, Choice%)
DECLARE SUB MAmenu (BYVAL Address, Selection, Start, Count, ExitCode, FileColr, HiLiteColr, Rows, Columns, Spacing, Row, Column)
DECLARE SUB MEditor (Edit$, Length, ScanCode, NumOnly, CapsOn, NormColor, EditColor, Row, Column, CurCol)
DECLARE SUB MPaintBox (UlRow, UlCol, LrRow, LrCol, Colr)
DECLARE SUB MQPrint (X$, Colr)
DECLARE SUB MScrnSave (UlRow, UlCol, LrRow, LrCol, SEG Element)
DECLARE SUB MScrnRest (UlRow, UlCol, LrRow, LrCol, SEG Element)
DECLARE SUB Pause3 (MillaSec, Fudg&)
DECLARE SUB QPrintRC (X$, Row%, Col%, Clr%)
DECLARE SUB SetError (ErrNum%)
DECLARE SUB ShowCursor ()

DIM SHARED X$, Fudge&

SUB ConcaveFrame (UlR, UlC, BrR, BrC) STATIC

   HideCursor

   Length = BrC - UlC - 1

   R = UlR
   C = UlC
   Clr = 120

   Txt$ = "É"
   GOSUB ShowIt1

   C = UlC + 1
   Txt$ = STRING$(Length, "Í")
   GOSUB ShowIt1

   C = BrC
   Clr = 127
   Txt$ = "¸"
   GOSUB ShowIt1

   FOR N = UlR + 1 TO BrR - 1
      R = N
      C = UlC
      Clr = 120
      Txt$ = "º"
      GOSUB ShowIt1

      C = BrC
      Clr = 127
      Txt$ = "³"
      GOSUB ShowIt1
   NEXT

   R = BrR
   C = UlC
   Clr = 120
   Txt$ = "Ó"
   GOSUB ShowIt1

   C = UlC + 1
   Clr = 127
   Txt$ = STRING$(Length, "Ä")
   GOSUB ShowIt1

   C = BrC
   Txt$ = "Ù"
   GOSUB ShowIt1

   ShowCursor

   EXIT SUB

ShowIt1:
   QPrintRC Txt$, R, C, Clr
RETURN

END SUB

SUB ConvexFrame (UlR, UlC, BrR, BrC)

   HideCursor
   Length = BrC - UlC - 1
   Block$ = "Û"

   R = UlR
   C = UlC
   Clr = 127
   Txt$ = Block$
   GOSUB ShowIt2

   C = C + 1
   Txt$ = STRING$(Length, "ß")
   GOSUB ShowIt2

   C = BrC
   Clr = 120
   Txt$ = "Ü"
   GOSUB ShowIt2

   Txt$ = Block$
   FOR N = UlR + 1 TO BrR - 1
      R = N
      C = UlC
      Clr = 15
      GOSUB ShowIt2

      C = BrC
      Clr = 8
      GOSUB ShowIt2
   NEXT

   R = BrR
   C = UlC
   Clr = 127
   Txt$ = "ß"
   GOSUB ShowIt2

   C = C + 1
   Clr = 120
   Txt$ = STRING$(Length, "Ü")
   GOSUB ShowIt2

   C = BrC
   Txt$ = Block$
   GOSUB ShowIt2

   ShowCursor

   EXIT SUB

ShowIt2:
   QPrintRC Txt$, R, C, Clr
RETURN

END SUB

SUB DialogBox (Title$, PrmtAns$(), Dl() AS Dialog, Mult$(), ExitCode) STATIC

    Action = ExitCode \ 256
    IF Action = 5 THEN GOTO RestoreScr
    ExitCode = 0                                'Init. Exit Code
   
    '----- Determine the number of vertical pixels that the mouse uses.
    '      In OS2's DOS "Compatibility" mode, this must be adjusted.
    NPix = 8
    GetVMode 0, 0, 0, Rows, 0
    SaveError = WhichError
    IF Rows = 50 AND DOSVer >= 1000 THEN NPix = 7
    SetError SaveError

    IF Fudge& = 0 THEN Fudge& = FudgeFactor& \ 16 'EW:11/19/98

    MaxRow = 0                                  'Init. window size variables
    MaxCol = 0

    AltChars$ = "1234567890     QWERTYUIOP    ASDFGHJKL     ZXCVBNM"
    HotKeys$ = ""

    '----- Determine the size of the dialog box based on the longest line and
    '      lowest row.
    FOR N = 1 TO UBOUND(Dl)                     'Examine each input variable

        SELECT CASE Dl(N).DTyp                  'Calculate according to Type

           '----- Input Box
           CASE 1, 2
              MaxRow = MaxInt(MaxRow, Dl(N).DRow + 1)
              IF Dl(N + 1).DTyp = Dl(N).DTyp AND Dl(N + 1).DRow = Dl(N).DRow + 1 THEN
                 MaxCol = MaxInt(MaxCol, Dl(N).DCol + Dl(N).DLen + 1)
              ELSE
                 MaxCol = MaxInt(MaxCol, Dl(N).DCol + LEN(PrmtAns$(N, 0)) + Dl(N).DLen + 1)
              END IF

           '----- Check Box
           CASE 3
              Dl(N).DLen = 1
              MaxRow = MaxInt(MaxRow, Dl(N).DRow)
              MaxCol = MaxInt(MaxCol, Dl(N).DCol + LEN(PrmtAns$(N, 0)) + 1 + Dl(N).DLen + 2)

           '----- Multiple Choice
           CASE 4
              Rows = Dl(N).DLen MOD 256
              Cols = Dl(N).DLen \ 256
              MaxRow = MaxInt(MaxRow, Dl(N).DRow + Rows + 1)
              Wd = LEN(Mult$(1)) + 1
              MaxCol = MaxInt(MaxCol, Dl(N).DCol + 1 + Wd * Cols + 2)

           '----- Buttons
           CASE 5
              MaxRow = MaxInt(MaxRow, Dl(N).DRow + 1)
              MaxCol = MaxInt(MaxCol, Dl(N).DCol + 1 + LEN(PrmtAns$(N, 0)) + 3)

           '----- Message
           CASE 6
              MaxRow = MaxInt(MaxRow, Dl(N).DRow)
              MaxCol = MaxInt(MaxCol, Dl(N).DCol + LEN(PrmtAns$(N, 0)))

           CASE ELSE
        END SELECT

        FstChr$ = UCASE$(LEFT$(LTRIM$(PrmtAns$(N, 0)), 1))
        IF LEN(FstChr$) = 0 THEN FstChr$ = " "
        
        HotKey = INSTR(AltChars$, FstChr$)
        IF HotKey <= 15 THEN
           HotKey = 119 + HotKey
           IF HotKey = 119 THEN HotKey = 0
        END IF
        HotKeys$ = HotKeys$ + CHR$(HotKey)

    NEXT

    AltChars$ = ""

    BoxTop = CSRLIN                             'Box will start at cursor Loc.
    BoxBot = BoxTop + MaxRow + 1                'Calc. screen loc. of bottom

    BoxLC = 40 - (MaxCol + 1) \ 2               'Calc. left screen column for
                                                '  a centered box.
    BoxRC = BoxLC + MaxCol + 1                  'Ditto for right column

    HideCursor                                  'Turn the mouse cursor off

    '----- Draw the box frame
    IF Action = 2 THEN
       OverLay = -1
       ClearScr0 BoxTop + 1, BoxLC + 1, BoxBot - 1, BoxRC - 1, BoxClr
       ConvexFrame BoxTop, BoxLC, BoxBot, BoxRC
    ELSE
       OverLay = 0
       '----- Save the underlying screen
       REDIM ScrBuf((BoxBot - BoxTop + 2) * (BoxRC - BoxLC + 3))
       MScrnSave BoxTop, BoxLC, BoxBot + 1, BoxRC + 2, ScrBuf(0)

       Explode BoxTop, BoxLC, BoxBot, BoxRC
    END IF


    IF LEN(Title$) THEN
       LOCATE BoxTop, BoxLC + ((MaxCol - (LEN(Title$) + 2)) \ 2)
       MQPrint " " + Title$ + " ", BoxClr
    END IF
    LOCATE BoxBot - 2, BoxLC + 1
    'MQPrint "Ã" + STRING$(MaxCol, 196) + "´", BoxClr
    MQPrint STRING$(MaxCol, 196), BoxClr
    MPaintBox BoxTop + 1, BoxRC + 1, BoxBot, BoxRC + 2, 8
    MPaintBox BoxBot + 1, BoxLC + 2, BoxBot + 1, BoxRC + 2, 8


    REDIM Bx(UBOUND(Dl), 4)                     'Make an array to hold the
                                                '  coordinates of the input
                                                '  boxes.
    DfltBut = 0                                 'Default to double line boxes

    '----- Draw input text and fields
    FOR N = 1 TO UBOUND(Dl)                     'Compose each field according
        SELECT CASE Dl(N).DTyp                  '  to it's type

           '----- Input Box
           CASE 1, 2
              BTop = BoxTop + Dl(N).DRow - 1
              BLC = 0
              BrC = 0

              X = N - 1
              DO
                  X = X + 1

                  Temp = Dl(X + 1).DTyp <> Dl(X).DTyp OR Dl(X + 1).DRow <> Dl(X).DRow + 1
                  IF Temp THEN
                     Bx(X, 1) = BoxLC + Dl(X).DCol + LEN(PrmtAns$(X, 0))
                  ELSE
                     Bx(X, 1) = BoxLC + Dl(X).DCol
                  END IF
                  BLC = MaxInt(BLC, Bx(X, 1))
                  Bx(X, 2) = BoxTop + Dl(X).DRow
                  Bx(X, 3) = Bx(X, 1) + Dl(X).DLen + 1
                  BrC = MaxInt(BrC, Bx(X, 3))

                  IF X = N THEN
                     Bx(X, 0) = Bx(X, 2) - 1
                     Bx(X, 4) = 1
                  ELSE
                     Bx(X, 0) = Bx(X, 2)
                     Bx(X, 4) = 0
                  END IF

                  QPrintRC SPACE$(Dl(X).DLen), Bx(X, 2), Bx(X, 1) + 1, BoxClr
                  QPrintRC LEFT$(PrmtAns$(X, 1), Dl(X).DLen), Bx(X, 2), Bx(X, 1) + 1, BoxClr

              LOOP UNTIL Temp OR X = UBOUND(Dl)

              ConcaveFrame BTop, BLC, Bx(X, 2) + 1, BrC
              'Box0 BTop, BLC, Bx(X, 2) + 1, BrC, 1, BoxClr

              IF X <> N THEN
                 Bx(X, 2) = Bx(X, 0) + 1
                 QPrintRC "µ" + PrmtAns$(N, 0) + "Æ", BTop, Bx(N, 1) + 2, BoxClr
                 N = X
              ELSE
                 Bx(X, 2) = Bx(X, 0) + 2
                 QPrintRC PrmtAns$(N, 0), BoxTop + Dl(N).DRow, BoxLC + Dl(N).DCol, BoxClr
              END IF


           '----- Check Box
           CASE 3
              Bx(N, 0) = BoxTop + Dl(N).DRow
              Bx(N, 2) = Bx(N, 0)
              Bx(N, 1) = BoxLC + Dl(N).DCol
              Bx(N, 3) = Bx(N, 1) + 2
              LOCATE Bx(N, 0), Bx(N, 1)
              'MQPrint "[ ]", BoxClr
              MQPrint "Þ", 120
              LOCATE , Bx(N, 1) + 1
              MQPrint PrmtAns$(N, 1), BoxClr
              
              LOCATE , Bx(N, 1) + 2
              MQPrint "Ý", 127

              LOCATE , Bx(N, 1) + 4
              MQPrint PrmtAns$(N, 0), BoxClr

           '----- Multiple Choice
           CASE 4

              Rows = Dl(N).DLen MOD 256
              Cols = Dl(N).DLen \ 256
              Start = 1

              Bx(N, 0) = BoxTop + Dl(N).DRow
              Bx(N, 2) = Bx(N, 0) + Rows + 1
              Bx(N, 1) = BoxLC + Dl(N).DCol
              Bx(N, 3) = Bx(N, 1) + Wd * Cols + 2
              LOCATE Bx(N, 0), Bx(N, 1)


              ListBox Mult$(), 1, Rows, Cols, 1, SCode, 1, CurCol, QPValI%(PrmtAns$(N, 1))

              LOCATE Bx(N, 0), Bx(N, 1) + 2
              IF LEN(PrmtAns$(N, 0)) THEN MQPrint " " + PrmtAns$(N, 0) + " ", BoxClr


           '----- Buttons
           CASE 5

              Bx(N, 0) = BoxTop + Dl(N).DRow
              Bx(N, 2) = Bx(N, 0) + 1
              Bx(N, 1) = BoxLC + Dl(N).DCol
              Bx(N, 3) = Bx(N, 1) + LEN(PrmtAns$(N, 0)) + 3

              LOCATE Bx(N, 0) + 1, Bx(N, 1)
              MQPrint "Þ ", 127
              'MQPrint "( ", 127
              LOCATE , Bx(N, 1) + 1
              MQPrint " " + PrmtAns$(N, 0) + " ", BoxClr
              LOCATE , Bx(N, 1) + LEN(PrmtAns$(N, 0)) + 2
              MQPrint " Ý", 120
              'MQPrint " )", 120
             
              IF DfltBut = 0 THEN
                 DfltBut = N
                 MPaintBox Bx(N, 0) + 1, Bx(N, 1) + 1, Bx(N, 0) + 1, Bx(N, 3) - 1, HiLite
              END IF


           '----- Message
           CASE 6
              LOCATE BoxTop + Dl(N).DRow, BoxLC + Dl(N).DCol
              MQPrint PrmtAns$(N, 0), BoxClr

              Bx(N, 0) = 0
              Bx(N, 2) = 0
              Bx(N, 1) = 0
              Bx(N, 3) = 0
                     
           CASE ELSE
        END SELECT

    NEXT

    DO
       GetCursor 0, 0, Button
    LOOP WHILE Button <> 0 AND AltKey% <> 0

    ShowCursor
    Ch$ = ""
    N = 1
    UserN = N
    AltDown = 0
    SCode = 0
    DO
        LOCATE , , 1
        SELECT CASE Dl(N).DTyp
           CASE 1, 2
              LOCATE Bx(N, 0) + Bx(N, 4), Bx(N, 1) + 1

              EditLine PrmtAns$(N, 1), Bx(N, 1) + 1, Bx(N, 3) - 1, Dl(N).DTyp - 1, SCode, MRow, CurCol

              GOSUB CheckUser
              N = UserN


           CASE 3
              LOCATE Bx(N, 0), Bx(N, 1) + 1
             
              DO WHILE AltKey AND PeekBuf <> 0
                 X$ = INKEY$
              LOOP

              SCode = MGetKey(MRow, CurCol)

              IF SCode = 32 THEN
                 IF PrmtAns$(N, 1) = "û" THEN
                    PrmtAns$(N, 1) = " "
                 ELSE
                    PrmtAns$(N, 1) = "û"
                 END IF
                 MQPrint CHR$(15), BoxClr
                 Pause3 50 * 16, Fudge&          'EW:11/19/98
                 MQPrint PrmtAns$(N, 1), BoxClr
              END IF
              GOSUB CheckUser

              N = UserN
             
           CASE 4
              
              Choice = MaxInt(1, QPValI%(PrmtAns$(N, 1)))
              Rows = Dl(N).DLen MOD 256
              Cols = Dl(N).DLen \ 256

              LOCATE Bx(N, 0), Bx(N, 1), 0
              ListBox Mult$(), Start, Rows, Cols, 3, SCode, MRow, CurCol, Choice

              GOSUB CheckUser
              IF Choice > 1 AND LEFT$(Mult$(Choice), 1) = "Ä" THEN
                 SCode = 0
              ELSE
                 PrmtAns$(N, 1) = STR$(Choice)
                 N = UserN
              END IF

           CASE 5
              IF N <> DfltBut THEN
                 SWAP N, DfltBut
                 Clr = BoxClr
                 GOSUB ButtonBox
                 SWAP N, DfltBut
              END IF

              Clr = HiLite
              GOSUB ButtonBox

              LOCATE Bx(N, 0) + 1, Bx(N, 1) + 2, 1
              
              IF AltDown OR SCode > 32 THEN
                 SCode = 13
              ELSE
                 SCode = MGetKey(MRow, CurCol)
                 GOSUB CheckUser
              END IF
              
              

              IF SCode = 13 THEN
                 ExitCode = N - DfltBut + 1
                 SCode = 27
              ELSEIF UserN <> N AND SCode <> 0 THEN
                 Clr = BoxClr
                 GOSUB ButtonBox

                 IF (N = UBOUND(Dl) AND UserN < DfltBut) OR (N = DfltBut AND UserN < N) THEN
                    N = DfltBut
                    Clr = HiLite
                    GOSUB ButtonBox
                 END IF
              END IF
              N = UserN

           CASE 6
              N = N + 1
              IF N > UBOUND(Dl) THEN N = 1
              UserN = N

           CASE ELSE
        END SELECT
       
      
    LOOP UNTIL SCode = 13 OR SCode = 27

    IF SCode = 13 THEN ExitCode = 1

RestoreScr:
    IF Action = 0 OR Action = 5 THEN
       MScrnRest BoxTop, BoxLC, BoxBot + 1, BoxRC + 2, ScrBuf(0)
       ERASE ScrBuf, Bx
    END IF
    HotKeys$ = ""

    EXIT SUB






CheckUser:

    IF SCode THEN
       SELECT CASE SCode
          CASE 9, -80, -77
             UserN = N
             DO
                IF SCode = -77 AND Dl(N).DTyp < 3 THEN EXIT DO
                IF UserN = UBOUND(Dl) THEN
                   UserN = 1
                ELSE
                   UserN = UserN + 1
                END IF
             LOOP WHILE Dl(UserN).DTyp = 6 OR Dl(UserN).DTyp = 0

          CASE -15, -72, -75
             UserN = N
             DO
                IF SCode = -75 AND Dl(N).DTyp < 3 THEN EXIT DO
                IF UserN = 1 THEN
                   UserN = UBOUND(Dl)
                ELSE
                   UserN = UserN - 1
                END IF
             LOOP WHILE Dl(UserN).DTyp = 6 OR Dl(UserN).DTyp = 0

          CASE IS < 0                          'Look for Alt keys
             IF INSTR(HotKeys$, CHR$(ABS(SCode))) THEN

                X = N + 1
                DO
                   UserN = INSTR(X, HotKeys$, CHR$(ABS(SCode)))

                   IF UserN = 0 THEN
                      X = 1
                   ELSEIF Dl(UserN).DTyp = 6 OR Dl(UserN).DTyp = 0 THEN
                      X = UserN + 1
                      UserN = 0
                   ELSEIF UserN >= DfltBut THEN
                      AltDown = -1
                   ELSE
                      AltDown = 0
                   END IF

                LOOP WHILE UserN = 0 AND X = 1

                IF UserN = 0 THEN UserN = N
             END IF

          CASE 1 TO 255
             IF N >= DfltBut THEN
                Ch$ = UCASE$(CHR$(SCode))
                FOR X = DfltBut TO UBOUND(Dl)
                    IF Ch$ = UCASE$(LEFT$(PrmtAns$(X, 0), 1)) OR (SCode = 32 AND X = N) THEN
                       MPaintBox Bx(N, 0) + 1, Bx(N, 1) + 1, Bx(N, 0) + 1, Bx(N, 3) - 1, BoxClr
                       MPaintBox Bx(X, 0) + 1, Bx(X, 1) + 1, Bx(X, 0) + 1, Bx(X, 3) - 1, HiLite
                       'Pause 2
                       Pause3 50 * 16, Fudge&    'EW:11/19/98
                       IF SCode = 32 THEN
                          SCode = 13
                       ELSE
                          SCode = 27
                       END IF
                       ExitCode = X - DfltBut + 1
                       EXIT FOR
                    END IF
                NEXT
             ELSE
                UserN = UserN + 1
                UserN = N
                DO
                   IF UserN = UBOUND(Dl) THEN
                      UserN = 1
                   ELSE
                      UserN = UserN + 1
                   END IF
                LOOP WHILE Dl(UserN).DTyp = 6 OR Dl(UserN).DTyp = 0
             END IF

          CASE 1000
             FOR X = 1 TO UBOUND(Dl)
                 IF MRow >= Bx(X, 0) AND MRow <= Bx(X, 2) THEN
                    IF CurCol >= Bx(X, 1) AND CurCol <= Bx(X, 3) THEN
               
                       SELECT CASE Dl(X).DTyp
                          CASE 1, 2
                             'IF X = N THEN LOCATE Bx(X, 0) + Bx(X, 4), CurCol

                          CASE 3
                             IF PrmtAns$(X, 1) = "û" THEN
                                PrmtAns$(X, 1) = " "
                             ELSE
                                PrmtAns$(X, 1) = "û"
                             END IF
                             LOCATE Bx(X, 0), Bx(X, 1) + 1
                             MQPrint CHR$(15), BoxClr
                             'Pause 2
                             Pause3 50 * 16, Fudge&      'EW:11/19/98
                             MQPrint PrmtAns$(X, 1), BoxClr

                          CASE 4
                             PrmtAns$(X, 1) = STR$(MinInt(Dl(X).DLen, MaxInt(1, MRow - Bx(X, 0))))
                    
                          CASE 5

                             MPaintBox Bx(DfltBut, 0) + 1, Bx(DfltBut, 1) + 1, Bx(DfltBut, 0) + 1, Bx(DfltBut, 3) - 1, BoxClr

                             MPaintBox Bx(X, 0) + 1, Bx(X, 1), Bx(X, 0) + 1, Bx(X, 1), 120'Highlight
                             MPaintBox Bx(X, 0) + 1, Bx(X, 1) + 1, Bx(X, 0) + 1, Bx(X, 3) - 1, HiLite
                             MPaintBox Bx(X, 0) + 1, Bx(X, 3), Bx(X, 0) + 1, Bx(X, 3), 127'Highlight

                             'Pause 2
                             Pause3 50 * 16, Fudge&      'EW:11/19/98
                             DO
                                GetCursor 0, 0, Buttons
                             LOOP WHILE Buttons
                             SCode = 27
                             ExitCode = X - DfltBut + 1
                 
                          CASE ELSE
                       END SELECT

                       UserN = X
                       EXIT FOR
                    END IF
                 END IF

             NEXT


          CASE 1001
             SCode = 27

          CASE ELSE
       END SELECT
      
    END IF

RETURN




ButtonBox:

    MPaintBox Bx(N, 0) + 1, Bx(N, 1) + 1, Bx(N, 0) + 1, Bx(N, 3) - 1, Clr
RETURN



END SUB

SUB EditLine (Edit$, Bg, En, Code, SCode, Row, CurCol)

    Temp$ = SPACE$(En - Bg + 1)
    LSET Temp$ = Edit$

    Row = CSRLIN
    CurCol = Bg + LEN(Edit$)

    LOCATE Row, Bg
    MQPrint Temp$, BoxClr
    MQPrint Edit$, HiLite
   
    IF Peek1%(Zero, &H463) = &HB4 THEN
       LOCATE , CurCol, 1, 12, 15
    ELSE
       LOCATE , CurCol, 1, 6, 7
    END IF

    DO WHILE AltKey% <> 0 AND PeekBuf <> 0
       X$ = INKEY$
    LOOP

    DO
        SCode = PeekBuf
        GetCursor 0, 0, Button
    LOOP UNTIL SCode OR Button

    LOCATE Row, Bg
    MQPrint Temp$, BoxClr

    IF AltKey% THEN
       X$ = INKEY$
       EXIT SUB
    END IF

    SELECT CASE SCode
       CASE 32 TO 254
          LSET Temp$ = ""
          LOCATE Row, Bg
          MQPrint Temp$, BoxClr
          CurCol = Bg
       CASE ELSE
    END SELECT

    MEditor Temp$, ALen, SCode, Code, 0, BoxClr, BoxClr, Row, Bg, CurCol

    IF SCode >= 1000 THEN
       CurCol = Bg
       ButtonPress 1, 0, 0, 0, 0
    END IF

    Edit$ = LEFT$(Temp$, ALen)

END SUB

SUB Explode (UlR, UlC, BrR, BrC)

    IF Monitor% <> 3 THEN
       IF Fudge& = 0 THEN Fudge& = FudgeFactor& \ 16 'EW:11/19/98
    
       ML = (BrR - UlR + 1) \ 2
       MC = (BrC - UlC + 1) \ 2 - 1

       MSec = MinInt(30 \ ML, 120 \ MC)

       FOR N = 2 TO MC STEP 2
           LN = MaxInt((ML * N) \ MC, 1)
           ClearScr0 UlR + ML - LN, 40 - N, BrR - ML + LN, 40 + N, BoxClr
           Pause3 MSec * 16, Fudge&      'EW:11/19/98
       NEXT
    END IF

    ClearScr0 UlR + 1, UlC + 1, BrR - 1, BrC - 1, BoxClr

    ConvexFrame UlR, UlC, BrR, BrC

END SUB

SUB ListBox (A$(), Start, Rows, Cols, Action, SCode, Row, Col, Choice)

    'IF Choice = 0 THEN Choice = 1
    SCode = 0

    '----- Get max. width
    ColWd = LEN(A$(1)) + 1
    LastCh = FindLast(VARPTR(A$(UBOUND(A$))), UBOUND(A$))
    GOSUB InRange

    UlRow = CSRLIN
    UlCol = POS(0)
    BrRow = UlRow + Rows + 1
    BrCol = UlCol + ColWd * Cols + 2

    IF Action < 2 THEN
       HideCursor
       ClearScr0 UlRow + 1, UlCol + 1, BrRow - 1, BrCol - 1, BoxClr
       'Box0 UlRow, UlCol, BrRow, BrCol, 1, BoxClr
       ConcaveFrame UlRow, UlCol, BrRow, BrCol

       FOR R = 1 TO Rows
           FOR C = 1 TO Cols
               LOCATE UlRow + R, (C - 1) * ColWd + UlCol + 2
               Temp = (C - 1) * Rows + Start - 1 + R
               IF Temp <= LastCh THEN
                  IF Temp = Choice THEN
                     Clr = 127 'Highlight
                  ELSE
                     Clr = BoxClr
                  END IF
                  MQPrint A$(Temp), Clr
               END IF
           NEXT
       NEXT
       ShowCursor
       Action = 3
    
       EXIT SUB

    END IF




    DO
       Row = UlRow + 1
       Col = UlCol + 2

       MAmenu VARPTR(A$(1)), Choice, Start, LastCh, SCode, BoxClr, HiLite, Rows, Cols, 1, Row, Col

       IF SCode = 32 THEN SCode = 13
       IF AltKey% THEN EXIT SUB

       SELECT CASE SCode
          CASE 13, 27, 9, -15
             EXIT DO
          CASE 33 TO 255
             Was = Choice
             IF SCode > 96 THEN SCode = SCode - 32
             DO
                Choice = Choice + 1
                IF Choice > LastCh THEN Choice = 1
                IF Choice = Was THEN EXIT DO
                Temp = ASCII(A$(Choice))
                IF Temp > 96 THEN Temp = Temp - 32
             LOOP UNTIL SCode = Temp

             GOSUB InRange

          CASE -73                      'Page up
             Start = MaxInt(Start - Rows * Cols, 1)
             Choice = MaxInt(Choice - Rows * Cols, 1)
          CASE -81                      'Page down
             Start = MinInt(Start + Rows * Cols, (((LastCh \ Rows) - (LastCh MOD Rows > 0)) * Rows) - (Rows * (Cols - 1)))
             Start = MaxInt(1, Start)
             Choice = MinInt(Choice + Rows * Cols, LastCh)
          CASE -71                      'Home
             Start = 1
             Choice = 1
          CASE -79                      'End
             Start = (((LastCh \ Rows) - (LastCh MOD Rows > 0)) * Rows) - (Rows * (Cols - 1))
             Start = MaxInt(1, Start)
             Choice = LastCh
          CASE 1000
             ButtonPress 1, 0, 0, 0, 0
             IF Row >= UlRow AND Row <= BrRow AND Col >= UlCol AND Col <= BrCol THEN
                IF Col = UlCol THEN
                   Choice = MaxInt(Choice - Rows, 1)
                   Start = MaxInt(Start - Rows, 1)
                ELSEIF Col = BrCol THEN
                   Choice = MinInt(Choice + Rows, LastCh)
                   Start = MinInt(Start + Rows, (((LastCh \ Rows) - (LastCh MOD Rows > 0)) * Rows) - (Rows * (Cols - 1)))
                END IF
             ELSE
                EXIT DO
             END IF
          CASE 1001
             ButtonPress 2, 0, 0, 0, 0
             DO
                GetCursor 0, 0, Buttons
             LOOP WHILE Buttons
             SCode = 27
             EXIT DO
          CASE 1003
             ButtonPress 1, 0, 0, 0, 0
             DO
                GetCursor 0, 0, Buttons
             LOOP WHILE Buttons
             SCode = 13
             EXIT DO
          CASE ELSE
       END SELECT

    LOOP

    R = (Choice - 1) MOD Rows + UlRow + 1
    C = ((Choice - Start) \ Rows) * ColWd + UlCol + 2
    MPaintBox R, C, R, C + ColWd - 2, 127

    EXIT SUB

InRange:
    IF Choice < Start OR Choice >= Start + (Rows * Cols) THEN
       Start = MinInt((Choice \ Cols) * Cols, (((LastCh \ Rows) - (LastCh MOD Rows > 0)) * Rows) - (Rows * (Cols - 1)))
       Start = MaxInt(1, Start)
    END IF
RETURN

END SUB

