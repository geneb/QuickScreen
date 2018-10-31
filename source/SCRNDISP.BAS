'***** ScrnDisp.Bas *********************************************************
'Module used for displaying screens with various special effects (wipes).
'
'Copyright (c) 1989, 1990 Crescent Software Inc.
'By Don Malin
'
'The following routines can be called from your program:
'
'   ChangeClr - Use this routine to selectivly change colors in an area of
'               the screen.
'
' DisplayScrn - Displays a screen from an array.  Used by "Lib2Scrn" or
'               "LoadScreen" and can be used with an array that was created
'               using an object screen CALL for wipes.
'****************************************************************************

DEFINT A-Z

DECLARE FUNCTION FudgeFactor& ()
DECLARE FUNCTION MaxInt% (Var1, Var2)
DECLARE FUNCTION MinInt% (Var1%, Var2%)
DECLARE FUNCTION Monitor% ()
DECLARE FUNCTION Peek1% (Segment, Address)
DECLARE FUNCTION TrapInt% (Value%, LoLimit%, HiLimit%)

DECLARE SUB GetVMode (Mode, Page, PageSize, Rows, Columns)
DECLARE SUB HideCursor ()
DECLARE SUB MouseTrap (UlRow, UlCol, BrRow, BrCol)
DECLARE SUB MoveScreen (SEG FromAddr, SEG ToAddr, ClrFlag)
DECLARE SUB MPaintBox (UlRow, UlCol, LrRow, LrCol, Colr)
DECLARE SUB MPRestore (UlRow, UlCol, LrRow, LrCol, OrigWidth, SEG ArrayEl)
DECLARE SUB MScrnRest (UlR, UlC, LRR, LRC, SEG Address)
DECLARE SUB MScrnSave (UlR, UlC, LRR, LRC, SEG Address)
DECLARE SUB Pause (Ticks)
DECLARE SUB Pause3 (MillaSec, Fudg&)
DECLARE SUB ScrollU (UlRow, UlCol, BrRow, BrCol, NLines, Page)
DECLARE SUB ScrollD (UlRow, UlCol, BrRow, BrCol, NLines, Page)
DECLARE SUB ScrollL (UlRow, UlCol, BrRow, BrCol, NLines, Page)
DECLARE SUB ScrollR (UlRow, UlCol, BrRow, BrCol, NLines, Page)
DECLARE SUB ShowCursor ()
DECLARE SUB WriteScreen (SEG Element, ColorCode)

'----- Wipe Declarations
DECLARE SUB DisplayScrn (BTmp%(), Element%, MonoCode%, WipeType%)
DECLARE SUB Dissolve (Scr%(), El%)
DECLARE SUB ExplodBox (UlRow, UlCol, BrRow, BrCol, Scr%(), El%)
DECLARE SUB OpenCurtain (Orig%(), OE%, NewScr%(), NE%)
DECLARE SUB PushDown (Scr%(), El%)
DECLARE SUB PushL2R (Scr%(), El%)
DECLARE SUB PushR2L (Scr%(), El%)
DECLARE SUB PushUp (Scr%(), El%)
DECLARE SUB RollAway (Orig%(), OE%, NewScr%(), NE%)
DECLARE SUB VertBlinds (Scr%(), El%)
DECLARE SUB WipeL2R (Scr%(), El%)

'****************************************************************************
'Will selectivly change colors in an area of the screen.
'
'UlR is the upper left row of the area to change
'UlC is the upper left column of the area to change
'BrR is the bottom right row of the area to change
'BrC is the bottom right column of the area to change
'FromClr is the color to change
'ToClr is the new color to use
'****************************************************************************
'
SUB ChangeClr (UlR, UlC, BrR, BrC, FromClr, ToClr) STATIC

    Size = (BrR - UlR + 1) * (BrC - UlC + 1)
    REDIM Tmp(Size)
    MScrnSave UlR, UlC, BrR, BrC, Tmp(0)

    DEF SEG = VARSEG(Tmp(0))

    Size = Size * 2
    FOR N = 1 TO Size STEP 2
        IF PEEK(N) = FromClr THEN POKE N, ToClr
    NEXT

    MScrnRest UlR, UlC, BrR, BrC, Tmp(0)

    ERASE Tmp

END SUB

'****************************************************************************
'Special effects routine which will restore a screen by moving it inward from
'the edges.
'Note:  This routine is not called by "DisplayScrn" but is included so that
'your program can use it to compliment the "OpenCurtain" routine.
'
'Orig() is the array that holds the screen.  The screen can be any number of
'  rows but must be 80 columns.
'
'El is the array element where the screen starts.
'****************************************************************************
'
SUB CloseCurtain (Orig(), El) STATIC

    IF Fudge& = 0 THEN Fudge& = FudgeFactor& \ 16 'EW:11/19/98
    LastLine = UBOUND(Orig) \ 80

    FOR L = 1 TO 39
        MPRestore 1, 1, LastLine, L, 80, Orig(40 - L + El)
        R = 81 - L
        MPRestore 1, R, LastLine, 80, 80, Orig(40 + El)

        Pause3 L * 16, Fudge&    'EW:11/19/98
    NEXT

    MPRestore 1, 1, LastLine, 80, 80, Orig(El)

END SUB

'****************************************************************************
' Displays a screen from an array using whatever wipe type is specified
' in the "WipeType" code variable.  This routine is called by "Lib2Scrn"
' and "LoadScreen"
'
' BTmp() must contain row and column information for the screen as well as
'   the contents of the screen.  It should be structured as follows:
'       "BTmp(Element)" must contain the upper left row and column and
'       "BTmp(Element + 1)" must contain the bottom right row and column.
'       "BTmp(Element + 2)..." should contain the screen data.
'
' Element specifies the array element where the row and column information
'   starts.
'
' MonoCode is a flag used to force a black and white display.  Use 3 for true
'   monochrome colors.
'
' WipeType specifies the type of special effects to use for the display.
'   0 = Direct to Screen (no special effects)
'   1 = Clear Screen before displaying
'   2 = Opening Curtain
'   3 = Dissolves old screen
'   4 = Exploding Box
'   5 = Pushes existing screen Down
'   6 = Pushes existing screen from Left to Right
'   7 = Pushes existing screen from Right to Left
'   8 = Pushes existing screen Up
'   9 = Roles the existing screen away to the left
'   10 = Vertical Blinds
'   11 = Wipe Left to Right
'
' Example:
'       REDIM BTmp((BrRow - UlRow + 1) * (BrCol - UlCol + 1) + 2)
'       BTmp(0) = UlCol * 256 + UlRow                   'Pack Row and Column
'       BTmp(1) = BrCol * 256 + BrRow                   '  into word
'       CALL MScrnSave(UlRow, UlCol, BrRow, BrCol, SEG BTmp(2))
'                             ..
'                             ..
'       '----- Display the old screen
'       CALL DisplayScrn(BTmp(), 0, MonoCode, WipeType)
'****************************************************************************
'
SUB DisplayScrn (BTmp(), Element, MonoCode, WipeType) STATIC

    IF WipeType < 2 THEN
       HideCursor
       IF WipeType = 1 THEN CLS

       WriteScreen BTmp(Element), MonoCode
       ShowCursor
    ELSE
       ScrRows = BTmp(Element + 1) MOD 256

       REDIM NewScr(ScrRows * 80)
       MScrnSave 1, 1, ScrRows, 80, NewScr(0)

       MoveScreen BTmp(Element), NewScr(0), MonoCode

       SELECT CASE WipeType
          CASE 2
             REDIM OldScr(ScrRows * 80)
             MScrnSave 1, 1, ScrRows, 80, OldScr(0)
             OpenCurtain OldScr(), 0, NewScr(), 0
             ERASE OldScr
          CASE 3
             Dissolve NewScr(), 0
          CASE 4
             UlRow = BTmp(Element) MOD 256
             UlCol = BTmp(Element) \ 256
             BrRow = BTmp(Element + 1) MOD 256
             BrCol = BTmp(Element + 1) \ 256
             ExplodBox UlRow, UlCol, BrRow, BrCol, NewScr(), 0
          CASE 5
             PushDown NewScr(), 0
          CASE 6
             PushL2R NewScr(), 0
          CASE 7
             PushR2L NewScr(), 0
          CASE 8
             PushUp NewScr(), 0
          CASE 9
             REDIM OldScr(ScrRows * 80)
             MScrnSave 1, 1, ScrRows, 80, OldScr(0)
             RollAway OldScr(), 0, NewScr(), 0
             ERASE OldScr
          CASE 10
             VertBlinds NewScr(), 0
          CASE 11
             WipeL2R NewScr(), 0
          CASE ELSE
             HideCursor
             CLS
             WriteScreen BTmp(Element), MonoCode
             ShowCursor
       END SELECT

       ERASE NewScr

    END IF

END SUB

'****************************************************************************
'Special effects routine which will restore a screen by "Disolving" the
'current screen.
'
'Scr() is the array that holds the screen.  The screen can be any number of
'  rows but must be 80 columns.
'
'El is the array element where the screen starts.
'****************************************************************************
'
SUB Dissolve (Scr(), El) STATIC

    LastLine = UBOUND(Scr) \ 80

    FOR CO = 3 TO 0 STEP -1
        FOR Row = 1 TO LastLine STEP 4
            FOR RO = 3 TO 0 STEP -1
                R = Row + RO
                IF R <= LastLine THEN
                   FOR Col = 1 TO 77 STEP 4
                       C = Col + CO + RO
                       IF C > 80 THEN C = C - 80
                       MPRestore R, C, R, C, 80, Scr((R - 1) * 80 + C - 1 + El)
                   NEXT
                END IF
            NEXT
        NEXT
    NEXT

END SUB

'****************************************************************************
'Special effects routine which will restore a window by "Exploding" it onto
'the current screen.
'
'UlRow is the upper left row of the window
'UlCol is the upper left column of the window
'BrRow is the lower right row of the window
'BrCol is the lower right column of the window
'
'Scr() is the array that holds the screen.
'
'El is the array element where the screen starts.
'****************************************************************************
'
SUB ExplodBox (UlRow, UlCol, BrRow, BrCol, Scr(), El)

    IF Fudge& = 0 THEN Fudge& = FudgeFactor& \ 16 'EW:11/19/98

    MidRow = (BrRow - UlRow) \ 2 + UlRow
    MidCol = (BrCol - UlCol) \ 2 + UlCol

    MSec = MinInt(150 \ MidRow, 480 \ MidCol)

    FOR Col = MidCol TO UlCol STEP -2
        Row = MaxInt((MidRow * Col) \ MidCol, UlRow)
                                           
        BRow = Row + (MidRow - Row) * 2
        BCol = Col + (MidCol - Col) * 2

        MPRestore Row, Col, BRow, BCol, 80, Scr((Row - 1) * 80 + Col - 1 + El)
        Pause3 MSec * 16, Fudge&         'EW:11/19/98
    NEXT

    MPRestore UlRow, UlCol, BrRow, BrCol, 80, Scr((UlRow - 1) * 80 + UlCol - 1 + El)

END SUB

'****************************************************************************
'Special effects routine which will restore a screen by pushing the existing
'screen outward.
'
'Orig() is the array that holds the existing screen.  The screen can be any
'  number of rows but must be 80 columns.
'
'OE is the array element where the existing screen starts
'
'NewScr() is the array that holds the new screen.  The screen can be any
'  number of rows but must be 80 columns.
'
'NE is the array element where the new screen starts.
'****************************************************************************
'
SUB OpenCurtain (Orig(), OE, NewScr(), NE) STATIC

    IF Fudge& = 0 THEN Fudge& = FudgeFactor& \ 16 'EW:11/19/98

    LLOrig = UBOUND(Orig) \ 80
    LLNew = UBOUND(NewScr) \ 80

    FOR L = 40 TO 2 STEP -1
        Pause3 L * 16, Fudge&    'EW:11/19/98

        MPRestore 1, 1, LLOrig, L - 1, 80, Orig(41 - L + OE)
        R = 81 - L
        MPRestore 1, R + 1, LLOrig, 80, 80, Orig(40 + OE)
        MPRestore 1, L, LLNew, R, 80, NewScr(L - 1 + NE)
    NEXT
    MPRestore 1, 1, LLNew, 80, 80, NewScr(NE)

END SUB

'****************************************************************************
'Special effects routine which will restore a screen by pushing the existing
'screen down.
'
'Scr() is the array that holds the screen.  The screen can be any number of
'  rows but must be 80 columns.
'
'El is the array element where the screen starts.
'****************************************************************************
'
SUB PushDown (Scr(), El) STATIC

    LastLine = UBOUND(Scr) \ 80
    HideCursor

    FOR Row = 1 TO LastLine - 1
        ScrollD Row, 1, LastLine, 80, 1, 0
        MPRestore 1, 1, Row, 80, 80, Scr((LastLine - Row) * 80 + El)
        'CALL Pause(1)
    NEXT

    MPRestore 1, 1, Row, 80, 80, Scr((LastLine - Row) * 80 + El)

    ShowCursor
END SUB

'****************************************************************************
'Special effects routine which will restore a screen by pushing the existing
'screen from left to right.
'
'Scr() is the array that holds the screen.  The screen can be any number of
'  rows but must be 80 columns.
'
'El is the array element where the screen starts.
'****************************************************************************
'
SUB PushL2R (Scr(), El) STATIC

    LastLine = UBOUND(Scr) \ 80
    HideCursor

    FOR Col = 1 TO 79 STEP 2
        ScrollR 1, Col, LastLine, 80, 1, 0
        MPRestore 1, 1, LastLine, Col, 80, Scr(81 - Col - 1 + El)
    NEXT
    MPRestore 1, 1, LastLine, 80, 80, Scr(El)

    ShowCursor
END SUB

'****************************************************************************
'Special effects routine which will restore a screen by pushing the existing
'screen from right to left.
'
'Scr() is the array that holds the screen.  The screen can be any number of
'  rows but must be 80 columns.
'
'El is the array element where the screen starts.
'****************************************************************************
'
SUB PushR2L (Scr(), El) STATIC

    LastLine = UBOUND(Scr) \ 80
    HideCursor

    FOR Col = 77 TO 1 STEP -2
        ScrollL 1, 1, LastLine, Col + 2, 1, 0
        MPRestore 1, Col, LastLine, 80, 80, Scr(El)
    NEXT

    ShowCursor
END SUB

'****************************************************************************
'Special effects routine which will restore a screen by pushing the existing
'screen up.
'
'Scr() is the array that holds the screen.  The screen can be any number of
'  rows but must be 80 columns.
'
'El is the array element where the screen starts.
'****************************************************************************
'
SUB PushUp (Scr(), El) STATIC

    LastLine = UBOUND(Scr) \ 80
    HideCursor

    FOR Row = LastLine TO 2 STEP -1
        ScrollU 1, 1, Row, 80, 1, 0
        MPRestore Row, 1, LastLine, 80, 80, Scr(El)
        'CALL Pause(1)
    NEXT
    MPRestore Row, 1, LastLine, 80, 80, Scr(El)

    ShowCursor
END SUB

'****************************************************************************
'Special effects routine which will restore a screen by appearing to role the
'existing screen away to the left.
'
'Orig() is the array that holds the existing screen.  The screen can be any
'  number of rows but must be 80 columns.
'
'OE is the array element where the existing screen starts.
'
'NewScr() is the array that holds the new screen.  The screen can be any
'  number of rows but must be 80 columns.
'
'NE is the array element where the existing screen starts.
'****************************************************************************
'
SUB RollAway (Orig(), OE, NewScr(), NE) STATIC

    IF Fudge& = 0 THEN Fudge& = FudgeFactor& \ 16 'EW:11/19/98
    HideCursor
    LLOrig = UBOUND(Orig) \ 80
    LLNew = UBOUND(NewScr) \ 80

    FOR C = 79 TO 1 STEP -1
        MPRestore 1, C, LLOrig, C, 80, Orig(C + OE)
        Pause3 9 * 16, Fudge&    'EW:11/19/98

        IF C > 2 THEN
           Wide = TrapInt%(80 - C + 1, 1, 10)
           ScrollL 1, MaxInt(1, C - Wide), LLNew, C, 2, 0
        END IF
     
        MPRestore 1, C, LLNew, C + 1, 80, NewScr(C - 1 + NE)
    NEXT

    ShowCursor
END SUB

'****************************************************************************
'Special effects routine which will restore a screen by appearing to open
'vertical blinds.
'
'Scr() is the array that holds the screen.  The screen can be any number of
'  rows but must be 80 columns.
'
'El is the array element where the screen starts.
'****************************************************************************
'
SUB VertBlinds (Scr(), El) STATIC

    LastLine = UBOUND(Scr) \ 80

    FOR N = 0 TO 9
        FOR Col = 1 TO 71 STEP 10
            MPRestore 1, Col, LastLine, Col + N, 80, Scr(Col - 1 + El)
        NEXT
        Pause 1
    NEXT

END SUB

'****************************************************************************
'Special effects routine which will restore a screen by sliding the new
'screen over the existing one.
'
'Scr() is the array that holds the screen.  The screen can be any number of
'  rows but must be 80 columns.
'
'El is the array element where the screen starts.
'****************************************************************************
'
SUB WipeL2R (Scr(), El) STATIC

    LastLine = UBOUND(Scr) \ 80

    FOR Col = 2 TO 80 STEP 2
        MPRestore 1, 1, LastLine, Col, 80, Scr(80 - Col + El)
    NEXT

END SUB

