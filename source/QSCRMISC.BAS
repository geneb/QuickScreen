'****** QScrMisc.Bas  -   Screen Builder Support Module ********************

'Copyright (C) Donald R. Malin  1987 - 1989
'Copyright (C) Crescent Software  1987 - 1989

'Note:  Should be compiled from the Command Line.
'       Compile Syntax = "BC QScrMisc /S/O;"
'***************************************************************************

DEFINT A-Z

'$INCLUDE: 'FieldInf.Bi'
'$INCLUDE: 'DialType.Bi'
'$INCLUDE: 'GenInfo.BI'

TYPE OldFrm
     Fields    AS INTEGER              '0
     Row       AS INTEGER              '1
     LCol      AS INTEGER              '2
     RCol      AS INTEGER              '3
     StorLen   AS INTEGER              '4
     FType     AS INTEGER              '5
     RelFile   AS STRING * 8           '6
     RelFld    AS INTEGER              '10
     Indexed   AS INTEGER              '11
     FldName   AS STRING * 8           '12
     Decimals  AS INTEGER              '16
     RelHandle AS INTEGER              '17
     Protected AS INTEGER              '18
     ScratchI  AS INTEGER
END TYPE

DECLARE FUNCTION AlertUser% (Message$, Prmt1$, Prmt2$, Prmt3$)
DECLARE FUNCTION ASCII% (Text$)
DECLARE FUNCTION ASCIIChart$ (Ok2Print%, Inf AS ANY)
DECLARE FUNCTION BaseName$ (FileName$)
DECLARE FUNCTION CheckSum% (A$)
DECLARE FUNCTION EndOfField% (Inf AS GenInfo)
DECLARE FUNCTION Eval# (E$, ErrCode%)
DECLARE FUNCTION Exist% (FileName$)
DECLARE FUNCTION Extension$ (FileName$)
DECLARE FUNCTION FindLast% (BYVAL Address, Size)
DECLARE FUNCTION FileNameOk% (ScrName$)
DECLARE FUNCTION FileSize& (FileName$)
DECLARE FUNCTION FLof& (Handle)
DECLARE FUNCTION FUsing$ (Number$, Mask$)
DECLARE FUNCTION InRange% (R%, C%, UlR%, UlC%, BrR%, BrC%)
DECLARE FUNCTION IsDup% (FldName$, Form() AS FieldInfo, CurFld%)
DECLARE FUNCTION IsThere% (FileName$)
DECLARE FUNCTION LibNo% (ScrName$, ScrnLib%())
DECLARE FUNCTION LibScrName$ (ScrNo, ScrnLib())
DECLARE FUNCTION MaxInt% (Var1%, Var2%)
DECLARE FUNCTION MinInt% (Var1%, Var2%)
DECLARE FUNCTION Monitor% ()
DECLARE FUNCTION NoExtension$ (FileName$)
DECLARE FUNCTION Num2Date$ (Dte%)
DECLARE FUNCTION ENum2Date$ (Dte%)
DECLARE FUNCTION OneColor% (Fg%, Bg%)
DECLARE FUNCTION ParsPath$ (FileSpec$)
DECLARE FUNCTION PeekBuf% ()
DECLARE FUNCTION QPLen% (Text$)


DECLARE SUB BLPrint (LPTNo%, Text$, ErrCount%)
DECLARE SUB Box0 (Top, Lc, Bot, Rc, LType, Clr)
DECLARE SUB BreakLines (Text$, Array$(), Element%, Wdth%)
DECLARE SUB ChangeClr (UlR%, UlC%, BrR%, BrC%, FromClr%, ToClr%)
DECLARE SUB Chime (Flag)
DECLARE SUB ConvexFrame (UlRow, UlCol, LrRow, LrCol)
DECLARE SUB DelLib (ScrnLib%(), EllNo%, NoDel%)
DECLARE SUB DialogBox (Title$, PrmtAns$(), Dl() AS Dialog, Mult$(), ExitCode%)
DECLARE SUB DisplayScrn (BTmp%(), Element%, MonoCode%, WipeType%)
DECLARE SUB ErrMsg (Message$)
DECLARE SUB Extents (UlRow%, UlCol%, BrRow%, BrCol%)
DECLARE SUB FClose (Handle)
DECLARE SUB FCreate (FileName$)
DECLARE SUB FOpen (FileName$, Handle)
DECLARE SUB FPut (Handle, Text$)
DECLARE SUB FPutA (Handle, SEG Element AS ANY, NumBytes&)
DECLARE SUB FGet (Handle, Text$)
DECLARE SUB GetCursor (X, Y, Button)
DECLARE SUB GetVMode (Mode, Page, PageSize, Rows, Columns)
DECLARE SUB HideCursor ()
DECLARE SUB KillFile (FileName$)
DECLARE SUB InsLib (ScrnLib%(), EllNo%, NoIns%)
DECLARE SUB Lib2Scrn (NameInLib$, ScrnLib%(), MonoCode%, Attribute%, ErrorCode%)
DECLARE SUB LibFile2Scrn (LibName$, ScrnName$, MonoCode%, Attribute%, ErrorCode%)
DECLARE SUB LoadObj (ObjName$, Attribute%, ErrorCode%)
DECLARE SUB MakeMono (SEG Element, Size)
DECLARE SUB MakeNewFldName (OldName$)
DECLARE SUB MAsciiPick (Code, InClr, HiLite, XCode)
DECLARE SUB MColorPick (Clr, BoarderClr, ExitCode)
DECLARE SUB MouseTrap (UlRow, UlCol, BrRow, BrCol)
DECLARE SUB MPRestore (UlRow, UlCol, BrRow, BrCol, Wide, SEG Element)
DECLARE SUB MPaintBox (UlRow%, UlCol%, LrRow%, LrCol%, Colr%)
DECLARE SUB MQPrint (X$, Colr)
DECLARE SUB MScrnSave (UlRow%, UlCol%, LrRow%, LrCol%, SEG Address%)
DECLARE SUB MScrnRest (UlRow%, UlCol%, LrRow%, LrCol%, SEG Address%)
DECLARE SUB QBLoad (FileName$, SEG Element AS ANY)
DECLARE SUB QPrintRC (Text$, Row, Col, Clr)
DECLARE SUB RulerLine (Inf AS ANY, Action%)
DECLARE SUB Scr2Obj (ObjName$, WipeType%, Inf AS ANY)
DECLARE SUB ScrEdit (K$, Inf AS GenInfo)
DECLARE SUB Scrn2Str (BYVAL Segment, BYVAL Address, StrLen)
DECLARE SUB ScrSize (UlRow%, UlCol%, LrRow%, LrCol%, Inf AS ANY)
DECLARE SUB SetCsr (Inf AS GenInfo)
DECLARE SUB ShowCursor ()
DECLARE SUB StuffBuf (X$)
DECLARE SUB UndoBlock (B%(), ScrBuf%())


CONST Ruler$ = "¡¡¡¡≈¡¡¡¡1¡¡¡¡≈¡¡¡¡2¡¡¡¡≈¡¡¡¡3¡¡¡¡≈¡¡¡¡4¡¡¡¡≈¡¡¡¡5¡¡¡¡≈¡¡¡¡6¡¡¡¡≈¡¡¡¡7¡¡¡¡≈¡¡¡¡8"

FUNCTION ASCIIChart$ (Ok2Print, Inf AS GenInfo) STATIC

   ASCIIChart$ = ""
   Temp = (Inf.Rows \ 2) - 7

   '----- Save the screen
   REDIM Tmp(1000)
   MScrnSave Temp, 24, Temp + 18, 60, Tmp(0)

   MPaintBox Temp + 18, 26, Temp + 18, 60, 8
   MPaintBox Temp + 1, 58, Temp + 17, 60, 8
   LOCATE Temp, 24, 0

   IF CCode = 0 THEN CCode = 127

   MAsciiPick CCode, 112, 15 OR 128, XCode

   DO
      GetCursor 0, 0, Buttons
   LOOP WHILE Buttons

   MScrnRest Temp, 24, Temp + 18, 60, Tmp(0)

   IF XCode = 0 AND CCode <> 7 THEN

      ASCIIChart$ = CHR$(CCode)
      IF Ok2Print THEN
         LOCATE Inf.CurRow, Inf.CurCol
         MQPrint CHR$(CCode), Inf.SysColor
         Inf.CurCol = Inf.CurCol + 1
         IF Inf.CurCol > 80 THEN
            Inf.CurCol = 1
            Inf.CurRow = MinInt(Inf.CurRow + 1, Inf.Rows)
         END IF
      END IF
   END IF
   ERASE Tmp

END FUNCTION

'********** Paint an Area of the Screen
SUB BlockPaint (Inf AS GenInfo) STATIC

   Changed = Inf.ScrChanged

   IF Inf.Ruler THEN RulerLine Inf, 5

   REDIM ScrBuf(Inf.Rows * 80)
   MScrnSave 1, 1, Inf.Rows, 80, ScrBuf(0)
     
   ORow = Inf.CurRow
   OCol = Inf.CurCol
   AncRow = ORow
   AncCol = OCol

   REDIM B(3, 1)
   B(0, 0) = ORow
   B(0, 1) = ORow
   B(1, 0) = OCol
   B(1, 1) = OCol
   B(2, 0) = ORow
   B(2, 1) = ORow
   B(3, 0) = OCol
   B(3, 1) = OCol

   CALL MPaintBox(B(0, 0), B(1, 0), B(2, 0), B(3, 0), Inf.Clr)

   Inf.EdType = 1
   LOCATE , , 1, 1, 13

   'IF Inf.Ruler THEN RulerLine Inf, 1
   K$ = CHR$(1)

   DO
         
      ScrEdit K$, Inf

      IF LEN(K$) OR Inf.CurRow <> ORow OR Inf.CurCol <> OCol THEN

         IF K$ = CHR$(27) THEN
            IF Inf.Ruler THEN RulerLine Inf, 5

            MScrnRest 1, 1, Inf.Rows, 80, ScrBuf(0)
          
            Inf.ScrChanged = Changed
            Inf.CurRow = AncRow
            Inf.CurCol = AncCol
            IF Inf.Ruler THEN RulerLine Inf, 1
          
            EXIT DO
         ELSEIF K$ = CHR$(13) THEN
            EXIT DO
         END IF

         B(0, 0) = MinInt(AncRow, Inf.CurRow)
         B(1, 0) = MinInt(AncCol, Inf.CurCol)
         B(2, 0) = MaxInt(AncRow, Inf.CurRow)
         B(3, 0) = MaxInt(AncCol, Inf.CurCol)

         UndoBlock B(), ScrBuf()

         '----- Paint the Block
         CALL MPaintBox(B(0, 0), B(1, 0), B(2, 0), B(3, 0), Inf.Clr)

         ORow = Inf.CurRow
         OCol = Inf.CurCol

         Inf.ScrChanged = -1

      END IF

   LOOP

   Inf.EdType = 0

   SetCsr Inf

   ERASE ScrBuf, B

END SUB

SUB BreakLines (Text$, Array$(), Element, Wdth)

    BrkTbl$ = " +-*/\)"
    I = Element
    P1 = 1
    P2 = Wdth
    Ln = QPLen(Text$)

    DO UNTIL P1 > Ln OR I > UBOUND(Array$)

       DO UNTIL P2 >= Ln OR INSTR(BrkTbl$, MID$(Text$, P2, 1)) = 0
          P2 = P2 + 1
       LOOP

       DO UNTIL P2 = 1 OR INSTR(BrkTbl$, MID$(Text$, P2, 1))
          P2 = P2 - 1
       LOOP

       Array$(I, 1) = RTRIM$(MID$(Text$, P1, P2 - P1 + 1))

       I = I + 1
       P1 = P2 + 1
       P2 = P1 + Wdth - 1

    LOOP

END SUB

SUB CvtBLScrns (ScrName$, ErrCode) STATIC

    'Copyright (c) 1987 Ethan Winer

    ASize& = (FileSize&(ScrName$) - 7) \ 2
    Rows = ASize& \ 80 - (ASize& MOD 80 > 0)

    SELECT CASE Rows
       CASE 1 TO 25
          Temp = 25
       CASE 26 TO 43
          Temp = 43
       CASE 44 TO 50
          Temp = 50
       CASE ELSE
          ErrCode = 3
          EXIT SUB
    END SELECT

    WIDTH , Temp

    REDIM BTmp(Rows * 80)               'the array to BLoad/BSave
    QBLoad ScrName$, BTmp(0)            'Load the File into a Buffer

    FOR C = 1 TO ASize&
       IF BTmp(C) MOD 256 = 7 THEN
          BTmp(C) = 256 * (BTmp(C) \ 256) + 248
       END IF
    NEXT
   
    MScrnRest 1, 1, Rows, 80, BTmp(0)

    ERASE BTmp

END SUB

SUB CvtFrm (FrmName$, Frm() AS FieldInfo)

    REDIM RelTemp(1 TO 1) AS OldFrm

    BLen& = FileSize&(FrmName$) - 7
   
    Fields = BLen& \ 40

    REDIM Temp(Fields) AS OldFrm
    QBLoad FrmName$, Temp(0)

    Frm(0).Fields = Temp(0).Fields
    Frm(0).Row = Temp(0).Row - 1

    FOR N = 1 TO Temp(0).Fields
        Frm(N).Fields = Frm(0).LCol + 1
        Frm(0).LCol = Frm(0).LCol + 1

        Frm(N).Row = Temp(N).Row
        Frm(N).LCol = Temp(N).LCol
        Frm(N).RCol = Temp(N).RCol
        Frm(N).StorLen = Temp(N).StorLen

        IF Temp(N).FType >= Relational AND Temp(N).FType <= MultChFld THEN

           Frm(N).RelFile = Temp(N).RelFile
           Frm(N).RelFld = Temp(N).RelFld

           RelName$ = ParsPath$(ScrName$) + Temp(N).RelFile + ".FRM"

           IF RelName$ <> LastRelName$ THEN

              FOpen RelName$, Handle
              BLen& = FLof&(Handle) - 7
              FGet Handle, Byte$
              FClose Handle

              IF Byte$ = BHeader$ THEN
                 Fields = BLen& \ 40 + 1

                 REDIM RelTemp(1 TO Fields) AS OldFrm
                 QBLoad RelName$, RelTemp(0)
              ELSE
                 OPEN RelName$ FOR RANDOM AS #1 LEN = FrmWdth
              END IF
           END IF

           IF Byte$ = BHeader$ THEN
              Frm(N).ScratchI = RelTemp(Temp(N).RelFld).FType
           ELSE
              GET #1, Temp(N).RelFld + 1, RelTemp(1)
              Frm(N).ScratchI = RelTemp(1).FType
           END IF
           LastRelName$ = RelName$
        END IF

        Frm(N).FType = Temp(N).FType

        Frm(N).Indexed = Temp(N).Indexed
        Frm(N).FldName = Temp(N).FldName
        Frm(N).Decimals = Temp(N).Decimals
        Frm(N).RelHandle = Temp(N).RelHandle
        Frm(N).Protected = Temp(N).Protected

        SELECT CASE Frm(N).FType
           CASE IntFld
              Frm(N).LowRange = -32768#
              Frm(N).HiRange = 32767#
           CASE DateFld, EuroDateFld
              Frm(N).LowRange = -29219#
              Frm(N).HiRange = 31368#
           CASE LongFld
              Frm(N).LowRange = -2147483648#
              Frm(N).HiRange = 2147483647#
           CASE SngFld
              Frm(N).LowRange = -3.402823D+38
              Frm(N).HiRange = 3.402823D+38
           CASE DblFld, MoneyFld
              Frm(N).LowRange = -1.79769313486231D+308
              Frm(N).HiRange = 1.79769313486231D+308
           CASE ELSE
        END SELECT

    NEXT

    CLOSE

    RelName$ = ""
    LastRelName$ = ""
    ERASE Temp, RelTemp

END SUB

SUB CvtLib (LibName$, NewLib()) STATIC

    ASize& = (FileSize&(Path$ + LibName$) - 8) \ 2

    REDIM OldLib(ASize&)

    QBLoad LibName$, OldLib(0)          'Load the File into a Buffer
   
    NewLib(0) = OldLib(0)
    NewLib(1) = 26

    ScrOff = (NewLib(0) + 1) * 7

    FOR N = 1 TO OldLib(0)

        StartScr = OldLib(N * 11)
        Length = OldLib((N * 11) + 1)
        Offset = OldLib((N * 11) + 2)

        UlRow = (Offset \ 160) + 1
        UlCol = 1
        BrRow = (Length \ 80) + UlRow - 1
        BrCol = 80

        NewLib(ScrOff) = 256 * UlCol + UlRow
        NewLib(ScrOff + 1) = 256 * BrCol + BrRow

        ALen = (BrRow - UlRow + 1) * 160 + 4

    '0  - Offset into array of beginning of screen
    '1  - Screen Length
    '2  - byte 1 = clear screen flag, byte 2 = Screen lines
    '3  - Name starts here
    '4  -       .
    '5  -       .
    '6  - Name ends here

        CALL BCopy(VARSEG(OldLib(StartScr)), VARPTR(OldLib(StartScr)), VARSEG(NewLib(ScrOff + 2)), VARPTR(NewLib(ScrOff + 2)), ALen - 4, 0)

        FOR C = ScrOff + 2 TO ScrOff + (ALen \ 2) - 1
           IF NewLib(C) MOD 256 = 7 THEN
              NewLib(C) = 256 * (NewLib(C) \ 256) + 248
           END IF
        NEXT

        DicOff = N * 7
        NewLib(DicOff) = ScrOff
        NewLib(DicOff + 1) = ALen

        NewLib(DicOff + 2) = 25 * 256

        DEF SEG = VARSEG(NewLib(0))
        Offset = VARPTR(NewLib(DicOff + 3))
        FOR C = 1 TO 8
            CTR = OldLib((N * 11) + 2 + C)
            IF CTR = 0 THEN EXIT FOR

            POKE Offset + C - 1, CTR
        NEXT

        ScrOff = ScrOff + (ALen \ 2)'/ 2 + .1)

    NEXT

    NewLib(2) = ScrOff - 1

    ERASE OldLib

END SUB

SUB CvtQBScr (ScrName$, ErrCode) STATIC

    ASize& = (FileSize&(ScrName$) - 8) \ 2
    IF ASize& > 3000 THEN
       ErrCode = 3
       EXIT SUB
    END IF

    REDIM BTmp(ASize&)

    QBLoad ScrName$, BTmp(0)           'Load the File into a Buffer

    UlRow = (BTmp(0) \ 160) + 1
    BrRow = (ASize& \ 80) + UlRow - 1

    FOR C = 1 TO ASize&
       IF BTmp(C) MOD 256 = 7 THEN
          BTmp(C) = 256 * (BTmp(C) \ 256) + 248
       END IF
    NEXT


    MScrnRest UlRow, 1, BrRow, 80, BTmp(1)

    ERASE BTmp

END SUB

SUB DefineMenus (MenuFile$, MMenu$(), ChStat(), MenuHelp() AS STRING * 80) STATIC

REDIM MenuHelp(16, 6) AS STRING * 80
IF Exist(MenuFile$) THEN

   OPEN MenuFile$ FOR INPUT AS #1
   FOR M = 0 TO 6
       FOR C = 0 TO 16
           IF EOF(1) THEN EXIT FOR
           LINE INPUT #1, MenuHelp(C, M)
       NEXT
   NEXT
   CLOSE #1
END IF


'*******  Define Main Menu
REDIM MMenu$(16, 6)               'Main Menu Choices
REDIM ChStat(16, 6)               'Main Menu Choice Status

MMenu$(0, 0) = "File"
MMenu$(1, 0) = "New Screen"
MMenu$(2, 0) = "Open Screen File..."
MMenu$(3, 0) = "Save as Screen File..."
MMenu$(4, 0) = "-"
MMenu$(5, 0) = "Import Text File..."
MMenu$(6, 0) = "Export Screen as Text..."
MMenu$(7, 0) = "-"
MMenu$(8, 0) = "Load Object File..."
MMenu$(9, 0) = "Save as Object File..."
ChStat(9, 0) = 9 * 256
MMenu$(10, 0) = "-"
MMenu$(11, 0) = "DOS Shell"
MMenu$(12, 0) = "Exit"
ChStat(12, 0) = 256


MMenu$(0, 1) = "Library"
MMenu$(1, 1) = "Display Screen...         F2"
MMenu$(2, 1) = "-"
MMenu$(3, 1) = "Create New Library..."
MMenu$(4, 1) = "Open Library..."
MMenu$(5, 1) = "Save Library..."
MMenu$(6, 1) = "-"
MMenu$(7, 1) = "Add Screen to Library..."
MMenu$(8, 1) = "Replace Screen in Library..."


MMenu$(0, 2) = "Edit"
MMenu$(1, 2) = "Paint Block      Ctrl+P"
MMenu$(2, 2) = "Replace Color..."
MMenu$(3, 2) = "-"
MMenu$(4, 2) = "Box Drawing      Ctrl+B"
MMenu$(5, 2) = "Line Drawing     Ctrl+D"
MMenu$(6, 2) = "Fill Character   Ctrl+F"
MMenu$(7, 2) = "-"
MMenu$(8, 2) = "Cut Block     Shift+Del"
ChStat(8, 2) = 2 * 256
MMenu$(9, 2) = "Copy Block     Ctrl Ins"
MMenu$(10, 2) = "Paste Block   Shift+Ins"
MMenu$(11, 2) = "Move Block       Ctrl+M"
MMenu$(12, 2) = "Clear               Del"
ChStat(12, 2) = 2 * 256
MMenu$(13, 2) = "-"
MMenu$(14, 2) = "Center Line/Block    F5"
ChStat(14, 2) = 2 * 256
MMenu$(15, 2) = "-"
MMenu$(16, 2) = "Repeat Last Key  Ctrl+R"
ChStat(16, 2) = 12 * 256


MMenu$(0, 3) = "View"
MMenu$(1, 3) = "ASCII Chart              Ctrl+A"
MMenu$(2, 3) = "  Ruler Line                 F4"
ChStat(2, 3) = 2 * 256
MMenu$(3, 3) = "-"
MMenu$(4, 3) = "Monochrome - Via `DisplayScrn'"
ChStat(4, 3) = 18 * 256
MMenu$(5, 3) = "Monochrome - Standard Mono Card"
ChStat(5, 3) = 13 * 256
MMenu$(6, 3) = "Monochrome - CGA+Mono Monitor"
ChStat(6, 3) = 14 * 256


MMenu$(0, 4) = "Settings"
MMenu$(1, 4) = "Painting Color...   Ctrl+C"
MMenu$(2, 4) = "Blinking    "
MMenu$(3, 4) = "  Draw Using Current Color"
ChStat(3, 4) = 13 * 256
MMenu$(4, 4) = "-"
MMenu$(5, 4) = "Drawing Line Types..."
MMenu$(6, 4) = "-"
MMenu$(7, 4) = "25 Line Mode"
MMenu$(8, 4) = "43 Line Mode"
MMenu$(9, 4) = "50 Line Mode"
MMenu$(10, 4) = "-"
MMenu$(11, 4) = "  Beep on Errors"
ChStat(11, 4) = 10 * 256


MMenu$(0, 5) = "Compose-Fields"
MMenu$(1, 5) = "Define Data Fields..."
MMenu$(2, 5) = "  Paint when Defined..."
ChStat(2, 5) = 2 * 256
MMenu$(3, 5) = "-"
MMenu$(4, 5) = "Rearrange Data Fields..."
MMenu$(5, 5) = "Print Field Definitions"
MMenu$(6, 5) = "-"                              'DRM3.1 Added new item
MMenu$(7, 5) = "Try Data Entry in Form"


MMenu$(0, 6) = "F1=Help"
ChStat(0, 6) = 3 * 256 + 15


END SUB

'********** SUBTRACT Space in the Users Library
SUB DelLib (ScrnLib(), EllNo, NoDel) STATIC

   NScrns = ScrnLib(0)             'Find number of Screens
   TopDic = NScrns * 7             'Find Last Dictionary Address
   TopLib = ScrnLib(TopDic) + ((ScrnLib(TopDic + 1) \ 2) + (ScrnLib(TopDic + 1) MOD 2))

   FOR N = EllNo TO TopLib
      ScrnLib(N) = ScrnLib(N + NoDel)
   NEXT N
END SUB

FUNCTION EndOfField% (Inf AS GenInfo) STATIC

    LOCATE , , 0
    HideCursor

    CurChar = SCREEN(Inf.CurRow, Inf.CurCol)
    CurClr = SCREEN(Inf.CurRow, Inf.CurCol, 1)
   
    FOR N = Inf.CurCol + 1 TO 80
        
        Clr = SCREEN(Inf.CurRow, N, 1)
        Char = SCREEN(Inf.CurRow, N)

        IF Clr <> CurClr THEN EXIT FOR
        
        IF Char >= 174 AND Char <= 223 THEN
           IF CurChar < 174 OR CurChar > 223 THEN EXIT FOR
        END IF

    NEXT

    LOCATE , , 1
    ShowCursor

    EndOfField = N - 1

END FUNCTION

FUNCTION Eval# (E$, ErrCode)

    ErrCode = -1
    Eval# = 0
   
    Sign = 0
    S1 = 0
    Decm = 0
    Xpon = 0

    X$ = E$
    T$ = " "
    Tbl$ = "-+.eEdD0123456789"

    Ln = LEN(X$)
    X = 1
    DO UNTIL X > Ln

       LSET T$ = MID$(X$, X, 1)

       OK = INSTR(Tbl$, T$)

       SELECT CASE OK
          CASE 0
             MID$(X$, X) = MID$(X$, X + 1)
             MID$(X$, Ln) = " "
             Ln = Ln - 1
          CASE 1, 2
             Sign = Sign + 1
             IF X < Xpon AND Sign > 1 THEN
                EXIT DO
             ELSEIF X < Xpon THEN
                S1 = X
             END IF
          CASE 3
             IF Decm = 0 THEN
                Decm = X
             ELSE
                EXIT DO
             END IF
          CASE 4 TO 7
             IF Xpon = 0 THEN
                Xpon = X
             ELSE
                EXIT DO
             END IF
          CASE ELSE
       END SELECT

       X = X + 1
    LOOP


    Whole = Decm - S1 - 1
    IF Whole > 308 THEN EXIT FUNCTION

    IF Xpon THEN
       Num# = VAL(LEFT$(X$, Xpon))
       Xpon = VAL(MID$(X$, Xpon + 1))

       Num# = Num# / (10 ^ (Whole - 1))
       Xpon = Xpon + Whole - 1

       IF Xpon > 308 THEN
          EXIT FUNCTION
       ELSEIF Xpon = 308 AND ABS(Num#) > 1.79769313486232# THEN
          EXIT FUNCTION
       END IF

    END IF

    Eval# = VAL(X$)
    ErrCode = 0

END FUNCTION

FUNCTION Extension$ (FileName$) STATIC

    Per = INSTR(FileName$, ".")
    IF Per THEN
       Extension$ = MID$(FileName$, Per)
    ELSE
       Extension$ = ""
    END IF

END FUNCTION

SUB FillChar (Inf AS GenInfo) STATIC

   FlChar$ = ASCIIChart$(0, Inf)
   IF LEN(FlChar$) = 0 THEN EXIT SUB

   AncRow = Inf.CurRow
   AncCol = Inf.CurCol

   REDIM B(3, 1)
   IF Inf.MarkBlock THEN
      B(0, 0) = Inf.BUlRow
      B(0, 1) = Inf.BUlRow
      B(1, 0) = Inf.BUlCol
      B(1, 1) = Inf.BUlCol
      B(2, 0) = Inf.BBrRow
      B(2, 1) = Inf.BBrRow
      B(3, 0) = Inf.BBrCol
      B(3, 1) = Inf.BBrCol

      StuffBuf (CHR$(1))
      ScrEdit "", Inf

      IF Inf.Ruler THEN RulerLine Inf, 5

      GOSUB JustFill

      Inf.WasMarked = 0
      Inf.MarkBlock = 0
      Inf.ScrChanged = -1
      IF Inf.Ruler THEN RulerLine Inf, 1
      EXIT SUB
   ELSE
      B(0, 0) = AncRow
      B(0, 1) = AncRow
      B(1, 0) = AncCol
      B(1, 1) = AncCol
      B(2, 0) = AncRow
      B(2, 1) = AncRow
      B(3, 0) = AncCol
      B(3, 1) = AncCol
   END IF

   IF Inf.Ruler THEN RulerLine Inf, 5

   REDIM ScrBuf(Inf.Rows * 80)
   MScrnSave 1, 1, Inf.Rows, 80, ScrBuf(0)

   LOCATE Inf.CurRow, Inf.CurCol, 1, 1, 13
   CALL MQPrint(FlChar$, Inf.SysColor)         'Print the Top Left Corner

   Inf.EdType = 1

   K$ = CHR$(1)

   DO
      ScrEdit K$, Inf

      IF QPLen(K$) THEN

         IF K$ = CHR$(13) THEN                  'If Enter pressed then Done
            Inf.ScrChanged = -1
            EXIT DO

         ELSEIF K$ = CHR$(27) THEN
            MScrnRest 1, 1, Inf.Rows, 80, ScrBuf(0)
            Inf.CurRow = AncRow
            Inf.CurCol = AncCol
            IF Inf.Ruler THEN RulerLine Inf, 1
            EXIT DO

         ELSE
            GOSUB DoFill

         END IF
      
      END IF

   LOOP
   Inf.EdType = 0

   SetCsr Inf

   ERASE ScrBuf
   Temp$ = ""

   EXIT SUB

DoFill:
   B(0, 0) = MinInt(AncRow, Inf.CurRow)
   B(1, 0) = MinInt(AncCol, Inf.CurCol)
   B(2, 0) = MaxInt(AncRow, Inf.CurRow)
   B(3, 0) = MaxInt(AncCol, Inf.CurCol)

   UndoBlock B(), ScrBuf()
JustFill:
   HideCursor

   FOR N = B(0, 0) TO B(2, 0)
       QPrintRC STRING$(B(3, 0) - B(1, 0) + 1, FlChar$), N, B(1, 0), Inf.SysColor
   NEXT

   ShowCursor

RETURN

END SUB

SUB GetColor (Clr, ExitCode)

    LOCATE 3, 45
    Temp = 4
    X = 46

    '----- Save the screen
    REDIM Tmp(1000)
    MScrnSave Temp - 1, X - 1, Temp + 17, X + 34, Tmp(0)

    MPaintBox Temp, X + 33, Temp + 16, X + 34, 8
    MPaintBox Temp + 17, X + 1, Temp + 17, X + 34, 8

    MColorPick Clr, 112, ExitCode

    MScrnRest Temp - 1, X - 1, Temp + 17, X + 34, Tmp(0)
    ERASE Tmp

END SUB

' ********** Display the Help Screens
SUB GetHelp (ProgLib$, MonoCode, Inf AS GenInfo)

   REDIM ScrBuf(Inf.Rows * 80)
   MScrnSave 1, 1, Inf.Rows, 80, ScrBuf(0)
   LOCATE , , 0
   IF LEN(NameLib$) = 0 THEN
      NameLib$ = "H1"
      Nxt1$ = "BLOCKOPS"
      Nxt2$ = "BOXES"
   END IF

   DO
      LibFile2Scrn ProgLib$, NameLib$, MonoCode, 0, 0

      DO
         Get$ = INKEY$
      LOOP UNTIL LEN(Get$)

      IF Get$ <> CHR$(27) THEN
         SWAP NameLib$, Nxt1$
         SWAP Nxt1$, Nxt2$
      END IF
   LOOP UNTIL Get$ = CHR$(27)

   MScrnRest 1, 1, Inf.Rows, 80, ScrBuf(0)
  
   ERASE ScrBuf
   LOCATE , , 1

END SUB

FUNCTION InRange% (R, C, UlR, UlC, BrR, BrC)

   InRange% = 0

   IF R >= UlR AND R <= BrR THEN
      IF C >= UlC AND C <= BrC THEN
         InRange% = -1
      END IF
   END IF

END FUNCTION

'********** ADD (Insert) Space in the Users Library
SUB InsLib (ScrnLib(), EllNo, NoIns) STATIC

   NScrns = ScrnLib(0)                  'Find number of Screens
   TopDic = NScrns * 7                  'Find Last Dictionary Address
   TopLib = ScrnLib(TopDic) + ((ScrnLib(TopDic + 1) \ 2) + (ScrnLib(TopDic + 1) MOD 2))

   FOR N = TopLib TO EllNo STEP -1
      ScrnLib(N + NoIns) = ScrnLib(N)
   NEXT N
END SUB

'************** Check for a Duplicate Field Name
FUNCTION IsDup% (FldName$, Form() AS FieldInfo, CurFld%)

   NewName$ = SPACE$(8)
   LSET NewName$ = UCASE$(FldName$)

   IsDup% = 0

   FOR Fld = 1 TO Form(0).Fields
      IF Form(Fld).FldName = NewName$ AND Fld <> CurFld THEN
         IsDup = -1
         EXIT FOR
      END IF
   NEXT

END FUNCTION

FUNCTION IsThere (FileName$) STATIC

   IsThere = 0
   IF Exist(FileName$) THEN
      XCode = AlertUser(FileName$ + " already exists.", "Cancel", "Delete File", "")

      IF XCode = 2 THEN
         KILL FileName$
      ELSE
         IsThere = -1
      END IF
   END IF

END FUNCTION

SUB LibScrMenu (ScrName$, ScrnLib(), Inf AS GenInfo)

   ScrName$ = ""

   'MostChoices = MinInt(ScrnLib(0), 10)

   REDIM Dial(5) AS Dialog
   Dial(1).DRow = 1
   Dial(1).DCol = 2
   Dial(1).DTyp = 6

   Dial(2).DRow = 2
   Dial(2).DCol = 2
   Dial(2).DLen = 256 * 4 + 10
   Dial(2).DTyp = 4

   Dial(3).DRow = 4 + 10
   Dial(3).DCol = 2
   Dial(3).DTyp = 5

   Dial(4).DRow = 4 + 10
   Dial(4).DCol = 17
   Dial(4).DTyp = 5

   Dial(5).DRow = 4 + 10
   Dial(5).DCol = 31
   Dial(5).DTyp = 5

   REDIM Fd$(5, 1)

   Fd$(1, 0) = "Select a Screen Name to Edit:"

   Fd$(3, 0) = "Display"
   Fd$(4, 0) = "Delete"
   Fd$(5, 0) = "Cancel"

   REDIM Mult$(ScrnLib(0))

   FOR N = 1 TO ScrnLib(0)
       Mult$(N) = SPACE$(8)
       LSET Mult$(N) = LibScrName$(N, ScrnLib())
   NEXT
  
   CALL SortStr(BYVAL VARPTR(Mult$(1)), ScrnLib(0), 0)

   LOCATE (Inf.Rows \ 2) - 7
   DialogBox "Display", Fd$(), Dial(), Mult$(), ExitCode


   NameLib$ = Mult$(VAL(Fd$(2, 1)))

   IF ExitCode = 1 THEN
      IF Inf.MarkBlock THEN
         StuffBuf CHR$(255)
         ScrEdit "", Inf
      END IF

      REDIM ScrBuf(Inf.Rows * 80)
      MScrnSave 1, 1, Inf.Rows, 80, ScrBuf(0)
      HideCursor
      CLS                       'Find the screen & display it
      ShowCursor
      Attribute = -3
      Lib2Scrn NameLib$, ScrnLib(), 0, Attribute, ErrorCode
      Inf.Lines = Attribute \ 256
      Inf.DispType = Attribute MOD 256
         
      IF ErrorCode THEN
         SELECT CASE ErrorCode
            CASE 1
               Msg$ = NameLib$ + " is not in the library."
            CASE 2
               Msg$ = "This screen is too big to fit on this monitor."
            CASE ELSE
         END SELECT
         IF Inf.BeepOn THEN Chime 6
         ErrMsg Msg$
         MScrnRest 1, 1, Inf.Rows, 80, ScrBuf(0)
      ELSE
         ScrName$ = NameLib$
         Inf.FromLib = -1
      END IF
      GetVMode 0, 0, 0, Inf.Rows, 0
      ERASE ScrBuf

   ELSEIF ExitCode = 2 THEN

      N = LibNo(NameLib$, ScrnLib())

      DicOff = N * 7                            'Offset into dictionary
      Offset = ScrnLib(DicOff)
      ScrLen = (ScrnLib(DicOff + 1) \ 2) + (ScrnLib(DicOff + 1) MOD 2)

      CALL DelLib(ScrnLib(), Offset, ScrLen)    'Delete the screen

      FOR C = N + 1 TO ScrnLib(0)               'Adjust Dictionary Offsets
         ScrnLib(C * 7) = ScrnLib(C * 7) - ScrLen
      NEXT

      CALL DelLib(ScrnLib(), DicOff, 7)         'Delete the Dictionary Entry
      ScrnLib(0) = ScrnLib(0) - 1
      FOR C = 1 TO ScrnLib(0)                   'Adjust Dictionary Offsets
         ScrnLib(C * 7) = ScrnLib(C * 7) - 7
      NEXT

      Inf.LibChanged = 1                'Library was changed so set flag

   END IF

   ERASE Dial, Fd$, Mult$

END SUB

SUB LoadObj (ObjName$, Attribute, ErrorCode)

    ErrorCode = 0
    HideCursor
    CLS
    ShowCursor

    IF NOT Exist(ObjName$) THEN
       ErrorCode = 1
       EXIT SUB
    END IF

    OPEN ObjName$ FOR BINARY AS #1

    A$ = " "
    GET #1, 2, A$
    A = ASC(A$) - 6
    ObjType$ = SPACE$(7)
    GET #1, (75 + 2 * A), ObjType$

' Two types of object files are possible, the first (PUTSCRN) writes
' directly to the active screen and the second (OBJ2ARAY) transfers the
' screen to a full screen integer array.  The latter type facilitates
' screen wipes.  We have to determine which one is being loaded by
' searching for "PUTSCRN" or "OBJ2ARA".

    SELECT CASE ObjType$
       CASE "OBJ2ARA"
          Start = 137 + 3 * A
          Leng = 987
       CASE "PUTSCRN"
          Start = 131 + 3 * A
          Leng = 992
       CASE ELSE
          ErrorCode = 3
          CLOSE #1
          EXIT SUB
    END SELECT

    StrLen = LOF(1) - Start - 5
    A$ = SPACE$(StrLen)
    GET #1, Start, A$                'load OBJ file in one shot
    CLOSE

    WipeType = 0
    IF ObjType$ = "OBJ2ARA" THEN
       WipeType = ASC(RIGHT$(A$, 1))
    END IF

    UlRow = ASC(MID$(A$, 1, 1))
    UlCol = ASC(MID$(A$, 2, 1))
    BrRow = ASC(MID$(A$, 3, 1))
    BrCol = ASC(MID$(A$, 4, 1))

    SELECT CASE Monitor%
       CASE 7, 11
          Rows = 50
       CASE 5
          Rows = 43
       CASE ELSE
          Rows = 25
    END SELECT

    IF BrRow > Rows THEN
       ErrorCode = 2
       CLOSE #1
       EXIT SUB
    END IF

    IF BrRow <= 25 THEN
       ReqLines = 25
    ELSEIF BrRow <= 43 THEN
       ReqLines = 43
    ELSE
       ReqLines = 50
    END IF


    GetVMode 0, 0, 0, ScrRows, 0
    IF ReqLines <> ScrRows THEN
       WIDTH , ReqLines
       MouseTrap 1, 1, Rows, 80
    END IF


    Start = 5
    IF StrLen < Leng + 4 THEN Leng = StrLen - 4
    Strg$ = MID$(A$, Start, Leng)

    Start = Start + Leng + 28
    Leng = 1020

    WHILE Start < StrLen
       Remaining = StrLen - Start + 1
       IF Leng > Remaining THEN Leng = Remaining
       Strg$ = Strg$ + MID$(A$, Start, Leng)
       Start = Start + 1027
    WEND

    ScrLen = (BrRow - UlRow + 1) * (BrCol - UlCol + 1) + 2
    REDIM ScrBuf(ScrLen)
    ScrBuf(1) = 256 * UlCol + UlRow
    ScrBuf(2) = 256 * BrCol + BrRow

    Ptr = 1
    Leng = LEN(Strg$)

    FOR I = 3 TO ScrLen
       Count = 0
       A$ = MID$(Strg$, Ptr, 2)
       Ptr = Ptr + 2
       IF ASC(A$) = 7 THEN                    'repeat character
          '----- number of times to repeat
          Count = ASC(RIGHT$(A$, 1))
          '----- character and attribute to put on screen
          A$ = MID$(Strg$, Ptr, 2)
          Ptr = Ptr + 2
       END IF

       A = 256 * ASC(RIGHT$(A$, 1)) + ASC(A$)
       FOR J = 0 TO Count
           ScrBuf(I + J) = A
       NEXT
       I = I + Count
    NEXT

    Attribute = ReqLines * 256 + WipeType

    DisplayScrn ScrBuf(), 1, 0, WipeType

END SUB

SUB MakeNewFldName (OldName$)

   '----- Find the end of the field's name
   FOR Nd = 1 TO 8
      Temp = ASC(MID$(OldName$, Nd, 1))
      IF Temp = 32 OR Temp = 0 THEN EXIT FOR
   NEXT

   '----- Look backwards for non-number
   FOR I = Nd - 1 TO 1 STEP -1
      Temp = ASC(MID$(OldName$, I, 1))
      IF Temp < 48 OR Temp > 57 THEN EXIT FOR
   NEXT
   I = I + 1

   '----- Find their field number if any
   Temp = VAL(MID$(OldName$, I))
   Temp = Temp + 1
   IF Temp = 1 THEN Temp = 2

   '----- Put the new number in the name
   Num$ = LTRIM$(STR$(Temp))
   I = MinInt(I, 8 - LEN(Num$) + 1)

   MID$(OldName$, I) = Num$

END SUB

SUB MakeUniqueFldName (FldNo, Form() AS FieldInfo)

   DO
      FOR I = 1 TO Form(0).Fields
         IF I <> FldNo AND Form(FldNo).FldName = Form(I).FldName THEN
            MakeNewFldName Form(FldNo).FldName
            EXIT FOR
         END IF
      NEXT
   LOOP UNTIL I > Form(0).Fields

END SUB

SUB MonoDisp (Choice, Inf AS GenInfo) STATIC

    REDIM Orig(Inf.Rows * 80)
    REDIM Mono(Inf.Rows * 80)
    One = 1

    MScrnSave One, One, Inf.Rows, 80, Orig(0)
    MScrnSave One, One, Inf.Rows, 80, Mono(0)

    IF Choice = 4 THEN
       MakeMono Mono(0), Inf.Rows * 80
    END IF

    DEF SEG = VARSEG(Mono(0))
    Addr = VARPTR(Mono(0))
    FOR N = 1 TO Inf.Rows * 160 STEP 2

        CALL SplitColor(PEEK(N + Addr), Fg, Bg)

        SELECT CASE Fg MOD 16
           CASE 1 TO 8
              Fg = 2
           CASE 9 TO 15
              Fg = 10
           CASE ELSE
        END SELECT

        SELECT CASE Choice
           CASE 4
              IF Bg > 0 AND (Fg = 0 OR Fg = 10) THEN
                 Bg = 2
              ELSE
                 Bg = 0
              END IF
           CASE 5
              IF Bg < 7 THEN
                 'IF Fg = 0 OR Fg = 10 THEN Fg = 2
                 Bg = 0
              ELSEIF Fg = 0 OR Fg = 10 THEN
                 Bg = 2
              ELSE
                 Bg = 0
              END IF
           CASE 6
              IF Bg > 0 THEN Bg = 2
           CASE ELSE
        END SELECT

        POKE N + Addr, OneColor(Fg, Bg)
    NEXT

    MScrnRest One, One, Inf.Rows, 80, Mono(0)
    DO UNTIL PeekBuf
    LOOP

    MScrnRest One, One, Inf.Rows, 80, Orig(0)
    ERASE Orig, Mono

END SUB

FUNCTION NoExtension$ (FileName$)

   L = LEN(FileName$)
   FOR P = L TO 1 STEP -1
      Char$ = MID$(FileName$, P, 1)
      IF Char$ = "." THEN
         L = P - 1
         EXIT FOR
      END IF
      IF INSTR(":\", Char$) THEN EXIT FOR
   NEXT

   NoExtension$ = LEFT$(FileName$, L)

END FUNCTION

'******* Print the form definition
SUB PrintFRM (ScrName$, Form() AS FieldInfo, Calc$(), Help$(), Inf AS GenInfo)

   REDIM FType$(24)
   FType$(1) = "String$"
   FType$(2) = "UCase$"
   FType$(3) = "Integer%"
   FType$(4) = "Single!"
   FType$(5) = "Double#"
   FType$(6) = "Date"
   FType$(7) = "Phone"
   FType$(8) = "Soc Sec"
   FType$(9) = "Zip"
   FType$(10) = "$Money"
   FType$(11) = "Relational"
   FType$(12) = "Virtual"
   FType$(13) = "Choice"
   FType$(14) = "Logical"
   FType$(15) = "Sequencing"
   FType$(16) = "Numeric$"
   FType$(17) = "Notes"
   FType$(18) = "Long Int."
   FType$(19) = "Euro. Date"
   FType$(20) = "Record #"
   FType$(21) = "Last Rec."
   FType$(22) = "Choice Ar."
   FType$(23) = "Prop. Name"
   FType$(24) = "Button"

   REDIM Temp$(3, 1)

   PLine$ = SPACE$(79)

   Page = 0
   LCount = 0
   ErrCount = -1
   GOSUB PrintHeader

   Offset = 1
   
   FOR X = 1 TO Form(0).Fields

      LSET PLine$ = FUsing$(STR$(X), "###")     'the field sequence #
                                                'offset in the field
      MID$(PLine$, 6) = FUsing$(STR$(Offset), "#####")
      Offset = Offset + Form(X).StorLen         'accumulate the offset

      MID$(PLine$, 12) = Form(X).FldName        'Field Name

      MID$(PLine$, 21) = FType$(Form(X).FType)  'the field type
                                                'the field length
      MID$(PLine$, 34) = FUsing$(STR$(Form(X).RCol - Form(X).LCol + 1), "##")


      FType = Form(X).FType
      IF FType >= 11 AND FType <= 13 THEN
         FType = Form(X).ScratchI
      END IF
                                                '# of decimal places
      IF FType = 4 OR FType = 5 OR FType = 10 THEN
         MID$(PLine$, 36) = "." + LTRIM$(STR$(Form(X).Decimals))
      END IF
                                                'length within record
      MID$(PLine$, 39) = FUsing$(STR$(Form(X).StorLen), "######")

                                                'the field row and column
      MID$(PLine$, 47) = FUsing$(STR$(Form(X).Row), "##") + ", "
      MID$(PLine$, 51) = FUsing$(STR$(Form(X).LCol), "##")

      IF Form(X).RelFld THEN                    'Relation information
         MID$(PLine$, 54) = Form(X).RelFile
         MID$(PLine$, 65) = FUsing$(STR$(Form(X).RelFld), "###")
      END IF

      IF Form(X).Indexed THEN                   'see if indexed or not
         MID$(PLine$, 71) = "Yes"
      ELSE
         MID$(PLine$, 71) = "No"
      END IF

      IF Form(X).Protected THEN                 'see if protected or not
         MID$(PLine$, 77) = "Yes"
      ELSE
         MID$(PLine$, 77) = "No"
      END IF

      

      FType = Form(X).FType
      IF FType = Relational THEN FType = Form(X).ScratchI
      SELECT CASE FType
         CASE IntFld, LongIntFld, SngFld, DblFld, MoneyFld
            Lo$ = LTRIM$(STR$(Form(X).LowRange))
            Hi$ = LTRIM$(STR$(Form(X).HiRange))
         CASE DateFld, EuroDateFld
            IF FType = DateFld THEN
               Lo$ = Num2Date$(INT(Form(X).LowRange))
               Hi$ = Num2Date$(INT(Form(X).HiRange))
            ELSE
               Lo$ = ENum2Date$(INT(Form(X).LowRange))
               Hi$ = ENum2Date$(INT(Form(X).HiRange))
            END IF
         CASE ELSE
            Lo$ = ""
            Hi$ = ""
      END SELECT

      
      IF LEN(Calc$(X)) OR LEN(Help$(X)) OR LEN(Lo$) THEN
         FieldDone = 0
      ELSE
         FieldDone = -1
      END IF

      GOSUB PrintIt

      IF LEN(Lo$) THEN
         LSET PLine$ = "Range: " + Lo$ + " to " + Hi$
         GOSUB PrintIt
      END IF

      IF LEN(Calc$(X)) THEN
         REDIM Temp$(3, 1)
         BreakLines Calc$(X), Temp$(), 1, 69
         LSET PLine$ = "FORMULA:"
         GOSUB PrintText
      END IF

      IF LEN(Help$(X)) THEN
         REDIM Temp$(3, 1)
         BreakLines Help$(X), Temp$(), 1, 69
         LSET PLine$ = "HELP:"
         GOSUB PrintText
      END IF

      IF LEN(Calc$(X)) OR LEN(Help$(X)) OR LEN(Lo$) THEN
         LSET PLine$ = "--- ------ -------- ---------- ------ ------ ------- -------- ------ ----- ----"
         FieldDone = -1
         GOSUB PrintIt
      END IF
      FieldDone = 0

   NEXT

   BLPrint 1, CHR$(12), ErrCount                'Send a Form feed

   EXIT SUB




PrintHeader:
   GOSUB PrintIt

   Page = Page + 1
   LSET PLine$ = BaseName$(ScrName$) + ":" + STR$(Form(0).Fields) + " fields, record length =" + STR$(Form(0).Row)
   MID$(PLine$, 72) = "PAGE:" + STR$(Page)
   GOSUB PrintIt

   LSET PLine$ = DATE$ + ", " + TIME$
   MID$(PLine$, 54) = STRING$(15, "-")
   GOSUB PrintIt

   LSET PLine$ = ""
   MID$(PLine$, 55) = "Related/Choice"
   GOSUB PrintIt

   LSET PLine$ = "Fld Offset   Name      Type    FldLen RecLen Located   File   Field# Index Prot"
   GOSUB PrintIt

   LSET PLine$ = "=== ====== ======== ========== ====== ====== ======= ======== ====== ===== ===="
   GOSUB PrintIt

RETURN



PrintText:
   FOR N = 1 TO 3
      IF LEN(Temp$(N, 1)) THEN
         MID$(PLine$, 10) = Temp$(N, 1)
         GOSUB PrintIt
      END IF
      LSET PLine$ = ""
   NEXT
RETURN


PrintIt:

   IF INKEY$ <> "" THEN
      BLPrint 1, CHR$(12), ErrCount
      EXIT SUB
   END IF

   BLPrint 1, PLine$ + CHR$(13) + CHR$(10), ErrCount

   CountWas = 0
   DO WHILE ErrCount > -1
      LOCATE Inf.Rows \ 2 - 2
      IF Inf.BeepOn THEN Chime 6
      ExitCode = AlertUser("Printer Not Ready!", "Retry", "Cancel", "")

      IF ExitCode <> 1 THEN EXIT SUB

      BLPrint 1, MID$(PLine$, ErrCount + CountWas + 1) + CHR$(13) + CHR$(10), ErrCount
      CountWas = CountWas + ErrCount
   LOOP

   LCount = LCount + 1

   IF LCount >= 56 AND FieldDone THEN
      LSET PLine$ = ""
      LCount = LCount - 66
      FOR N = LCount TO -1
          GOSUB PrintIt
      NEXT
      GOSUB PrintHeader
   END IF

RETURN

END SUB

SUB Promt4Text (Title$, Prompt$, Text$, ExitCode)

    REDIM Dial(3) AS Dialog
    Dial(1).DRow = 2
    Dial(1).DLen = LEN(Text$)
    Dial(1).DTyp = 1

    Dial(2).DRow = 4
    Dial(2).DTyp = 5

    Dial(3).DRow = 4
    Dial(3).DTyp = 5
 
    Temp = 1 + MaxInt(LEN(Title$) + 6 + 10, LEN(Prompt$) + 2 + LEN(Text$))

    Dial(1).DCol = MaxInt(1 + ((Temp - (LEN(Prompt$) + 2 + LEN(Text$))) \ 2), 2)

    X = Temp - (LEN(Title$) + 6 + 8)
    Dial(2).DCol = 1 + MaxInt((X \ 3), 1)
    Dial(3).DCol = Dial(2).DCol + LEN(Title$) + 5 + (X \ 3)
 

    REDIM Fd$(3, 1)
    Fd$(1, 0) = Prompt$
    Fd$(1, 1) = RTRIM$(Text$)

    Fd$(2, 0) = Title$
    Fd$(3, 0) = "Cancel"
    REDIM Mult$(1)
                     
    GetVMode 0, 0, 0, Rows, 0
    LOCATE (Rows \ 2) - 3
    DialogBox Title$, Fd$(), Dial(), Mult$(), ExitCode
   

    Text$ = Fd$(1, 1)

    ERASE Dial, Fd$, Mult$

END SUB

SUB RearrangeFields (Form() AS FieldInfo, Clc$(), Help$(), Rows, Changed)

   'Changed = 0

   REDIM Dl(1 TO 4) AS Dialog
   REDIM Fd$(4, 1)
   REDIM Fld$(Form(0).Fields)
   DIM Temp AS FieldInfo

   FOR N = 1 TO Form(0).Fields
       Fld$(N) = Form(N).FldName
   NEXT

   Dl(1).DRow = 2
   Dl(1).DCol = 2
   Dl(1).DLen = 5 * 256 + 10
   Dl(1).DTyp = 4

   Dl(2).DRow = 1
   Dl(2).DCol = 2
   Dl(2).DTyp = 6

   Dl(3).DRow = 10 + 4
   Dl(3).DCol = 12
   Dl(3).DTyp = 5

   Dl(4).DRow = 10 + 4
   Dl(4).DCol = 31
   Dl(4).DTyp = 5

   DO

      Fd$(2, 0) = "Select the field to move."
      Fd$(3, 0) = "Move"
      Fd$(4, 0) = "Quit"


      LOCATE (Rows - 10 - 4) \ 2
      DialogBox "Rearrange", Fd$(), Dl(), Fld$(), ExitCode


      IF ExitCode = 1 THEN

         Choice = VAL(Fd$(1, 1))
         Cho$ = Fld$(Choice)
         FOR N = Choice TO Form(0).Fields - 1
             Fld$(N) = Fld$(N + 1)
         NEXT
         LSET Fld$(N) = ""

         Fd$(2, 0) = "Insert field: " + Cho$ + " above?"
         Fd$(3, 0) = "Insert"
         Fd$(4, 0) = "Cancel"

         LOCATE (Rows - 10 - 4) \ 2
         DialogBox "Rearrange", Fd$(), Dl(), Fld$(), ExitCode

         IF ExitCode = 1 THEN
            Changed = -1

            Temp = Form(Choice)
            Calc$ = Clc$(Choice)
            Hlp$ = Help$(Choice)

            FOR N = Choice TO Form(0).Fields - 1
                Form(N) = Form(N + 1)
                Clc$(N) = Clc$(N + 1)
                Help$(N) = Help$(N + 1)
            NEXT

            Choice = VAL(Fd$(1, 1))
            FOR N = Form(0).Fields TO Choice + 1 STEP -1
                Fld$(N) = Fld$(N - 1)
                Form(N) = Form(N - 1)
                Clc$(N) = Clc$(N - 1)
                Help$(N) = Help$(N - 1)
            NEXT

            Fld$(N) = Cho$
            Form(N) = Temp
            Clc$(N) = Calc$
            Help$(N) = Hlp$
         END IF
      END IF

   LOOP WHILE ExitCode = 1

END SUB

SUB ReplaceColor (Inf AS GenInfo)

    Row = 3
    Col = 22

    MarkingBlock = Inf.MarkBlock

    StuffBuf CHR$(1)
    ScrEdit "", Inf

    HideCursor
    SClr = SCREEN(Inf.CurRow, Inf.CurCol, 1)
    ShowCursor
    IF RClr = 0 THEN RClr = SClr

    '----- Save the screen
    REDIM Tmp(1000)

    MScrnSave Row, Col, Row + 21, Col + 35, Tmp(0)

    MPaintBox Row + 1, Col + 34, Row + 20, Col + 35, 8
    MPaintBox Row + 21, Col + 2, Row + 21, Col + 35, 8

    HideCursor
    ConvexFrame Row, Col, Row + 2, Col + 33
    ShowCursor
    LOCATE Row + 1, Col + 1, 0
    MQPrint " Choose the color to replace.   ", 112

    LOCATE Row + 3, Col
    MColorPick SClr, 112, ExitCode
    IF ExitCode <> 2 THEN

       LOCATE Row + 1, Col + 2
       MQPrint "Now choose the New color.   ", 112

       LOCATE Row + 3, Col
       MColorPick RClr, 112, ExitCode
    END IF

    MScrnRest Row, Col, Row + 21, Col + 35, Tmp(0)
    ERASE Tmp

    IF ExitCode = 2 THEN
       IF MarkingBlock THEN
          Inf.CurRow = Inf.BBrRow
          Inf.CurCol = Inf.BBrCol
          Inf.MarkBlock = -1
          ScrEdit CHR$(2), Inf
       END IF
       EXIT SUB
    END IF

    IF MarkingBlock THEN
       UlR = Inf.BUlRow
       UlC = Inf.BUlCol
       BrR = Inf.BBrRow
       BrC = Inf.BBrCol
       Inf.WasMarked = 0
    ELSE
       GetVMode 0, 0, 0, Rows, 0
       UlR = 1
       UlC = 1
       BrR = Rows
       BrC = 80
    END IF

    IF Inf.Ruler THEN RulerLine Inf, 5

    ChangeClr UlR, UlC, BrR, BrC, SClr, RClr

    IF Inf.Ruler THEN RulerLine Inf, 1

    Inf.ScrChanged = -1

END SUB

'******* Display the cursor location
SUB RulerLine (Inf AS GenInfo, Action) STATIC
  
   IF Action <= 1 THEN
      REDIM RulBuf(160)
      RuleLine = 0
      IF Action = 0 THEN
         OrgRow = Inf.CurRow
         OrgCol = Inf.CurCol
      END IF
   END IF

   IF Action = 5 THEN
      IF RuleLine THEN MScrnRest RuleLine, 1, RuleLine + 1, 80, RulBuf(0)
      EXIT SUB
   END IF

   HideCursor

   IF Action <= 2 THEN
      IF RuleLine THEN
         MScrnRest RuleLine, 1, RuleLine + 1, 80, RulBuf(0)
      END IF

      IF Inf.EdType = 3 OR Inf.EdType = 2 THEN
         RuleLine = Inf.CurRow + Inf.BBrRow - Inf.BUlRow + 1
         IF RuleLine > Inf.Rows - 1 THEN
            RuleLine = Inf.CurRow - 3
            IF RuleLine < 1 THEN RuleLine = Inf.CurRow
         END IF
      ELSE
         IF Inf.CurRow < Inf.Rows - 1 THEN
            RuleLine = Inf.CurRow + 1
         ELSE
            RuleLine = Inf.CurRow - 2
         END IF
      END IF

      MScrnSave RuleLine, 1, RuleLine + 1, 80, RulBuf(0)
   
      QPrintRC Ruler$, RuleLine, 1, 112
   END IF

   IF LEN(Temp$) <> 80 THEN
      Temp$ = SPACE$(80)
   END IF

   RSET Temp$ = "[F4] to remove "
   Info$ = "Row:" + STR$(Inf.CurRow) + ", Col:" + STR$(Inf.CurCol) + ", Offset =" + STR$(Inf.CurRow - OrgRow)
   Info$ = Info$ + ":" + STR$(Inf.CurCol - OrgCol) + ", Character:" + STR$(SCREEN(Inf.CurRow, Inf.CurCol)) + ", Color:" + STR$(SCREEN(Inf.CurRow, Inf.CurCol, 1))
   MID$(Temp$, 1) = Info$
   Info$ = ""

   QPrintRC Temp$, RuleLine + 1, 1, 112
   ShowCursor

END SUB

SUB SetCsr (Inf AS GenInfo) STATIC

   DEF SEG = 0                          'Look at low memory to see what type
   IF PEEK(&H463) = &HB4 THEN           '  of monitor we have.
      CsrBot = 11                       'Monochrome card
   ELSE
      CsrBot = 7                        'Color card
   END IF
 
   IF Inf.Ins THEN
      LOCATE , , 1, CsrBot \ 2, CsrBot
   ELSE
      LOCATE , , 1, CsrBot - 1, CsrBot
   END IF

END SUB

SUB Sketch (Ln$, Inf AS GenInfo)

   Other$ = ""
   SELECT CASE Ln$
      CASE "ƒ"
         LType = 1
      CASE "Õ"
         LType = 2
      CASE "≤"
         LType = 3
      CASE "€"
         LType = 4
      CASE ELSE
         LType = 5
         Other$ = Ln$
   END SELECT


   REDIM Ntr(1 TO 5) AS STRING * 11
   Ntr(1) = "≥¥ø¿¡¬√ƒ≈Ÿ⁄"
   Ntr(2) = "∫πª» ÀÃÕŒº…"

   REDIM DC(1 TO 4, 11, 1 TO 5) AS STRING * 4

   DC(1, 0, 1) = "ƒ¿ƒ⁄"
   DC(2, 0, 1) = "ø≥⁄≥"
   DC(3, 0, 1) = "ƒŸƒø"
   DC(4, 0, 1) = "Ÿ≥¿≥"

   DC(1, 1, 1) = "≈√√√"          '≥
   DC(2, 1, 1) = "¥≥√≥"
   DC(3, 1, 1) = "¥¥≈¥"
   DC(4, 1, 1) = "¥≥√≥"

   DC(1, 2, 1) = "≈≈≈≈"          '¥
   DC(2, 2, 1) = "¥¥≈¥"
   DC(3, 2, 1) = "¥¥≈¥"
   DC(4, 2, 1) = "¥¥≈¥"

   DC(1, 3, 1) = "¬≈¬¬"          'ø
   DC(2, 3, 1) = "ø¥¬ø"
   DC(3, 3, 1) = "ø¥¬ø"
   DC(4, 3, 1) = "¥¥≈¥"

   DC(1, 4, 1) = "¡¿¿√"          '¿
   DC(2, 4, 1) = "≈√√√"
   DC(3, 4, 1) = "¡¡¡≈"
   DC(4, 4, 1) = "¡¿¿√"

   DC(1, 5, 1) = "¡¡¡≈"          '¡
   DC(2, 5, 1) = "≈≈≈≈"
   DC(3, 5, 1) = "¡¡¡≈"
   DC(4, 5, 1) = "¡¡¡≈"

   DC(1, 6, 1) = "¬≈¬¬"          '¬
   DC(2, 6, 1) = "¬≈¬¬"
   DC(3, 6, 1) = "¬≈¬¬"
   DC(4, 6, 1) = "≈≈≈≈"

   DC(1, 7, 1) = "≈√√√"          '√
   DC(2, 7, 1) = "≈√√√"
   DC(3, 7, 1) = "≈≈≈≈"
   DC(4, 7, 1) = "≈√√√"

   DC(1, 8, 1) = "ƒ¡ƒ¬"          'ƒ
   DC(2, 8, 1) = "¬≈¬¬"
   DC(3, 8, 1) = "ƒ¡ƒ¬"
   DC(4, 8, 1) = "¡¡¡≈"
          
   DC(1, 9, 1) = "≈≈≈≈"          '≈
   DC(2, 9, 1) = "≈≈≈≈"
   DC(3, 9, 1) = "≈≈≈≈"
   DC(4, 9, 1) = "≈≈≈≈"

   DC(1, 10, 1) = "¡¡¡≈"         'Ÿ
   DC(2, 10, 1) = "¥¥≈¥"
   DC(3, 10, 1) = "ŸŸ¡¥"
   DC(4, 10, 1) = "ŸŸ¡¥"

   DC(1, 11, 1) = "¬√⁄⁄"         '⁄
   DC(2, 11, 1) = "¬√⁄⁄"
   DC(3, 11, 1) = "¬≈¬¬"
   DC(4, 11, 1) = "≈⁄√√"

   Char$ = " "
   FOR N = 0 TO 11
      FOR X = 1 TO 4
         FOR C = 1 TO 4

            MID$(DC(X, N, 2), C) = MID$(Ntr(2), INSTR(Ntr(1), MID$(DC(X, N, 1), C, 1)), 1)

            MID$(DC(X, N, 3), C) = "≤"
            MID$(DC(X, N, 4), C) = "€"

         NEXT
      NEXT
   NEXT

   IF Inf.Ruler THEN RulerLine Inf, 5

   REDIM ScrBuf(Inf.Rows * 80)
   MScrnSave 1, 1, Inf.Rows, 80, ScrBuf(0)

   CurCar$ = " "

   PrevDir = 0

   Quit$ = CHR$(0) + CHR$(13) + CHR$(27) + CHR$(4)

   LOCATE , , 1, 1, 13

   CWas = Inf.CurCol
   LWas = Inf.CurRow

   Inf.EdType = 1


   DO

      ScrEdit K$, Inf

      IF LEN(K$) = 2 OR K$ = CHR$(9) THEN

         DX = Inf.CurCol - CWas
         DY = Inf.CurRow - LWas

         XW = CWas
         YW = LWas

         IF DX THEN
            Y = Inf.CurRow
            FOR X = CWas + SGN(DX) TO Inf.CurCol STEP SGN(DX)
               GOSUB PutChar
               XW = X
            NEXT
         END IF

         IF DY THEN
            X = Inf.CurCol
            FOR Y = LWas + SGN(DY) TO Inf.CurRow STEP SGN(DY)
               GOSUB PutChar
               YW = Y
            NEXT
         END IF

      END IF

      CWas = Inf.CurCol
      LWas = Inf.CurRow
     
   LOOP UNTIL INSTR(Quit$, K$) > 1

   IF K$ = CHR$(27) THEN
      IF Inf.Ruler THEN RulerLine Inf, 5
      MScrnRest 1, 1, Inf.Rows, 80, ScrBuf(0)
      IF Inf.Ruler THEN RulerLine Inf, 1
   ELSE
      Inf.ScrChanged = -1
   END IF

   Inf.EdType = 0
   SetCsr Inf

   EXIT SUB



PutChar:

   XDir = X - XW
   YDir = Y - YW

   IF XDir THEN
      Dir = XDir + 2
   ELSE
      Dir = YDir + 3
   END IF
   IF PrevDir = 0 THEN PrevDir = Dir

   CC = X - XDir
   CL = Y - YDir

   CALL ReadScrn0(CL, CC, CurCar$)
   Hit = INSTR(Ntr(LType), CurCar$)

   IF LEN(Other$) THEN
      LSET Char$ = Other$
   ELSE
      LSET Char$ = MID$(DC(PrevDir, Hit, LType), Dir, 1)
   END IF

   PrevDir = Dir

   LOCATE CL, CC
   MQPrint Char$, Inf.SysColor

RETURN

END SUB

SUB UndoBlock (B(), ScrBuf()) STATIC

   '----- Did the Upper Left corner move Down?
   IF B(0, 0) > B(0, 1) THEN
      MPRestore B(0, 1), B(1, 1), B(0, 0) - 1, B(3, 1), 80, ScrBuf((B(0, 1) - 1) * 80 + B(1, 1) - 1)
   END IF

   '----- Did the Upper Left corner move Right?
   IF B(1, 0) > B(1, 1) THEN
      MPRestore B(0, 1), B(1, 1), B(2, 1), B(1, 0) - 1, 80, ScrBuf((B(0, 1) - 1) * 80 + B(1, 1) - 1)
   END IF

   '----- Did the Lower Right corner move Up?
   IF B(2, 0) < B(2, 1) THEN
      MPRestore B(2, 0) + 1, B(1, 1), B(2, 1), B(3, 1), 80, ScrBuf((B(2, 0) * 80) + B(1, 1) - 1)
   END IF

   '----- Did the Lower Right corner move Left?
   IF B(3, 0) < B(3, 1) THEN
      MPRestore B(0, 1), B(3, 0) + 1, B(2, 1), B(3, 1), 80, ScrBuf((B(0, 1) - 1) * 80 + B(3, 0))
   END IF

   B(0, 1) = B(0, 0)
   B(1, 1) = B(1, 0)
   B(2, 1) = B(2, 0)
   B(3, 1) = B(3, 0)

END SUB

