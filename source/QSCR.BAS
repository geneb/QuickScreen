'**************** QScr.Bas - Screen Builder MAIN Module ********************

'by Donald R. Malin
'Copyright (C) Crescent Software  1987 - 1991

'Note:  Should be compiled from the Command Line.
'       Compile Syntax = "BC QScr /S/O;"
'       Link Syntax    = "Link @Qscr.RSP"
'Don't even think about running this in the QB or QBX environments.

'Required Modules:
'   DIALOG.BAS
'   EVALUATE.BAS
'   FORMEDIT.BAS
'   GETFILE.BAS
'   PULLDNMS.BAS
'   QEDITS.BAS
'   QSCALC.BAS
'   QSCRMISC.BAS
'   QSCRSAVE.BAS
'   SCRNDISP.BAS
'   VERTMENU.BAS
'***************************************************************************


DEFINT A-Z

'$INCLUDE: 'FieldInf.Bi'
'$INCLUDE: 'DialType.Bi'
'$INCLUDE: 'GenInfo.BI'
'$INCLUDE: 'FormEdit.bi'
'$INCLUDE: 'DefCnf.bi'

'----- Character equivalents of block marking keys
CONST NumPad$ = "12346789"                     'Shifted arrow keys
CONST NumPad2$ = "stOPQKMGHK"                  'Unshifted arrow keys
CONST Demo = 0


'----- ASM Functions
DECLARE FUNCTION AltKey% ()
DECLARE FUNCTION ASCII% (Text$)
DECLARE FUNCTION CompareT% (SEG Type1 AS ANY, SEG Type2 AS ANY, NumBytes%)
DECLARE FUNCTION CtrlKey% ()
DECLARE FUNCTION Date2Num% (Dat$)
DECLARE FUNCTION DosError% ()
DECLARE FUNCTION DOSVer% ()
DECLARE FUNCTION EDate2Num% (Dat$)
DECLARE FUNCTION ENum2Date$ (Days)
DECLARE FUNCTION Eval# (Num$, ErrCode)
DECLARE FUNCTION ExeName$ ()
DECLARE FUNCTION Exist% (FileName$)
DECLARE FUNCTION FCount% (FileSpec$)
DECLARE FUNCTION FEof% (Handle)
DECLARE FUNCTION FileSize& (FileName$)
DECLARE FUNCTION FindLast% (BYVAL Address, Size)
DECLARE FUNCTION FLInput$ (Handle%, Buffer$)
DECLARE FUNCTION FLof& (Handle)
DECLARE FUNCTION GetDir$ (Drive$)
DECLARE FUNCTION GetDrive% ()
DECLARE FUNCTION MaxInt% (Var1%, Var2%)
DECLARE FUNCTION MinInt% (Var1%, Var2%)
DECLARE FUNCTION Monitor% ()
DECLARE FUNCTION Null% (Text$)
DECLARE FUNCTION Num2Date$ (Days)
DECLARE FUNCTION Peek1% (Segment, Address)
DECLARE FUNCTION QPLen% (Text$)
DECLARE FUNCTION QPRTrim$ (Text$)
DECLARE FUNCTION ShiftKey% ()

'----- ASM Sub programs
DECLARE SUB ButtonPress (Button, Status, Count, X, Y)
DECLARE SUB Chime (Flag)
DECLARE SUB CvtBLScrns (ScrName$, ErrCode%)
DECLARE SUB CvtFrm (FrmName$, Frm() AS FieldInfo)
DECLARE SUB CvtLib (LibName$, NewLib%())
DECLARE SUB CvtQBScr (ScrName$, ErrCode%)
DECLARE SUB Extents (UlRow%, UlCol%, BrRow%, BrCol%)
DECLARE SUB FCreate (FileName$)
DECLARE SUB FClose (Handle)
DECLARE SUB FOpen (FileName$, Handle)
DECLARE SUB FGet (Handle, Text$)
DECLARE SUB FGetA (Handle, SEG Element AS ANY, NumBytes&)
DECLARE SUB FGetT (Handle, Variable AS ANY, NumBytes&)
DECLARE SUB FPut (Handle, Text$)
DECLARE SUB FPutA (Handle, SEG Element AS ANY, NumBytes&)
DECLARE SUB FPutT (Handle, Variable AS ANY, NumBytes&)
DECLARE SUB FSeek (Handle, Location&)
DECLARE SUB GetCursor (X, Y, Button)
DECLARE SUB GetVMode (Mode, Page, PageSize, Rows, Columns)
DECLARE SUB HideCursor ()
DECLARE SUB InitMouse (MouseThere)
DECLARE SUB KillFile (FileName$)
DECLARE SUB LoadObj (ObjName$, Attribute%, ErrorCode%)
DECLARE SUB MouseTrap (UlRow, UlCol, BrRow, BrCol)
DECLARE SUB MQPrint (X$, Colr)
DECLARE SUB MPRestore (UlRow, UlCol, BrRow, BrCol, Wide, SEG Element)
DECLARE SUB MPaintBox (UlRow%, UlCol%, LrRow%, LrCol%, Colr%)
DECLARE SUB MScrnSave (UlRow%, UlCol%, LrRow%, LrCol%, SEG Address%)
DECLARE SUB MScrnRest (UlRow%, UlCol%, LrRow%, LrCol%, SEG Address%)
DECLARE SUB Pause (Tixks)
DECLARE SUB QBLoad (FileName$, SEG Element AS ANY)
DECLARE SUB QPrintRC (Text$, Row, Col, Clr)
DECLARE SUB Scrn2Str (BYVAL Segment, BYVAL Address, StrLen)
DECLARE SUB ScrollD (UlRow%, UlCol%, LrRow%, LrCol%, Lines%, Page%)
DECLARE SUB ScrollU (UlRow%, UlCol%, LrRow%, LrCol%, Lines%, Page%)
DECLARE SUB ScrollL (UlRow, UlCol, LrRow, LrCol, Lines, Page)
DECLARE SUB ScrollR (UlRow, UlCol, LrRow, LrCol, Lines, Page)
DECLARE SUB ScrSize (UlRow%, UlCol%, LrRow%, LrCol%, Inf AS ANY)
DECLARE SUB SetCursor (Row, Column)
DECLARE SUB ShowCursor ()
DECLARE SUB StuffBuf (X$)
DECLARE SUB TextCursor (FG, Bg)



'----- BASIC Functions
DECLARE FUNCTION AlertUser% (Message$, Prmt1$, Prmt2$, Prmt3$)
DECLARE FUNCTION ASCIIChart$ (Ok2Print%, Inf AS ANY)
DECLARE FUNCTION BaseName$ (LongName$)
DECLARE FUNCTION EndOfField% (Inf AS GenInfo)
DECLARE FUNCTION ErrorPrompt% ()
DECLARE FUNCTION Extension$ (FileName$)
DECLARE FUNCTION FileNameOk% (ScrName$)
DECLARE FUNCTION InRange% (R%, C%, UlR%, UlC%, BrR%, BrC%)
DECLARE FUNCTION IsDup% (FldName$, Form() AS ANY, CurFld%)
DECLARE FUNCTION IsThere% (FileName$)
DECLARE FUNCTION LibNo% (ScrName$, ScrnLib%())
DECLARE FUNCTION LibScrName$ (ScrnNo, ScrnLib())
DECLARE FUNCTION NoExtension$ (FileName$)
DECLARE FUNCTION NoPath$ (FileSpec$)
DECLARE FUNCTION ParsPath$ (FileName$)
DECLARE FUNCTION PullMenKey% (Ky$, Choice$(), Stat())

'----- BASIC Sub programs
DECLARE SUB Add2Lib (NameLib$, Scrn%(), Inf AS GenInfo, ErrFlag%)
DECLARE SUB BarPrint (Choice$(), Stat())
DECLARE SUB BlockPaint (Inf AS GenInfo)
DECLARE SUB BreakLines (Text$, Array$(), Element%, Wdth%)
DECLARE SUB BuildFrmLib (LibName$, ScrnLib%())
DECLARE SUB ChkScrChanged (ScrName$, ScrnLib(), Form() AS FieldInfo, Help$(), Clc$(), Inf AS GenInfo, ExitCode)
DECLARE SUB ChkLibChanged (LibName$, ScrnLib(), Inf AS GenInfo, ExitCode)
DECLARE SUB CloseCurtain (Orig%(), El%)
DECLARE SUB ConvexFrame (UlR%, UlC%, BrR%, BrC%)
DECLARE SUB DefineMenus (MenuFile$, MMenu$(), ChStat%(), MenuHelp() AS STRING * 80)
DECLARE SUB RulerLine (Inf AS GenInfo, Action)
DECLARE SUB DefineForm (Form() AS FieldInfo, TypCh$())
DECLARE SUB DelLib (ScrnLib%(), EllNo%, NoDel%)
DECLARE SUB DialogBox (Title$, PrmtAns$(), Dl() AS Dialog, Mult$(), ExitCode%)
DECLARE SUB DrawBox ()
DECLARE SUB EdScrn (Inf AS GenInfo)
DECLARE SUB ErrMsg (Message$)
DECLARE SUB FillChar (Inf AS GenInfo)
DECLARE SUB EditForm (Form$(), Fld() AS FieldInfo, Frm AS FormInfo, Cnf AS Config, Action%)
DECLARE SUB GetColor (Clr%, ExitCode)
DECLARE SUB GetFile (SearchSpec$, FileSpec$)
DECLARE SUB LoadFldDef (ScrName$)
DECLARE SUB GetFldType (FldNo, TypCod, ExitCode%)
DECLARE SUB GetHelp (ProgLib$, MonoCode, Inf AS GenInfo)
DECLARE SUB GetRelation (Fld%, Form() AS FieldInfo, RelName$, RelFld$, RelFld%, RelType%, Decimals%, ExitCode%)
DECLARE SUB InsLib (ScrnLib%(), EllNo%, NoIns%)
DECLARE SUB LibFile2Scrn (LibName$, ScrnName$, MonoCode%, Attribute%, ErrorCode%)
DECLARE SUB LibScrMenu (ScrName$, ScrnLib(), Inf AS ANY)
DECLARE SUB LoadScreen (LName$, MonoCode, Attribute%, ErrorCode%)
DECLARE SUB MakeNewFldName (OldName$)
DECLARE SUB MakeUniqueFldName (FldNo%, Form() AS ANY)
DECLARE SUB MonoDisp (Choice%, Inf AS GenInfo)
DECLARE SUB PrintFRM (LibName$, Form() AS FieldInfo, Calc$(), Help$(), Inf AS GenInfo)
DECLARE SUB Promt4Text (Title$, Prompt$, Text$, ExitCode%)
DECLARE SUB PrSaveScr (Title$, ScrName$, Form() AS ANY, ExitCode%, Inf AS ANY)
DECLARE SUB PullDnMs (Choice$(), Stat%(), Menu%, Choice%, Ky$, Action%)
DECLARE SUB PushR2L (Scr%(), El%)
DECLARE SUB RearrangeFields (Form() AS FieldInfo, Calc$(), Help$(), Rows%, Changed%)
DECLARE SUB ReplaceColor (Inf AS GenInfo)
DECLARE SUB ReplaceInLib (NameLib$, ScrNum%, Scrn%(), Inf AS GenInfo, ErrFlag%)
DECLARE SUB Scr2Obj (ScrName$, WipeType%, Inf AS ANY)
DECLARE SUB ScrEdit (K$, Inf AS ANY)
DECLARE SUB SetCsr (Inf AS GenInfo)
DECLARE SUB SetStatus (Status%, Truth%)
DECLARE SUB Sketch (LN$, Inf AS GenInfo)
DECLARE SUB SvForm (ScrName$, Form() AS FieldInfo, Clc$(), Help$(), Inf AS GenInfo)
DECLARE SUB SvLib (LibName$, ScrnLib(), Changed%)
DECLARE SUB SvScr (ScrName$, Inf AS GenInfo)
DECLARE SUB Tokenize (Calc$, Fld() AS FieldInfo)
DECLARE SUB UndoBlock (B%(), ScrBuf%())

CLEAR , , 2000

'$INCLUDE: 'SetCnf.bi'                  'Sets monitor and color information


IF FRE(-1) < 131072 THEN
   PRINT "Not enough memory!  This program needs at least 410k of free DOS memory."
   END
END IF


Zero = 0
One = 1
DIM SHARED Chr0$
Chr0$ = CHR$(0)

InitMouse Temp


'*************  DIMENSION ARRAYS

DIM SHARED Ins                  'Share Insert Mode Status
DIM LMenu$(6, 5)                'Line Type Menu Choices
DIM BsStat(10, 5)               'Other Menus Choice Status
DIM LN(3)                       'Line Type Menu Pointers
DIM LN$(3)                      'Line Types
DIM OldLN(3)                    'Temporary Line Type Pointers
DIM TL$(6, 6)                   'Top Left Corner Character Array
DIM TR$(6, 6)                   'Top Right Corner Character Array
DIM BL$(6, 6)                   'Bottom Left Corner Character Array
DIM BR$(6, 6)                   'Bottom Right Corner Character Array
DIM Dial$(5)                    'Holds Prompt box for defining fields

REDIM PntScr(0)

REDIM ScrnLib(32766)            'Dimension Screen Library Memory
REDIM TopBuf(0)
                                'Form Definition/Information Array
REDIM Form(MaxFields) AS FieldInfo
REDIM Clc$(MaxFields)           'Holds field calculation text
REDIM Help$(MaxFields)          'Holds field help messages
DIM Blank AS FieldInfo
Blank.RelFile = ""
DIM Inf AS GenInfo              'General program information
DIM Frm AS FormInfo
REDIM Choice$(0, 0)


REDIM MMenu$(0, 0)               'Main Menu Choices.  Resized by "DefineMenus"
REDIM ChStat(0, 0)               'Main Menu Choice Status
REDIM MenuHelp(0, 0) AS STRING * 80



'*************  Define LINE TYPE Menu Array.

FOR R = 0 TO 6
   FOR C = 0 TO 4
      READ LMenu$(R, C)
   NEXT
NEXT

DATA "Top",     "Left",    "Bottom",  "Right",   "All"
DATA "  ÍÍÍÍ ", "  º    ", "  ÍÍÍÍ ", "  º    ", "  ÍÍÍÍ "
DATA "  ÄÄÄÄ ", "  ³    ", "  ÄÄÄÄ ", "  ³    ", "  ÄÄÄÄ "
DATA "  ÜÜÜÜ ", "  Ý    ", "  ßßßß ", "  Þ    ", "  ßßßß "
DATA "  ÛÛÛÛ ", "  Û    ", "  ÛÛÛÛ ", "  Û    ", "  ÛÛÛÛ "
DATA "  ²²²² ", "  ²    ", "  ²²²² ", "  ²    ", "  ²²²² "
DATA "    ?  ", "    ?  ", "    ?  ", "    ?  ", "    ?  "

LMenu$(0, 5) = "OK"



'Top Left Corner Character Types

FOR R = 1 TO 6
   FOR C = 1 TO 6
      READ TL$(R, C)
   NEXT
NEXT

DATA "É", "Ö", "Ü", "Û", "²", "º"
DATA "Õ", "Ú", "Ü", "Û", "²", "³"
DATA "É", "Ü", "Ü", "Û", "²", "Ý"
DATA "É", "Ü", "Ü", "Û", "²", "Û"
DATA "É", "Ü", "Ü", "Û", "²", "²"
DATA "Í", "Ä", "Ü", "Û", "²", " "

'Top Right Corner Character Types

FOR R = 1 TO 6
   FOR C = 1 TO 6
      READ TR$(R, C)
   NEXT
NEXT

DATA "»", "·", "Ü", "Û", "²", "º"
DATA "¸", "¿", "Ü", "Û", "²", "³"
DATA "»", "Ü", "Ü", "Û", "²", "Þ"
DATA "»", "Ü", "Ü", "Û", "²", "Û"
DATA "»", "Ü", "Ü", "Û", "²", "²"
DATA "Í", "Ä", "Ü", "Û", "²", " "

'Bottom Left Corner Character Types

FOR R = 1 TO 6
   FOR C = 1 TO 6
      READ BL$(R, C)
   NEXT
NEXT

DATA "È", "Ó", "ß", "Û", "²", "º"
DATA "Ô", "À", "ß", "Û", "²", "³"
DATA "È", "ß", "ß", "Û", "²", "Ý"
DATA "È", "ß", "ß", "Û", "²", "Û"
DATA "È", "ß", "ß", "Û", "²", "²"
DATA "Í", "Ä", "ß", "Û", "²", " "

'Bottom Right Corner Character Types

FOR R = 1 TO 6
   FOR C = 1 TO 6
      READ BR$(R, C)
   NEXT
NEXT

DATA "¼", "½", "ß", "Û", "²", "º"
DATA "¾", "Ù", "ß", "Û", "²", "³"
DATA "¼", "ß", "ß", "Û", "²", "Þ"
DATA "¼", "ß", "ß", "Û", "²", "Û"
DATA "¼", "ß", "ß", "Û", "²", "²"
DATA "Í", "Ä", "ß", "Û", "²", " "




'***** Read Field Type Choices
'$INCLUDE: 'FldTpsQS.Bi'


FOR N = 1 TO 5
   Dial$(N) = SPACE$(50)
NEXT



ScrSpec$ = "*.SCR"
LibSpec$ = "*.QSL"
ObjSpec$ = "*.OBJ"
ImpSpec$ = "*.*"

Inf.BeepOn = -1


MonoCode = ABS(Peek1%(Zero, &H463) = &HB4 OR INSTR(COMMAND$, "/B")) * 3
MonTyp = Monitor%

Inf.Ins = 0
SetCsr Inf


GetVMode 0, 0, 0, Inf.Rows, 0
DosRow = CSRLIN
DosVersion = DOSVer


'----- Get the programs directory name
ExePath$ = ExeName$
FOR N = LEN(ExePath$) TO 1 STEP -1
   IF MID$(ExePath$, N, 1) = "\" THEN EXIT FOR
NEXT
ExePath$ = LEFT$(ExePath$, N)

'----- Get the current directory
DfltDir$ = CHR$(GetDrive%) + ":" + GetDir$("")
IF RIGHT$(DfltDir$, 1) <> "\" THEN DfltDir$ = DfltDir$ + "\"

IF ExePath$ = "" THEN ExePath$ = DfltDir$

'----- Find the programs screen library
Temp$ = "QScreen.Psl"
ProgLib$ = ExePath$ + Temp$

IF NOT Exist(ProgLib$) THEN ProgLib$ = DfltDir$ + Temp$


IF NOT Exist(ProgLib$) THEN
   ErrMsg "File ``" + UCASE$(ProgLib$) + "'' Not Found!"
   END
END IF

'----- Find the configuration file
Temp$ = "QScreen.Cnf"
Temp = -1
CnfName$ = DfltDir$ + Temp$
IF NOT Exist(CnfName$) THEN
   IF Exist(ExePath$ + Temp$) THEN
      CnfName$ = ExePath$ + Temp$
   ELSE
      Temp = 0
   END IF
END IF
Temp$ = ""


Inf.Clr = BoxClr                                     'Set Default colors
LN(0) = 1: LN(1) = 1: LN(2) = 1: LN(3) = 1           'Set Default line type
Inf.CrncStr = "$"
Inf.Ruler = -1
Inf.SysColor = -1

IF Temp THEN            'See if there is a Configuration File
   FOpen CnfName$, Handle
   Temp$ = " "
   FGet Handle, Temp$
   IF Temp$ = "Q" THEN
      FGetT Handle, Inf.Clr, 2                       'Default Colors
      FOR N = 0 TO 3
         FGetT Handle, LN(N), 2                      'Default Line Types
      NEXT
      FOR N = 0 TO 3
         LN$(N) = " "
         FGet Handle, LN$(N)
         MID$(LMenu$(LN(N), N), 3, 1) = LN$(N)
      NEXT
      FGetT Handle, Inf.BeepOn, 2
      FGet Handle, Inf.CrncStr
      FGetT Handle, Inf.Ruler, 2
      IF NOT FEof(Handle) THEN
         FGetT Handle, Inf.SaveTYPE, 2
         FGetT Handle, Inf.SaveDATA, 2
         FGetT Handle, Inf.SysColor, 2
         FGetT Handle, Inf.PaintFields, 2
      END IF

   END IF
   FClose Handle
END IF


Temp$ = "QSMenu.HLP"
MenuFile$ = ExePath$ + Temp$
IF NOT Exist(MenuFile$) THEN MenuFile$ = DfltDir$ + Temp$

'----- Define the main pulldown menus
DefineMenus MenuFile$, MMenu$(), ChStat(), MenuHelp()



FOR N = 0 TO 3
   MID$(LMenu$(LN(N), N), 1) = "û"              'Put Check Marks on Line Types
NEXT

GOSUB FindCorners


DosRows = Inf.Rows
REDIM DosScr(DosRows * 80)
MScrnSave One, One, DosRows, 80, DosScr(0)


REDIM BotLine(80)

WIDTH 80, 25
Inf.Rows = 25


                                                'Display the BANNER Screen
LibFile2Scrn ProgLib$, "BANNER", MonoCode, Attribute, ErrorCode


IF ErrorCode THEN END

BarPrint MMenu$(), ChStat()


TextCursor -2, -2                       'Inverse foreground red background
SetCursor One, One                          'Locate it at upper right of screen
CALL ShowCursor                         'Turn it on

Inf.EdType = -1


'LPRINT FRE(""), FRE(-1)                'For testing only



'**************** MAIN MENU Functions

DO

   Row = CSRLIN: Col = POS(0)


   '*****  Set Menu Choice Status, -1 = Not Active
   NoScreen = LEN(ScrName$) = 0                         'Is there a Screen
   NoLib = LEN(LibName$) = 0                            'Is there a Library
   M = 0
   C = 3: T = NoScreen OR Demo: GOSUB SetMenuStat       'Set Save Status
   C = 5: T = NoScreen: GOSUB SetMenuStat               '    Import status
   C = 6: T = NoScreen OR Demo: GOSUB SetMenuStat       '    Export Status
   C = 9: T = NoScreen OR Demo: GOSUB SetMenuStat       '    Make .OBJ

   M = 1
   C = 1: T = NoLib OR ScrnLib(0) = 0: GOSUB SetMenuStat'    Display Screen
   C = 3: T = Demo: GOSUB SetMenuStat
   C = 5: T = NoLib OR Demo: GOSUB SetMenuStat          '    Save Library
                                                        '    Add Screen
   C = 7: T = NoLib OR NoScreen OR Demo: GOSUB SetMenuStat
                                                        '    Replace Screen
   C = 8: T = NoLib OR NoScreen OR Demo: GOSUB SetMenuStat
                                                        '    Delete Screen
   C = 9: T = NoLib OR ScrnLib(0) = 0 OR Demo: GOSUB SetMenuStat

   M = 2
   C = 1: T = NoScreen: GOSUB SetMenuStat               '    Paint Block
   C = 2: GOSUB SetMenuStat                             '    Replace Color
   C = 4: GOSUB SetMenuStat                             '    Draw Box
   C = 5: GOSUB SetMenuStat                             '    Draw Lines
   C = 6: GOSUB SetMenuStat                             '    Fill Character
   C = 8: T = Inf.MarkBlock = 0: GOSUB SetMenuStat      '    Cut Block
   C = 9: GOSUB SetMenuStat                             '    Copy Block
   C = 10: T = Inf.WasMarked = 0: GOSUB SetMenuStat     '    Paste Block
   C = 11: T = Inf.MarkBlock = 0: GOSUB SetMenuStat     '    Move Block
   C = 12: T = Inf.MarkBlock = 0: GOSUB SetMenuStat     '    Clear Block
   C = 14: T = NoScreen: GOSUB SetMenuStat              '    Center Block/Text
   C = 16: T = NoScreen: GOSUB SetMenuStat              '    Repeat Key

   M = 3
   T = NoScreen
   FOR C = 4 TO 6                                       '    Display Mono
      GOSUB SetMenuStat
   NEXT

   M = 4
   C = 7: T = Inf.Rows = 25: GOSUB SetMenuStat          '    25 Line Mode
   C = 8                                                '    43 Line Mode
   T = (MonTyp <> 5 AND MonTyp <> 6 AND MonTyp <> 7 AND MonTyp <> 11) OR Inf.Rows = 43
   T = T OR NoScreen: GOSUB SetMenuStat
   C = 9                                                '    50 Line Mode
   T = ((MonTyp <> 6 AND MonTyp <> 7 AND MonTyp <> 11) OR Inf.Rows = 50)
   T = T OR NoScreen: GOSUB SetMenuStat

   M = 5
   C = 1: T = NoScreen: GOSUB SetMenuStat               '    Define Fields
   C = 4: T = Form(0).Fields = 0: GOSUB SetMenuStat     '    Rearrange Fields
   C = 5: T = Form(0).Fields = 0: GOSUB SetMenuStat     '    Print Form Def.
   C = 7: T = Form(0).Fields = 0: GOSUB SetMenuStat     '    Try Form.

      
   MScrnSave Inf.Rows, One, Inf.Rows, 80, BotLine(0)
                 
   Action = 1
   DO
      IF Action < 3 THEN

         IF Inf.PaintFields THEN
            MID$(MMenu$(2, 5), 1) = "û"
         ELSE
            MID$(MMenu$(2, 5), 1) = " "
         END IF

         IF Inf.SysColor > -1 THEN
            MID$(MMenu$(3, 4), 1) = "û"
         ELSE
            MID$(MMenu$(3, 4), 1) = " "
         END IF

         IF Inf.BeepOn THEN
            MID$(MMenu$(11, 4), 1) = "û"
         ELSE
            MID$(MMenu$(11, 4), 1) = " "
         END IF

         IF Inf.Ruler THEN
            MID$(MMenu$(2, 3), 1) = "û"
         ELSE
            MID$(MMenu$(2, 3), 1) = " "
         END IF

         IF Inf.Clr < 128 THEN
            Temp$ = "Off"
         ELSE
            Temp$ = "On "
         END IF
         MID$(MMenu$(2, 4), LEN(MMenu$(2, 4)) - 2) = Temp$

      END IF

      IF Menu <> PMenu OR Choice <> PChoice OR Action = 1 THEN
         QPrintRC MenuHelp(Choice, Menu), Inf.Rows, One, 48
         PMenu = Menu
         PChoice = Choice
      END IF

      PullDnMs MMenu$(), ChStat(), Menu, Choice, Ky$, Action%

      IF Ky$ = CHR$(13) THEN
         SELECT CASE Menu
            CASE 3
               SELECT CASE Choice
                  CASE 2
                     Inf.Ruler = NOT Inf.Ruler
                  CASE ELSE
                     EXIT DO
               END SELECT
            CASE 4
               SELECT CASE Choice
                  CASE 2
                     IF Inf.Clr > 127 THEN
                        Inf.Clr = Inf.Clr AND 127
                     ELSE
                        Inf.Clr = Inf.Clr OR 128
                     END IF
                  CASE 3
                     IF Inf.SysColor > -1 THEN
                        Inf.SysColor = -1
                     ELSE
                        Inf.SysColor = Inf.Clr
                     END IF
                  CASE 11
                     Inf.BeepOn = NOT Inf.BeepOn
                  CASE ELSE
                     EXIT DO
               END SELECT
            CASE ELSE
               EXIT DO
         END SELECT

         IF NoScreen THEN Action = 2
      END IF

   LOOP UNTIL Action = 4

   IF Menu <> 4 OR Choice <> 1 OR Ky$ = CHR$(27) THEN
      PullDnMs MMenu$(), ChStat(), Menu, Choice, "", 5
      IF UBOUND(TopBuf) THEN MScrnRest One, One, One, 80, TopBuf(0)
   END IF

   MScrnRest Inf.Rows, One, Inf.Rows, 80, BotLine(0)

   IF Ky$ = CHR$(27) THEN                       'See if Esc key was pressed
      Choice = 0
                 
      IF NOT NoScreen THEN EdScrn Inf           'Edit the screen
   END IF

   Ins = 0
      

   SELECT CASE Menu

      '----- File Menu
      CASE 0

         SELECT CASE Choice

            '----- Start a New Screen
            CASE 1
                                        'See if screen has changed and save
               ChkScrChanged ScrName$, ScrnLib(), Form(), Help$(), Clc$(), Inf, ExitCode
               IF ExitCode <> 0 AND ExitCode <> 3 THEN
                  GOSUB ClearBlock
                  ScrName$ = "WORKING.SCR"
                  HideCursor
                  CLS
                  'IF Inf.SysColor > -1 THEN
                  '   MPaintBox 1, 1, Inf.Rows, 80, Inf.SysColor
                  'END IF
                  ShowCursor
                  REDIM Form(MaxFields) AS FieldInfo
                  REDIM Clc$(MaxFields), Help$(MaxFields)
                  Inf.FromLib = 0

                  Inf.EdType = -1
                  EdScrn Inf
                  'Choice = 0
               END IF
         

            '----- Load a file
            CASE 2

               OldScrName$ = ScrName$   'See if screen has changed and save

               GetFile ScrSpec$, ScrName$

               Temp$ = ParsPath$(ScrSpec$)
               LibSpec$ = Temp$ + NoPath$(LibSpec$)
               ObjSpec$ = Temp$ + NoPath$(ObjSpec$)
               ImpSpec$ = Temp$ + NoPath$(ImpSpec$)

               IF LEN(ScrName$) THEN
                  
                  IF ScrName$ = OldScrName$ THEN
                     ExitCode = 1
                  ELSE
                     ChkScrChanged OldScrName$, ScrnLib(), Form(), Help$(), Clc$(), Inf, ExitCode
                  END IF
                  GOSUB ClearBlock

                  IF ExitCode <> 0 AND ExitCode <> 3 THEN
                     
                     REDIM ScrBuf(Inf.Rows * 80)
                     MScrnSave One, One, Inf.Rows, 80, ScrBuf(0)
           
                     HideCursor
                     CLS                'Find the screen & display it
                     ShowCursor

                     IF Exist(ScrName$) THEN

                        FOpen ScrName$, Handle
                        FSeek Handle, 7&
                        FGetT Handle, Code%, 2
                        FClose Handle

                        IF Code MOD 256 <> 26 THEN
                           IF Code MOD 160 = 0 AND Code \ 160 < 25 THEN
                              CvtQBScr ScrName$, ErrorCode
                           ELSE
                              CvtBLScrns ScrName$, ErrorCode
                           END IF
                           IF ErrCode = 0 THEN
                              ErrMsg "Screen has been converted to Quick Screen format."
                              Inf.DispType = 1
                           END IF
                        ELSE
                           Attribute = -3
                           LoadScreen ScrName$, 0, Attribute, ErrorCode
                           Inf.DispType = Attribute MOD 256
                           GetVMode 0, 0, 0, Inf.Rows, 0
                        END IF
                        
                     ELSE
                        ErrorCode = 1
                     END IF
                     
                     
                     SELECT CASE ErrorCode
                        CASE 1
                           Msg$ = ScrName$ + " is not on disk!"
                        CASE 2
                           Msg$ = "This screen is too big to fit on this monitor."
                        CASE 3
                           Msg$ = ScrName$ + " is not a Quick Screen compatible screen."
                        CASE 0
                           NameLib$ = BaseName$(ScrName$)
                           LoadFldDef ScrName$
                           Inf.FromLib = 0
                           Inf.EdType = -1
                           Inf.Lines = Inf.Rows
                           EdScrn Inf
                           'Choice = 0
                        CASE ELSE
                     END SELECT

                     IF ErrorCode THEN
                        IF Inf.BeepOn THEN Chime 6
                        ErrMsg Msg$
                        MScrnRest One, One, Inf.Rows, 80, ScrBuf(0)
                        ScrName$ = OldScrName$
                     END IF

                     ERASE ScrBuf

                  ELSE
                     ScrName$ = OldScrName$
                     OldScrName$ = ""
                  END IF

               ELSE
                  ScrName$ = OldScrName$
                  OldScrName$ = ""
               END IF
         

            '----- Save the screen
            CASE 3

               OldScrName$ = ScrName$
               DO
                  FileName$ = SPACE$(50)
                  LSET FileName$ = ScrName$

                  PrSaveScr "Save", FileName$, Form(), ExitCode, Inf
                 
                  IF ExitCode <> 1 THEN EXIT DO

                  IF INSTR(FileName$, ".") = 0 THEN FileName$ = FileName$ + ".SCR"

                  There = 0
                  IF FileName$ <> ScrName$ THEN
                     LOCATE Inf.Rows \ 2 - 2
                     There = IsThere(FileName$)
                  END IF

                  ScrName$ = FileName$

               LOOP WHILE There

               IF ExitCode = 1 THEN     'Call the Screen Saver routine
                  SvScr ScrName$, Inf
                  SvForm ScrName$, Form(), Clc$(), Help$(), Inf
               ELSE
                  ScrName$ = OldScrName$
                  OldScrName$ = ""
               END IF

               EdScrn Inf
               'Choice = 0


            '----- Import a text file
            CASE 5

               GetFile ImpSpec$, ImpName$

               Temp$ = ParsPath$(ImpSpec$)
               LibSpec$ = Temp$ + NoPath$(LibSpec$)
               ObjSpec$ = Temp$ + NoPath$(ObjSpec$)
               ScrSpec$ = Temp$ + NoPath$(ScrSpec$)
             
               IF LEN(ImpName$) THEN
                  IF Inf.Ruler THEN RulerLine Inf, 5
                  MScrnRest One, One, One, 80, TopBuf(0)
                  GOSUB ClearBlock

                  OPEN ImpName$ FOR INPUT AS #1

                  R = 0
                  DO UNTIL EOF(1) OR R + Inf.CurRow > Inf.Rows
                     LINE INPUT #1, Txt$
                     LOCATE R + Inf.CurRow, Inf.CurCol
                     MQPrint LEFT$(Txt$, 81 - Inf.CurCol), Inf.SysColor
                     R = R + 1
                  LOOP
                  CLOSE #1
                  Txt$ = ""

                  MScrnSave One, One, One, 80, TopBuf(0)
                  Inf.ScrChanged = 1
                  IF Inf.Ruler THEN RulerLine Inf, 1

                  EdScrn Inf
                  'Choice = 0
               END IF

            '----- Export a file
            CASE 6

               ExpName$ = SPACE$(50)
      
               LSET ExpName$ = NoExtension$(ScrName$) + ".TXT"
             
               DO
                  FileName$ = SPACE$(50)
                  LSET FileName$ = ExpName$

                  Promt4Text "Export", "Export File as:", FileName$, ExitCode
                  ExpName$ = FileName$

                  IF ExitCode <> 1 THEN EXIT DO

                  IF INSTR(ExpName$, ".") = 0 THEN ExpName$ = ExpName$ + ".TXT"
            
               LOOP UNTIL FileNameOk(ExpName$)

               IF ExitCode = 1 THEN

                  ExitCode = 2
                  IF Exist(ExpName$) THEN
                     LOCATE Inf.Rows \ 2 - 2
                     ExitCode = AlertUser(ExpName$ + " already exists.", "Add", "Overwrite", "Cancel")
                  END IF

                  IF ExitCode <> 0 AND ExitCode <> 3 THEN
                 
                     'Find active area start and length
                     IF Inf.Ruler THEN RulerLine Inf, 5
                     ScrSize UlRow, UlCol, LrRow, LrCol, Inf
                    
                     IF ExitCode = 1 THEN
                        OPEN ExpName$ FOR APPEND AS #1
                     ELSE
                        OPEN ExpName$ FOR OUTPUT AS #1
                     END IF

                     Red$ = SPACE$(LrCol - UlCol + 1)
                     FOR R = UlRow TO LrRow
                        CALL ReadScrn0(R, UlCol, Red$)

                        PRINT #1, RTRIM$(Red$)
                     NEXT
                     Red$ = ""
                     CLOSE #1

                     IF Inf.Ruler THEN RulerLine Inf, 1
                  END IF
               END IF

               EdScrn Inf
               'Choice = 0


            '----- Load OBJect file
            CASE 8
               OldScrName$ = ScrName$

               GetFile ObjSpec$, ObjName$
               IF LEN(ObjName$) THEN
               
                  Temp$ = ParsPath$(ObjSpec$)
                  LibSpec$ = Temp$ + NoPath$(LibSpec$)
                  ScrSpec$ = Temp$ + NoPath$(ScrSpec$)
                  ImpSpec$ = Temp$ + NoPath$(ImpSpec$)
                                        'See if screen has changed and save
                  ChkScrChanged ScrName$, ScrnLib(), Form(), Help$(), Clc$(), Inf, ExitCode

                  GOSUB ClearBlock
                  IF ExitCode <> 0 AND ExitCode <> 3 THEN
             
                     REDIM ScrBuf(Inf.Rows * 80)
                     MScrnSave One, One, Inf.Rows, 80, ScrBuf(0)
          
                     LoadObj ObjName$, Attribute, ErrorCode

                     Inf.Lines = Attribute \ 256
                     Inf.DispType = Attribute MOD 256
                     GetVMode 0, 0, 0, Inf.Rows, 0
                    
                     SELECT CASE ErrorCode
                        CASE 1
                           Msg$ = ObjName$ + " is not on disk!"
                        CASE 2
                           Msg$ = "This screen is too big to fit on this monitor."
                        CASE 3
                           Msg$ = ObjName$ + " is not a compatible object screen."
                        CASE 0
                           NameLib$ = BaseName$(ObjName$)
                           ScrName$ = NoExtension$(ObjName$) + ".SCR"

                           CALL LoadFldDef(ScrName$)

                           Inf.FromLib = 0
                           Inf.EdType = -1
                           EdScrn Inf
                           'Choice = 0
                        CASE ELSE
                     END SELECT

                     IF ErrorCode THEN
                        IF Inf.BeepOn THEN Chime 6
                        ErrMsg Msg$
                        MScrnRest One, One, Inf.Rows, 80, ScrBuf(0)
                        ScrName$ = OldScrName$
                     END IF

                     ERASE ScrBuf

                  ELSE
                     ScrName$ = OldScrName$
                     OldScrName$ = ""
                  END IF

               ELSE
                  ScrName$ = OldScrName$
                  OldScrName$ = ""
               END IF



            '----- Save as OBJect file
            CASE 9
               ObjName$ = SPACE$(50)
               LSET ObjName$ = NoExtension$(ScrName$) + ".OBJ"

               OldObjName$ = RTRIM$(ObjName$)
               DO

                  PrSaveScr "Save", ObjName$, Form(), ExitCode, Inf
          
                  IF ExitCode <> 1 THEN EXIT DO

                  IF INSTR(ObjName$, ".") = 0 THEN ObjName$ = ObjName$ + ".OBJ"

                  There = 0
                  IF ObjName$ <> OldObjName$ THEN
                     LOCATE Inf.Rows \ 2 - 2
                     There = IsThere(ObjName$)
                  END IF

               LOOP WHILE There

               IF ExitCode = 1 THEN     'Call the Screen Saver routine
                  IF Inf.Ruler THEN RulerLine Inf, 5

                  Scr2Obj ObjName$, Inf.DispType, Inf

                  IF Inf.Ruler THEN RulerLine Inf, 1

                  ScrName$ = NoExtension$(ObjName$) + ".SCR"
                  SvForm ScrName$, Form(), Clc$(), Help$(), Inf
                  Inf.ScrChanged = 0    'Reset the changed attribute
               ELSE
                  ObjName$ = OldObjName$
               END IF

               EdScrn Inf
               'Choice = 0


            '----- DOS Shell
            CASE 11

               HideCursor
               REDIM ScrBuf(Inf.Rows * 80)
               MScrnSave One, One, Inf.Rows, 80, ScrBuf(0)
               WIDTH , DosRows
               CLS
               MScrnRest One, One, DosRows, 80, DosScr(0)
               LOCATE DosRow, One
               PRINT "Type EXIT to return."

               SHELL

               GetVMode 0, 0, 0, DosRows, 0
               DosRow = CSRLIN
               REDIM DosScr(DosRows * 80)
               MScrnSave One, One, DosRows, 80, DosScr(0)

               WIDTH , Inf.Rows
               MScrnRest One, One, Inf.Rows, 80, ScrBuf(0)
               ERASE ScrBuf
               ShowCursor

               IF NOT NoScreen THEN
                  EdScrn Inf
                  'Choice = 0
               END IF

            '----- X - Exit
            CASE 12
               ChkScrChanged ScrName$, ScrnLib(), Form(), Help$(), Clc$(), Inf, ExitCode
               IF ExitCode <> 0 AND ExitCode <> 3 THEN

                  ChkLibChanged LibName$, ScrnLib(), Inf, ExitCode
                  IF ExitCode <> 0 AND ExitCode <> 3 THEN
                     EXIT DO
                  END IF
               END IF

               EdScrn Inf
               'Choice = 0


            CASE ELSE
         END SELECT


      '----- Library Menu
      CASE 1

         SELECT CASE Choice

            '----- Display a screen from the library
            CASE 1

               ChkScrChanged ScrName$, ScrnLib(), Form(), Help$(), Clc$(), Inf, ExitCode
               GOSUB ClearBlock

               IF ExitCode = 1 OR ExitCode = 2 THEN
                  LibScrMenu SName$, ScrnLib(), Inf
                  IF LEN(SName$) THEN
                     ScrName$ = ParsPath$(LibName$) + SName$ + ".SCR"
                     LoadFldDef ScrName$

                     Inf.EdType = -1
                     EdScrn Inf
                     'Choice = 0
                  END IF
               END IF
         

            '----- Create a new library
            CASE 3

               ExitCode = 1
               IF Inf.FromLib AND Inf.ScrChanged THEN
                  ChkScrChanged ScrName$, ScrnLib(), Form(), Help$(), Clc$(), Inf, ExitCode
                  IF ExitCode <> 0 AND ExitCode <> 3 THEN
                     ChkLibChanged LibName$, ScrnLib(), Inf, ExitCode
                  END IF
               END IF
               IF ExitCode <> 0 AND ExitCode <> 3 THEN

                  ChkLibChanged LibName$, ScrnLib(), Inf, ExitCode
                  IF ExitCode <> 0 AND ExitCode <> 3 THEN

                     OldLibName$ = LibName$
                     LibName$ = "WORKING.QSL"

                     DO
                        FileName$ = SPACE$(50)
                        LSET FileName$ = LibName$

                        Promt4Text "Create", "Create New Library:", FileName$, ExitCode
                        LibName$ = FileName$
                        IF INSTR(LibName$, ".") = 0 THEN LibName$ = LibName$ + ".QSL"

                        IF ExitCode <> 1 THEN EXIT DO
                        LOCATE Inf.Rows \ 2 - 2

                     LOOP UNTIL FileNameOk(LibName$) AND NOT IsThere(LibName$)

                     IF ExitCode = 1 THEN
                        REDIM ScrnLib(32766)
                     ELSE
                        LibName$ = OldLibName$
                     END IF
                  END IF
               END IF

               IF NOT NoScreen THEN
                  EdScrn Inf
                  'Choice = 0
               END IF



            '----- Load a library
            CASE 4

               ExitCode = 1
               IF Inf.FromLib AND Inf.ScrChanged THEN
                  ChkScrChanged ScrName$, ScrnLib(), Form(), Help$(), Clc$(), Inf, ExitCode
                  IF ExitCode <> 0 AND ExitCode <> 3 THEN
                     ChkLibChanged LibName$, ScrnLib(), Inf, ExitCode
                  END IF
               END IF

               IF ExitCode <> 0 AND ExitCode <> 3 THEN

                  ChkLibChanged LibName$, ScrnLib(), Inf, ExitCode
                  IF ExitCode <> 0 AND ExitCode <> 3 THEN

                     OldLibName$ = LibName$

                     GetFile LibSpec$, LibName$

                     Temp$ = ParsPath$(LibSpec$)
                     ScrSpec$ = Temp$ + NoPath$(ScrSpec$)
                     ObjSpec$ = Temp$ + NoPath$(ObjSpec$)
                     ImpSpec$ = Temp$ + NoPath$(ImpSpec$)

                     IF LEN(LibName$) THEN

                        DO
                           FOpen LibName$, Handle
                           IF DosError THEN
                              IF ErrorPrompt = 0 THEN EXIT DO
                           END IF

                        LOOP WHILE DosError

                        IF NOT DosError THEN
                           DimSize& = (FLof&(Handle) - 8) \ 2

                           FSeek Handle, 9&
                           FGetT Handle, Code%, 2

                           IF NOT DosError AND DimSize& <= UBOUND(ScrnLib) THEN
                              FClose Handle
                              IF Code = 26 THEN
                                 QBLoad LibName$, ScrnLib(0)
                              ELSE
                                 CvtLib LibName$, ScrnLib()
                                 ErrMsg "Library has been converted to Quick Screen format."
                              END IF

                              MenL = 1
                              Choice = 1
                           ELSE
                              FClose Handle
                              IF Inf.BeepOn THEN Chime 6
                              ErrMsg LibName$ + " is not a Quick Screen compatible library."
                              LibName$ = OldLibName$
                           END IF
                        ELSE
                           FClose Handle
                        END IF

                     ELSE
                        LibName$ = OldLibName$
                     END IF
                  END IF
               END IF


            '----- Save library
            CASE 5

               OldLibName$ = LibName$
               DO
                  FileName$ = SPACE$(50)
                  LSET FileName$ = LibName$

                  Promt4Text "Save", "Save Library as:", FileName$, ExitCode

                  IF ExitCode <> 1 THEN EXIT DO

                  IF LibName$ <> OldLibName$ THEN
                     LOCATE Inf.Rows \ 2 - 2
                     There = IsThere(LibName$)
                  ELSE
                     There = 0
                  END IF

                  LibName$ = FileName$

               LOOP UNTIL FileNameOk(LibName$) AND NOT There

               IF ExitCode = 1 THEN
                  SvLib LibName$, ScrnLib(), Inf.LibChanged
                  BuildFrmLib LibName$, ScrnLib()
               ELSE
                  LibName$ = OldLibName$
               END IF

               IF NOT NoScreen THEN
                  EdScrn Inf
                  'Choice = 0
               END IF
         

            '----- Add screen to library
            CASE 7

               NScrns = ScrnLib(0)
               Path$ = ParsPath$(ScrName$)
               NameLib$ = BaseName$(ScrName$)
               Xtn$ = Extension$(ScrName$)
          
               NameLib$ = SPACE$(8)
               LSET NameLib$ = BaseName$(ScrName$)
               PrSaveScr "Add", NameLib$, Form(), ExitCode, Inf
               NameLib$ = UCASE$(NameLib$)
               NInLib = LibNo%(UCASE$(NameLib$), ScrnLib())

               IF NInLib = 0 THEN
                  IF ExitCode = 1 THEN

                     CALL Add2Lib(NameLib$, ScrnLib(), Inf, ErrFlag)
                     IF NOT ErrFlag THEN
                        ScrName$ = Path$ + NameLib$ + Xtn$
                        SvForm ScrName$, Form(), Clc$(), Help$(), Inf
                        Inf.FromLib = -1
                        Choice = 5
                     END IF
                  END IF
               ELSE
                  ErrMsg NameLib$ + " is already in the library!  Can't Add it."
                  Choice = 8
               END IF


            '----- Replace screen in library
            CASE 8

               NScrns = ScrnLib(0)
               Path$ = ParsPath$(ScrName$)
               NameLib$ = BaseName$(ScrName$)
               Xtn$ = Extension$(ScrName$)
               NInLib = LibNo%(UCASE$(NameLib$), ScrnLib())

               IF NInLib THEN

                  NameLib$ = SPACE$(8)
                  LSET NameLib$ = BaseName$(ScrName$)
                  PrSaveScr "Replace", NameLib$, Form(), ExitCode, Inf
                  NameLib$ = UCASE$(NameLib$)
                  NewNInLib = LibNo%(NameLib$, ScrnLib())
                  IF NewNInLib THEN NInLib = NewNInLib

                  IF ExitCode = 1 THEN

                     ReplaceInLib NameLib$, NInLib, ScrnLib(), Inf, ErrFlag

                     IF NOT ErrFlag THEN
                        ScrName$ = Path$ + NameLib$ + Xtn$
                        SvForm ScrName$, Form(), Clc$(), Help$(), Inf
                        Choice = 5
                     END IF
                  END IF
               ELSE
                  IF Inf.BeepOn THEN Chime 6
                  ErrMsg NameLib$ + " is not in the library!  Can't Replace."
                  Choice = 7
               END IF

            CASE ELSE
         END SELECT


      '----- Edit Menu Choices
      CASE 2

         SELECT CASE Choice

            '----- Paint an area of the screen
            CASE 1
               IF Inf.MarkBlock THEN
                  StuffBuf CHR$(10)
               ELSE
                  BlockPaint Inf
               END IF

            '----- Replace Color
            CASE 2
               ReplaceColor Inf

            '----- Draw a box
            CASE 4
               DrawBox

            '----- Draw Lines
            CASE 5
               Sketch LN$(0), Inf

            '----- Fill Character
            CASE 6
               FillChar Inf

            '----- Cut
            CASE 8
               ShiftFake = -1
               StuffBuf "."

            '----- Copy
            CASE 9
               StuffBuf Chr0$ + CHR$(4)

            '----- Paste
            CASE 10
               ShiftFake = -1
               StuffBuf "0"

            '----- Move
            CASE 11
               ShiftFake = -1
               StuffBuf CHR$(13)

            '----- Clear
            CASE 12
               StuffBuf Chr0$ + CHR$(83)

            '----- Center Line/Block
            CASE 14
               StuffBuf Chr0$ + CHR$(63)

            '----- Repeat Last Key
            CASE 16
               StuffBuf CHR$(18)

            CASE ELSE
         END SELECT

         IF NOT NoScreen AND Ky$ <> CHR$(27) THEN EdScrn Inf
         

      '----- View operations menu choices
      CASE 3
         SELECT CASE Choice

            '----- ASCII Chart
            CASE 1
               Ok2Print = NOT NoScreen
               LastKey$ = ASCIIChart$(Ok2Print, Inf)
               IF Ok2Print THEN
                  EdScrn Inf
               END IF
           
            '----- Display Position
            CASE 2
               IF Inf.Ruler THEN
                  RAction = 0
               ELSE
                  RAction = 5
               END IF
               RulerLine Inf, RAction

               EdScrn Inf

            '----- Monochrome Display
            CASE 4, 5, 6
               MonoDisp Choice, Inf
               EdScrn Inf


            CASE ELSE
         END SELECT


      '----- Settings operations menu choices
      CASE 4

         SELECT CASE Choice

            '----- Choose Colors
            CASE 1

               GetColor Inf.Clr, ExitCode

               IF Inf.SysColor > -1 THEN
                  Inf.SysColor = Inf.Clr
               END IF

               PullDnMs MMenu$(), ChStat(), Menu, Choice, Ky$, 5
               
               IF NOT NoScreen THEN
                  MScrnRest One, One, One, 80, TopBuf(0)
                  IF ExitCode = 0 AND Inf.MarkBlock THEN
                     StuffBuf CHR$(10)
                  END IF
                  EdScrn Inf
               END IF


            '----- Toggle Blinking, use of current color
            CASE 2, 3
               EdScrn Inf

            '----- Box type menu
            CASE 5

               Action = 1
               LChoice = 1
               DO
                  FOR N = 0 TO 3
                     OldLN(N) = LN(N)
                  NEXT

                  PullDnMs LMenu$(), BsStat(), LMenu, LChoice, Ky$, Action

                  IF Ky$ = CHR$(27) THEN EXIT DO

                  'Get a line type from menu choice
                  IF Action = 4 AND LMenu < 4 THEN
                     LN(LMenu) = LChoice
                     MID$(LMenu$(OldLN(LMenu), LMenu), 1) = " "
                     MID$(LMenu$(LN(LMenu), LMenu), 1) = "û"
                     Action = 2

                     IF LChoice = 6 THEN
                        Temp$ = " "
                        LSET Temp$ = ASCIIChart$(0, Inf)
                        MID$(LMenu$(LN(LMenu), LMenu), 3) = Temp$
                     END IF
                  END IF

                  IF Action = 4 AND LMenu = 4 THEN
                     Temp$ = " "
                     IF LChoice = 6 THEN
                        LSET Temp$ = ASCIIChart$(0, Inf)
                     END IF

                     FOR L = 0 TO 3
                        LN(L) = LChoice
                        MID$(LMenu$(OldLN(L), L), 1) = " "
                        MID$(LMenu$(LN(L), L), 1) = "û"
                        IF LChoice = 6 THEN
                           MID$(LMenu$(LN(L), L), 3) = Temp$
                        END IF
                     NEXT
                     LMenu = 5
                     Action = 2
                  END IF
               LOOP UNTIL Action = 4 AND LMenu = 5
               PullDnMs LMenu$(), BsStat(), LMenu, LChoice, Ky$, 5

               IF Ky$ <> CHR$(27) THEN GOSUB FindCorners

               LMenu = 0
               IF NOT NoScreen THEN
                  EdScrn Inf
                  'Choice = 0
               END IF


            '----- Change Line Mode
            CASE 7, 8, 9

               REDIM ScrBuf(Inf.Rows * 80)
               MScrnSave One, One, Inf.Rows, 80, ScrBuf(0)
               RowsWhere = Inf.Rows
               HideCursor

               CALL Extents(0, 0, ScrRows, 0)
               SELECT CASE Choice
                  CASE 7
                     IF ScrRows <= 25 THEN
                        WIDTH , 25
                        Inf.Rows = 25
                     END IF
                  CASE 8
                     IF ScrRows <= 43 THEN
                        WIDTH , 43
                        Inf.Rows = 43
                     END IF
                  CASE 9
                     WIDTH , 50
                     Inf.Rows = 50
                  CASE ELSE
               END SELECT
               MouseTrap One, One, Inf.Rows, 80
               ShowCursor

               MScrnRest One, One, MinInt(Inf.Rows, RowsWhere), 80, ScrBuf(0)
               Inf.CurRow = MinInt(Inf.CurRow, Inf.Rows)
               EdScrn Inf
               'Choice = 0

            '----- Beep on Errors
            CASE 11
               IF NOT NoScreen THEN EdScrn Inf
               'Choice = 0
         
            CASE ELSE
         END SELECT



      '----- Compose operations menu choices
      CASE 5
        
         SELECT CASE Choice

            '----- Generate a form
            CASE 1
               Row = Inf.CurRow
               Col = Inf.CurCol
               LOCATE Row, Col
               DefineForm Form(), TypCh$()
               Inf.CurRow = Row
               Inf.CurCol = Col
               IF Inf.Ruler THEN
                  RulerLine Inf, 5
                  RulerLine Inf, 1
               END IF

            '----- Choose field color
            CASE 2
               IF Inf.PaintFields THEN
                  Inf.PaintFields = 0
               ELSE
                  GetColor Inf.PaintFields, ExitCode
               END IF

            '----- Rearrange Fields
            CASE 4

               RearrangeFields Form(), Clc$(), Help$(), Inf.Rows, Inf.FrmChanged

            '----- Print the form definition
            CASE 5

               PrintFRM ScrName$, Form(), Clc$(), Help$(), Inf

            '----- Try Data Entry in Form
            CASE 7
               '----- Clear menus from memory
               REDIM MMenu$(0, 0)               'Main Menu Choices
               REDIM ChStat(0, 0)               'Main Menu Choice Status
               ERASE MenuHelp

               REDIM Choice$(5, 0)
               FOR R = 1 TO 5
                  Choice$(R, 0) = CHR$(R + 64) + " Choice"
               NEXT

               REDIM FData$(Form(0).Fields, 2)
               ErrCode = 0

               FOR N = 1 TO Form(0).Fields
                   FData$(N, 0) = SPACE$(Form(N).RCol - Form(N).LCol + 1)
                   FData$(N, 1) = Help$(N)
                   Help$(N) = ""
                   FData$(N, 2) = Clc$(N)
                   IF LEN(FData$(N, 2)) THEN Tokenize FData$(N, 2), Form()

                   IF Form(N).FType = MultChAFld THEN
                      IF LEN(Choice$(0, 0)) THEN Choice$(0, 0) = Choice$(0, 0) + ","
                      Choice$(0, 0) = Choice$(0, 0) + STR$(N)
                   END IF

                   IF FRE("") < 4096 THEN
                      ErrMsg "Not enough memory to try this form!"
                      ErrCode = -1
                      EXIT FOR
                   END IF
                   
                   IF N = 1 THEN
                      Form(1).Fields = 1
                   ELSE
                      Form(N).Fields = Form(N - 1).Fields + Form(N - 1).StorLen
                   END IF
               NEXT

               IF NOT ErrCode THEN
                  Form(0).Row = Form(N - 1).Fields + Form(N - 1).StorLen - 1

                  ERASE Help$


                  IF Inf.Ruler THEN RulerLine Inf, 5

                  REDIM ScrBuf(Inf.Rows * 80)
                  MScrnSave 1, 1, Inf.Rows, 80, ScrBuf(0)


                  Frm.FldNo = 1
                  Action = 1

                  '----- Poll the editor in a loop.
                  DO
                     EditForm FData$(), Form(), Frm, Cnf, Action
                  LOOP UNTIL Frm.KeyCode = 27


                  ErrMsg "Form editing complete."

                  MScrnRest 1, 1, Inf.Rows, 80, ScrBuf(0)
                                        
                  ERASE ScrBuf, Choice$
               END IF

               FOR N = 1 TO Form(0).Fields
                  FData$(N, 0) = ""
                  FData$(N, 2) = ""
               NEXT
               REDIM Help$(MaxFields)
               FOR N = 1 TO Form(0).Fields
                  Help$(N) = FData$(N, 1)
                  FData$(N, 1) = ""
               NEXT

               ERASE FData$
               
               DefineMenus MenuFile$, MMenu$(), ChStat(), MenuHelp()

               IF Inf.Ruler THEN RulerLine Inf, 1


            CASE ELSE
         END SELECT

         IF NOT NoScreen AND Ky$ <> CHR$(27) THEN EdScrn Inf

      CASE 6
         GetHelp ProgLib$, MonoCode, Inf

      CASE ELSE
   END SELECT

LOOP


LOCATE , , 0
FCreate CnfName$
FOpen CnfName$, Handle
FPut Handle, "Q"
FPutT Handle, Inf.Clr, 2                          'Default Colors
FOR N = 0 TO 3
   FPutT Handle, LN(N), 2                         'Default Line Types
NEXT
FOR N = 0 TO 3
   FPut Handle, LN$(N)                            'Default Line Types
NEXT
FPutT Handle, Inf.BeepOn, 2
FPut Handle, Inf.CrncStr
FPutT Handle, Inf.Ruler, 2
FPutT Handle, Inf.SaveTYPE, 2
FPutT Handle, Inf.SaveDATA, 2
FPutT Handle, Inf.SysColor, 2
FPutT Handle, Inf.PaintFields, 2
FClose Handle

HideCursor
WIDTH , DosRows
CloseCurtain DosScr(), Zero
LOCATE DosRow, , One

END






FindCorners:
   FOR N = 0 TO 3
      LN$(N) = MID$(LMenu$(LN(N), N), 3, 1)
   NEXT

   IF LN(1) = 6 AND LN(0) = 6 THEN TL$(6, 6) = LN$(0)
   TLCor$ = TL$(LN(1), LN(0))      'Get the TOP LEFT Corner type

   IF LN(3) = 6 AND LN(0) = 6 THEN TR$(6, 6) = LN$(0)
   TRCor$ = TR$(LN(3), LN(0))      'Get the TOP RIGHT Corner type

   IF LN(1) = 6 AND LN(2) = 6 THEN BL$(6, 6) = LN$(3)
   BLCor$ = BL$(LN(1), LN(2))      'Get the BOTTOM LEFT Corner type

   IF LN(3) = 6 AND LN(2) = 6 THEN BR$(6, 6) = LN$(3)
   BRCor$ = BR$(LN(3), LN(2))      'Get the BOTTOM RIGHT Corner type
RETURN


ClearBlock:
   StuffBuf CHR$(1)
   ScrEdit "", Inf
   Inf.WasMarked = 0
RETURN


SetMenuStat:
   SetStatus ChStat(C, M), T
RETURN

SUB DefineForm (Form() AS FieldInfo, TypCh$())
   SHARED Dial$()
   SHARED Inf AS GenInfo
   SHARED Clc$(), Help$()
   SHARED Blank AS FieldInfo
   SHARED ScrName$

   STATIC FldNo, PrevName$

   DIM B(3, 1)

   Press = 0
   IF PrevName$ <> ScrName$ THEN FldNo = 1
   PrevName$ = ScrName$
   FLen$ = "  "
   P = 1
   TypCod = 0
  
   IF Inf.Ruler THEN RulerLine Inf, 5

   REDIM ScrBuf(Inf.Rows * 80)
   MScrnSave 1, 1, Inf.Rows, 80, ScrBuf(0)
   LOCATE , , 1

   DIM Test AS FieldInfo

   K$ = CHR$(1)

   DO

      FldNo = MinInt(FldNo, Form(0).Fields + 1)
      IF FldNo > MaxFields THEN
         FldNo = MaxFields
         IF Inf.BeepOn THEN Chime 6
         ErrMsg "Last available field has been used!"
      END IF

      IF Press = 0 THEN
         MID$(Dial$(5), 1) = " (-) Move to Previous Field."
         IF Form(FldNo).Row THEN
            Inf.CurRow = Form(FldNo).Row
            Inf.CurCol = Form(FldNo).LCol
            MID$(Dial$(5), 29) = "  (+) for Next Field."
         ELSEIF FldNo > 1 THEN
            Inf.CurRow = Form(FldNo - 1).Row
            Inf.CurCol = Form(FldNo - 1).LCol
         END IF
      END IF
          
      GOSUB DialBx

      Inf.EdType = 1
      DO
         ScrEdit K$, Inf

         IF QPLen(K$) THEN

            IF Press = 2 AND TypCod <> NotesFld THEN
               Inf.CurRow = PrevRow
            END IF
            IF Inf.CurRow <> PrevRow THEN
               MPRestore P, 15, P + 9, 68, 80, ScrBuf((P - 1) * 80 + 14)
            END IF

            SELECT CASE Press
               CASE 1
                  SELECT CASE K$
                     CASE CHR$(13)
                        BegCol = Inf.CurCol
                        MScrnRest 1, 1, Inf.Rows, 80, ScrBuf(0)

                        IF Inf.Ruler THEN RulerLine Inf, 1
                        TypCod = Form(FldNo).FType
                        GetFldType FldNo, TypCod, ExitCode
                        MainType = TypCod
                        IF ExitCode = 1 AND TypCod >= 11 AND TypCod <= 13 THEN
                           GetRelation FldNo, Form(), RelName$, RelFld$, RelFld, RelType, Decimals, ExitCode
                           IF ExitCode = 1 THEN TypCod = RelType
                        END IF
                        IF Inf.Ruler THEN RulerLine Inf, 5

                        Inf.EdType = 1
                        IF ExitCode = 1 THEN
                           BegRow = Inf.CurRow
                           Lth = Form(FldNo).RCol - Form(FldNo).LCol + 1
                           EnCol = MinInt(80, BegCol + Lth - 1)
                           EnRow = BegRow

                           SELECT CASE TypCod
                              CASE LogicalFld                  'Logical
                                 EnCol = BegCol
                              CASE DateFld, EuroDateFld        'Date
                                 EnCol = BegCol + 9
                              CASE PhoneFld                    'Phone Number
                                 EnCol = BegCol + 13
                              CASE SoSecFld                    'Social Security #
                                 EnCol = BegCol + 10
                              CASE ZipFld                      'Zip Code
                                 EnCol = BegCol + 9
                              CASE NotesFld
                                 Lns = MaxInt(Form(FldNo).ScratchI, Form(FldNo).Row) - Form(FldNo).Row + 1
                                 EnRow = MinInt(Inf.Rows, BegRow + Lns - 1)
                              CASE ELSE
                           END SELECT
                
                           Inf.CurCol = EnCol
                           Inf.CurRow = EnRow

                           B(0, 0) = BegRow
                           B(0, 1) = BegRow
                           B(1, 0) = BegCol
                           B(1, 1) = BegCol
                           B(2, 0) = EnRow
                           B(2, 1) = EnRow
                           B(3, 0) = EnCol
                           B(3, 1) = EnCol

                           IF Inf.PaintFields THEN
                              HiClr = Inf.PaintFields
                           ELSEIF SCREEN(BegRow, BegCol, 1) = 112 THEN
                              HiClr = HiLite
                           ELSE
                              HiClr = BoxClr
                           END IF

                           Press = 2
                           GOSUB ShowFldLen
                        ELSE
                           Press = 0
                           EXIT DO
                        END IF
             

                     CASE CHR$(27), Chr0$ + CHR$(68)
                        EXIT DO

                     CASE "+"
                        FldNo = MinInt(FldNo + 1, MaxFields)
                        Press = 0
                        EXIT DO

                     CASE "-"
                        FldNo = MaxInt(FldNo - 1, 1)
                        Press = 0
                        EXIT DO

                     CASE Chr0$ + "R"                'Insert a field
                        IF Form(0).Fields < MaxFields THEN
                           FOR N = MaxFields TO FldNo + 1 STEP -1
                              Form(N) = Form(N - 1)
                              Clc$(N) = Clc$(N - 1)
                              Help$(N) = Help$(N - 1)
                           NEXT N
                           Inf.FrmChanged = -1
                           Form(0).Fields = Form(0).Fields + 1

                           Form(FldNo) = Blank
                           Form(FldNo).Row = Form(FldNo + 1).Row
                           Form(FldNo).LCol = Form(FldNo + 1).LCol
                           Form(FldNo).RCol = Form(FldNo).LCol
                           Clc$(FldNo) = ""
                           Help$(FldNo) = ""
                           Press = 0
                           EXIT DO
                        ELSE
                           IF Inf.BeepOn THEN Chime 6
                           ErrMsg "Too many fields!"
                        END IF

                     CASE Chr0$ + "S"                'Delete a field

                        FOR N = FldNo TO MaxFields - 1
                           Form(N) = Form(N + 1)
                           Clc$(N) = Clc$(N + 1)
                           Help$(N) = Help$(N + 1)
                        NEXT N
                        Form(MaxFields) = Blank
                        Clc$(MaxFields) = ""
                        Help$(MaxFields) = ""
                        Form(0).Fields = MaxInt(Form(0).Fields - 1, 0)
                        Inf.FrmChanged = -1
                        Press = 0
                        FldNo = MinInt(FldNo, Form(0).Fields)
                        FldNo = MaxInt(FldNo, 1)
                        EXIT DO

                     CASE ELSE
                  END SELECT

               CASE 2
                  SELECT CASE K$
                     CASE CHR$(13)
                        Press = 3
                        EXIT DO

                     CASE CHR$(27)
                        Press = 0
                        GOSUB DialBx
                        EXIT DO

                     CASE ELSE
                        GOSUB ShowFldLen
                  END SELECT
               CASE ELSE
            END SELECT

            IF Inf.CurRow <> PrevRow THEN
               GOSUB DialBx
            END IF

            PrevRow = Inf.CurRow
         END IF

      LOOP
      Inf.EdType = 0


      IF Press = 3 THEN
         REDIM Dl(17) AS Dialog
         REDIM Fd$(17, 1)

         Dl(1).DRow = 2
         Dl(1).DCol = 2
         Dl(1).DLen = 8
         Dl(1).DTyp = 1
         Fd$(1, 0) = "Field Name: "

         IF Null(Form(FldNo).FldName) THEN
            SELECT CASE MainType
               CASE StrFld
                  Temp$ = "STRING"
               CASE PropStrFld
                  Temp$ = "PROPSTR"
               CASE UCaseStrFld
                  Temp$ = "UCASE"
               CASE NumericStrFld
                  Temp$ = "NUMERIC"
               CASE LogicalFld
                  Temp$ = "LOGICAL"
               CASE IntFld
                  Temp$ = "INTEGER"
               CASE LongIntFld
                  Temp$ = "LONGINT"
               CASE SngFld
                  Temp$ = "SINGLE"
               CASE DblFld
                  Temp$ = "DOUBLE"
               CASE MoneyFld
                  Temp$ = "MONEY"
               CASE DateFld
                  Temp$ = "USDATE"
               CASE EuroDateFld
                  Temp$ = "EURDATE"
               CASE PhoneFld
                  Temp$ = "PHONE"
               CASE SoSecFld
                  Temp$ = "SOSEC"
               CASE ZipFld
                  Temp$ = "ZIPCODE"
               CASE SequFld
                  Temp$ = "SEQUENCE"
               CASE MultChAFld
                  Temp$ = "LIST"
               CASE Relational
                  Temp$ = "RELATE"
               CASE Button
                  Temp$ = "BUTTON"
               CASE ELSE
                  Temp$ = "FIELD"
            END SELECT
            Form(FldNo).FldName = Temp$

            MakeUniqueFldName FldNo, Form()
         END IF

         Fd$(1, 1) = RTRIM$(Form(FldNo).FldName)
                  
         FOR N = 2 TO 10
            Dl(N).DRow = 4
            Dl(N).DCol = 1
            Dl(N).DTyp = 6
         NEXT

         IF MainType <> MoneyFld THEN
            Dl(2).DRow = 2
            Dl(2).DCol = 25
            Dl(2).DLen = 8
            Dl(2).DTyp = 1
            Fd$(2, 0) = "User Data: "
            IF Null(Form(FldNo).ScratchS) THEN Form(FldNo).ScratchS = ""
            Fd$(2, 1) = RTRIM$(Form(FldNo).ScratchS)
         END IF

         IF MainType <> MultChFld AND MainType <> MultChAFld AND MainType <> Button THEN
            Dl(3).DRow = 2
            Dl(3).DCol = 47
            Dl(3).DLen = 1
            Dl(3).DTyp = 3
            Fd$(3, 0) = "Protected Field "
            IF Form(FldNo).Protected OR TypCod = TotRecsFld THEN
               Fd$(3, 1) = "û"
            ELSE
               Fd$(3, 1) = " "
            END IF
         END IF

        
         TempCode = TypCod
         'IF MainType = MultChFld OR MainType = VirtualFld THEN TempCode = MainType

         NextLine = 5

         SELECT CASE TempCode
            CASE Button
               Dl(4).DRow = 5
               Dl(4).DCol = 19'47'22
               Dl(4).DLen = 4
               Dl(4).DTyp = 2
               Fd$(4, 0) = "Return Key Code: "
               IF Form(FldNo).ScratchI THEN
                  Fd$(4, 1) = STR$(Form(FldNo).ScratchI)
               ELSE
                  Fd$(4, 1) = STR$(Form(FldNo).Indexed)
                  Form(FldNo).Indexed = 0
               END IF

               Dl(5).DRow = 5
               Dl(5).DCol = 43
               Dl(5).DTyp = 6
               Fd$(5, 0) = "Use - for extended key."
               NextLine = 8

            '----- Indexing
            CASE StrFld, PropStrFld, UCaseStrFld, NumericStrFld, IntFld, LongIntFld, SngFld, DblFld, MoneyFld, DateFld, EuroDateFld, PhoneFld, SoSecFld, ZipFld, SequFld, MultChAFld
               Dl(4).DRow = 3
               Dl(4).DCol = 47
               Dl(4).DLen = 1
               Dl(4).DTyp = 3
               Fd$(4, 0) = "Indexed Field "
               IF Form(FldNo).Indexed THEN
                  Fd$(4, 1) = "û"
               ELSE
                  Fd$(4, 1) = " "
               END IF

            '----- Check Marks
            CASE LogicalFld
               IF MainType < 11 OR MainType > 13 THEN
                  Dl(4).DRow = 5
                  Dl(4).DCol = 2
                  Dl(4).DLen = 1
                  Dl(4).DTyp = 1
                  Fd$(4, 0) = "True Character: "
                  Fd$(4, 1) = MID$(Form(FldNo).RelFile, 3, 1)

                  Dl(5).DRow = 5
                  Dl(5).DCol = 24
                  Dl(5).DLen = 1
                  Dl(5).DTyp = 1
                  Fd$(5, 0) = "False Character: "
                  Fd$(5, 1) = MID$(Form(FldNo).RelFile, 1, 1)

                  NextLine = 8
               END IF

            CASE ELSE
         END SELECT


         '----- Decimal Places
         SELECT CASE TypCod
            CASE IntFld, LongIntFld, SngFld, DblFld, MoneyFld
               SELECT CASE TypCod
                  CASE SngFld, DblFld, MoneyFld
                     'NextLine = NextLine + 1
                     Dl(5).DRow = NextLine
                     Dl(5).DCol = 2
                     Dl(5).DLen = 3
                     Dl(5).DTyp = 2
                     Fd$(5, 0) = "Decimal Places: "
                     IF Form(FldNo).Decimals > -1 THEN
                        Fd$(5, 1) = STR$(Form(FldNo).Decimals)
                     END IF
                     IF TypCod = MoneyFld THEN
                        IF Null(Form(FldNo).ScratchS) THEN
                           Fd$(5, 1) = "2"
                        END IF
                     END IF
                     Temp = 1
                  CASE ELSE
                     NextLine = NextLine - 1
                     Temp = 0
               END SELECT

               IF TypCod = MoneyFld THEN
                  Dl(6).DRow = NextLine
                  Dl(6).DCol = 30
                  Dl(6).DLen = 8
                  Dl(6).DTyp = 1
                  Fd$(6, 0) = "Currency Symbol:"
                  IF NOT Null(Form(FldNo).ScratchS) THEN
                     Fd$(6, 1) = QPRTrim$(Form(FldNo).ScratchS)
                  ELSE
                     Fd$(6, 1) = QPRTrim$(Inf.CrncStr)
                  END IF
               ELSE
                  Dl(6).DRow = NextLine
                  Dl(6).DCol = 47
                  Dl(6).DTyp = 3
                  Fd$(6, 0) = "No Formatting"
                  IF Form(FldNo).Decimals = -1 THEN Fd$(6, 1) = "û"
               END IF
                     
               NextLine = NextLine + 2 + Temp

            CASE ELSE
         END SELECT



         '----- Ranges and formulas
         SELECT CASE TypCod
            CASE IntFld, LongIntFld, DateFld, EuroDateFld, SngFld, DblFld, MoneyFld

               IF Form(FldNo).FType <> TypCod THEN
                  SELECT CASE TypCod
                     CASE IntFld
                        Low$ = "-32768"
                        High$ = "32767"
                     CASE DateFld
                        Low$ = Num2Date$(-29219)
                        High$ = Num2Date$(31368)
                     CASE EuroDateFld
                        Low$ = ENum2Date$(-29219)
                        High$ = ENum2Date$(31368)
                     CASE LongIntFld
                        Low$ = "-2147483648#"
                        High$ = "2147483647#"
                     CASE SngFld
                        Low$ = "-3.402823E+38"
                        High$ = "3.402823E+38"
                     CASE DblFld, MoneyFld
                        Low$ = "-1.79769313486231D+308"
                        High$ = "1.79769313486231D+308"
                  END SELECT
               ELSE
                  IF TypCod = DateFld THEN
                     Low$ = Num2Date$(CINT(Form(FldNo).LowRange))
                     High$ = Num2Date$(CINT(Form(FldNo).HiRange))
                  ELSEIF TypCod = EuroDateFld THEN
                     Low$ = ENum2Date$(CINT(Form(FldNo).LowRange))
                     High$ = ENum2Date$(CINT(Form(FldNo).HiRange))
                  ELSE
                     IF Form(FldNo).LowRange < -1.79769313486231D+308 THEN Form(FldNo).LowRange = -1.79769313486231D+308
                     IF Form(FldNo).HiRange > 1.79769313486231D+308 THEN Form(FldNo).HiRange = 1.79769313486231D+308
                     Low$ = STR$(Form(FldNo).LowRange)
                     High$ = STR$(Form(FldNo).HiRange)
                  END IF
               END IF
                      
               Dl(7).DRow = NextLine
               Dl(7).DCol = 2
               Dl(7).DLen = 23
               Dl(7).DTyp = 2
               Fd$(7, 0) = "Range: "
               Fd$(7, 1) = Low$

               Dl(8).DRow = NextLine
               Dl(8).DCol = 34
               Dl(8).DLen = 23
               Dl(8).DTyp = 2
               Fd$(8, 0) = " .. "
               Fd$(8, 1) = High$
                     
               NextLine = NextLine + 3

               Dl(9).DRow = NextLine
               Dl(9).DCol = 1
               Dl(9).DLen = 64
               Dl(9).DTyp = 1
               Fd$(9, 0) = "Formula: ie. QUANTITY * UNITCOST + 1.3"

               Dl(10).DRow = NextLine + 1
               Dl(10).DCol = 1
               Dl(10).DLen = 64
               Dl(10).DTyp = 1

               Dl(11).DRow = NextLine + 2
               Dl(11).DCol = 1
               Dl(11).DLen = 64
               Dl(11).DTyp = 1

               BreakLines Clc$(FldNo), Fd$(), 9, 64

               NextLine = NextLine + 5

            CASE StrFld, PropStrFld, UCaseStrFld, NumericStrFld, PhoneFld, SoSecFld, ZipFld

               Dl(9).DRow = NextLine
               Dl(9).DCol = 1
               Dl(9).DLen = 64
               Dl(9).DTyp = 1
               Fd$(9, 0) = "Formula: ie. FRSTNAME + LASTNAME"

               Dl(10).DRow = NextLine + 1
               Dl(10).DCol = 1
               Dl(10).DLen = 64
               Dl(10).DTyp = 1

               Dl(11).DRow = NextLine + 2
               Dl(11).DCol = 1
               Dl(11).DLen = 64
               Dl(11).DTyp = 1

               BreakLines Clc$(FldNo), Fd$(), 9, 64

               NextLine = NextLine + 5
            CASE ELSE
         END SELECT



         IF MainType = SequFld THEN
            Dl(7).DRow = NextLine
            Dl(7).DCol = 2
            Dl(7).DLen = 12
            Dl(7).DTyp = 2
            Fd$(7, 0) = "Initial Whole Value: "
            Fd$(7, 1) = STR$(CLNG(Form(FldNo).LowRange))

            Dl(8).DRow = NextLine + 1
            Dl(8).DCol = 38
            Dl(8).DTyp = 6
            Fd$(8, 0) = "-2147483647..2147450880"

            NextLine = NextLine + 3
         END IF


         '----- Set up Help Text
         Dl(12).DRow = NextLine
         Dl(12).DCol = 1
         Dl(12).DLen = 64
         Dl(12).DTyp = 1

         Dl(13).DRow = NextLine + 1
         Dl(13).DCol = 1
         Dl(13).DLen = 64
         Dl(13).DTyp = 1

         Dl(14).DRow = NextLine + 2
         Dl(14).DCol = 1
         Dl(14).DLen = 64
         Dl(14).DTyp = 1

         Fd$(12, 0) = "Help Message:"
         BreakLines Help$(FldNo), Fd$(), 12, 64
         NextLine = NextLine + 4


         '----- Set Relational Message
         IF MainType >= Relational AND MainType <= MultChFld THEN

            Dl(15).DRow = NextLine
            Dl(15).DCol = 2
            Dl(15).DTyp = 6
            Fd$(15, 0) = "Related to Field `" + RTRIM$(RelFld$) + "' in `" + RelName$ + ".FRM'"

            NextLine = NextLine + 1
         END IF


         '----- Set Buttons
         Dl(16).DRow = NextLine
         Dl(16).DCol = 18
         Dl(16).DTyp = 5
         Fd$(16, 0) = "Ok"
                   
         Dl(17).DRow = NextLine
         Dl(17).DCol = 39
         Dl(17).DTyp = 5
         Fd$(17, 0) = "Cancel"

         DO

            LOCATE (Inf.Rows - NextLine - 1) \ 2
            DialogBox "Field #" + STR$(FldNo) + " Settings", Fd$(), Dl(), TypCh$(), ExitCode
                   
            IF ExitCode = 1 THEN
               IF IsDup%(Fd$(1, 1), Form(), FldNo) THEN
                  IF Inf.BeepOn THEN Chime 6
                  ErrMsg "Field name `" + Fd$(1, 1) + "' is already in use."
               ELSE
                  EXIT DO
               END IF
            ELSE
               EXIT DO
            END IF

         LOOP

         '----- SET then New Form Definition
         IF ExitCode = 1 THEN

            Test = Form(FldNo)
            Test.RelFile = ""
            Form(FldNo) = Blank
            OldClc$ = Clc$(FldNo)
            OldHelp$ = Help$(FldNo)

            IF Test.Fields = 0 THEN
               Test.Fields = Form(0).LCol + 1
               Form(0).LCol = Form(0).LCol + 1
            ELSE
               Form(FldNo).Fields = Test.Fields
            END IF

            '----- Set Field Position
            Form(FldNo).Row = BegRow
            Form(FldNo).LCol = BegCol
            Form(FldNo).RCol = EnCol
            IF TypCod = NotesFld THEN Form(FldNo).ScratchI = EnRow

            '----- Set Storage Length
            SELECT CASE TypCod
               CASE IntFld, DateFld, EuroDateFld
                  Form(FldNo).StorLen = 2
               CASE NotesFld, LongIntFld, SngFld, SequFld
                  Form(FldNo).StorLen = 4
               CASE DblFld, MoneyFld
                  Form(FldNo).StorLen = 8
               CASE RecNoFld, TotRecsFld, Button
                  Form(FldNo).StorLen = 0
               CASE ELSE
                  Form(FldNo).StorLen = EnCol - BegCol + 1
            END SELECT

            IF MainType = VirtualFld THEN Form(FldNo).StorLen = 4
            IF MainType = MultChFld THEN Form(FldNo).StorLen = 2
            

            '----- Set Field Type
            Form(FldNo).FType = MainType

            '----- Set Relations
            IF MainType >= 11 AND MainType <= 13 THEN
               Form(FldNo).RelFile = RelName$
               Form(FldNo).RelFld = RelFld
               Form(FldNo).ScratchI = RelType
               Form(FldNo).RelHandle = Test.RelHandle
            END IF

            '----- Set Indexing
            SELECT CASE TempCode
               CASE StrFld, PropStrFld, UCaseStrFld, NumericStrFld, IntFld, LongIntFld, SngFld, DblFld, MoneyFld, DateFld, EuroDateFld, PhoneFld, SoSecFld, ZipFld, Relational, MultChFld, MultChAFld
                  Form(FldNo).Indexed = Fd$(4, 1) = "û"
               CASE Button
                  Form(FldNo).ScratchI = VAL(Fd$(4, 1))
               CASE ELSE
            END SELECT

            '----- Field Name
            Form(FldNo).FldName = UCASE$(Fd$(1, 1))

            '----- Decimal Places
            SELECT CASE TypCod
               CASE SngFld, DblFld, MoneyFld
                  IF Fd$(6, 1) = "û" THEN
                     Form(FldNo).Decimals = -1
                  ELSE
                     Form(FldNo).Decimals = VAL(Fd$(5, 1))
                  END IF
                  IF TypCod = MoneyFld THEN
                     Form(FldNo).ScratchS = Fd$(6, 1)
                     Inf.CrncStr = Fd$(6, 1)
                  END IF
               CASE IntFld, LongIntFld
                  IF Fd$(6, 1) = "û" THEN
                     Form(FldNo).Decimals = -1
                  ELSE
                     Form(FldNo).Decimals = 0
                  END IF
               CASE ELSE
            END SELECT

            '----- Set Ranges
            SELECT CASE TypCod
               CASE IntFld, LongIntFld, SngFld, DblFld, MoneyFld
                  Form(FldNo).LowRange = Eval#(Fd$(7, 1), ErrCode)
                  IF ErrCode THEN Form(FldNo).LowRange = -1.79769313486231D+308
                  Form(FldNo).HiRange = Eval#(Fd$(8, 1), ErrCode)
                  IF ErrCode THEN Form(FldNo).HiRange = 1.79769313486231D+308
               CASE ELSE
            END SELECT

            SELECT CASE TypCod
               CASE IntFld
                  IF Form(FldNo).LowRange < -32768 THEN Form(FldNo).LowRange = -32768
                  IF Form(FldNo).HiRange > 32768 THEN Form(FldNo).HiRange = 32767
               CASE DateFld
                  Form(FldNo).LowRange = MaxInt(Date2Num(Fd$(7, 1)), -29219)
                  Form(FldNo).HiRange = MinInt(Date2Num(Fd$(8, 1)), 31368)
               CASE EuroDateFld
                  Form(FldNo).LowRange = MaxInt(EDate2Num(Fd$(7, 1)), -29219)
                  Form(FldNo).HiRange = MinInt(EDate2Num(Fd$(8, 1)), 31368)
               CASE LongIntFld
                  IF Form(FldNo).LowRange < -2147483648# THEN Form(FldNo).LowRange = -2147483648#
                  IF Form(FldNo).HiRange > 2147483647# THEN Form(FldNo).HiRange = 2147483647#
               CASE SngFld
                  IF Form(FldNo).LowRange < -3.402823E+38 THEN Form(FldNo).LowRange = -3.402823E+38
                  IF Form(FldNo).HiRange > 3.402823E+38 THEN Form(FldNo).HiRange = 3.402823E+38
               CASE ELSE
            END SELECT

            '----- Read Formula
            Clc$(FldNo) = ""
            FOR I = 9 TO 11
               Clc$(FldNo) = Clc$(FldNo) + UCASE$(Fd$(I, 1)) + " "
            NEXT
            Clc$(FldNo) = RTRIM$(Clc$(FldNo))

            '----- Read Help Text
            Help$(FldNo) = ""
            FOR I = 12 TO 13
               IF LEN(Fd$(I, 1)) AND RIGHT$(Fd$(I, 1), 1) <> " " THEN
                  Fd$(I, 1) = Fd$(I, 1) + " "
               END IF
               Help$(FldNo) = Help$(FldNo) + Fd$(I, 1)
            NEXT
            Help$(FldNo) = Help$(FldNo) + Fd$(I, 1)


            '----- Set User Flag data
            IF MainType <> MoneyFld THEN
               Form(FldNo).ScratchS = Fd$(2, 1)
            END IF

            '----- Set Protection
            IF MainType <> MultChFld AND MainType <> MultChAFld THEN
               Form(FldNo).Protected = Fd$(3, 1) = "û"
            END IF

            '----- Sequencing initial value
            IF MainType = SequFld THEN
               Form(FldNo).LowRange = VAL(Fd$(7, 1))
               IF Form(FldNo).LowRange < -2147483647 THEN Form(FldNo).LowRange = -2147483647
               IF Form(FldNo).LowRange > 2147450880 THEN Form(FldNo).LowRange = 2147450880
            END IF

            '----- Set Logical characters
            IF MainType = LogicalFld THEN
               IF Fd$(5, 1) = Chr0$ THEN Fd$(5, 1) = CHR$(32)
               MID$(Form(FldNo).RelFile, 1) = Fd$(5, 1)
               IF Fd$(4, 1) = Chr0$ THEN Fd$(4, 1) = CHR$(32)
               MID$(Form(FldNo).RelFile, 3) = Fd$(4, 1)
            END IF

            Form(FldNo).Fields = Test.Fields

            '----- See if the form has changed
            IF NOT CompareT(Test, Form(FldNo), FrmWdth) OR Clc$(FldNo) <> OldClc$ OR Help$(FldNo) <> OldHelp$ THEN
               Inf.FrmChanged = -1
            END IF

            Form(0).Fields = MaxInt(Form(0).Fields, FldNo)
            FldNo = FldNo + 1
         END IF

         Press = 0
      END IF

      IF Inf.PaintFields THEN
         '----- Poke/Paint the field on the screen buffer
         DEF SEG = VARSEG(ScrBuf(0))
         FOR R = BegRow - 1 TO EnRow - 1
            FOR C = BegCol - 1 TO EnCol - 1
               POKE R * 160 + (C * 2) + 1, Inf.PaintFields
            NEXT
         NEXT
      END IF

      MScrnRest 1, 1, Inf.Rows, 80, ScrBuf(0)

   LOOP UNTIL Press = 1 AND (K$ = CHR$(27) OR K$ = Chr0$ + CHR$(68))

   IF Inf.Ruler THEN RulerLine Inf, 1

EXIT SUB



ShowFldLen:

   EnCol = Inf.CurCol
   IF EnCol < BegCol THEN SWAP EnCol, BegCol
   EnRow = Inf.CurRow
   IF EnRow < BegRow THEN SWAP EnRow, BegRow

   SELECT CASE TypCod
      CASE LogicalFld                          'Logical
         BegCol = EnCol
      CASE DateFld, EuroDateFld                'Date
         BegCol = EnCol - 9
      CASE PhoneFld                            'Phone Number
         BegCol = EnCol - 13
      CASE SoSecFld                            'Social Security #
         BegCol = EnCol - 10
      CASE ZipFld                              'Zip Code
         BegCol = EnCol - 9
      CASE ELSE
   END SELECT

   FLen = EnCol - BegCol + 1
   EnRow = Inf.CurRow
   IF EnRow < BegRow THEN SWAP EnRow, BegRow

   B(0, 0) = BegRow
   B(1, 0) = BegCol
   B(2, 0) = EnRow
   B(3, 0) = EnCol
   UndoBlock B(), ScrBuf()
      
   MPaintBox BegRow, BegCol, EnRow, EnCol, HiClr

   LOCATE , , 1


DialBx:
   IF Inf.CurRow > Inf.Rows - 8 THEN
      P = Inf.CurRow - 8
   ELSE
      P = Inf.CurRow + 2
   END IF

   IF Press < 2 THEN
      IF Press = 0 THEN
         LSET Dial$(1) = " Move the Cursor to the Beginning of Field #"
         MID$(Dial$(1), 45) = STR$(FldNo) + ","

         LSET Dial$(2) = " then Press Enter.  Press [F10] when Finished."
         LSET Dial$(4) = " [Ins] - Insert Field, [Del] - Delete Field."
         Press = 1
      END IF

   ELSEIF Press = 2 THEN
      MID$(Dial$(1), 1) = "   Now move the Cursor to the End"
      MID$(Dial$(2), 19) = "            Field Length =   "
      LSET Dial$(4) = ""
      LSET Dial$(5) = ""
   END IF


   HideCursor
   'IF Inf.Ruler AND PrevRow <> Inf.CurRow THEN RulerLine Inf, 5

   
   ConvexFrame P, 15, P + 6, 66
   FOR N = 1 TO 5
       QPrintRC Dial$(N), P + N, 16, BoxClr
   NEXT
   MPaintBox P + 1, 67, P + N + 1, 68, 8
   MPaintBox P + N + 1, 17, P + N + 1, 66, 8

   'IF Inf.Ruler AND PrevRow <> Inf.CurRow THEN RulerLine Inf, 1
   PrevRow = Inf.CurRow
  
   IF Press = 2 THEN
      LOCATE P + 2, 61, 0
      RSET FLen$ = MID$(STR$(FLen), 2)
      QPrintRC FLen$, P + 2, 61, HiLite
   END IF

   ShowCursor
   
RETURN



END SUB

' *********  Draw a Box
SUB DrawBox STATIC
   SHARED Inf AS GenInfo
   SHARED LN$()                            'Share Line Types
   SHARED TLCor$, TRCor$, BLCor$, BRCor$   'Share Corner Types

   DIM B(3, 1)

   AncRow = Inf.CurRow
   AncCol = Inf.CurCol

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

      GOSUB JustBox

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

   Inf.EdType = 1
   LOCATE Inf.CurRow, Inf.CurCol, 1, 1, 13

   CALL MQPrint(TLCor$, Inf.SysColor)      'Print the Top Left Corner

   K$ = CHR$(1)

   DO
      ScrEdit K$, Inf

      IF QPLen(K$) THEN

         IF K$ = CHR$(13) THEN             'If Enter pressed then Done
            Inf.ScrChanged = -1
            EXIT DO

         ELSEIF K$ = CHR$(27) THEN
            MScrnRest 1, 1, Inf.Rows, 80, ScrBuf(0)
            Inf.CurRow = AncRow
            Inf.CurCol = AncCol
            IF Inf.Ruler THEN RulerLine Inf, 1
            EXIT DO

         ELSE
            GOSUB DoBox

         END IF
       
      END IF

   LOOP
   Inf.EdType = 0

   SetCsr Inf

   ERASE ScrBuf
   Temp$ = ""

   EXIT SUB

DoBox:
   B(0, 0) = MinInt(AncRow, Inf.CurRow)
   B(1, 0) = MinInt(AncCol, Inf.CurCol)
   B(2, 0) = MaxInt(AncRow, Inf.CurRow)
   B(3, 0) = MaxInt(AncCol, Inf.CurCol)

   IF B(0, 0) < B(0, 1) THEN
      MPRestore B(0, 1), B(1, 1), B(0, 1), B(3, 1), 80, ScrBuf((B(0, 1) - 1) * 80 + B(1, 1) - 1)
   END IF

   IF B(1, 0) < B(1, 1) THEN
      MPRestore B(0, 1), B(1, 1), B(2, 1), B(1, 1), 80, ScrBuf((B(0, 1) - 1) * 80 + B(1, 1) - 1)
   END IF

   IF B(2, 0) > B(2, 1) THEN
      MPRestore B(2, 1), B(1, 1), B(2, 1), B(3, 1), 80, ScrBuf((B(2, 1) - 1) * 80 + B(1, 1) - 1)
   END IF

   IF B(3, 0) > B(3, 1) THEN
      MPRestore B(0, 1), B(3, 1), B(2, 1), B(3, 1), 80, ScrBuf((B(0, 1) - 1) * 80 + B(3, 1) - 1)
   END IF

   UndoBlock B(), ScrBuf()
JustBox:
   HideCursor

   IF B(1, 0) < B(3, 0) THEN
      IF B(0, 0) < B(2, 0) THEN
         Temp$ = TLCor$ + STRING$(B(3, 0) - B(1, 0) - 1, LN$(0)) + TRCor$
      ELSE
         Temp$ = STRING$(B(3, 0) - B(1, 0) + 1, LN$(0))
      END IF
      QPrintRC Temp$, B(0, 0), B(1, 0), Inf.SysColor
   END IF

   FOR N = B(0, 0) - (B(1, 0) < B(3, 0)) TO B(2, 0) + (B(1, 0) < B(3, 0))
       QPrintRC LN$(1), N, B(1, 0), Inf.SysColor
       IF B(1, 0) < B(3, 0) THEN
          QPrintRC LN$(3), N, B(3, 0), Inf.SysColor
       END IF
   NEXT

   IF B(1, 0) < B(3, 0) THEN
      IF B(0, 0) < B(2, 0) THEN
         Temp$ = BLCor$ + STRING$(B(3, 0) - B(1, 0) - 1, LN$(2)) + BRCor$
         QPrintRC Temp$, B(2, 0), B(1, 0), Inf.SysColor
      END IF
   END IF

   ShowCursor

RETURN

END SUB

'********** Edit the Current Screen
SUB EdScrn (Inf AS GenInfo) STATIC

   SHARED LN$()                                 'Share Line Types
   SHARED MMenu$(), ChStat()
   SHARED TLCor$, TRCor$, BLCor$, BRCor$        'Share Corner Types
   SHARED ScrnLib(), TopBuf()           'Share the Screen Library
   SHARED Form() AS FieldInfo, Help$(), Clc$()
   SHARED Menu, Choice
   SHARED ScrName$, LibName$, ProgLib$
   SHARED LastKey$, MonoCode

   SetCsr Inf
   ShowCursor

   DO
      ScrEdit Ch$, Inf
     
      IF Inf.EdType = 0 THEN

         AltDown = AltKey
         IF AltDown AND WasDown = 0 THEN
            WasDown = -1
            REDIM TopBuf(80)
            GOSUB SaveTop
            BarPrint MMenu$(), ChStat()
         END IF

         IF WasDown THEN
            Temp = -1
            DO
               IF NOT Temp THEN Ch$ = INKEY$
               Temp = 0

               NewMenu = PullMenKey(Ch$, MMenu$(), ChStat())
               IF NewMenu > -1 AND Ch$ <> Chr0$ + CHR$(4) AND Ch$ <> Chr0$ + CHR$(3) THEN
                  Menu = NewMenu
                  WasDown = 0
                  GOSUB RestTop
                  EXIT SUB
               END IF
            LOOP UNTIL LEN(Ch$)
         
            IF Ch$ <> CHR$(27) THEN
               StuffBuf Ch$
            ELSE
               Ch$ = ""'Chr0$
            END IF
         END IF

         IF WasDown THEN ' AND LEN(Ch$) THEN
            WasDown = 0
            GOSUB RestTop
         END IF

      

         IF LEN(Ch$) THEN

            SCode = ASCII(RIGHT$(Ch$, 1))
            IF LEN(Ch$) = 2 THEN SCode = -SCode
            SELECT CASE SCode

            CASE 1                   'Ctrl+A - ASCII Chart
               LastKey$ = ASCIIChart$(-1, Inf)

            CASE 2                   'Ctrl+B Draw Box Command
               DrawBox               'Draw the Box

            CASE 3
               LOCATE , , 0
               GetColor Inf.Clr, ExitCode
               IF ExitCode = 0 AND Inf.MarkBlock THEN
                  StuffBuf CHR$(10)
                  Inf.ScrChanged = -1'Screen was changed so set flag
               END IF

            CASE 4                   'Ctrl+D Draw Lines
               Sketch LN$(0), Inf

            CASE 6                   'Ctrl+F Fill Character
               FillChar Inf

            CASE 27
               REDIM TopBuf(80)
               EXIT DO

            CASE -59                 '[F1] Help pressed
               GetHelp ProgLib$, MonoCode, Inf

            CASE -60                 '[F2] Display Screen names
               IF ScrnLib(0) THEN
                  ChkScrChanged ScrName$, ScrnLib(), Form(), Help$(), Clc$(), Inf, ExitCode
                  IF ExitCode = 1 OR ExitCode = 2 THEN
                     LibScrMenu SName$, ScrnLib(), Inf
                     IF LEN(SName$) THEN
                        ScrName$ = ParsPath$(LibName$) + SName$ + ".SCR"
                        LoadFldDef ScrName$
                        Inf.EdType = -1
                     END IF
                  END IF
               END IF

            CASE 16                  'Ctrl+P Block Paint
               CurCol = POS(0)       'Save the cursor column
                    
               IF Inf.MarkBlock THEN
                  StuffBuf CHR$(10)
               ELSE
                  BlockPaint Inf
               END IF
               Inf.ScrChanged = -1   'Screen was changed so set flag

            CASE ELSE
            END SELECT
         END IF
      END IF
   LOOP

   GOSUB SaveTop
   EXIT SUB

SaveTop:
   MScrnSave 1, 1, 1, 80, TopBuf(0)
RETURN

RestTop:
   MScrnRest 1, 1, 1, 80, TopBuf(0)
RETURN

END SUB

SUB GetFldType (FldNo, TypCod, ExitCode) STATIC

   SHARED Inf AS GenInfo
   SHARED TypCh$(), TypCh()


   REDIM Dl(6) AS Dialog

   Dl(1).DRow = 2
   Dl(1).DCol = 2
   Dl(1).DLen = 11 + 512
   Dl(1).DTyp = 4

   Dl(2).DRow = 1
   Dl(2).DCol = 4
   Dl(2).DTyp = 6

   Dl(5).DRow = 15
   Dl(5).DCol = 14
   Dl(5).DTyp = 5
           
   Dl(6).DRow = 15
   Dl(6).DCol = 34
   Dl(6).DTyp = 5

   REDIM Fd$(6, 1)

   IF TypCod THEN
      FOR N = 1 TO UBOUND(TypCh)
         IF TypCod = TypCh(N) THEN EXIT FOR
      NEXT
      IF N > UBOUND(TypCh) THEN N = 1
   ELSE
      N = 1
   END IF
   Fd$(1, 1) = STR$(N)

   Fd$(2, 0) = "Select a field type for field number:" + STR$(FldNo)
   'Fd$(4, 0) = "(*) Available in QBase only."

   Fd$(5, 0) = "Ok"
   Fd$(6, 0) = "Cancel"

   LOCATE Inf.Rows \ 2 - 7
   DialogBox "Field Types", Fd$(), Dl(), TypCh$(), ExitCode

   IF ExitCode = 1 THEN
      TypCod = TypCh(VAL(Fd$(1, 1)))
   END IF
   ERASE Dl, Fd$

END SUB

SUB GetRelation (Fld, Form() AS FieldInfo, RelName$, RelFld$, RelFld, RelType, Decimals, ExitCode)

   SHARED ScrName$
   SHARED Inf AS GenInfo
   REDIM Relat(0) AS FieldInfo

   Byte$ = " "
   BHeader$ = CHR$(253)

   ExitCode = 0

   REDIM Dl(4) AS Dialog
   REDIM Fd$(5, 4)

   RName$ = RTRIM$(Form(Fld).RelFile) + ".FRM"
   Path$ = ParsPath$(ScrName$)
   FileSpec$ = Path$ + "*.FRM"                  'Get the names of Form Files

   NFiles = FCount%(FileSpec$)

   REDIM Temp$(NFiles + 1)

   FOR X = 1 TO NFiles
      Temp$(X) = SPACE$(12)
   NEXT

   Temp$(0) = FileSpec$
   CALL ReadFile(BYVAL VARPTR(Temp$(0)))

   OurName$ = UCASE$(BaseName$(ScrName$)) + ".FRM"
   N = NFiles
   FOR X = 1 TO NFiles
      IF INSTR(Temp$(X), OurName$) THEN N = N - 1
   NEXT

   REDIM FileNames$(N)
   
   N = 0
   FOR X = 1 TO NFiles
      IF INSTR(Temp$(X), OurName$) = 0 THEN
         N = N + 1
         FileNames$(N) = Temp$(X)
      END IF
   NEXT
   ERASE Temp$
   NFiles = N

   IF NFiles = 0 THEN
      IF Inf.BeepOn THEN Chime 6
      ErrMsg "No files for a relation."
      EXIT SUB
   END IF

   CALL SortStr(BYVAL VARPTR(FileNames$(1)), NFiles, 0)

   Choice = 1
   FOR N = 1 TO NFiles
      IF INSTR(FileNames$(N), RName$) THEN Choice = N
   NEXT N
   FileNames$(0) = ""

   DispNo = 10'MinInt(10, NFiles)
   Dl(1).DRow = 2
   Dl(1).DCol = 2
   Dl(1).DLen = 3 * 256 + DispNo
   Dl(1).DTyp = 4
      
   Fd$(1, 1) = STR$(Choice)
      

   Dl(2).DRow = 1
   Dl(2).DCol = 2
   Dl(2).DTyp = 6

   Fd$(2, 0) = "Select the file for relation."


   Dl(3).DRow = DispNo + 4
   Dl(3).DCol = 9
   Dl(3).DTyp = 5
          
   Dl(4).DRow = DispNo + 4
   Dl(4).DCol = 25
   Dl(4).DTyp = 5
      
   Fd$(3, 0) = "Ok"
   Fd$(4, 0) = "Cancel"


   LOCATE (Inf.Rows - DispNo - 4) \ 2
   DialogBox "FILES", Fd$(), Dl(), FileNames$(), ExitCode

   IF ExitCode <> 1 THEN
      ERASE FileNames$, Fd$, Dl
      EXIT SUB
   END IF


   '----- Get the field name, type and decimals
   RName$ = Path$ + FileNames$(VAL(Fd$(1, 1)))

   ERASE FileNames$

      
   GOSUB LoadFormDef                    'Load the Relations Form Definition

   REDIM FileNames$(Relat(0).Fields)            'Get the Field Names from
   FOR N = 1 TO Relat(0).Fields                 'Relation
      FileNames$(N) = Relat(N).FldName
   NEXT
       
   DispNo = MinInt(10, Relat(0).Fields)
  
   Dl(1).DLen = 4 * 256 + DispNo
   Fd$(1, 1) = STR$(MaxInt(1, Form(Fld).RelFld))
     
   Fd$(2, 0) = "Select the field for relation."
      
   Dl(3).DRow = DispNo + 4
   Dl(4).DRow = DispNo + 4
      

   LOCATE (Inf.Rows - DispNo - 4) \ 2
   DialogBox "FIELDS", Fd$(), Dl(), FileNames$(), ExitCode

  
   RelName$ = BaseName$(RName$)
   RelFld = VAL(Fd$(1, 1))
   RelFld$ = FileNames$(RelFld)
   RelType = Relat(RelFld).FType

   DO WHILE RelType >= Relational AND RelType <= MultChFld
     
      RName$ = Path$ + Relat(RelFld).RelFile + ".FRM"
      RelFld = Relat(RelFld).RelFld

      GOSUB LoadFormDef
      
      RelType = Relat(RelFld).FType

   LOOP

   IF RelType = NotesFld THEN
      ExitCode = 0
      IF Inf.BeepOn THEN Chime 6
      ErrMsg "Can't relate to Multi Line Text fields."
   END IF

   Decimals = Relat(RelFld).Decimals

   ERASE Relat, FileNames$, Fd$, Dl

   RName$ = ""
   Path$ = ""

   EXIT SUB



LoadFormDef:

   DO                                   'Load the Relations Form Definition
      FOpen RName$, Handle

      IF DosError THEN
         IF ErrorPrompt = 0 THEN
            ERASE FileNames$, Fd$, Dl
            RName$ = ""
            ExitCode = 0
            EXIT SUB
         END IF
      END IF
   LOOP WHILE DosError
     
   FGet Handle, Byte$
   FSeek Handle, 0&

   NumBytes& = FLof&(Handle)

   IF Byte$ = BHeader$ THEN
      FClose Handle
      REDIM Relat(CINT((NumBytes& - 7) \ 40)) AS FieldInfo
      CvtFrm RName$, Relat()
   ELSE
      REDIM Relat(CINT(NumBytes& \ FrmWdth) + 1) AS FieldInfo
      FGetA Handle, Relat(0), NumBytes&
      FClose Handle
   END IF

RETURN

END SUB

'****** Load Field Definition information
SUB LoadFldDef (ScrName$)

   SHARED Form() AS FieldInfo
   SHARED Clc$(), Help$()

   Byte$ = " "
   BHeader$ = CHR$(253)

   Nam$ = NoExtension$(ScrName$)

   REDIM Form(MaxFields) AS FieldInfo
                      
   IF Exist(Nam$ + ".NEW") THEN         'See if there is a Form File on Disk
      FrmName$ = Nam$ + ".NEW"
   ELSEIF Exist(Nam$ + ".FRM") THEN
      FrmName$ = Nam$ + ".FRM"
   ELSE
      EXIT SUB
   END IF

   DO
      FOpen FrmName$, Handle

      IF DosError THEN
         IF ErrorPrompt = 0 THEN EXIT SUB
      END IF
     
   LOOP WHILE DosError

   FGet Handle, Byte$
   FSeek Handle, 0&

   REDIM Clc$(MaxFields), Help$(MaxFields)

   IF Byte$ = BHeader$ THEN
      FClose Handle
      CvtFrm FrmName$, Form()
   ELSE
      FGetT Handle, NumFlds, 2
      NumBytes& = (NumFlds + 1) * FrmWdth
      FSeek Handle, 0&
      FGetA Handle, Form(0), NumBytes&

      FGet Handle, Byte$

      Buffer$ = SPACE$(194)
      DO UNTIL DosError
         IF Byte$ = "H" THEN
            FOR N = 1 TO NumFlds
               Help$(N) = FLInput$(Handle, Buffer$)
               IF DosError THEN EXIT DO
            NEXT
         ELSE
            FOR N = 1 TO NumFlds
               Clc$(N) = FLInput$(Handle, Buffer$)
               IF DosError THEN EXIT DO
            NEXT
         END IF

         FGet Handle, Byte$
      LOOP
      Buffer$ = ""
      FClose Handle
   END IF

END SUB

'
SUB ScrEdit (K$, Inf AS GenInfo) STATIC

   SHARED PntScr()
   SHARED LastKey$, ShiftFake, DosVersion
   SHARED Form() AS FieldInfo
   SHARED Clc$(), Help$()
   SHARED Blank AS FieldInfo

   DIM B(3, 1)

   IF LR = 0 THEN
      LR = 1
      LC = 1
      RC = 80
   END IF
   RR = Inf.Rows

   IF Inf.EdType = -1 THEN

      Inf.CurRow = 1
      Inf.CurCol = 1
      Inf.EdType = 0
     
      IF Inf.Ruler THEN RulerLine Inf, 0

   END IF


   IF QPLen(K$) OR Presses THEN
      IF Inf.EdType > 0 AND Inf.Ruler THEN RulerLine Inf, 1
   END IF

   CurRowWas = Inf.CurRow
   CurColWas = Inf.CurCol


   IF Inf.EdType = 0 THEN
      '----- If in block marking mode, paint the marked block
      IF Inf.MarkBlock AND QPLen(K$) THEN

         IF Inf.Ruler THEN RulerLine Inf, 5

         '----- If block marking key was just pressed
         IF FrstBlkKey THEN
            FrstBlkKey = 0                    'Reset first key flag
           
            REDIM ScrBuf(Inf.Rows * 80)
            GOSUB SaveScrn

            B(0, 0) = AncRow
            B(0, 1) = AncRow
            B(1, 0) = AncCol
            B(1, 1) = AncCol
            B(2, 0) = AncRow
            B(2, 1) = AncRow
            B(3, 0) = AncCol
            B(3, 1) = AncCol
         END IF

         B(0, 0) = MinInt(AncRow, Inf.CurRow)
         B(1, 0) = MinInt(AncCol, Inf.CurCol)
         B(2, 0) = MaxInt(AncRow, Inf.CurRow)
         B(3, 0) = MaxInt(AncCol, Inf.CurCol)

         UndoBlock B(), ScrBuf()

         '----- Paint the Block
         MPaintBox B(0, 0), B(1, 0), B(2, 0), B(3, 0), Clr

         IF Inf.Ruler THEN RulerLine Inf, 1

         Inf.BUlRow = B(0, 0)
         Inf.BUlCol = B(1, 0)
         Inf.BBrRow = B(2, 0)
         Inf.BBrCol = B(3, 0)

      END IF
   END IF



   LOCATE Inf.CurRow, Inf.CurCol, 1

   IF PrLastKey THEN
      K$ = LastKey$
      PrLastKey = 0
   ELSE
      K$ = INKEY$
   END IF



   ButtonPress 1, Temp, Presses, X, Y
       
   '----- If User pressed a button
   IF Presses OR Temp = 1 THEN
      IF Presses = 0 THEN GetCursor X, Y, Temp

      IF Inf.Rows = 50 AND DosVersion >= 1000 THEN
         Inf.CurRow = (Y \ 7) + 1
      ELSE
         Inf.CurRow = (Y \ 8) + 1
      END IF
      Inf.CurCol = (X \ 8) + 1

          
      IF Presses = 0 THEN
         IF Inf.CurRow <> MRow OR Inf.CurCol <> MCol THEN
            IF NOT Inf.MarkBlock AND Inf.EdType = 0 THEN
               FrstBlkKey = -1           'Set first key flag

               AncRow = MRow
               AncCol = MCol

               HideCursor
               ClrWas = SCREEN(AncRow, AncCol, 1)
               ShowCursor
               IF ClrWas = BoxClr THEN
                  Clr = HiLite
               ELSE
                  Clr = BoxClr
               END IF

               Inf.MarkBlock = -1            'Set block marking flag
               Inf.WasMarked = -1
            END IF

            K$ = Chr0$ + CHR$(3)
         END IF
      ELSE
         IF TIMER < PressTime! + .5 THEN
            K$ = CHR$(13)
         ELSE
            K$ = Chr0$ + CHR$(4)
         END IF
         PressTime! = TIMER
      END IF
      MRow = Inf.CurRow
      MCol = Inf.CurCol
   ELSE
      
      GetCursor X, Y, Temp
      IF Temp AND 2 THEN
         K$ = CHR$(27)

         DO
            GetCursor X, Y, Temp
         LOOP WHILE Temp AND 2

         ButtonPress 2, Temp, Press2, X, Y

      END IF

   END IF
       
   IF ShiftKey THEN ShiftFake = -1

   IF Inf.EdType = 0 AND QPLen(K$) > 0 THEN

      '----- If they pressed a shift cursor key,
      IF ShiftFake AND ((QPLen(K$) = 1 AND INSTR(NumPad$, K$)) OR (QPLen(K$) = 2 AND INSTR(NumPad2$, RIGHT$(K$, 1)))) THEN
         IF NOT Inf.MarkBlock THEN              'If it's a new block
            FrstBlkKey = -1                     'Set first key flag

            AncRow = Inf.CurRow
            AncCol = Inf.CurCol

            HideCursor

            ClrWas = SCREEN(AncRow, AncCol, 1)
            ShowCursor
            IF ClrWas = BoxClr THEN
               Clr = HiLite
            ELSE
               Clr = BoxClr
            END IF

            Inf.MarkBlock = -1                  'Set block marking flag
            Inf.WasMarked = -1
         END IF

         '----- Reset the key codes to their un-shifted states for
         '      normal key processing
         SELECT CASE ASCII(K$)
            CASE 49                           '1/End Key
               K$ = Chr0$ + CHR$(79)
            CASE 50                           '2/Down Key
               K$ = Chr0$ + CHR$(80)
            CASE 51                           '3/PgDn Key
               K$ = Chr0$ + CHR$(81)
            CASE 52                           '4/Left Key
               K$ = Chr0$ + CHR$(75)
            CASE 54                           '6/Right Key
               K$ = Chr0$ + CHR$(77)
            CASE 55                           '7/Home Key
               K$ = Chr0$ + CHR$(71)
            CASE 56                           '8/Up Key
               K$ = Chr0$ + CHR$(72)
            CASE 57                           '9/PgUp Key
               K$ = Chr0$ + CHR$(73)
            CASE ELSE
         END SELECT

      '----- If a block is marked and they didn't press a shifted cursor
      '----- key or they pressed "Ctrl Y", capture the Block to the
      '----- Clipboard array
      ELSEIF Inf.MarkBlock THEN

         SELECT CASE ASC(K$)
            CASE 27, 16, 3, 2, 6
               EXIT SUB
            CASE ELSE
         END SELECT
         
                  'Mouse key                F4 - Ruler line
         IF K$ <> Chr0$ + CHR$(3) AND K$ <> Chr0$ + CHR$(62) THEN

            IF Inf.Ruler THEN RulerLine Inf, 5
              
                     'Delete key
            IF K$ <> Chr0$ + CHR$(83) OR ShiftFake THEN
               GOSUB RestScrn

               IF K$ <> CHR$(1) AND K$ <> CHR$(10) THEN
                  Size = (Inf.BBrRow - Inf.BUlRow + 1) * (Inf.BBrCol - Inf.BUlCol + 1)
                  REDIM BlockM(Size)
                  BlockM(0) = (Inf.BBrCol - Inf.BUlCol + 1) * 256 + (Inf.BBrRow - Inf.BUlRow + 1)
                  MScrnSave Inf.BUlRow, Inf.BUlCol, Inf.BBrRow, Inf.BBrCol, BlockM(1)

                  Temp = 0
                  FOR N = 1 TO Form(0).Fields
                     IF InRange%(Form(N).Row, Form(N).LCol, Inf.BUlRow, Inf.BUlCol, Inf.BBrRow, Inf.BBrCol) THEN
                        Temp = Temp + 1
                     END IF
                  NEXT
                  REDIM FormBuf(Temp) AS FieldInfo
                  REDIM ClcBuf$(Temp)
                  REDIM HelpBuf$(Temp)
                  Temp = 0
                  FOR N = 1 TO Form(0).Fields
                     IF InRange%(Form(N).Row, Form(N).LCol, Inf.BUlRow, Inf.BUlCol, Inf.BBrRow, Inf.BBrCol) THEN
                        Temp = Temp + 1
                        FormBuf(Temp) = Form(N)
                        ClcBuf$(Temp) = Clc$(N)
                        HelpBuf$(Temp) = Help$(N)
                     END IF
                  NEXT

                  Ok2Paste = -1
               END IF
            END IF
           
                'Delete key                     'Delete key          'Ctrl M
            IF (ShiftFake AND K$ = ".") OR K$ = Chr0$ + CHR$(83) OR (K$ = CHR$(13) AND (CtrlKey OR ShiftFake)) OR K$ = Chr0$ + CHR$(63) THEN

               CRow = Inf.BUlRow
               CCol = Inf.BUlCol

               IF Inf.BUlCol > 1 THEN
                  CCol = Inf.BUlCol - 1
               ELSEIF Inf.BUlRow > 1 THEN
                  CRow = Inf.BUlRow - 1
               ELSEIF Inf.BBrCol < 80 THEN
                  CRow = Inf.BBrRow
                  CCol = Inf.BBrCol + 1
               ELSEIF Inf.BBrRow < RR THEN
                  CRow = Inf.BBrRow + 1
                  CCol = Inf.BBrCol
               END IF

               IF CRow = Inf.BUlRow AND CCol = Inf.BUlCol THEN
                  CellAttr = 256 * 7 + 32
               ELSE
                  HideCursor
                  CellAttr = 256 * SCREEN(CRow, CCol, 1) + 32
                  ShowCursor
               END IF

               FOR R = Inf.BUlRow TO Inf.BBrRow
                  ROff = (R - 1) * 80
                  FOR C = Inf.BUlCol TO Inf.BBrCol
                     ScrBuf(ROff + C - 1) = CellAttr
                  NEXT
               NEXT

               '----- Delete fields within range
               N = 1
               DO WHILE N <= Form(0).Fields
                  IF InRange%(Form(N).Row, Form(N).LCol, Inf.BUlRow, Inf.BUlCol, Inf.BBrRow, Inf.BBrCol) THEN
                     FOR I = N TO MaxFields - 1
                        Form(I) = Form(I + 1)
                        Clc$(I) = Clc$(I + 1)
                        Help$(I) = Help$(I + 1)
                     NEXT
                     Form(MaxFields) = Blank
                     Clc$(MaxFields) = ""
                     Help$(MaxFields) = ""
                     Form(0).Fields = MaxInt(Form(0).Fields - 1, 0)
                     Inf.FrmChanged = -1
                  ELSE
                     N = N + 1
                  END IF
               LOOP
            END IF

                    'Delete key          'Not from menu
            IF K$ = Chr0$ + CHR$(83) AND NOT ShiftFake THEN
               Inf.BUlRow = BUlRWas
               Inf.BUlCol = BUlCWas
               Inf.BBrRow = BBrRWas
               Inf.BBrCol = BBrCWas
            ELSE
               BUlRWas = Inf.BUlRow
               BUlCWas = Inf.BUlCol
               BBrRWas = Inf.BBrRow
               BBrCWas = Inf.BBrCol
            END IF

                'Delete key from menu           'Delete key
            IF (ShiftFake AND K$ = ".") OR K$ = Chr0$ + CHR$(83) THEN
               GOSUB RestScrn
               K$ = ""
               Inf.ScrChanged = -1
            END IF
       
            Inf.MarkBlock = 0
                'Ctrl M                                           F5 - Center block
            IF (K$ = CHR$(13) AND (CtrlKey OR ShiftFake)) OR K$ = Chr0$ + CHR$(63) THEN
               MoveOk = -1
               Inf.CurRow = AncRow
               Inf.CurCol = AncCol
            ELSE
               MoveOk = 0
            END IF

            IF Inf.Ruler THEN RulerLine Inf, 1

         END IF


      ELSEIF ShiftFake AND (K$ = "0" OR K$ = Chr0$ + CHR$(82)) THEN

         IF Ok2Paste THEN
            IF Inf.Ruler THEN RulerLine Inf, 5

            REDIM ScrBuf(Inf.Rows * 80)
            GOSUB SaveScrn

            IF Inf.Ruler THEN RulerLine Inf, 1

            Inf.EdType = 2
            Pasting = -1
            CrT = 1
            LOCATE , , 1, 1, 13
         END IF

      END IF

   END IF



   
   IF LEN(K$) = 1 THEN

      SELECT CASE ASCII(K$)

         CASE 32 TO 254                        'Text keys
            IF Inf.EdType = 0 THEN
               HideCursor

               IF Inf.Ins THEN
                  Temp = EndOfField(Inf)
                  IF Temp > Inf.CurCol THEN
                     ScrollR Inf.CurRow, Inf.CurCol, Inf.CurRow, Temp, 1, 0
                  END IF
               END IF
               QPrintRC K$, Inf.CurRow, Inf.CurCol, Inf.SysColor
               Inf.CurCol = MinInt(Inf.CurCol + 1, RC)
               ShowCursor
               LastKey$ = K$
               Inf.ScrChanged = -1
            END IF

         CASE IS = 8 AND Inf.CurCol > LC        'Back Space
            IF Inf.EdType = 0 AND Inf.CurCol > LC THEN
               HideCursor
               IF Inf.Ins THEN
                  Temp = EndOfField(Inf)
                  IF Temp > Inf.CurCol THEN
                     ScrollL Inf.CurRow, Inf.CurCol - 1, Inf.CurRow, Temp, 1, 0
                  END IF
               ELSE
                  QPrintRC " ", Inf.CurRow, Inf.CurCol - 1, Inf.SysColor
               END IF
               Inf.CurCol = Inf.CurCol - 1
               
               ShowCursor
               Inf.ScrChanged = -1
            END IF
                                                 
         CASE IS = 18 AND Inf.EdType = 0        'Ctrl+R - Repeat last key
            IF LastKey$ < " " THEN
               MQPrint LastKey$, Inf.SysColor
               Inf.CurCol = Inf.CurCol + 1
               IF Inf.CurCol > RC THEN
                  Inf.CurCol = LC
                  Inf.CurRow = MinInt(Inf.CurRow + 1, RR)
               END IF
            ELSE
               IF LEN(LastKey$) THEN PrLastKey = -1
               Inf.ScrChanged = -1
            END IF

         CASE IS = 13 AND (CtrlKey OR ShiftFake) 'Ctrl+M - Move block
            IF MoveOk THEN
               Inf.EdType = 2
               CrT = 1
               LOCATE , , 1, 1, 13
               IF Inf.Ruler THEN
                  RulerLine Inf, 5
                  RulerLine Inf, 1
               END IF
            END IF



         CASE IS = 25 AND Inf.EdType = 0       'Ctrl Y - Delete line
            REDIM BlockM(80)
            Ok2Paste = -1

            Inf.BUlRow = Inf.CurRow
            Inf.BUlCol = LC
            Inf.BBrRow = Inf.CurRow
            Inf.BBrCol = RC
            BlockM(0) = 80 * 256 + 1

            IF Inf.Ruler THEN RulerLine Inf, 5

            MScrnSave Inf.BUlRow, Inf.BUlCol, Inf.BBrRow, Inf.BBrCol, BlockM(1)

            MPaintBox Inf.CurRow, LC, Inf.CurRow, RC, BoxClr
            Pause 1
            HideCursor
            ScrollU Inf.CurRow, LC, Inf.Rows, RC, 1, 0

            LOCATE Inf.Rows, LC
            MQPrint SPACE$(RC - LC + 1), 7

            IF Inf.Ruler THEN RulerLine Inf, 1

            FOR N = 1 TO Form(0).Fields
                IF Form(N).Row > Inf.CurRow THEN
                   Form(N).Row = Form(N).Row - 1
                   IF Form(N).FType = NotesFld THEN
                      Form(N).ScratchI = Form(N).ScratchI - 1
                   END IF
                   Inf.FrmChanged = -1
                END IF
            NEXT
            ShowCursor
            Inf.ScrChanged = -1


         CASE IS = 14 AND Inf.EdType = 0       'Ctrl N - Insert Line
            HideCursor
            IF Inf.Ruler THEN RulerLine Inf, 5

            ScrollD Inf.CurRow, LC, Inf.Rows, RC, 1, 0
            IF Inf.CurRow = 1 THEN
               ClrWas = 7
            ELSE
               ClrWas = SCREEN(Inf.CurRow, LC, 1)
            END IF
            LOCATE Inf.CurRow, LC
            MQPrint SPACE$(RC - LC + 1), ClrWas

            IF Inf.Ruler THEN RulerLine Inf, 1

            FOR N = 1 TO Form(0).Fields
                IF Form(N).Row >= Inf.CurRow THEN
                   Form(N).Row = Form(N).Row + 1
                   IF Form(N).FType = NotesFld THEN
                      Form(N).ScratchI = Form(N).ScratchI + 1
                   END IF
                   Inf.FrmChanged = -1
                END IF
            NEXT
            ShowCursor
            Inf.ScrChanged = -1

         CASE 27
            IF Inf.EdType > 1 THEN
               GOSUB RestScrn
               SetCsr Inf
               K$ = ""
               Inf.CurRow = AncRow
               Inf.CurCol = AncCol
               IF Inf.EdType = 2 THEN
                  Inf.CurRow = Inf.BUlRow
                  Inf.CurCol = Inf.BUlCol
                  IF NOT Pasting THEN
                     IF UBOUND(BlockM) >= ((Inf.BBrRow - Inf.BUlRow + 1) * (Inf.BBrCol - Inf.BUlCol + 1)) \ 2 THEN
                        GOSUB PutBlock
                     END IF
                  END IF
               END IF
               Inf.EdType = 0
               MoveOk = 0
               IF Inf.Ruler THEN RulerLine Inf, 1
            END IF

         CASE 13                               'Enter
            IF Inf.EdType > 0 THEN
               SetCsr Inf
               CurRowWas = 0
               IF Inf.EdType > 1 THEN

                  IF Pasting OR MoveOk THEN
                     FOR B = 1 TO UBOUND(FormBuf)
                           
                        NRow = Inf.CurRow + FormBuf(B).Row - Inf.BUlRow
                        NLCol = Inf.CurCol + FormBuf(B).LCol - Inf.BUlCol
                        NRCol = Inf.CurCol + FormBuf(B).RCol - Inf.BUlCol

                        FOR N = 1 TO Form(0).Fields
                           SELECT CASE SGN(Form(N).Row - NRow)
                              CASE 1
                                 EXIT FOR
                              CASE 0
                                 IF Form(N).LCol > NLCol THEN EXIT FOR
                              CASE ELSE
                           END SELECT
                        NEXT

                        IF Form(0).Fields < MaxFields THEN
                           FOR I = MaxFields TO N + 1 STEP -1
                              Form(I) = Form(I - 1)
                              Clc$(I) = Clc$(I - 1)
                              Help$(I) = Help$(I - 1)
                           NEXT
                           Inf.FrmChanged = -1
                           Form(0).Fields = Form(0).Fields + 1

                           Form(N) = FormBuf(B)
                           Form(N).Row = NRow
                           Form(N).LCol = NLCol
                           Form(N).RCol = NRCol
                           IF Form(N).FType = NotesFld THEN
                              Form(N).ScratchI = Inf.CurRow + FormBuf(B).ScratchI - Inf.BUlRow
                           END IF
                           Clc$(N) = ClcBuf$(B)
                           Help$(N) = HelpBuf$(B)

                           MakeUniqueFldName N, Form()

                        ELSE
                           IF Inf.BeepOn THEN Chime 6
                           ErrMsg "Too many fields where created durring copy!"
                           EXIT FOR
                        END IF
                     NEXT
                     IF MoveOk THEN MoveOk = 0
                     IF Pasting THEN Pasting = 0
                  END IF
                  Inf.ScrChanged = -1
               END IF
               Inf.EdType = 0
               
            ELSE
               GOSUB GoHome
               Inf.CurRow = MinInt(Inf.CurRow + 1, Inf.Rows)
            END IF

         CASE IS = 9 AND Inf.CurCol < RC       'If Tab Key pressed
            ColWas = Inf.CurCol
            Inf.CurCol = ((Inf.CurCol \ 8) + 1) * 8 'Find next Tab Position
            IF Inf.Ins THEN
               ScrollR Inf.CurRow, ColWas, Inf.CurRow, 80, Inf.CurCol - ColWas, 0
               Inf.ScrChanged = -1
            END IF

         CASE 10
            IF Inf.Ruler THEN RulerLine Inf, 5
            MPaintBox Inf.BUlRow, Inf.BUlCol, Inf.BBrRow, Inf.BBrCol, Inf.Clr
            Inf.ScrChanged = -1
            IF Inf.Ruler THEN RulerLine Inf, 1

         CASE ELSE
      END SELECT

   ELSEIF LEN(K$) = 2 THEN
      Ky = ASCII(RIGHT$(K$, 1))                'Get the ASCII value of the
      SELECT CASE Ky                           'Extended Code
                                               
         CASE IS = 80 AND Inf.CurRow < RR      'Down Arrow
            Inf.CurRow = Inf.CurRow + 1

         CASE IS = 77 AND Inf.CurCol < RC      'Right Arrow
            Inf.CurCol = Inf.CurCol + 1

         CASE IS = 72 AND Inf.CurRow > LR      'Up Arrow
            Inf.CurRow = Inf.CurRow - 1

         CASE IS = 75 AND Inf.CurCol > LC      'Left Arrow
            Inf.CurCol = Inf.CurCol - 1

         CASE 15                               'If Shift Tab pressed
            ColWas = Inf.CurCol
            IF Inf.CurCol > LC + 7 THEN        'See if past 8
               IF Inf.CurCol MOD 8 = 0 THEN    'If on a tab stop then
                  Inf.CurCol = Inf.CurCol - 1  'back up a space.
               END IF                          'Find previous Tab Position
               Inf.CurCol = (Inf.CurCol \ 8) * 8
            ELSE
               Inf.CurCol = LC                 'If past 8 then make it 1
            END IF
            IF Inf.Ins THEN
               ScrollL Inf.CurRow, Inf.CurCol, Inf.CurRow, RC, ColWas - Inf.CurCol, 0
               Inf.ScrChanged = -1
            END IF

         CASE 79                               'End key
            Look$ = " "
            FOR N = RC TO LC STEP -1
               CALL ReadScrn0(Inf.CurRow, N, Look$)
               IF Look$ <> " " THEN EXIT FOR
            NEXT
            IF N = RC OR N = LC - 1 OR N + 1 = Inf.CurCol THEN N = RC - 1
            Inf.CurCol = N + 1

         CASE 71                               'Home key
            GOSUB GoHome

         CASE 73                               'PgUp key
            Inf.CurRow = LR

         CASE 81                               'PgDn key
            Inf.CurRow = RR

         CASE IS = 82 AND Inf.EdType = 0       'Insert
            IF Inf.EdType = 0 THEN
               Inf.Ins = NOT Inf.Ins
               SetCsr Inf
            END IF

         CASE IS = 83 AND Inf.EdType = 0       'Delete
            Temp = EndOfField(Inf)
            IF Temp > Inf.CurCol THEN
               ScrollL Inf.CurRow, Inf.CurCol, Inf.CurRow, Temp, 1, 0
            ELSE
               MQPrint " ", -1
            END IF
            Inf.ScrChanged = -1

         CASE 63                               '[F5] Center Text
            IF Inf.EdType = 0 AND NOT MoveOk THEN
               TextLine$ = SPACE$(RC - LC + 1)
               CALL ReadScrn0(Inf.CurRow, LC, TextLine$)
               Text$ = LTRIM$(RTRIM$(TextLine$))
               ColWas = INSTR(TextLine$, Text$)

               REDIM Ctr(80)
               MScrnSave Inf.CurRow, ColWas, Inf.CurRow, ColWas + LEN(Text$) - 1, Ctr(0)
               HideCursor
               ClrWas = SCREEN(Inf.CurRow, LC, 1)
               ShowCursor
               LOCATE , ColWas
               MQPrint SPACE$(LEN(Text$)), ClrWas

               CCol = ((RC - LC + 1) \ 2) - (LEN(Text$) \ 2) - (LEN(Text$) MOD 2) + 1
               MScrnRest Inf.CurRow, CCol, Inf.CurRow, CCol + LEN(Text$) - 1, Ctr(0)

               Inf.CurCol = MinInt(MaxInt(Inf.CurCol + CCol - ColWas, LC), RC)
               Text$ = "": TextLine$ = ""
               ERASE Ctr
            ELSE
               Lnth = B(3, 0) - B(1, 0) + 1
               Inf.CurCol = ((RC - LC + 1) \ 2) - (Lnth \ 2) - (Lnth MOD 2) + 1
               IF MoveOk THEN
                  Inf.EdType = 2
                  CrT = 1
                  LOCATE , , 1, 1, 13
                  IF Inf.Ruler THEN
                     RulerLine Inf, 5
                     RulerLine Inf, 1
                  END IF
               END IF
            END IF
            Inf.ScrChanged = -1

         CASE IS = 115 AND Inf.CurCol > LC     'Ctrl Left arrow
            IF ShiftKey THEN
               Temp$ = " "
               X = Inf.CurCol - 1
               CALL ReadScrn0(Inf.CurRow, X, Temp$)

               IF ASC(Temp$) = 32 THEN
                  FOR X = Inf.CurCol - 2 TO LC STEP -1
                     CALL ReadScrn0(Inf.CurRow, X, Temp$)
                     IF ASC(Temp$) <> 32 THEN EXIT FOR
                  NEXT
               END IF

               IF X >= LC THEN
                  Inf.CurCol = X
                  FOR X = Inf.CurCol TO LC STEP -1
                     CALL ReadScrn0(Inf.CurRow, X, Temp$)
                     IF ASC(Temp$) = 32 THEN EXIT FOR
                  NEXT
                  IF X >= LC THEN Inf.CurCol = X
               END IF
            ELSE
               IF Inf.EdType = 0 AND UnPaintOk THEN
                  Inf.CurCol = Inf.CurCol - 1
                  MScrnRest Inf.CurRow, Inf.CurCol, Inf.CurRow, Inf.CurCol, PntScr(Inf.CurCol)
                  Inf.ScrChanged = -1
               ELSE
                  Ky = 0
               END IF
            END IF

         CASE IS = 116 AND Inf.CurCol < RC     'Ctrl Right arrow
            IF ShiftKey THEN
               Temp$ = " "
               X = Inf.CurCol + 1
               CALL ReadScrn0(Inf.CurRow, X, Temp$)
               IF ASC(Temp$) = 32 THEN
                  FOR X = Inf.CurCol + 2 TO RC
                      CALL ReadScrn0(Inf.CurRow, X, Temp$)
                      IF ASC(Temp$) <> 32 THEN EXIT FOR
                  NEXT
               END IF
               IF X <= RC THEN
                  Inf.CurCol = X
                  FOR X = Inf.CurCol TO RC
                     CALL ReadScrn0(Inf.CurRow, X, Temp$)
                     IF ASC(Temp$) = 32 THEN EXIT FOR
                  NEXT
                  IF X <= RC THEN Inf.CurCol = X
               END IF
            ELSE
               IF Inf.EdType = 0 THEN
                  IF KyWas <> 116 AND KyWas <> 115 THEN
                     REDIM PntScr(80)
                     MScrnSave Inf.CurRow, 1, Inf.CurRow, 80, PntScr(1)
                  END IF
                  MPaintBox Inf.CurRow, Inf.CurCol, Inf.CurRow, Inf.CurCol, Inf.Clr
                  Inf.CurCol = Inf.CurCol + 1
                  Inf.ScrChanged = -1
                  UnPaintOk = -1
               END IF
            END IF
        
         CASE 62
            Inf.Ruler = NOT Inf.Ruler
            IF Inf.Ruler THEN
               RAction = 0
            ELSE
               RAction = 5
            END IF
            RulerLine Inf, RAction

         CASE ELSE
      END SELECT

      KyWas = Ky
   END IF

   ShiftFake = 0

   IF Inf.CurRow <> CurRowWas THEN UnPaintOk = 0


   IF QPLen(K$) OR Presses THEN
      IF Inf.EdType > 0 THEN
         IF Inf.Ruler THEN RulerLine Inf, 5
         IF Inf.EdType > 1 THEN
            GOSUB PutBlock
            UndoBlock B(), ScrBuf()
         END IF
      ELSE
         IF Inf.Ruler THEN
            IF Inf.CurRow <> CurRowWas THEN
               RAction = 2
            ELSE
               RAction = 3
            END IF
            RulerLine Inf, RAction
         END IF
      END IF
   END IF



EXIT SUB


GoHome:
   Look$ = " "
   FOR N = LC TO RC
      CALL ReadScrn0(Inf.CurRow, N, Look$)
      IF Look$ <> " " THEN EXIT FOR
   NEXT
   IF N = RC + 1 THEN N = LC
   IF Inf.CurCol = N THEN N = LC
   Inf.CurCol = N
RETURN



PutBlock:

   Inf.CurRow = MinInt(Inf.CurRow, Inf.Rows - (BlockM(0) MOD 256) + 1)'Inf.BBrRow - Inf.BUlRow))
   Inf.CurCol = MinInt(Inf.CurCol, 80 - (BlockM(0) \ 256) + 1)'Inf.BBrCol - Inf.BUlCol))

   B(0, 0) = Inf.CurRow
   B(1, 0) = Inf.CurCol
   B(2, 0) = Inf.CurRow + (BlockM(0) MOD 256) - 1'(Inf.BBrRow - Inf.BUlRow)
   B(3, 0) = Inf.CurCol + (BlockM(0) \ 256) - 1'(Inf.BBrCol - Inf.BUlCol)

   MScrnRest B(0, 0), B(1, 0), B(2, 0), B(3, 0), BlockM(1)

RETURN


SaveScrn:
   MScrnSave 1, 1, Inf.Rows, 80, ScrBuf(0)
RETURN

RestScrn:
   MScrnRest 1, 1, Inf.Rows, 80, ScrBuf(0)
RETURN


END SUB

