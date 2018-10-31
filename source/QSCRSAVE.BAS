DEFINT A-Z

'$INCLUDE: 'FieldInf.Bi'
'$INCLUDE: 'DialType.Bi'
'$INCLUDE: 'GenInfo.BI'

TYPE HeaderInfo
   FrmName AS STRING * 8
   Offset AS LONG
END TYPE

DECLARE FUNCTION AlertUser% (Message$, Prmt1$, Prmt2$, Prmt3$)
DECLARE FUNCTION ASCII% (Char$)
DECLARE FUNCTION BaseName$ (FileName$)
DECLARE FUNCTION CheckSum% (a$)
DECLARE FUNCTION Exist% (FileName$)
DECLARE FUNCTION Extension$ (FileName$)
DECLARE FUNCTION FindLast% (BYVAL Address, Size)
DECLARE FUNCTION FileNameOk% (ScrName$)
DECLARE FUNCTION FileSize& (FileName$)
DECLARE FUNCTION FLof& (Handle)
DECLARE FUNCTION IsThere% (FileName$)
DECLARE FUNCTION LibNo% (ScrName$, ScrnLib%())
DECLARE FUNCTION MaxInt% (Var1%, Var2%)
DECLARE FUNCTION NoExtension$ (FileName$)
DECLARE FUNCTION ParsPath$ (FileSpec$)
DECLARE FUNCTION Peek1% (Segment, Address)
DECLARE FUNCTION QPRTrim$ (Text$)

DECLARE SUB Chime (Flag)
DECLARE SUB DelLib (ScrnLib%(), EllNo%, NoDel%)
DECLARE SUB DialogBox (Title$, PrmtAns$(), Dl() AS Dialog, Mult$(), ExitCode%)
DECLARE SUB ErrMsg (Text$)
DECLARE SUB Extents (UlRow%, UlCol%, BrRow%, BrCol%)
DECLARE SUB FClose (Handle)
DECLARE SUB FCreate (FileName$)
DECLARE SUB FGet (Handle, Text$)
DECLARE SUB FOpen (FileName$, Handle)
DECLARE SUB FPut (Handle, Text$)
DECLARE SUB FPutA (Handle, SEG Element AS ANY, NumBytes&)
DECLARE SUB FSeek (Handle, Offset&)
DECLARE SUB HideCursor ()
DECLARE SUB InsLib (ScrnLib%(), EllNo%, NoIns%)
DECLARE SUB KillFile (FileName$)
DECLARE SUB MakeData (ScrName$, Form() AS ANY, Clc$(), Help$())
DECLARE SUB MPaintBox (UlRow%, UlCol%, LrRow%, LrCol%, Colr%)
DECLARE SUB MScrnSave (UlRow%, UlCol%, LrRow%, LrCol%, SEG Address%)
DECLARE SUB MScrnRest (UlRow%, UlCol%, LrRow%, LrCol%, SEG Address%)
DECLARE SUB PrSaveScr (Title$, ScrName$, Form() AS ANY, ExitCode%, Inf AS ANY)
DECLARE SUB ReplaceInLib (NameLib$, NInLib%, ScrnLib%(), Inf AS GenInfo, ErrFlag%)
DECLARE SUB RulerLine (Inf AS ANY, Action%)
DECLARE SUB Scr2Obj (ObjName$, WipeType%, Inf AS ANY)
DECLARE SUB ScrEdit (K$, Inf AS ANY)
DECLARE SUB Scrn2Str (BYVAL Segment, BYVAL Address, StrLen)
DECLARE SUB ScrSize (UlRow%, UlCol%, LrRow%, LrCol%, Inf AS ANY)
DECLARE SUB ShowCursor ()
DECLARE SUB StuffBuf (X$)
DECLARE SUB SvForm (ScrName$, Form() AS ANY, Clc$(), Help$(), Inf AS ANY)
DECLARE SUB SvLib (LibName$, ScrnLib%(), Changed%)
DECLARE SUB SvScr (ScrName$, Inf AS ANY)

'----- DATA for SUB - "Scr2Obj"

Obj.to.Array.Data:
   '----- Second SEGDEF, GRPDEF and EXTDEF records
   DATA 152,7,0,72,0,0,6,4,1,14,154,4,0,2,255,2,95,140,11,0,8,79
   DATA 66,74,50,65,82,65,89,0,39
   '----- LEDATA record
   DATA 160,0,4,1,0,0,85,139,236,196,126,6,184,0,0,141,54,0
   DATA 0,154,0,0,0,0,139,236,139,118,10,137,4,93,202,6,0
   '----- FIXUPP record
   DATA 156,18,0,200,7,4,1,1,196,11,0,1,1,29,0,204,14,38,1,1,141

Obj.to.Scrn.Data:
   '----- Second SEGDEF, GRPDEF and EXTDEF records
   DATA 152,7,0,72,0,0,6,4,1,14,154,4,0,2,255,2
   DATA 95,140,10,0,7,80,85,84,83,67,82,78,0,52
   '----- LEDATA record
   DATA 160,0,0,1,0,0,85,139,236,139,118,6,139,60
   DATA 141,54,0,0,184,0,0,154,0,0,0,0,93,202,2,0
   '----- FIXUPP record
   DATA 156,18,0,196,10,0,1,1,24,0,200,13,4,1,1,204,16,38,1,1,139

SUB Add2Lib (NameLib$, ScrnLib(), Inf AS GenInfo, ErrFlag) STATIC

   ErrFlag = -1                        'Guilty until proven otherwise


   NScrns = ScrnLib(0)                 'Find number of Screens plus one
   TopDic = NScrns * 7                 'Find Last Dictionary Address


   '0  - Offset into array of beginning of screen
   '1  - Screen Length in bytes
   '2  - byte 1 = clear screen flag, byte 2 = Screen lines
   '3  - Name starts here
   '4  -       .
   '5  -       .
   '6  - Name ends here



   IF NScrns THEN                      'If not a new library
                                       'Find Offset for new screen
      ScrLen = ScrnLib(TopDic + 1)
      NewScr = ScrnLib(TopDic) + ((ScrLen \ 2) + (ScrLen MOD 2)) + 7
   ELSE
      NewScr = 14
   END IF

   IF Inf.Ruler THEN RulerLine Inf, 5

   ScrSize UlRow, UlCol, BrRow, BrCol, Inf
   IF UlRow = 0 THEN EXIT SUB

   REDIM Strg(1 TO 1) AS STRING * 8005
   Strg(1) = CHR$(UlRow) + CHR$(UlCol) + CHR$(BrRow) + CHR$(BrCol)
   Scrn2Str VARSEG(Strg(1)), VARPTR(Strg(1)), ALen

   IF Inf.Ruler THEN RulerLine Inf, 1
  
   IF NewScr + (ALen \ 2&) + (ALen MOD 2) > UBOUND(ScrnLib) THEN
      IF Inf.BeepOn THEN Chime 6
      ErrMsg "This screen is too big to fit into the current library."
   ELSE

      CALL InsLib(ScrnLib(), TopDic, 7)         'Push everything up

      FOR N = 1 TO NScrns                       'Fix other offset pointers
         ScrnLib(N * 7) = ScrnLib(N * 7) + 7
      NEXT
      ScrnLib(0) = NScrns + 1           'Add 1 to the number of Screens
      
                                        'Copy screen to library memory
      CALL BCopy(VARSEG(Strg(1)), VARPTR(Strg(1)), VARSEG(ScrnLib(NewScr)), VARPTR(ScrnLib(NewScr)), ALen, 0)
      
      TopDic = TopDic + 7

      ScrnLib(TopDic) = NewScr          'Put New screen Offset into Dictionary
      ScrnLib(TopDic + 1) = ALen

      ScrnLib(TopDic + 2) = 256 * Inf.Lines + Inf.DispType

      NameLib$ = RTRIM$(UCASE$(NameLib$))       'Make it upper case
      DEF SEG = VARSEG(ScrnLib(0))
      Offset = VARPTR(ScrnLib(TopDic + 3))
      FOR N = 1 TO 8
         IF N <= LEN(NameLib$) THEN
            Temp = ASCII(MID$(NameLib$, N, 1))
         ELSE
            Temp = 0
         END IF
         POKE Offset + N - 1, Temp
      NEXT

      Inf.ScrChanged = 0
      Inf.LibChanged = -1

      ErrFlag = 0

   END IF

   ERASE Strg

END SUB

SUB BuildFrmLib (LibName$, ScrnLib())

    Path$ = ParsPath$(LibName$)
    Forms = 0
    
    DIM Header(ScrnLib(0)) AS HeaderInfo
    
    LibSeg = VARSEG(ScrnLib(0))

    FOR N = 1 TO ScrnLib(0)

       Temp$ = SPACE$(8)
       Offset = VARPTR(ScrnLib(N * 7 + 3))

       FOR P = 1 TO 8
          PeekChar = Peek1%(LibSeg, Offset + P - 1)
          IF PeekChar = 0 THEN EXIT FOR

          MID$(Temp$, P) = CHR$(PeekChar)
       NEXT

       IF Exist(Path$ + RTRIM$(Temp$) + ".FRM") THEN
          Forms = Forms + 1
          Header(Forms).FrmName = Temp$
       END IF

    NEXT

    IF Forms = 0 THEN EXIT SUB

    FrmLibName$ = Path$ + BaseName$(LibName$) + ".QFL"
    FCreate FrmLibName$
    FOpen FrmLibName$, LibHandle

    IF DosError THEN
       BS = ErrorPrompt
       KillFile FrmLibName$
       EXIT SUB
    END IF

    Offset& = Forms * LEN(Header(1))
    FPutA LibHandle, Header(1), Offset&

    IF FRE(Temp$) < 4096 + 2048& THEN
       BlockSize = FRE(Temp$) - 2048
       IF BlockSize < 2 THEN
          ErrMsg "Not enough memory to build forms library."

          KillFile FrmLibName$
          EXIT SUB
       END IF
    ELSE
       BlockSize = 4096
    END IF

    FOR N = 1 TO Forms
       Header(N).Offset = Offset& + 1

       FOpen Path$ + RTRIM$(Header(N).FrmName) + ".FRM", FrmHandle
       LenOfForm& = FLof&(FrmHandle)

       FrmOffset& = 1
       DO
          Bytes& = LenOfForm& - FrmOffset& + 1
          IF Bytes& > BlockSize THEN Bytes& = BlockSize

          Temp$ = SPACE$(Bytes&)

          FGet FrmHandle, Temp$
          FPut LibHandle, Temp$

          IF DosError THEN
             BS = ErrorPrompt
             KillFile FrmLibName$
             EXIT SUB
          END IF

          Offset& = Offset& + Bytes&

          FrmOffset& = FrmOffset& + Bytes&
       LOOP UNTIL FrmOffset& >= LenOfForm&

       FClose FrmHandle
    NEXT

    FSeek LibHandle, 0
    FPutA LibHandle, Header(1), Forms * LEN(Header(1))

    FClose LibHandle

END SUB

'********* See if the Library needs to be Saved
SUB ChkLibChanged (LibName$, ScrnLib(), Inf AS GenInfo, ExitCode) STATIC

   IF Inf.LibChanged AND NOT Demo THEN
      LOCATE Inf.Rows \ 2 - 2
      IF Inf.BeepOn THEN Chime 6
      ExitCode = AlertUser("The Screen Library has changed!  Save it now?", "Yes", "No", "Cancel")
      IF ExitCode = 1 THEN
         CALL SvLib(LibName$, ScrnLib(), Inf.LibChanged)
      ELSEIF ExitCode = 2 THEN
         Inf.LibChanged = 0
      END IF
   ELSE
      ExitCode = 1
   END IF

END SUB

'See if Screen or Form needs to be Saved
SUB ChkScrChanged (ScrName$, ScrnLib(), Form() AS FieldInfo, Help$(), Clc$(), Inf AS GenInfo, ExitCode)
   
   IF Inf.ScrChanged AND NOT Demo THEN
      LOCATE Inf.Rows \ 2 - 2
      IF Inf.BeepOn THEN Chime 6
      ExitCode = AlertUser("This screen has changed since it was last saved.  Save it now?", "Yes", "No", "Cancel")

      IF ExitCode = 1 THEN
         IF ScrName$ = "WORKING.SCR" THEN
            DO
               FileName$ = SPACE$(50)
               LSET FileName$ = ScrName$

               PrSaveScr "Save", FileName$, Form(), ExitCode, Inf
              
               IF ExitCode <> 1 THEN EXIT SUB

               IF INSTR(FileName$, ".") = 0 THEN FileName$ = FileName$ + ".SCR"

               There = 0
               IF FileName$ <> ScrName$ THEN
                  LOCATE Inf.Rows \ 2 - 2
                  There = IsThere(FileName$)
               END IF

               ScrName$ = FileName$

            LOOP WHILE There
         END IF

         IF Inf.FromLib THEN
            NameLib$ = BaseName$(ScrName$)
            NInLib = LibNo%(NameLib$, ScrnLib())

            IF NInLib THEN
               ReplaceInLib NameLib$, NInLib, ScrnLib(), Inf, ErrFlag
               IF ErrFlag THEN Inf.FromLib = 0
            ELSE
               Inf.FromLib = 0
            END IF
         ELSE
            SvScr ScrName$, Inf
            SvForm ScrName$, Form(), Clc$(), Help$(), Inf
         END IF

      ELSEIF ExitCode = 2 THEN
         Inf.ScrChanged = 0
         Inf.FrmChanged = 0
      END IF

   ELSEIF Inf.FrmChanged AND NOT Demo THEN
      LOCATE Inf.Rows \ 2 - 2
      IF Inf.BeepOn THEN Chime 6
      ExitCode = AlertUser("The field definitions have changed!  Save them now?", "Yes", "No", "Cancel")
      IF ExitCode = 1 THEN
         SvForm ScrName$, Form(), Clc$(), Help$(), Inf
      ELSEIF ExitCode = 2 THEN
         Inf.FrmChanged = 0
      END IF
   ELSE
      ExitCode = 1
   END IF

END SUB

SUB MakeData (ScrName$, Form() AS FieldInfo, Clc$(), Help$())

   Quote$ = CHR$(34)
   Comma$ = ","

   IF Form(0).Fields THEN
     
      '----- Calculate pointers into whole record buffer
      Form(1).Fields = 1
      FOR N = 2 TO Form(0).Fields
         Form(N).Fields = Form(N - 1).Fields + Form(N - 1).StorLen
      NEXT
      Form(0).Row = Form(N - 1).Fields + Form(N - 1).StorLen - 1


      Nam$ = BaseName$(ScrName$)
      Path$ = ParsPath$(ScrName$)

      OPEN Path$ + Nam$ + ".DTA" FOR OUTPUT AS #10

      PRINT #10, UCASE$(Nam$) + ".FieldInfo:"

      '----- Write the DATA statements
      FOR N = 0 TO Form(0).Fields
         PRINT #10, "DATA ";

         PRINT #10, Form(N).Fields; Comma$;
         PRINT #10, Form(N).Row; Comma$;
         PRINT #10, Form(N).LCol; Comma$;
         PRINT #10, Form(N).RCol; Comma$;
         PRINT #10, Form(N).StorLen; Comma$;
         PRINT #10, Form(N).FType; Comma$;
         CALL ReplaceChar(Form(N).RelFile, CHR$(0), " ")
         PRINT #10, Quote$; QPRTrim$(Form(N).RelFile); Quote$; Comma$;
         PRINT #10, Form(N).RelFld; Comma$;
         PRINT #10, Form(N).Indexed; Comma$;
         PRINT #10, Quote$; QPRTrim$(Form(N).FldName); Quote$; Comma$;
         PRINT #10, Form(N).Decimals; Comma$;
         PRINT #10, Form(N).RelHandle; Comma$;
         PRINT #10, Form(N).Protected; Comma$;
         PRINT #10, Form(N).ScratchI; Comma$;
         PRINT #10, Form(N).LowRange; Comma$;
         PRINT #10, Form(N).HiRange; Comma$;
         PRINT #10, Quote$; QPRTrim$(Form(N).ScratchS); Quote$
      NEXT
      PRINT #10,


      PRINT #10, UCASE$(Nam$) + ".Help:"

      FOR N = 0 TO Form(0).Fields
         Temp$ = Help$(N)
         CALL ReplaceChar(Temp$, Quote$, "'")
         PRINT #10, "DATA "; Quote$; Temp$; Quote$
      NEXT
      PRINT #10,


      PRINT #10, UCASE$(Nam$) + ".Formulas:"

      FOR N = 0 TO Form(0).Fields
         Temp$ = Clc$(N)
         CALL ReplaceChar(Temp$, Quote$, "'")
         PRINT #10, "DATA "; Quote$; Temp$; Quote$
      NEXT
      PRINT #10,


      CLOSE #10

   END IF

END SUB

SUB PrSaveScr (Title$, ScrName$, Form() AS FieldInfo, ExitCode, Inf AS GenInfo)

   ObjSave = INSTR(ScrName$, ".OBJ") <> 0

   REDIM Dial(18) AS Dialog

   Dial(1).DRow = 2
   Dial(1).DCol = 2
   Dial(1).DLen = LEN(ScrName$)
   Dial(1).DTyp = 1

   Dial(4).DRow = 7
   Dial(4).DCol = 2
   Dial(4).DLen = 3

   Dial(5).DRow = 7
   Dial(5).DCol = 19
   Dial(5).DTyp = 6

   IF ObjSave THEN
      Dial(4).DTyp = 6
      Dial(5).DRow = 5
      Dial(5).DCol = 32
   ELSE
      Dial(4).DTyp = 2
   END IF

   Dial(6).DRow = 4
   Dial(6).DCol = 48
   Dial(6).DLen = 12 + 256
   Dial(6).DTyp = 4
      
   Dial(7).DRow = 18
   Dial(7).DTyp = 5

   Dial(8).DRow = 18
   Dial(8).DTyp = 5

   Temp = 1 + MaxInt(LEN(Title$) + 6 + 10, 16 + 2 + LEN(ScrName$))
   Temp = MaxInt(Temp, 57)
          
   X = Temp - (LEN(Title$) + 6 + 8)
   Dial(7).DCol = MaxInt((X \ 3), 1)
   Dial(8).DCol = Dial(7).DCol + LEN(Title$) + 5 + (X \ 3)

   FOR N = 9 TO 18
      Dial(N).DRow = N - 2
      Dial(N).DCol = 2
      Dial(N).DTyp = 6
   NEXT

   REDIM Fd$(18, 1)
   Fd$(1, 0) = "Name screen as: "
   Fd$(1, 1) = RTRIM$(ScrName$)


   IF Form(0).Fields THEN
      Dial(2).DRow = 4
      Dial(2).DCol = 2
      Dial(2).DTyp = 3
      Fd$(2, 0) = "Save TYPE structure"
      IF Inf.SaveTYPE THEN
         Fd$(2, 1) = "û"
      ELSE
         Fd$(2, 1) = " "
      END IF
         
      Dial(3).DRow = 5
      Dial(3).DCol = 2
      Dial(3).DTyp = 3
      Fd$(3, 0) = "Save fields to DATA"
      IF Inf.SaveDATA THEN
         Fd$(3, 1) = "û"
      ELSE
         Fd$(3, 1) = " "
      END IF
   END IF

   IF ObjSave THEN
      Fd$(5, 0) = "Display using ->"

      Fd$(9, 0) = "`Direct to Screen' is the simplest method"
      Fd$(10, 0) = "requiring only a single call of the form:"
      Fd$(11, 0) = "  CALL ScrName(MonoCode)"
      Fd$(13, 0) = "Any other display method requires the"
      Fd$(14, 0) = "following code:"
      Fd$(15, 0) = "  REDIM Scrn(2000)"
      Fd$(16, 0) = "  CALL MScrnSave(1, 1, 25, 80, SEG Scrn(0))"
      Fd$(17, 0) = "  CALL ScrName(Wipe, SEG Scrn(0))"
      Fd$(18, 0) = "  CALL DisplayScrn(Scrn(), El, MonoCode, Wipe)"
      
   ELSE
      Fd$(4, 0) = "Display in "
      Fd$(4, 1) = STR$(Inf.Rows)

      Fd$(5, 0) = "Line Mode.   Display using ->"

      Fd$(11, 0) = "If an area is high-lighted, only that area"
      Fd$(12, 0) = "will be saved.  Otherwise any block of the"
      Fd$(13, 0) = "screen that starts or ends with anything"
      Fd$(14, 0) = "other than a space with a color of 7 (white"
      Fd$(15, 0) = "on black) will be saved."
   END IF

   Fd$(6, 1) = STR$(Inf.DispType + 1)
   REDIM Mult$(12)
   IF ObjSave THEN
      Mult$(1) = "Direct to Screen  "
   ELSE
      Mult$(1) = "Overlay           "
   END IF
   Mult$(2) = "Clear Screen      "
   Mult$(3) = "Curtain           "
   Mult$(4) = "Dissolve          "
   Mult$(5) = "Exploding Box     "
   Mult$(6) = "Push Down         "
   Mult$(7) = "Push Left to Right"
   Mult$(8) = "Push Right to Left"
   Mult$(9) = "Push Up           "
   Mult$(10) = "Roll Away         "
   Mult$(11) = "Vertical Blinds   "
   Mult$(12) = "Wipe Left to Right"


   Fd$(7, 0) = Title$
   Fd$(8, 0) = "Cancel"
          
   DO
      Again = 0

      LOCATE (Inf.Rows \ 2) - 9
      DialogBox Title$, Fd$(), Dial(), Mult$(), ExitCode
       
      IF ExitCode <> 1 THEN EXIT DO

   LOOP UNTIL FileNameOk(Fd$(1, 1))

   IF ExitCode = 1 THEN
      ScrName$ = Fd$(1, 1)

      IF Form(0).Fields THEN
         Inf.SaveTYPE = Fd$(2, 1) = "û"
         Inf.SaveDATA = Fd$(3, 1) = "û"
      END IF
    
      IF NOT ObjSave THEN
         Inf.Lines = VAL(Fd$(4, 1))
         IF Inf.Lines <= 25 THEN Inf.Lines = 25
         IF Inf.Lines > 25 AND Inf.Lines <= 43 THEN Inf.Lines = 43
         IF Inf.Lines > 43 THEN Inf.Lines = 50
      END IF
      Inf.DispType = VAL(Fd$(6, 1)) - 1
   END IF

END SUB

SUB ReplaceInLib (NameLib$, NInLib, ScrnLib(), Inf AS GenInfo, ErrFlag) STATIC

   ErrFlag = -1

   DicOff = NInLib * 7                         'Offset into dictionary
     
   '----- Get old size of screen in library
   LibScrLen = (ScrnLib(DicOff + 1) \ 2) + (ScrnLib(DicOff + 1) MOD 2)
             
   '----- Find the total size of the library
   TopDic = ScrnLib(0) * 7
   EndOfLib = ScrnLib(TopDic) + (ScrnLib(TopDic + 1) \ 2) + (ScrnLib(TopDic + 1) MOD 2) - 1
  
   IF Inf.Ruler THEN RulerLine Inf, 5

   '----- Get the size of the new screen
   ScrSize UlRow, UlCol, BrRow, BrCol, Inf
   IF UlRow = 0 THEN EXIT SUB

   REDIM Strg(1 TO 1) AS STRING * 8005
   Strg(1) = CHR$(UlRow) + CHR$(UlCol) + CHR$(BrRow) + CHR$(BrCol)
   Scrn2Str VARSEG(Strg(1)), VARPTR(Strg(1)), ALen

   ScrLen = (ALen \ 2) + (ALen MOD 2)

   IF Inf.Ruler THEN RulerLine Inf, 1

   '----- See if the new screen will fit
   IF EndOfLib + CLNG(ScrLen) - LibScrLen > UBOUND(ScrnLib) THEN
      IF Inf.BeepOn THEN Chime 6
      ErrMsg "This screen is too big to fit into the current library."
      ERASE Strg
      EXIT SUB
   END IF

   '----- Insert space in library for larger screen
   IF ScrLen > LibScrLen THEN
      NoIns = ScrLen - LibScrLen
      CALL InsLib(ScrnLib(), ScrnLib(DicOff), NoIns)
        
      FOR C = NInLib + 1 TO ScrnLib(0)
         ScrnLib(C * 7) = ScrnLib(C * 7) + NoIns
      NEXT C
   END IF

   '----- Delete space in library for smaller screen
   IF ScrLen < LibScrLen THEN
      NoDel = LibScrLen - ScrLen
      CALL DelLib(ScrnLib(), ScrnLib(DicOff), NoDel)

      FOR C = NInLib + 1 TO ScrnLib(0)
         ScrnLib(C * 7) = ScrnLib(C * 7) - NoDel
      NEXT C
   END IF

   ScrnLib(DicOff + 1) = ALen
      
   ScrnLib(DicOff + 2) = 256 * Inf.Lines + Inf.DispType

   CALL BCopy(VARSEG(Strg(1)), VARPTR(Strg(1)), VARSEG(ScrnLib(ScrnLib(DicOff))), VARPTR(ScrnLib(ScrnLib(DicOff))), ALen, 0)
   ERASE Strg

   NameLib$ = RTRIM$(UCASE$(NameLib$))         'Make it upper case
   DEF SEG = VARSEG(ScrnLib(0))
   Offset = VARPTR(ScrnLib(DicOff + 3))
   FOR N = 1 TO 8
       IF N <= LEN(NameLib$) THEN
          Temp = ASCII(MID$(NameLib$, N, 1))
       ELSE
          Temp = 0
       END IF
       POKE Offset + N - 1, Temp
   NEXT

   Inf.ScrChanged = 0
   Inf.LibChanged = -1
     
   ErrFlag = 0
  
END SUB

' QSCR.BAS - routine to create object file of screen    - CWM 3/1/1989
'----------------------------------------------------
SUB Scr2Obj (ObjName$, WipeType, Inf AS GenInfo) STATIC

   IF WipeType THEN
      RESTORE Obj.to.Array.Data
      D = 31
      E = 35
   ELSE
      RESTORE Obj.to.Scrn.Data
      C = StrLen + 24
      D = 30
      E = 30
   END IF

   IF INSTR(ObjName$, ".") = 0 THEN
      ObjName$ = ObjName$ + ".Obj"
   END IF

   Obj$ = BaseName$(ObjName$)


   a = LEN(Obj$)

   ScrSize UlRow, UlCol, LrRow, LrCol, Inf

   REDIM Strg(1 TO 1) AS STRING * 8005
   Strg(1) = CHR$(UlRow) + CHR$(UlCol) + CHR$(LrRow) + CHR$(LrCol)

   Scrn2Str VARSEG(Strg(1)), VARPTR(Strg(1)), StrLen

   IF WipeType THEN
      StrLen = StrLen + 1
      MID$(Strg(1), StrLen, 1) = CHR$(WipeType)
   END IF

   IF FRE("") < 2048& THEN
      ErrMsg "Not enough memory"
      EXIT SUB
   END IF

   IF Exist(ObjName$) THEN KILL ObjName$
   OPEN ObjName$ FOR BINARY AS #1

   '-------------------------------------------------------------------------
   'THEADR record

   a$ = CHR$(128) + CHR$(a + 6) + CHR$(0) + CHR$(a + 4) + LCASE$(Obj$) + ".ASM"
   a$ = a$ + CHR$(CheckSum(a$))
   PUT #1, , a$
   '-------------------------------------------------------------------------
   'LNAMES record

   a$ = CHR$(150) + CHR$(a + 31) + CHR$(0) + CHR$(0) + CHR$(6) + "DGROUP"
   a$ = a$ + CHR$(a + 5) + UCASE$(Obj$) + "_TEXT" + CHR$(4) + "DATA"
   a$ = a$ + CHR$(4) + "CODE" + CHR$(5) + "_DATA"
   a$ = a$ + CHR$(CheckSum(a$))
   PUT #1, , a$
   '-------------------------------------------------------------------------
   'First SEGDEF

   C = StrLen + E - 6
   a$ = CHR$(152) + CHR$(7) + CHR$(0) + CHR$(72) + CHR$(C MOD 256)
   a$ = a$ + CHR$(C \ 256) + CHR$(3) + CHR$(5) + CHR$(1)
   a$ = a$ + CHR$(CheckSum(a$))
   PUT #1, , a$
   '-------------------------------------------------------------------------
   'Second SEGDEF, GRPDEF and EXTDEF records

   a$ = SPACE$(D)
   FOR I = 1 TO D
      READ B
      MID$(a$, I) = CHR$(B)
   NEXT
   PUT #1, , a$

   '-------------------------------------------------------------------------
   'PUBDEF record

   a$ = CHR$(144) + CHR$(a + 7) + CHR$(0) + CHR$(0) + CHR$(1)
   a$ = a$ + CHR$(a) + UCASE$(Obj$) + CHR$(0) + CHR$(0) + CHR$(0)
   a$ = a$ + CHR$(CheckSum(a$))
   PUT #1, , a$
   '-------------------------------------------------------------------------
   'COMENT record

   a$ = CHR$(136) + CHR$(4) + CHR$(0) + CHR$(0) + CHR$(162)
   a$ = a$ + CHR$(1) + CHR$(209)
   PUT #1, , a$
   '-------------------------------------------------------------------------
   'LEDATA record

   a$ = SPACE$(E)
   FOR I = 1 TO E
      READ B
      MID$(a$, I) = CHR$(B)
   NEXT

   Start = 1
   Count = 0

   DO
      Leng = 1026 - LEN(a$)

      IF Start + Leng > StrLen THEN Leng = StrLen - Start + 1  'Don 9/29

      a$ = a$ + MID$(Strg(1), Start, Leng)
      Start = Start + Leng
      a = LEN(a$) - 2
      MID$(a$, 2, 1) = CHR$(a MOD 256)
      MID$(a$, 3, 1) = CHR$(a \ 256)
      a$ = a$ + CHR$(CheckSum(a$))
      PUT #1, , a$
     

         IF Count = 0 THEN
            a$ = SPACE$(21)                              'FIXUPP record
            FOR I = 1 TO 21
               READ B
               MID$(a$, I) = CHR$(B)
            NEXT
            PUT #1, , a$
         END IF

      IF Start < StrLen THEN                             'Don 9/29
         Count = Count + 1020
         a$ = CHR$(160) + CHR$(0) + CHR$(0) + CHR$(1)
         a$ = a$ + CHR$(Count MOD 256) + CHR$(Count \ 256)
      END IF

   LOOP WHILE Start < StrLen

   '-------------------------------------------------------------------------
   'MODEND record
   a$ = CHR$(138) + CHR$(2) + CHR$(0) + CHR$(0) + CHR$(116)
   PUT #1, , a$
   CLOSE #1

   a$ = ""
   Obj$ = ""
   ERASE Strg

END SUB

'********* Find starting address and length of the active screen area
SUB ScrSize (UlRow, UlCol, BrRow, BrCol, Inf AS GenInfo) STATIC

   IF Inf.MarkBlock THEN
      UlRow = Inf.BUlRow
      UlCol = Inf.BUlCol
      BrRow = Inf.BBrRow
      BrCol = Inf.BBrCol

      StuffBuf CHR$(1)
      ScrEdit "", Inf
      Inf.WasMarked = 0
   ELSE
      HideCursor
      Extents UlRow, UlCol, BrRow, BrCol
      ShowCursor
   END IF

   REDIM ScrBuf((BrRow - UlRow + 1) * (BrCol - UlCol + 1) + 1)
   MScrnSave UlRow, UlCol, BrRow, BrCol, ScrBuf(0)
   MPaintBox UlRow, UlCol, BrRow, BrCol, 112
   T! = TIMER + .05
   IF Inf.BeepOn THEN Chime 5
   DO
   LOOP UNTIL TIMER > T!

   MScrnRest UlRow, UlCol, BrRow, BrCol, ScrBuf(0)
   ERASE ScrBuf

END SUB

SUB SvForm (ScrName$, Form() AS FieldInfo, Clc$(), Help$(), Inf AS GenInfo)

   DIM Test AS FieldInfo

   CrLf$ = CHR$(13) + CHR$(10)
       
   IF Form(1).Row THEN

      Form(0).Fields = 0
      Form(0).Row = 0
      N = 1

      DO WHILE Form(N).Row <> 0 AND Form(0).Fields < MaxFields
         Form(0).Row = Form(0).Row + Form(N).StorLen
         Form(0).Fields = Form(0).Fields + 1
         N = N + 1
      LOOP
  
      Nam$ = BaseName$(ScrName$)
      PNam$ = NoExtension$(ScrName$)

      Ext$ = ".FRM"

      IF Inf.FrmChanged THEN
         IF FileSize&(PNam$ + ".DAT") > 2 THEN
            DatExtn$ = ".DAT"
         ELSEIF Exist(PNam$ + ".DBF") THEN
            DatExtn$ = ".DBF"
         ELSE
            DatExtn$ = ""
         END IF

         IF LEN(DatExtn$) THEN
            Ext$ = ".NEW"
            OPEN PNam$ + ".FRM" FOR RANDOM AS #1 LEN = FrmWdth
            GET #1, 1, Test
            Recs = Test.Fields

            R = 1
            N = 0
            Different = 0
            DO
               DO
                  R = R + 1
                  IF R > Recs + 1 THEN EXIT DO
                  GET #1, R, Test
               LOOP WHILE Test.FType = Button
               DO
                  N = N + 1
                  IF N > Form(0).Fields THEN EXIT DO
               LOOP WHILE Form(N).FType = Button

               IF R > Recs + 1 AND N > Form(0).Fields THEN
                  EXIT DO
               ELSEIF R > Recs + 1 AND N <= Form(0).Fields THEN
                  Different = -1
               ELSEIF R <= Recs + 1 AND N > Form(0).Fields THEN
                  Different = -1
               ELSE
                  IF Test.StorLen <> Form(N).StorLen THEN
                     Different = -1
                  ELSEIF Form(N).FType >= 11 AND Form(N).FType <= 13 THEN
                     IF Test.ScratchI <> Form(N).ScratchI THEN
                        Different = -1
                     END IF
                  END IF
               END IF

            LOOP UNTIL Different

            CLOSE #1

            IF Different THEN
            
               IF Inf.BeepOn THEN Chime 6

               REDIM Msg1(4) AS Dialog
               REDIM Fd$(4, 1)
               REDIM Mult$(0)

               Msg1(1).DRow = 1
               Msg1(1).DCol = 2
               Msg1(1).DTyp = 6
               Fd$(1, 0) = "Field definition changes require that `" + Nam$ + DatExtn$ + "' be rebuilt!"

               Msg1(2).DRow = 2
               Msg1(2).DCol = 2
               Msg1(2).DTyp = 6
               Fd$(2, 0) = "New definitions will be saved to `" + Nam$ + ".NEW'."

               Msg1(3).DRow = 3
               Msg1(3).DCol = 10
               Msg1(3).DTyp = 5
               Fd$(3, 0) = "Save New Definitions"

               Msg1(4).DRow = 3
               Msg1(4).DCol = 43
               Msg1(4).DTyp = 5
               Fd$(4, 0) = "Cancel"

               WHILE INKEY$ <> "": WEND
               DialogBox "", Fd$(), Msg1(), Mult$(), ExitCode

               IF ExitCode <> 1 THEN
                  Inf.FrmChanged = 0
                  EXIT SUB
               END IF
            ELSE
               IF Exist(PNam$ + ".NEW") THEN KILL PNam$ + ".NEW"
               Ext$ = ".FRM"
            END IF
         END IF
      END IF

      IF Exist(PNam$ + ".NEW") THEN Ext$ = ".NEW"


      FrmName$ = PNam$ + Ext$

      FCreate FrmName$
      FOpen FrmName$, Handle
      BLen& = (Form(0).Fields + 1) * FrmWdth
      FPutA Handle, Form(0), BLen&
      IF DosError THEN
         BS = ErrorPrompt
         FClose Handle
         KillFile FrmName$
         EXIT SUB
      END IF


      IF FindLast(VARPTR(Clc$(MaxFields)), MaxFields) THEN
         FPut Handle, "C"
      
         FOR N = 1 TO Form(0).Fields
            FPut Handle, UCASE$(Clc$(N)) + CrLf$
         NEXT
      END IF


      IF FindLast(VARPTR(Help$(MaxFields)), MaxFields) THEN
         FPut Handle, "H"

         FOR N = 1 TO Form(0).Fields
            FPut Handle, Help$(N) + CrLf$
         NEXT
      END IF

      FClose Handle


      IF Inf.SaveTYPE THEN
         FrmName$ = PNam$ + ".BI"
         FCreate FrmName$
         FOpen FrmName$, Handle

         FPut Handle, "TYPE " + Nam$ + CrLf$
         FOR N = 1 TO Form(0).Fields
             IF Form(N).FType <> Button THEN
                FPut Handle, "    " + Form(N).FldName$ + " AS "
                SELECT CASE Form(N).FType
                   CASE IntFld, DateFld, EuroDateFld
                      Temp$ = "INTEGER"
                   CASE LongIntFld, NotesFld, MultChFld, RecNoFld, TotRecsFld
                      Temp$ = "LONG"
                   CASE SngFld
                      Temp$ = "SINGLE"
                   CASE DblFld, MoneyFld
                      Temp$ = "DOUBLE"
                   CASE ELSE
                      Temp$ = "STRING *" + STR$(Form(N).StorLen)
                END SELECT
                FPut Handle, Temp$ + CrLf$
             END IF
         NEXT
         FPut Handle, "END TYPE" + CrLf$

         FClose Handle
      END IF

      IF Inf.SaveDATA THEN
         MakeData ScrName$, Form(), Clc$(), Help$()
      END IF

      Inf.FrmChanged = 0
   END IF

END SUB

'********** Save the current Library to disk
SUB SvLib (LibName$, ScrnLib(), Changed)

   ScrnLib(1) = 26

   '----- Find the total size of the library
   TopDic = ScrnLib(0) * 7
   EndOfLib = ScrnLib(TopDic) + (ScrnLib(TopDic + 1) \ 2) + (ScrnLib(TopDic + 1) MOD 2)

   Length& = 2& * EndOfLib      'Find Length of Library in bytes
   IF Length& = 0 THEN Length& = 1

   CALL QBSave(LibName$, SEG ScrnLib(0), Length&)

   IF DosError THEN
      BS = ErrorPrompt
      KillFile LibName$
      EXIT SUB
   END IF

   Changed = 0

END SUB

'********** Save the Current Screen to Disk
SUB SvScr (ScrName$, Inf AS GenInfo)

   IF Inf.Ruler THEN RulerLine Inf, 5

   ScrSize UlRow, UlCol, BrRow, BrCol, Inf 'Find the Top & Bottom of screen

   REDIM Strg(1 TO 1) AS STRING * 8010
   Strg(1) = CHR$(UlRow) + CHR$(UlCol) + CHR$(BrRow) + CHR$(BrCol)
   Scrn2Str VARSEG(Strg(1)), VARPTR(Strg(1)), ALen

   IF Inf.Ruler THEN RulerLine Inf, 1

   MID$(Strg(1), 5) = Strg(1)
   MID$(Strg(1), 1) = CHR$(26) + CHR$(0) + CHR$(Inf.DispType) + CHR$(Inf.Lines)

   ALen = ALen + 4

   CALL QBSave(ScrName$, BYVAL VARSEG(Strg(1)), BYVAL VARPTR(Strg(1)), ALen)

   IF DosError THEN
      BS = ErrorPrompt
      KillFile ScrName$
      EXIT SUB
   END IF

   Inf.ScrChanged = 0                     'Reset the changed attribute

END SUB

