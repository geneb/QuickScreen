'********** SCRNCAP.BAS - TSR text mode screen capture program

'Copyright (c) 1989 Ethan Winer & Crescent Software

'This program must be compiled to disk before running.


'To take as little DOS memory as possible when this program is installed as
'a TSR, you should link it with the STR00512.OBJ and other "stub" files:
'
'    BC SCRNCAP /O;
'    LINK /NOD /NOE SCRNCAP STR00512 _NOREAD _NOVAL _LOCATE, , NUL, PDQ;


DEFINT A-Z
'$INCLUDE: 'PDQDECL.BAS'

DIM Registers AS RegType                'RegType is defined in PDQDECL.BAS
One = 1                                 'these variable save code size below
Seven = 7

ID$ = "ScrnCap (c) 1989 Crescent Software - press Ctrl-S to activate"
Row = CSRLIN                            'print at the current cursor location
Column = POS(0)

DGROUP = TSRInstalled%(ID$)             'see if we're already installed
IF INSTR(UCASE$(COMMAND$), "/U") THEN   'do they want to un-install?
   IF DGROUP THEN                       'are we already installed?
      Okay = PopDeinstall%(DGROUP, ID$) 'yes, try to remove the earlier copy
      IF NOT Okay THEN                  'deinstallation failed, so tell 'em
         PRINT "Unable to de-install SCRNCAP"
      END IF
   ELSE
      PRINT "SCRNCAP is not resident -- try again without /U"
   END IF
   END                                  'either way, end the program
ELSE
   IF DGROUP THEN                       'already resident, they didn't use /U
      PRINT "SCRNCAP already installed -- press Ctrl-S to activate"
      END
   END IF
END IF

'PDQPrint ID$, Row, Column, 7
PRINT ID$

ScrnSize = 480                          '3 lines * 80 cols for prompt & input
BufSeg = AllocMem%(ScrnSize)            'allocate memory to save the screen
FileName$ = "                                  "

'----- set up the TSR as a pop-up using Alt-S
CALL PopUpHere(&H41F, ID$)              'Ctrl-S, pass the unique ID string
GOTO EndIt                              'skip past the interrupt handler below

'----- the following code is invoked each time Ctrl-S is pressed
DEF SEG = 0                                     'see what type of monitor
CONST ScrCols = 80                              'default to 80 columns
ScrRows = 25                                    'assume monochrome
ScrSeg = &HB000
IF PEEK(&H463) <> &HB4 THEN                     'it might be an EGA or VGA
   ScrSeg = &HB800                              'use the color segment
   Registers.AX = &H1200                        'ask for the EGA info service
   Registers.BX = &H10
   Interrupt &H10, Registers
   IF (Registers.BX AND &HFF) <> &H10 THEN      'if BL <> 10h then it's EGA
      ScrRows = PEEK(&H484) + 1                 'get rows, adjust to 1-based
   END IF
END IF

BlockCopy ScrSeg, Zero, BufSeg, Zero, ScrnSize  'save the underlying screen
SaveCsr& = CursorSave&                          'and the cursor location too

'----- draw a box and prompt for the file name
PDQPrint "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿", One, One, Seven
PDQPrint "³ Enter a name for the screen:                                    ³", 2, One, Seven
PDQPrint "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ", 3, One, Seven

'LSET FileName$ = ""                            'unREM to clear the field
LOCATE 2, 32                                    'position the cursor
BIOSInput FileName$, 112                        'get name, add .SCR if needed

IF INSTR(FileName$, ".") = 0 THEN               'this block is optional
   LSET FileName$ = RTRIM$(FileName$) + ".SCR"  'remove it to not add the
END IF                                          'default .SCR file extension

BlockCopy BufSeg, Zero, ScrSeg, Zero, ScrnSize  'restore the screen contents
CursorRest SaveCsr&                             'and the cursor location/size

DEF SEG = ScrSeg                                'use the appropriate segment
BSAVE RTRIM$(FileName$), 0, ScrRows * ScrCols * 2    'save the screen

CALL PopDown                                        'back to underlying app.

EndIt:
CALL EndTSR(ID$)                        'this installs us as a TSR

