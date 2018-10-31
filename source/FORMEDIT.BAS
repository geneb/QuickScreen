'****** FormEdit.Bas - Forms Editing Module *********************************
'by Donald R. Malin,  Copyright (c) 1989, 1990 Crescent Software, Inc.

'The following routines are provided to manage editing of data entry forms
'from your programs:

'    EditForm - This is the routine that handles all User input, cursor and
'               mouse activity.  It is a pollable routine so that your
'               program can monitor all the User's activity while editing.
'  PrintArray - Prints the contents of fields on the screen form.  This
'               routine is used internally by [EditForm] but can be used in
'               your program to force a redisplay after altering any data.
'   SaveField - Checks and converts a fields data and places it in the record
'               buffer [Form$(0, 0)].  As above, this routine is used
'               internaly by [EditForm], but can be used by your program to
'               check and format data before calling [PrintArray].
'UnPackBuffer - Copies and converts all data from the record buffer
'               [Form$(0, 0)] and places it in the individual elements of the
'               [Form$()] array.
'****************************************************************************

DEFINT A-Z

'$INCLUDE: 'DefCnf.bi'                  'Defines TYPE for monitor/color info.
'$INCLUDE: 'FieldInf.bi'                '        "        field information
'$INCLUDE: 'FormEdit.bi'                '        "        form editing info.
'$INCLUDE: 'QEditype.bi'                '        "        notes editing info.


'----- Control Constants
CONST StayOnField = 0                   'When true (-1) causes cursor to stay
                                        '  on fields after typing off the end.
CONST SkipProtected = -1                'When true (-1) causes the cursor to
                                        '  skip over "Protected" fields.

'----- Internal Constants
CONST IntChars$ = "+- 1234567890"               'Legal integer characters
CONST FloatChars$ = " +-.1234567890EeDd"        '  "   floating point chars.
CONST MoneyChars$ = " $+-,.1234567890"          '  "   currency characters

CONST BlankInt% = -32767                        'Specifies blank integer
CONST BlankLng& = -2147483647                   '       "        long int.
CONST BlankSng! = -3.402823E+38                 '       "        single
'CONST BlankSng! = -1E+38                       'UnREM when using /MBF
CONST BlankDbl# = -1.79769313486231D+308        'Specifies blank double
'CONST BlankDbl# = -1E+38                       'UnREM when using /MBF
CONST BlankDate$ = "  -  -19  "


'----- Define functions
DECLARE FUNCTION ASCII% (Strng$)
DECLARE FUNCTION Date2Num% (Dat$)
DECLARE FUNCTION DOSVer% ()
DECLARE FUNCTION EDate2Num% (Dat$)
DECLARE FUNCTION EndOfForms% (Fld() AS FieldInfo)
DECLARE FUNCTION ENum2Date$ (Days)
DECLARE FUNCTION FindLast% (BYVAL Address, Size)
DECLARE FUNCTION FldNum% (FldName$, Fld() AS ANY)
DECLARE FUNCTION FUsing$ (Number$, Image$)
DECLARE FUNCTION MaxInt% (Var1%, Var2%)
DECLARE FUNCTION MinInt% (Var1%, Var2%)
DECLARE FUNCTION Null% (Text$)
DECLARE FUNCTION Num2Date$ (Days)
DECLARE FUNCTION Peek1% (Segment%, Offset%)
DECLARE FUNCTION QPRTrim$ (Text$)
DECLARE FUNCTION Value# (E$, ErrCode%)

'----- Define subs
DECLARE SUB APrint0 (BYVAL Address, NumEls, MidFirst, NumChars, Colr)
DECLARE SUB Box0 (UlRow, UlCol, BrRow, BrCol, Char, Clr)
DECLARE SUB ButtonPress (ButNo%, Down%, Presses%, X%, Y%)
DECLARE SUB CalcFields (StartOfForm%, FldNo%, Form$(), Fld() AS FieldInfo)
DECLARE SUB FEInitForm (Form$(), Fld() AS ANY, Frm AS ANY)
DECLARE SUB FEDownArrow (Fld() AS ANY, Frm AS ANY)
DECLARE SUB FEPressButton (Fld AS ANY)
DECLARE SUB FESetupMult (Form$(), Fld() AS ANY, Frm AS ANY, Cnf AS ANY)
DECLARE SUB FESetUpNotes (Form$(), Fld() AS ANY, Frm AS ANY)
DECLARE SUB FEUpArrow (Fld() AS ANY, Frm AS ANY)
DECLARE SUB Format (Float#, Fld AS FieldInfo, Image$)
DECLARE SUB FixDate (Dat$)
DECLARE SUB GetCursor (X, Y, Button)
DECLARE SUB GetVMode (Mode, Page, PageSize, Rows, Columns)
DECLARE SUB HideCursor ()
DECLARE SUB Message (Msg$)
DECLARE SUB MouseTrap (UlRow, UlCol, BrRow, BrCol)
DECLARE SUB MQPrint (X$, Colr)
DECLARE SUB MPaintBox (UlRow, UlCol, LRRow, LRCol, Colr)
DECLARE SUB MScrnSave (UlRow, UlCol, LRRow, LRCol, SEG Address)
DECLARE SUB MScrnRest (UlRow, UlCol, LRRow, LRCol, SEG Address)
DECLARE SUB Pause (Ticks)
DECLARE SUB PrintArray (FirstFld%, LastFld%, Form$(), Fld() AS FieldInfo)
DECLARE SUB ProperName (Text$)
DECLARE SUB QEdit (Array$(), X$, Action%, Ed AS ANY)
DECLARE SUB SaveField (FldNo%, Form$(), Fld() AS ANY, BadFld%)
DECLARE SUB ShowCursor ()
DECLARE SUB SplitColor (Colr%, Fg%, Bg%)
DECLARE SUB StuffBuf (Ky$)
DECLARE SUB Tokenize (Calc$, Fld() AS FieldInfo)
DECLARE SUB VertMenu (Item$(), Choice%, MaxLen%, BoxBot%, Ky$, Action%, Cnf AS Config)

DIM Ed AS EditInfo
DIM SHARED Temp$

REDIM Work$(0)

'****************************************************************************
'Handles all User input, cursor and mouse activity.  It is a pollable routine
'so that your program can monitor all the User's activity while editing.
'
'Form$() holds the field data, Help messages and formulas for the form.  It
'  must be dimensioned to the number of fields by 2 [REDIM Form$(Fields, 2)].
'  Field data is kept in the first column [Form$(N, 0)], Help messages are
'  kept in the second column [Form$(N, 1)] and Formulas go in the third
'  [Form$(N, 2)].
'
'Fld() is a TYPE array containing information about each field [Row, Column,
'  Type, etc.].  See "FieldInf.Bi" for structure definition.
'
'Frm is a TYPE containing general information about the current edit session
'  such as the current field number, whether the form has been change, etc..
'  See "FormEdit.Bi" for details.
'
'Cnf is a TYPE variable containing monitor and color information required by
'  [VertMenu].
'
'Action is a flag used to control the form in the following ways:
'  Action = 1 - Initialize the form for editing.  Pads all Form$ elements
'               to their propper lengths and formats.  Displays the contents
'               of all fields on the form.  Resets Action to 3.
'  Action = 3 - This is the idle state where the actual editing is done.
'****************************************************************************
'
SUB EditForm (Form$(), Fld() AS FieldInfo, Frm AS FormInfo, Cnf AS Config, Action) STATIC

    SHARED Ed AS EditInfo                       'For QEditS
    SHARED Work$()
    SHARED Choice$()                            'Multiple choice array
    SHARED ScrRows%, NPixLines%, EndOfForm%, TopRow%, BottomRow%, Instate
    SHARED VertLen, VAction, Choice

    IF CsrSize = 0 THEN
       '----- Find the cursor's size in Scan Lines
       IF Peek1(0, &H463) = &HB4 THEN           'Peek at low memory to see
          CsrSize = 12                          'what type of monitor we have
       ELSE                                     'Monochrome uses 13 scan lines
          CsrSize = 7                           'Color uses 8
       END IF
    END IF


    '----- Initialize variables/arrays if action isn't 3
    IF Action < 2 THEN
       FEInitForm Form$(), Fld(), Frm
    END IF

    IF Action < 3 THEN
       Frm.PrevFld = 0
       Action = 3                               'Set Action to 3 (idle state)
       GOSUB SetCursor                          'Set the cursor size
    END IF




    '----- Are we moving to a new field?
    IF Frm.FldNo <> Frm.PrevFld OR TermKey THEN
       
       Temp = Frm.FldNo
       IF SkipProtected THEN
          '----- Jump over protected fields
          IF Frm.FldNo > Frm.PrevFld THEN       'Going forward?
             GOSUB ForwardSkipProtected
          ELSE                                  'Going backwards
             GOSUB BackwardSkipProtected
          END IF
       END IF

       '----- Setup for Field
       Frm.DoingMult = 0                        'Init. multiple choice flag
       DoingNotes = 0                           'Init. Notes field flag
       Frm.PrevFld = Temp                       'Set previous field #
       Frm.FldEdited = 0                        'Init. "field edited" flag
       LeftCol = Fld(Frm.FldNo).LCol            'Set left column for field

       IF Fld(Frm.FldNo).FType = NotesFld THEN  'Set up for "Notes" field

          FESetUpNotes Form$(), Fld(), Frm

          QAction = 1                           'Set QEdit's action flag to
                                                '  initialize state
          Ky$ = ""                              'Clear previous key press
          DoingNotes = -1                       'Set "Notes" flag
       END IF

       FieldWas$ = Form$(Frm.FldNo, 0)
       Mask$ = ""
       Frm.TxtPos = 1                           'Start editing at left column
       PosWas = Frm.TxtPos
       FirstKey = -1                            'Flag first key

       FType = Fld(Frm.FldNo).FType             'Get fields type
       IF FType = Relational THEN FType = Fld(Frm.FldNo).ScratchI

       SELECT CASE FType                        'Do additional setup
          CASE StrFld
             EdType = 0                         'Set flag for normal string
          CASE PropStrFld
             EdType = Propr
          CASE UCaseStrFld
             EdType = UCase                     'Flag "Upper Case" strings
          CASE NumericStrFld
             EdType = Intgr                     'Flag "Numeric" strings
          CASE IntFld, LongIntFld
             EdType = Intgr                     'Flag "Integer" editing
          CASE SngFld, DblFld
             EdType = FloatP                    'Flag "Floating Point" editing
          CASE MoneyFld
             EdType = Money                     'Flag "Currency" editing
          CASE DateFld, EuroDateFld
             EdType = Intgr                     'Flag "Integer" editing
             Mask$ = ""
          CASE PhoneFld
             EdType = Intgr                     'Flag "Integer" editing
             Frm.TxtPos = 2
             Mask$ = ""
          CASE SoSecFld
             EdType = Intgr                     'Flag "Integer" editing
             Mask$ = ""
          CASE ZipFld
             EdType = Intgr                     'Flag "Integer" editing
             Mask$ = ""
          CASE LogicalFld
             EdType = LogicalFld                'Flag "Logical" editing
             LogicChars$ = MID$(Fld(Frm.FldNo).RelFile, 1, 1) + MID$(Fld(Frm.FldNo).RelFile, 3, 1)
          CASE MultChAFld
             EdType = MultFld                   'Flag Multiple choice editing
          CASE Button
             EdType = ButtonFld
          CASE ELSE
       END SELECT
            
    END IF



    '----- Poll for key/mouse activity
    TermKey = 0                                 'Init. "Un-handled" key flag

    IF Frm.DoingMult THEN                       'Multiple Choice field
       
       Frm.Presses = 0                          'No mouse presses

       VertMenu Work$(), Choice, VertLen, 0, Ky$, VAction, Cnf

    ELSEIF DoingNotes THEN                      'Notes field

       QEdit Work$(), Ky$, QAction, Ed          'Poll "QEdit"

       IF Ed.UnKnownKey AND LagKey <> -59 THEN
          TermKey = -1                          'Key not handled by "QEdit"?
       END IF
       Frm.Presses = Ed.Presses                 'Copy QEdit's mouse info.
       Frm.MRow = Ed.MRow
       Frm.MCol = Ed.MCol

    ELSE                                        'All other fields
                                                'Locate cursor, turn it on
       IF Fld(Frm.FldNo).FType = Button THEN
          LOCATE Fld(Frm.FldNo).Row, (Fld(Frm.FldNo).RCol - Fld(Frm.FldNo).LCol) \ 2 + Fld(Frm.FldNo).LCol, 1
       ELSE
          LOCATE Fld(Frm.FldNo).Row, LeftCol + Frm.TxtPos - 1, 1
       END IF

       Ky$ = INKEY$                             'Get a key stroke

       '----- Check Mouse activity
       ButtonPress 1, N, Frm.Presses, X, Y
       IF Frm.Presses THEN
          Frm.MRow = (Y \ NPixLines) + 1        'Convert X, Y to Row and Col.
          Frm.MCol = (X \ 8) + 1
       END IF

    END IF


    '----- Handle mouse activity
    IF Frm.Presses THEN

       FOR N = Frm.StartEl + 1 TO EndOfForm     'Check all fields to see if
                                                '   mouse pressed in one
           IF Fld(N).FType = NotesFld THEN      'Set rows for notes field
              X = Fld(N).ScratchI
           ELSE
              X = Fld(N).Row
           END IF
                                                'Check fields row range
           IF Frm.MRow >= Fld(N).Row AND Frm.MRow <= X THEN
                                                'Check fields column range
              IF Frm.MCol >= Fld(N).LCol AND Frm.MCol <= Fld(N).RCol THEN
                                                'Is the field protected?
                 IF NOT (SkipProtected AND Fld(N).Protected) THEN
                    IF N = Frm.FldNo THEN       'Pressed on the current field?
                       SELECT CASE Fld(N).FType
                       CASE MultChAFld
                          Ky$ = CHR$(13)
                       CASE LogicalFld
                          Ky$ = CHR$(32)
                       CASE IS <> Button        'Move the cursor to mouse
                          Frm.TxtPos = Frm.MCol - LeftCol + 1
                       CASE ELSE
                       END SELECT
                       FirstKey = 0
                    ELSE                        'New field
                       DoingNotes = 0
                       Frm.FldNo = N            'Set the new field number
                    END IF
                    
                    EXIT FOR                    'All done, bail out
                 END IF
              END IF
           END IF
       NEXT
      
       IF N > EndOfForm THEN                    'Not on any fields?
          TermKey = -1
       ELSEIF Fld(Frm.FldNo).FType = Button THEN
          Ky$ = CHR$(ABS(Fld(Frm.FldNo).ScratchI))
          IF Fld(Frm.FldNo).ScratchI < 0 THEN Ky$ = CHR$(0) + Ky$
          FEPressButton Fld(Frm.FldNo)
       END IF
    END IF



    '----- Turn key $ into code
    SELECT CASE LEN(Ky$)
       CASE 0
          Frm.KeyCode = 0                       'No key pressed
       CASE 1
          Frm.KeyCode = ASC(Ky$)                'Single character key
       CASE ELSE
          Frm.KeyCode = -ASC(RIGHT$(Ky$, 1))    'Extended keys are negative
    END SELECT



    '----- Check for Button commands
    IF Frm.KeyCode = 13 THEN
       IF Fld(Frm.FldNo).FType = Button THEN
          Frm.KeyCode = Fld(Frm.FldNo).ScratchI
          FEPressButton Fld(Frm.FldNo)
       END IF
    END IF



    '----- Handle keys from "Multiple Choice" and "Notes" fields
    IF Frm.DoingMult THEN
                                                'Enter or Escape pressed?
       IF Frm.KeyCode = 13 OR Frm.KeyCode = 27 THEN
          Frm.DoingMult = 0                     'All done picking a choice
                                                'Erase the menu
          VertMenu Work$(), Choice, VertLen, 0, Ky$, 5, Cnf

          IF Frm.KeyCode = 13 THEN              'Enter pressed
             
             LSET Form$(Frm.FldNo, 0) = Work$(Choice) 'Copy the choice to edit $
             EdType = 0
             Frm.FldEdited = -1                 'Field was edited
          ELSE
             Frm.KeyCode = 0                    'Clear Escape key
          END IF
          ERASE Work$                           'Clean up temporary array
       END IF

    ELSEIF DoingNotes THEN

       IF Frm.KeyCode AND LEN(Msg$) THEN        'Clear any help message
          Msg$ = ""
          Message Msg$
       END IF

       SELECT CASE Frm.KeyCode                  'Check for exit keys
          CASE 9, -15, -117, -119
             DoingNotes = 0
             TermKey = -1
          CASE -80                              'Down arrow. If at end of
             IF Ed.CurLine = UBOUND(Work$) THEN '  array, goto next field
                DoingNotes = 0
                TermKey = -1
             END IF
          CASE -72                              'Up arrow. If at top of array
             IF Ed.CurLine = 1 THEN             '  goto previous field
                DoingNotes = 0
                TermKey = -1
             END IF
          CASE -59                              'Help key
             IF LEN(Form$(Frm.FldNo, 1)) AND LEN(Msg$) = 0 THEN
                Msg$ = Form$(Frm.FldNo, 1)      'Display help message
                Message Msg$
             END IF
          CASE ELSE
       END SELECT

       IF DoingNotes THEN
          SWAP Frm.KeyCode, LagKey
       END IF

    END IF



    '----- Handle keys in regular fields
    IF Frm.KeyCode <> 0 AND NOT DoingNotes AND NOT Frm.DoingMult THEN

       '----- Branch according to the key pressed
       SELECT CASE Frm.KeyCode

          '----- Backspace
          CASE 8
             IF EdType <> MultFld AND EdType <> ButtonFld THEN
                Frm.TxtPos = Frm.TxtPos - 1     'Back up the text pointer
             
                IF Frm.TxtPos > 0 THEN          'Still within the field?
                   LOCATE , LeftCol + Frm.TxtPos - 1, 0   'Locate 1 to the left
                   IF ASCII(MID$(Mask$, Frm.TxtPos)) <> 29 THEN
                      IF Frm.InsStat THEN       'Truncate the string
                         GOSUB DeleteChar
                      ELSE                      'Blank the letter
                         MID$(Form$(Frm.FldNo, 0), Frm.TxtPos) = " "
                      END IF
                                                'Print the new part of text
                      MQPrint MID$(Form$(Frm.FldNo, 0), Frm.TxtPos), -1
                      Frm.FldEdited = -1
                   END IF
                END IF
             END IF

          '----- Letter keys
          CASE 32 TO 254
             SELECT CASE EdType                 'Filter keys
                CASE 0                          'Normal strings (no setup)

                CASE Propr                      'Capitalize first letter of
                   IF Frm.TxtPos = 1 THEN       '  words in "Proper Name"
                      Ky$ = UCASE$(Ky$)         '  Fields
                   ELSEIF MID$(Form$(Frm.FldNo, 0), Frm.TxtPos - 1, 1) = " " THEN
                      Ky$ = UCASE$(Ky$)
                   END IF
                CASE UCase
                   Ky$ = UCASE$(Ky$)            'Make key upper case

                CASE Intgr                      'Skip if not in legal table
                   Temp = INSTR(IntChars$, Ky$)

                   SELECT CASE FType
                      CASE NumericStrFld, IntFld, LongIntFld
                         IF Temp = 0 THEN EXIT SUB
                      CASE ELSE
                         IF Temp < 3 THEN EXIT SUB
                   END SELECT

                CASE FloatP                     'Skip if not in legal table
                   IF INSTR(FloatChars$, Ky$) = 0 THEN EXIT SUB

                CASE Money                   'Skip if not in legal table
                   IF INSTR(MoneyChars$, Ky$) = 0 THEN EXIT SUB

                CASE LogicalFld
                   Ky$ = UCASE$(Ky$)            'Make it upper case
                   IF Frm.KeyCode = 32 THEN     'Space bar pressed. Toggle it
                      IF INSTR(LogicChars$, Form$(Frm.FldNo, 0)) = 2 THEN
                         Ky$ = MID$(LogicChars$, 1, 1)
                      ELSE
                         Ky$ = MID$(LogicChars$, 2, 1)
                      END IF
                   END IF                       'Skip if not one of the two
                   IF INSTR(LogicChars$, Ky$) = 0 THEN EXIT SUB
                   FirstKey = -1                'Force previous letter to be
                                                '  cleared
                CASE MultFld
                   'GOSUB SetupMult
                   FESetupMult Form$(), Fld(), Frm, Cnf
                   StuffBuf Ky$
                   EXIT SUB

                CASE ELSE
                   Ky$ = ""
             END SELECT

             IF FirstKey THEN                   'Blank if first key and
                SELECT CASE FType               '  number field
                   CASE IntFld, LongIntFld, SngFld, DblFld, MoneyFld', LogicalFld
                      LSET Form$(Frm.FldNo, 0) = ""
                      Frm.TxtPos = 1
                   CASE ELSE
                END SELECT
             END IF


             IF EdType <> ButtonFld THEN
                IF Frm.InsStat THEN             'Expand the text string
                   Nd = INSTR(Frm.TxtPos, Mask$, "")
                   IF Nd = 0 THEN Nd = LEN(Form$(Frm.FldNo, 0)) + 1
                   MID$(Form$(Frm.FldNo, 0), Frm.TxtPos) = Ky$ + MID$(Form$(Frm.FldNo, 0), Frm.TxtPos, Nd - Frm.TxtPos - 1)
                ELSE                            'Put the new letter in string
                   MID$(Form$(Frm.FldNo, 0), Frm.TxtPos) = Ky$
                END IF

                LOCATE , LeftCol, 0             'Print the new letter
                MQPrint Form$(Frm.FldNo, 0), -1
                Frm.FldEdited = -1

                IF EdType = LogicalFld THEN     'If this is a Logical field,
                                                '  check formulas for reference
                   CalcFields Frm.StartEl, Frm.FldNo, Form$(), Fld()
                   IF Frm.KeyCode = 32 THEN Frm.TxtPos = Frm.TxtPos - 1
                END IF

                IF FType <> MultChAFld THEN
                   IF NOT StayOnField OR Frm.TxtPos < LEN(Form$(Frm.FldNo, 0)) THEN
                      Frm.TxtPos = Frm.TxtPos + 1 'Advance the cursor
                   END IF
                END IF
             END IF

          '----- Left arrow
          CASE -75
             Frm.TxtPos = Frm.TxtPos - 1        'Decrement the text pointer

          '----- Right arrow
          CASE -77
             IF Fld(Frm.FldNo).FType = Button THEN
                Frm.FldNo = Frm.FldNo + 1       'Bump field number
             ELSE
                Frm.TxtPos = Frm.TxtPos + 1     'Increment the text pointer
             END IF


          '----- Home
          CASE -71
             Frm.TxtPos = 1                     'Move text pointer to 1

          '----- End
          CASE -79                              'Look backwards for non-blank
             FOR N = LEN(Form$(Frm.FldNo, 0)) TO 1 STEP -1
                IF ASCII(MID$(Form$(Frm.FldNo, 0), N, 1)) <> 32 THEN EXIT FOR
             NEXT
             Frm.TxtPos = MinInt(N + 1, LEN(Form$(Frm.FldNo, 0)))

          '----- Insert key
          CASE -82
             Frm.InsStat = NOT Frm.InsStat      'Toggle the Insert state
             GOSUB SetCursor

          '----- Delete
          CASE -83                              'Truncate the text
             IF EdType <> MultFld AND EdType <> ButtonFld THEN
                GOSUB DeleteChar                'Print the truncated part
                MQPrint MID$(Form$(Frm.FldNo, 0), Frm.TxtPos), -1
                Frm.FldEdited = -1
             END IF

          '----- Up Arrow Procedure
          CASE -72
             FEUpArrow Fld(), Frm

          '----- Down Arrow Proceedure
          CASE -80
             FEDownArrow Fld(), Frm

          '----- Enter or Tab
          CASE 13
             SELECT CASE EdType
                CASE MultFld                    'If on a Multiple choice field
                   'GOSUB SetupMult
                   FESetupMult Form$(), Fld(), Frm, Cnf
                   Frm.KeyCode = 0
                CASE ELSE                       'Regular fields
                   Frm.FldNo = Frm.FldNo + 1    'Bump field number
             END SELECT

          '----- Tab
          CASE 9
             Frm.FldNo = Frm.FldNo + 1          'Bump field number

          '----- Shift Tab pressed
          CASE -15
             Frm.FldNo = Frm.FldNo - 1          'Decrement field number

          '----- Ctrl Home
          CASE -119
             Frm.FldNo = Frm.StartEl + 1        'Set to first field

          '----- Ctrl End
          CASE -117
             Frm.FldNo = EndOfForm              'Set to last field

          '----- Page up
          CASE -73
             Frm.FldNo = Frm.StartEl

          '----- Page down
          CASE -81
             Frm.FldNo = EndOfForm + 1

          '----- F1 - Help
          CASE -59
             IF LEN(Msg$) = 0 THEN
                IF LEN(Form$(Frm.FldNo, 1)) THEN    'If this field has a help $,
                   Msg$ = Form$(Frm.FldNo, 1)       '  use it.
                ELSE
                   Msg$ = "No help for this field."
                END IF
                Message Msg$                    'Display the message
             END IF

          CASE ELSE                             'Unknown key?
             TermKey = -1                       'Set flag
       END SELECT

       FirstKey = 0                             'No longer the first key for
                                                '  field
    END IF

    '----- See if the cursor has moved
    Movement = Frm.TxtPos - PosWas
    IF Movement THEN
       IF Frm.PrevFld = Frm.FldNo THEN

          '----- Adjust position
          DO
             '----- Did we get past the end of the Edit$?
             IF Frm.TxtPos > LEN(Form$(Frm.FldNo, 0)) THEN
                Frm.FldNo = Frm.FldNo + 1       'Bump the field number
                Frm.TxtPos = 1
                EXIT DO
             '----- Did we get past the beginning of the Edit$?
             ELSEIF Frm.TxtPos < 1 THEN
                Frm.FldNo = Frm.FldNo - 1       'Decrement field number
                Frm.TxtPos = 1
                EXIT DO
             END IF
             '----- Skip over delimiters
             IF ASCII(MID$(Mask$, Frm.TxtPos)) <> 29 THEN EXIT DO
             Frm.TxtPos = Frm.TxtPos + Movement
             IF FType = DateFld OR FType = EuroDateFld THEN
                IF Frm.TxtPos = 7 THEN Frm.TxtPos = 9
             END IF
          LOOP

       END IF
       PosWas = Frm.TxtPos
    END IF



    '----- Did we move to a new field or encounter an unknown key
    IF Frm.FldNo <> Frm.PrevFld OR TermKey THEN

       '----- Erase prvious messages
       IF LEN(Msg$) THEN
          Msg$ = ""
          Message Msg$                          'Null string clears box
       END IF

       '----- Were we on a "Notes" field?
       IF Fld(Frm.PrevFld).FType = NotesFld THEN
                                                'Paint the window to clear
                                                '  painted block
          MPaintBox Fld(Frm.PrevFld).Row, Fld(Frm.PrevFld).LCol, Fld(Frm.PrevFld).ScratchI, Fld(Frm.PrevFld).RCol, Ed.AColor

          Frm.FldEdited = Ed.Changed            'Copy editors "Changed" flag
          IF Frm.FldEdited THEN                 'Was the text edited?
             Form$(Frm.PrevFld, 0) = ""         'Un-wrap the text and put it
             Temp = UBOUND(Work$)               '  in the form array
             Ed.LCount = FindLast%(VARPTR(Work$(Temp)), Temp)
             FOR N = 1 TO Ed.LCount
                 Form$(Frm.PrevFld, 0) = Form$(Frm.PrevFld, 0) + Work$(N) + ""
             NEXT
          END IF
                                                'If moving to a new field,
          'IF Frm.FldNo <> Frm.PrevFld THEN ERASE Work$ ' erase the array

          Frm.InsStat = Ed.InsStat = -1         'Copy QEdit's insert state
       END IF

       '----- If the field was edited, save and print it.
       IF Frm.FldEdited THEN
          Frm.Edited = -1                       'Set "Form Changed" flag
                                                'Save the field into buffer
                                                '  and format numbers
          SaveField Frm.PrevFld, Form$(), Fld(), BadFld

          IF BadFld THEN SWAP Form$(Frm.PrevFld, 0), FieldWas$
         

          IF Fld(Frm.PrevFld).FType <> NotesFld THEN  'If it's not a "Notes" field
                                                '  print it.
             PrintArray Frm.PrevFld, Frm.PrevFld, Form$(), Fld()
          END IF

          '----- Display message if value out of range
          IF BadFld THEN
             '----- Make Range strings for message
             IF FType = DateFld THEN            'US date field
                Temp$ = Num2Date$(CINT(Fld(Frm.PrevFld).LowRange))
                Temp2$ = Num2Date$(CINT(Fld(Frm.PrevFld).HiRange))
             ELSEIF FType = EuroDateFld THEN    'European date field
                Temp$ = ENum2Date$(CINT(Fld(Frm.PrevFld).LowRange))
                Temp2$ = ENum2Date$(CINT(Fld(Frm.PrevFld).HiRange))
             ELSE                               'Number fields
                Temp$ = STR$(Fld(Frm.PrevFld).LowRange)
                Temp2$ = STR$(Fld(Frm.PrevFld).HiRange)
             END IF
            
             '----- Make the message string
             IF BadFld = -1 THEN
                Msg$ = "Value out of range!  ÄÄÄ(" + FieldWas$ + ")" + CHR$(13)
                Msg$ = Msg$ + Temp$ + " .. " + Temp2$
             ELSE
                Msg$ = "Invalid Date!  ÄÄÄ(" + FieldWas$ + ")"
             END IF

             Message Msg$                       'Display the message box
             BEEP

             LeftCol = Fld(Frm.PrevFld).LCol    'Reset editing parameters
             Frm.TxtPos = 1
             FirstKey = -1
             Frm.FldNo = Frm.PrevFld
             Frm.KeyCode = 0
             EXIT SUB                           'Bail out
          END IF

          '----- Recalculate fields.  NOTE: This routine can be called from
          '      your module after changing a number.
          IF UBOUND(Form$, 2) THEN
             CalcFields Frm.StartEl, Frm.PrevFld, Form$(), Fld()
          END IF
       END IF

    END IF


    '----- Make sure we stay within the form
    IF Frm.FldNo > EndOfForm THEN               'Off the end?
       GOSUB SetNextPage
    ELSEIF Frm.FldNo <= Frm.StartEl THEN        'Past the beginning?
       GOSUB SetPrevPage
    END IF

   
    EXIT SUB


DeleteChar:
    Nd = INSTR(Frm.TxtPos + 1, Mask$, "")      'Look for skip character
    IF Nd = 0 THEN Nd = LEN(Form$(Frm.FldNo, 0)) + 1

    MID$(Form$(Frm.FldNo, 0), Frm.TxtPos) = MID$(Form$(Frm.FldNo, 0), Frm.TxtPos + 1, Nd - Frm.TxtPos - 1) + " "
RETURN


'----- Adjust the cursor size
SetCursor:
    Instate = Frm.InsStat
    IF Frm.InsStat THEN
       LOCATE , , , CsrSize \ 2, CsrSize
    ELSE
       LOCATE , , , CsrSize - 1, CsrSize
    END IF
RETURN


'-----
ForwardSkipProtected:
    DO WHILE Fld(Frm.FldNo).Protected
       Frm.FldNo = Frm.FldNo + 1
       IF Frm.FldNo > EndOfForm THEN
          GOSUB SetNextPage
       END IF
    LOOP
RETURN

BackwardSkipProtected:
    DO WHILE Fld(Frm.FldNo).Protected
       Frm.FldNo = Frm.FldNo - 1
       IF Frm.FldNo <= Frm.StartEl THEN
          GOSUB SetPrevPage
       END IF
    LOOP
RETURN




SetNextPage:

    N = Frm.StartEl + Fld(Frm.StartEl).Fields + 1
    IF N < UBOUND(Fld) THEN
       IF Fld(N).Fields THEN
          Frm.StartEl = N
          Frm.FldNo = Frm.StartEl + 1
          EndOfForm = N + Fld(N).Fields
          IF Fld(Frm.FldNo).Protected AND SkipProtected THEN
             GOSUB ForwardSkipProtected
          END IF
          Action = 1
          EXIT SUB
       END IF
    END IF
    Frm.FldNo = EndOfForm
    Frm.PrevFld = 0
    IF Fld(Frm.FldNo).Protected AND SkipProtected THEN
       GOSUB BackwardSkipProtected
    END IF

RETURN

SetPrevPage:

    FOR N = Frm.StartEl - 1 TO 0 STEP -1
       IF Fld(N).FType = 0 THEN EXIT FOR
    NEXT

    IF N < Frm.StartEl AND N >= 0 THEN
       Frm.FldNo = Frm.StartEl - 1
       Frm.StartEl = N
       EndOfForm = N + Fld(N).Fields
       GOSUB BackwardSkipProtected
       Action = 1
       EXIT SUB
    ELSE
       Frm.FldNo = Frm.StartEl + 1
       IF (Fld(Frm.FldNo).Protected AND SkipProtected) THEN
          GOSUB ForwardSkipProtected
       END IF
    END IF

RETURN

END SUB

FUNCTION EndOfForms (Fld() AS FieldInfo) STATIC

    S = 0
    DO
       LFld = S + Fld(S).Fields
       IF LFld + 1 < UBOUND(Fld) THEN
          IF Fld(LFld + 1).Fields THEN
             S = LFld + 1
          END IF
       END IF
    LOOP WHILE S = LFld + 1

    EndOfForms = LFld

END FUNCTION

SUB FEDownArrow (Fld() AS FieldInfo, Frm AS FormInfo)
    SHARED BottomRow, EndOfForm

    IF Frm.FldNo < EndOfForm THEN               'If not on last field
       Row = Fld(Frm.FldNo).Row                 'Save current row #
       Col = POS(0)                             'Save column #
       NxtRow = BottomRow                       'Init. "Next Row" variable
                                                'Examine all field's row #ers
                                                '  to find the next row down
       FOR N = Frm.StartEl + 1 TO EndOfForm
          IF Fld(N).Row > Row AND NOT (SkipProtected AND Fld(N).Protected) THEN
             NxtRow = MinInt(NxtRow, Fld(N).Row)
          END IF
       NEXT
       IF NxtRow > Row THEN                     'If there is a field below ours
                                                'Make an array to hold all field
          REDIM Temp(Fld(Frm.StartEl).Fields)
          X = 0                                 '  #ers on that row
          FOR N = Frm.StartEl + 1 TO EndOfForm
             IF Fld(N).Row = NxtRow AND NOT (SkipProtected AND Fld(N).Protected) THEN
                X = X + 1
                Temp(X) = N
             END IF
          NEXT
                                                'Look backwards for the field
          FOR N = X TO 2 STEP -1                '  above ours
             IF Fld(Temp(N)).LCol <= Col THEN EXIT FOR
          NEXT

          Frm.FldNo = Temp(N)                   'Assign the new field
       ELSE                                     'Otherwise set to last field
          Frm.FldNo = EndOfForm
       END IF
    ELSE
       Frm.FldNo = EndOfForm + 1
    END IF

END SUB

SUB FEInitForm (Form$(), Fld() AS FieldInfo, Frm AS FormInfo)

    SHARED EndOfForm, ScrRows, TopRow, BottomRow, NPixLines, Instate
    SHARED Ed AS EditInfo

    Frm.Edited = 0                              'New form hasn't been edited
    GetVMode 0, 0, 0, ScrRows, 0                'See how many lines on screen
    MouseTrap 1, 1, ScrRows, 80                 'Let mouse use full screen
    NPixLines = 8                               'Set mouse pixels/character
    IF ScrRows > 43 AND DOSVer% >= 1000 THEN NPixLines = 7

    Instate = Frm.InsStat
    IF Frm.InsStat THEN                         'Set QEdit's insert state
       Ed.InsStat = Frm.InsStat
    ELSE
       Ed.InsStat = 1
    END IF

    Frm.PrevFld = 0                             'Set previous field # to 0
                                                'Check current field #
    IF Frm.FldNo <= Frm.StartEl THEN Frm.FldNo = Frm.StartEl + 1
                                                'Pad the record buffer
    IF LEN(Form$(0, 0)) = 0 THEN
       Form$(0, 0) = SPACE$(Fld(0).Row)
    END IF

    EndOfForm = Frm.StartEl + Fld(Frm.StartEl).Fields
    BottomRow = 0
    TopRow = 60

    '----- Check all field strings for length and format
    FOR N = 1 TO EndOfForms(Fld())

        IF Fld(N).FType = 0 THEN N = N + 1

        BottomRow = MaxInt(BottomRow, Fld(N).Row)
        TopRow = MinInt(TopRow, Fld(N).Row)

        '----- Skip Notes fields and Buttons
        IF Fld(N).FType <> NotesFld AND Fld(N).FType <> Button THEN
           Temp = LEN(Form$(N, 0))              'Get current length
           Temp2 = Fld(N).RCol - Fld(N).LCol + 1

           IF Temp < Temp2 THEN                 'field too short?
              Form$(N, 0) = Form$(N, 0) + SPACE$(Temp2 - Temp)
           ELSEIF Temp > Temp2 THEN             'field too long
              Form$(N, 0) = LEFT$(Form$(N, 0), Temp2)
           END IF

           SELECT CASE Fld(N).FType             'Check formating
              CASE DateFld, EuroDateFld
                 MID$(Form$(N, 0), 3) = "-"
                 MID$(Form$(N, 0), 6) = "-"
                 IF MID$(Form$(N, 0), 7, 2) = "  " THEN
                    MID$(Form$(N, 0), 7, 2) = "19"
                 END IF
              CASE PhoneFld
                 MID$(Form$(N, 0), 1) = "("
                 MID$(Form$(N, 0), 5) = ")"
                 MID$(Form$(N, 0), 10) = "-"
              CASE SoSecFld
                 MID$(Form$(N, 0), 4) = "-"
                 MID$(Form$(N, 0), 7) = "-"
              CASE ZipFld
                 MID$(Form$(N, 0), 6) = "-"
              CASE ELSE
           END SELECT

                                                'Save field into buffer
           SaveField N, Form$(), Fld(), BadFld

        END IF

    NEXT
                                                'Display the field on form
    PrintArray Frm.StartEl + 1, EndOfForm, Form$(), Fld()

END SUB

SUB FEPressButton (Fld AS FieldInfo)

   Cols = Fld.RCol - Fld.LCol + 1
   REDIM Btn(Cols)
   REDIM Btn2(Cols)
   MScrnSave Fld.Row, Fld.LCol, Fld.Row, Fld.RCol, Btn(1)
   
   FOR I = 1 TO Cols

      Car = Btn(I) MOD 256
      Clr = Btn(I) \ 256

      SplitColor Clr, Fg, Bg

      IF Car = 221 AND Fg <= 8 THEN
         Fg = 15
      ELSEIF Car = 222 AND Fg > 8 THEN
         Fg = 8
      ELSE
         IF Fg > 7 THEN Fg = Fg - 7
         SWAP Fg, Bg
      END IF

      Clr = (Fg AND 16) * 8 + ((Bg AND 7) * 16) + (Fg AND 15)

      Btn2(I) = (Clr * 256) + Car MOD 256
   NEXT

   MScrnRest Fld.Row, Fld.LCol, Fld.Row, Fld.RCol, Btn2(1)

   Pause 2
   DO
      GetCursor 0, 0, I
   LOOP WHILE I

   MScrnRest Fld.Row, Fld.LCol, Fld.Row, Fld.RCol, Btn(1)

END SUB

SUB FESetupMult (Form$(), Fld() AS FieldInfo, Frm AS FormInfo, Cnf AS Config)
    SHARED Work$()
    SHARED Choice$()                            'Multiple choice array
    SHARED VertLen, VAction, Choice, ScrRows

    FOR C = 0 TO UBOUND(Choice$, 2)             'Find the array column that
        N = 0                                   '  goes with this field
        DO
           IF VAL(MID$(Choice$(0, C), N + 1)) = Frm.FldNo THEN EXIT FOR
           N = INSTR(N + 1, Choice$(0, C), ",")
        LOOP WHILE N
    NEXT

    IF C <= UBOUND(Choice$, 2) THEN             'If we found it, make a temp.
       REDIM Work$(UBOUND(Choice$))             '  array to hold choices
       Temp$ = SPACE$(Fld(Frm.FldNo).StorLen)
       Choice = 1                               'Preset Choice to 1
       Row = 0                                  'Init number of rows
       VertLen = 0
       FOR N = 1 TO UBOUND(Choice$)             'Read choices into array
           IF LEN(Choice$(N, C)) THEN
              LSET Temp$ = Choice$(N, C)
                                                'If current $ matches Choice $
              IF Temp$ = Form$(Frm.FldNo, 0) THEN Choice = N
              Work$(N) = Choice$(N, C)
              VertLen = MaxInt(LEN(Work$(N)), VertLen)
              Row = Row + 1                     'Bump number of rows
           ELSE
              EXIT FOR
           END IF
       NEXT
       Temp$ = ""
       Frm.DoingMult = -1                       'Set Multiple choice flag
       VAction = 1                              'Set VertMenu's Init. Flag
                                                'Calc position for menu

       IF Fld(Frm.FldNo).RCol + 1 > 80 - VertLen - 5 THEN
          C = 80 - VertLen - 5
          Rw = Fld(Frm.FldNo).Row + 1
       ELSE
          C = Fld(Frm.FldNo).RCol + 1
          Rw = Fld(Frm.FldNo).Row
       END IF
       IF Rw > ScrRows - 4 THEN
          Rw = MinInt(Rw, MaxInt(1, ScrRows - Row - 2))
       END IF
       LOCATE Rw, C, 0

    ELSE
       Frm.FldNo = Frm.FldNo + 1                'Bump field number
    END IF

END SUB

SUB FESetUpNotes (Form$(), Fld() AS FieldInfo, Frm AS FormInfo)

    SHARED Work$(), Ed AS EditInfo

    Ed.Changed = 0
                                                'Set number of rows
    Ed.Rows = Fld(Frm.FldNo).ScratchI - Fld(Frm.FldNo).Row + 1
                                                'Set width
    Ed.Wide = Fld(Frm.FldNo).RCol - Fld(Frm.FldNo).LCol + 1
    Ed.Wrap = Ed.Wide - 1                       'Set word wrap column

    X& = FRE(Temp$) - 2000                      'Create array to hold text
    REDIM Work$((X& \ 2 + LEN(Form$(Frm.FldNo, 0))) \ Ed.Wrap)
    Work$(1) = Form$(Frm.FldNo, 0)              'Put text in first line.
                                                '  QEdit will format it later
    HideCursor                                  'Find the color of edit window
    Ed.AColor = SCREEN(Fld(Frm.FldNo).Row, Fld(Frm.FldNo).LCol, 1)
    ShowCursor
    Ed.Frame = 0                                'No frame around edit window
    IF Frm.InsStat THEN                         'Set QEdit's insert state
       Ed.InsStat = Frm.InsStat
    ELSE
       Ed.InsStat = 1
    END IF
    Ed.Presses = 0                              'No mouse presses yet
                                                'Put cursor at upper left
    LOCATE Fld(Frm.FldNo).Row, Fld(Frm.FldNo).LCol, 0

    Ed.TL = 1
    Ed.LC = 1
    Ed.CurLine = 1
    Ed.CurCol = 1

END SUB

SUB FEUpArrow (Fld() AS FieldInfo, Frm AS FormInfo)
    SHARED TopRow, EndOfForm

    IF Frm.FldNo > Frm.StartEl + 1 THEN         'If not on first field
       Row = Fld(Frm.FldNo).Row                 'Save current row #
       Col = POS(0)                             'Save column #
       NxtRow = TopRow                          'Init. "Next Row" variable
                                                'Examine all field's row #ers
                                                '  to find the next row up
       FOR N = Frm.StartEl + 1 TO EndOfForm
          IF Fld(N).Row < Row AND NOT (SkipProtected AND Fld(N).Protected) THEN
             NxtRow = MaxInt(NxtRow, Fld(N).Row)
          END IF
       NEXT
       IF NxtRow < Row THEN                     'If there is a field above ours
                                                'Make an array to hold all field
          REDIM Temp(Fld(Frm.StartEl).Fields)
          X = 0                                 '  #ers on that row
          FOR N = Frm.StartEl + 1 TO EndOfForm
              IF Fld(N).Row = NxtRow AND NOT (SkipProtected AND Fld(N).Protected) THEN
                 X = X + 1
                 Temp(X) = N
              END IF
          NEXT
                                                'Look backwards for the field
          FOR N = X TO 2 STEP -1                '  above ours
              IF Fld(Temp(N)).LCol <= Col THEN EXIT FOR
          NEXT
          Frm.FldNo = Temp(N)                   'Assign the new field
       ELSE                                     'Otherwise set to field #1
          Frm.FldNo = Frm.StartEl + 1
       END IF

    ELSE
       Frm.FldNo = Frm.FldNo - 1
    END IF

END SUB

'----- Fixes improperly formatted date strings
'
SUB FixDate (Dat$) STATIC
      
    Psn = 1                                     'Examine first character
    DO                                          'Trim the number part
       Temp$ = LTRIM$(RTRIM$(MID$(Dat$, Psn, 2)))
       IF LEN(Temp$) = 1 THEN                   'If it's only one character,
          MID$(Dat$, Psn) = "0" + Temp$         'Add a leading 0
       END IF
       MID$(Dat$, Psn + 2) = "-"                'Force "-" for delimiter
       Psn = Psn + 3                            'Set to next part
    LOOP UNTIL Psn > 4

    Temp$ = LTRIM$(RTRIM$(MID$(Dat$, 7)))       'Get the trimmed year part
    IF LEN(Temp$) < 4 THEN                      'If its not 4 characters,
       IF Temp$ <> "19" AND Temp$ <> "20" THEN  '  add century to year
          MID$(Dat$, 7) = "19" + Temp$
       END IF
    END IF
    Temp$ = ""

END SUB

'----- Returns the field number for a given field name.
'
FUNCTION FldNum% (FldName$, Fld() AS FieldInfo) STATIC
    
    IF LEN(Test$) = 0 THEN Test$ = SPACE$(8)
    LSET Test$ = UCASE$(FldName$)

    FOR N = 1 TO Fld(0).Fields
       IF Fld(N).FldName = Test$ THEN EXIT FOR
    NEXT

    IF N <= Fld(0).Fields THEN
       FldNum = N
    ELSE
       FldNum = 0
    END IF

END FUNCTION

'----- Formats a number into a "Field" string (Image$)
'
SUB Format (Float#, Fld AS FieldInfo, Image$) STATIC

    FType = Fld.FType                    'Get the field's type
    IF FType = Relational THEN FType = Fld.ScratchI
                                                'If places greater than -1
    IF Fld.Decimals > -1 THEN                   '  do formatting
       EffLen = LEN(Image$)                     'Find effective length
       LSET Image$ = STRING$(EffLen, "#")       'Fill field with "#"s

       IF Fld.Decimals THEN                     'If there are decimal places
          EffLen = EffLen - Fld.Decimals - 1    'Find place to put ",."
                                                'Put them in field
          IF EffLen > 0 THEN MID$(Image$, EffLen) = ",."
       ELSE                                     'Otherwise
                                                '  put a "," at end of string
          IF EffLen > 0 THEN MID$(Image$, EffLen) = ","
       END IF

       IF FType = MoneyFld THEN MID$(Image$, 1) = "$$" 'If this is a currency field,
                                                '  put "$$" at beginning
       LSET Image$ = FUsing$(STR$(Float#), Image$)'Format the number into the
                                                '  field
       IF FType = MoneyFld THEN
          IF INSTR(Fld.ScratchS, "$") = 0 THEN
             Temp$ = QPRTrim$(Fld.ScratchS)
             D = INSTR(Image$, "$")
             P = D - LEN(Temp$) + 1
             IF LEN(Temp$) > 1 THEN
                Temp$ = Temp$ + " "
                P = P - 1
             END IF
             P = MaxInt(1, P)
             MID$(Image$, P, D) = Temp$
          END IF
       END IF

    ELSE                                        'Otherwise let basic make the
       RSET Image$ = LTRIM$(STR$(Float#))       '  string
    END IF

END SUB

'----- Displayes or clears a message within a box on screen
'
SUB Message (Msg$) STATIC

    IF LEN(Msg$) THEN                           'If the message isn't null
                                                'Are we using color or mono
       IF ABS(Peek1%(0, &H463) = &HB4 OR INSTR(COMMAND$, "/B")) THEN
          FrameClr = 112                        'Monochrome colors
          TextClr = 112
       ELSE                                     'Color monitor
          FrameClr = 49
          TextClr = 48
       END IF

       Row = CSRLIN + 1                         'Save the Row

       REDIM MArray$(4)                         'Make a temporary array to
                                                '  hold the parsed up lines
       MesLen = LEN(Msg$)                       'Get the total length of the
       Wdth = MinInt(64, MesLen)
                                                '  help message
       Ln = 0                                   'Line number
       Strt = 1                                 'Starting parse position
       '*** Parse the message into lines
       DO
           Nd = Strt + Wdth                     'Guess at the end of line

           CR = INSTR(Strt, Msg$, CHR$(13))     'Look for Chr$(13)
           IF CR > 0 AND CR < Nd THEN           'Chop line off
              Nd = CR
           ELSE                                 'Loop backwards to find
              WHILE MID$(Msg$, Nd, 1) <> " " AND Nd < MesLen AND Nd > Strt
                  Nd = Nd - 1                   '  blank space
              WEND
           END IF

           Ln = Ln + 1                          'Increment the Line number
                                                'Pull out the new line
           MArray$(Ln) = MID$(Msg$, Strt, Nd - Strt)
           Strt = Nd + 1                        'Reset starting posit. for
                                                '  next line
       LOOP UNTIL Nd >= MesLen                  'Get more unless @ end of mes

       Wdth = 0
       FOR N = 1 TO Ln
           Wdth = MaxInt(Wdth, LEN(MArray$(N)))
       NEXT

       GetVMode 0, 0, 0, Rows, 0
       BotRow = Row + Ln + 1                    'Find the bottom row #
       IF BotRow >= Rows THEN
          Row = Row - Ln - 4
          BotRow = Row + Ln + 1
       END IF

       BoxWdth = Wdth + 2                       'Find the outer box width
       ColPos = 40 - (BoxWdth \ 2)              'Starting position for
                                                '   centered box
       ArraySize = (BoxWdth + 2) * (BotRow - Row + 2)'Find the array size to use
       REDIM ScrBuf(ArraySize)                  '  for the background screen
       MScrnSave Row, ColPos, BotRow + 1, ColPos + BoxWdth + 1, ScrBuf(0)
       HideCursor
       Box0 Row, ColPos, BotRow, ColPos + BoxWdth - 1, 1, FrameClr

       LOCATE Row + 1, ColPos + 1
       APrint0 VARPTR(MArray$(1)), Ln, 1, Wdth, TextClr
       ShowCursor

       MPaintBox Row + 1, ColPos + BoxWdth, N + Row, ColPos + BoxWdth + 1, 8
       MPaintBox N + Row + 1, ColPos + 2, N + Row + 1, ColPos + BoxWdth + 1, 8

       Pass = 1                                 'Set flag say we've been here
       ERASE MArray$                            'Don't need this anymore

    ELSEIF Pass = 1 THEN                        'If we've been here before,
                                                '  restore the screen
       MScrnRest Row, ColPos, BotRow + 1, ColPos + BoxWdth + 1, ScrBuf(0)
       ERASE ScrBuf                             'Clean up memory
       Pass = 0                                 'Reset flag for next call
    END IF

END SUB

'****************************************************************************
'Prints the contents of fields on the screen form.  This routine is used
'internally by [EditForm] but can be called from your program to force a
'redisplay after altering any data.
'
'FirstFld is the first field number to print.
'
'LastFld is the last field to print.  These two parameters are used to
'  specify a range of fields to print.
'
'Form$() is the form data array containing the text of all fields.
'
'Fld() is the field definition TYPE array.
'****************************************************************************
'
SUB PrintArray (FirstFld, LastFld, Form$(), Fld() AS FieldInfo) STATIC
    SHARED Ed AS EditInfo, Instate

    LOCATE , , 0                                'Turn the cursor off

    FirstFld = MaxInt(1, FirstFld)              'Make sure first and last are
    IF LastFld = 0 THEN LastFld = EndOfForms%(Fld()) '  set

    FOR N = FirstFld TO LastFld                 'Do each field within range

        IF Fld(N).FType = 0 THEN N = N + 1

        LOCATE Fld(N).Row, Fld(N).LCol          'Position the cursor

        SELECT CASE Fld(N).FType
        CASE NotesFld                           'Is this a "Notes" field

           IF Instate THEN                      'Set QEdit's insert state
              Ed.InsStat = Instate
           ELSE
              Ed.InsStat = 1
           END IF

           Ed.Rows = Fld(N).ScratchI - Fld(N).Row + 1
           Ed.Wide = Fld(N).RCol - Fld(N).LCol + 1
           Ed.Wrap = Ed.Wide - 1
           HideCursor
           Ed.AColor = SCREEN(Fld(N).Row, Fld(N).LCol, 1)
           
           Ed.Frame = 0

           X& = FRE("") - 1000                  'Create array to hold text
           REDIM Work$((X& \ 2 + LEN(Form$(N, 0))) \ Ed.Wrap)
           Work$(1) = Form$(N, 0)               'Put text in array

           'QEdit Work$(), "", 1, Ed             'Let QEdit display the text

           ShowCursor
           ERASE Work$                          'Erase the temporary array

        CASE IS <> Button                       'Regular fields
           MQPrint Form$(N, 0), -1              'Print the Form$ element

        CASE ELSE
        END SELECT
    NEXT

END SUB

'****************************************************************************
'Checks and converts a fields data and places it in the record buffer
'[Form$(0, 0)].  Numbers are converted to IEEE format for the buffer and also
'formatted for display and replaced in their element of the array.  Dates are
'converted to integers and check for validity.  Fields that contain sub-fields
'such as Phone Numbers and Zips Code are packed to remove delimiters before
'being placed in the buffer.
'
'Note: This routine is used internaly by [EditForm], but can be called by
'your program to check and format data before calling [PrintArray] to display.
'
'FldNo is the Field number to save.
'
'Form$() is the form data array.  Form$(FldNo, 0) must contain the data to be
'  checked, formatted and saved.
'
'Fld() is the field definition TYPE array.
'
'BadFld is a flag which will be returned with a -1 to indicate that the data
'  was invalid or out of range.
'****************************************************************************
'
SUB SaveField (FldNo, Form$(), Fld() AS FieldInfo, BadFld) STATIC

    BadFld = 0                                  'No problems yet

    FType = Fld(FldNo).FType                    'Get the field's type
    IF FType = Relational THEN FType = Fld(FldNo).ScratchI

    SELECT CASE FType                           'Branch according to type
                                                'Numeric fields
       CASE IntFld, LongIntFld, SngFld, DblFld, MoneyFld

          IF Null(Form$(FldNo, 0)) THEN         'If the field is blank, set
             SELECT CASE FType                  '  value to flag it as blank
                CASE IntFld
                   Temp$ = MKI$(BlankInt)
                CASE LongIntFld
                   Temp$ = MKL$(BlankLng&)
                CASE SngFld
                   Temp$ = MKS$(BlankSng!)
                CASE DblFld, MoneyFld
                   Temp$ = MKD$(BlankDbl#)
             END SELECT

          ELSE
                                                'Get the value of the string
             Float# = Value#(Form$(FldNo, 0), ErrCode)
                                                'Check for out of range
             IF ErrCode OR Float# > Fld(FldNo).HiRange OR Float# < Fld(FldNo).LowRange THEN
                BadFld = -1
                EXIT SUB
             ELSE
                                                'Format number back into array
                Format Float#, Fld(FldNo), Form$(FldNo, 0)
                                               
                SELECT CASE FType               'Set the buffer to IEEE format
                   CASE IntFld
                      Temp$ = MKI$(INT(Float#))
                   CASE LongIntFld
                      Temp$ = MKL$(CLNG(Float#))
                   CASE SngFld
                      Temp$ = MKS$(CSNG(Float#))
                   CASE DblFld, MoneyFld
                      Temp$ = MKD$(Float#)
                END SELECT
             END IF
          END IF

          MID$(Form$(0, 0), Fld(FldNo).Fields) = Temp$

       CASE DateFld, EuroDateFld

          FixDate Form$(FldNo, 0)               'Make date $ the proper format
                                                '  NN-NN-NNNN
                                                'Convert to an integer and
                                                '  check validity
          IF Form$(FldNo, 0) = BlankDate$ THEN
             Days = BlankInt
          ELSE
             IF FType = DateFld THEN            'US dates
                Days = Date2Num(Form$(FldNo, 0))
                BadFld = Num2Date$(Days) <> Form$(FldNo, 0)
             ELSE                               'European dates
                Days = EDate2Num(Form$(FldNo, 0))
                BadFld = ENum2Date$(Days) <> Form$(FldNo, 0)
             END IF
             BadFld = BadFld * 2
          END IF

          IF Days > BlankInt THEN               'Check range
             BadFld = BadFld OR Days > Fld(FldNo).HiRange OR Days < Fld(FldNo).LowRange
          END IF

          IF NOT BadFld THEN                    'Save number to buffer as IEEE
             MID$(Form$(0, 0), Fld(FldNo).Fields) = MKI$(Days)
          END IF

       CASE PhoneFld                            'Pack Phone number $
          MID$(Form$(0, 0), Fld(FldNo).Fields) = MID$(Form$(FldNo, 0), 2, 3) + MID$(Form$(FldNo, 0), 7, 3) + MID$(Form$(FldNo, 0), 11, 4)

       CASE SoSecFld                            'Pack Social Security number $
          MID$(Form$(0, 0), Fld(FldNo).Fields) = MID$(Form$(FldNo, 0), 1, 3) + MID$(Form$(FldNo, 0), 5, 2) + MID$(Form$(FldNo, 0), 8, 4)

       CASE ZipFld                              'Pack Zip Code $
          MID$(Form$(0, 0), Fld(FldNo).Fields) = MID$(Form$(FldNo, 0), 1, 5) + MID$(Form$(FldNo, 0), 7, 4)

       CASE NotesFld                            'Do nothing for Notes field

       CASE PropStrFld                          'Capitalize first letters of
          ProperName Form$(FldNo, 0)            '  words for Proper Name $
          MID$(Form$(0, 0), Fld(FldNo).Fields) = Form$(FldNo, 0)
         
       CASE UCaseStrFld                         'Capitalize Upper Case fields
          LSET Form$(FldNo, 0) = UCASE$(Form$(FldNo, 0))
          MID$(Form$(0, 0), Fld(FldNo).Fields) = Form$(FldNo, 0)

       CASE Button

       CASE ELSE                                'All others, just save text
          MID$(Form$(0, 0), Fld(FldNo).Fields) = Form$(FldNo, 0)

    END SELECT

END SUB

'****************************************************************************
'Copies and converts all data from the record buffer [Form$(0, 0)] and places
'it in the individual elements of the [Form$()] array.  This routine can be
'called from your program after reading a record from a random access data
'file (BASIC, Btrieve or ISAM) which uses Form$(0, 0) as the record buffer.
'Note:  If your programs do not use random access files, this routine can be
'deleted from this module.
'
'FirstFld is the field number to start unpacking.
'
'LastFld is the last field number to unpack.
'
'Form$() is the form data array to operate on.  Form$(0, 0) must contain all
'  data within the range for unpacking.  Numeric data must be in packed IEEE
'  form.  Dates must be IEEE integers.  Fields that contain sub-fields such
'  as Phone numbers and Zip Codes must not contain any delimiters.
'
'Fld() is the field definition TYPE array.
'****************************************************************************
'
SUB UnPackBuffer (FirstFld, LastFld, Form$(), Fld() AS FieldInfo) STATIC

    FirstFld = MaxInt(1, FirstFld)              'Check first and last params.

    IF LastFld = 0 THEN LastFld = EndOfForms(Fld())

    FOR N = FirstFld TO LastFld                 'Do each field within range

        IF Fld(N).FType = 0 THEN
           N = N + 1
        END IF

        FType = Fld(N).FType                    'Get the fields type
        IF FType = 11 THEN FType = Fld(N).ScratchI

        Length = Fld(N).RCol - Fld(N).LCol + 1
        IF LEN(Form$(N, 0)) <> Length AND FType <> NotesFld THEN
           Form$(N, 0) = SPACE$(Length)
        END IF

        SELECT CASE FType                       'Process acording to type
                                                'Number fields
           CASE IntFld, LongIntFld, SngFld, DblFld, MoneyFld
              SELECT CASE FType
                 CASE IntFld                    'Make a number
                    Float# = CVI(MID$(Form$(0, 0), Fld(N).Fields, 2))
                    Blank = Float# = BlankInt   'Check for a blank
                 CASE LongIntFld
                    Float# = CVL(MID$(Form$(0, 0), Fld(N).Fields, 4))
                    Blank = Float# = BlankLng&
                 CASE SngFld
                    Float# = CVS(MID$(Form$(0, 0), Fld(N).Fields, 4))
                    Blank = Float# = BlankSng!
                 CASE DblFld, MoneyFld
                    Float# = CVD(MID$(Form$(0, 0), Fld(N).Fields, 8))
                    Blank = Float# <= BlankDbl#
              END SELECT

              IF Blank THEN                     'If it isn't blank, format it
                 LSET Form$(N, 0) = ""
              ELSE
                 Format Float#, Fld(N), Form$(N, 0)
              END IF

           CASE DateFld                         'Get date number
              X = CVI(MID$(Form$(0, 0), Fld(N).Fields, 2))
              IF X > BlankInt THEN              'If it isn't blank, convert
                 LSET Form$(N, 0) = Num2Date$(X) '  number into date $
              ELSE                              'Otherwise,
                 LSET Form$(N, 0) = BlankDate$  '  make a blank date $
              END IF

           CASE EuroDateFld                     'Get date number
              X = CVI(MID$(Form$(0, 0), Fld(N).Fields, 2))
              IF X > BlankInt THEN              'If it isn't blank, convert
                 LSET Form$(N, 0) = ENum2Date$(X) '  number into date $
              ELSE                              'Otherwise,
                 LSET Form$(N, 0) = BlankDate$  '  make a blank date $
              END IF

           CASE PhoneFld
              LSET Form$(N, 0) = "(   )    -    " 'Make a phone # template then
                                                '  fill in the blanks
              MID$(Form$(N, 0), 2) = MID$(Form$(0, 0), Fld(N).Fields, 3)
              MID$(Form$(N, 0), 7) = MID$(Form$(0, 0), Fld(N).Fields + 3, 3)
              MID$(Form$(N, 0), 11) = MID$(Form$(0, 0), Fld(N).Fields + 6, 4)

           CASE SoSecFld
              LSET Form$(N, 0) = "   -  -    "  'Make a template then fill in
                                                '  the blanks
              MID$(Form$(N, 0), 1) = MID$(Form$(0, 0), Fld(N).Fields, 3)
              MID$(Form$(N, 0), 5) = MID$(Form$(0, 0), Fld(N).Fields + 3, 2)
              MID$(Form$(N, 0), 8) = MID$(Form$(0, 0), Fld(N).Fields + 5, 4)

           CASE ZipFld
              LSET Form$(N, 0) = "     -    "   'Make a template then fill in
                                                '  the blanks
              MID$(Form$(N, 0), 1) = MID$(Form$(0, 0), Fld(N).Fields, 5)
              MID$(Form$(N, 0), 7) = MID$(Form$(0, 0), Fld(N).Fields + 5, 4)

           CASE NotesFld, Button

           CASE ELSE                            'Just copy other field types
              LSET Form$(N, 0) = MID$(Form$(0, 0), Fld(N).Fields, Fld(N).StorLen)

        END SELECT

    NEXT

END SUB

'----- Returns the value of a numeric string which may contain non numeric
'      characters such as "$," also returns an error if value would cause
'      an overflow
'
FUNCTION Value# (E$, ErrCode) STATIC

    ErrCode = -1                                'Guilty until proven otherwise
    Value# = 0                                  'No value yet
   
    Temp$ = RTRIM$(E$)                          'Make a copy of string
    Ln = LEN(Temp$)                             'Save the length of string
    Sign = 0                                    'number of signs
    S1 = 0                                      'Mantissa's sign position
    Decm = 0                                    'Decimal position
    Xpon = 0                                    'Exponent position
    N = 0                                       'Number of digits
    X = 1                                       'Start at the first character

    DO UNTIL X > Ln                             'Check each character
                                                'See if character is in table
       Ok = INSTR(FloatChars$, MID$(Temp$, X, 1))

       IF N = 0 THEN
          IF Ok >= 15 AND Ok <= 18 THEN Ok = 0
       END IF

       SELECT CASE Ok                           'Branch according to position

          CASE 0, 1                             'Illegal character
             MID$(Temp$, X) = MID$(Temp$, X + 1)'Erase it
             MID$(Temp$, Ln) = " "
             X = X - 1                          'Back up one position
             Ln = Ln - 1                        'Decrement length

          CASE 2, 3                             'Sign character
             Sign = Sign + 1                    'Bump number of signs
             IF Sign > 2 THEN EXIT DO           'If too many signs, bail out
             IF Xpon = 0 THEN                   'Is this the mantissa's sign?
                S1 = X                          'Save position
             END IF

          CASE 4                                'Decimal place
             IF Decm = 0 THEN                   'If its the first decimal,
                Decm = X                        '  save it's position
             ELSE                               'Otherwise bail out
                EXIT DO
             END IF

          CASE 15 TO 18                         'Exponent character
             IF Xpon = 0 THEN                   'If its the first one,
                Xpon = X                        '  save it's position
             ELSE                               'Otherwise bail out
                EXIT DO
             END IF

          CASE ELSE
             N = N + 1
       END SELECT

       X = X + 1                                'Bump character pointer
    LOOP


    Whole = Decm - S1 - 1                       'Find number of digits before
                                                '  decimal place
    IF Whole > 308 THEN EXIT FUNCTION           'Bail out if too many

    IF Xpon THEN                                'If there is an exponent,
       Num# = VAL(LEFT$(Temp$, Xpon))           'Get mantissa's value
       Xpon = VAL(MID$(Temp$, Xpon + 1))        'Get exponents value

       Num# = Num# / (10 ^ (Whole - 1))         'Shift decimal to right of
                                                '  first digit
       Xpon = ABS(Xpon + Whole - 1)             'Adjust exponent

       IF Xpon > 308 THEN                       'See if number will overflow
          EXIT FUNCTION
       ELSEIF Xpon = 308 AND ABS(Num#) > 1.79769313486232# THEN
          EXIT FUNCTION
       END IF

    END IF

    Value# = VAL(Temp$)                         'Assign the function
    ErrCode = 0                                 'Clear error flag

END FUNCTION

