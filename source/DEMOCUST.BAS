'***** DEMOCUST.BAS *********************************************************
'Illustrates using "FormEdit" to allow data entry in a form.  Also
'demonstrates use of screen/form library files and random access file
'technics.  See also the "InputBox$" and "MsgBox%" functions that demonstrate
'popping a form on top of an existing form to get additional information from
'or display information to the user.

'Copyright (c) 1989 - 1991 Crescent Software, Inc.
'by Don Malin
'****************************************************************************

DEFINT A-Z

'$INCLUDE: 'DefCnf.bi'          'Defines a TYPE for monitor and color info..
'$INCLUDE: 'FieldInf.bi'        '        "          field information
'$INCLUDE: 'FormEdit.bi'        '        "          form editing information

'----- ASM Procedures from FORMS[7].QLB/LIB
DECLARE SUB BCopy (FromSeg%, FromAddr%, ToSeg%, ToAddr%, NumBytes%, Direction%)
DECLARE FUNCTION Exist% (FileName$)
DECLARE SUB HideCursor ()
DECLARE FUNCTION Monitor% ()
DECLARE SUB MScrnSave (UlRow, UlCol, BRRow, BRCol, SEG Address)
DECLARE SUB MScrnRest (UlRow, UlCol, BRRow, BRCol, SEG Address)
DECLARE SUB ShowCursor ()
DECLARE SUB TextCursor (Fg%, Bg%)

'----- Procedures from our Main Module
DECLARE FUNCTION InputBox$ (LibName$, FormName$)
DECLARE SUB LibLoadDisplayForm (LibName$, FormName$, Form$(), Fld() AS ANY)
DECLARE FUNCTION MsgBox% (LibName$, FormName$)

'----- Procedures from FORMLIB.BAS
DECLARE SUB LibGetFldDef (LibName$, ScrName$, StartEl, Fld() AS FieldInfo, Form$(), ErrCode)
DECLARE FUNCTION LibNumberOfFields% (LibName$, ScrName$)

'----- Procedures from RANDOMIO.BAS
DECLARE SUB GetRec (RecNo&, Form$(), Fld() AS ANY)
DECLARE SUB OpenFiles (FormName$, Form$(), Fld() AS ANY)
DECLARE SUB SaveRec (RecNo&, Form$(), Fld() AS ANY)

'----- Procedures from FORMEDIT.BAS
DECLARE SUB EditForm (Form$(), Fld() AS FieldInfo, Frm AS FormInfo, Cnf AS Config, Action%)
DECLARE SUB UnPackBuffer (FirstFld%, LastFld%, Form$(), Fld() AS FieldInfo)

'----- Procedure from SCRNLIB.BAS
DECLARE SUB LibFile2Scrn (LibName$, ScrnName$, MonoCode%, Attribute%, ErrorCode%)


'$INCLUDE: 'SetCnf.bi'                  'Sets monitor and color information

'----- Define the key codes we will use.
CONST F2Key = -60
CONST F3Key = -61
CONST F4KEY = -62
CONST ESC = 27

'----- Define mandatory arrays
DIM Frm AS FormInfo                     'See "FormEdit.bi" for more info.
REDIM Form$(0, 0)                       'Holds the data for the form
REDIM Fld(0) AS FieldInfo               'Holds information about the form
REDIM Choice$(0, 0)                     'Used to hold multiple choices


'----- Create a background screen.
CLS
PRINT STRING$(1999, "±");

'----- Turn the mouse cursor on
TextCursor -2, -2                       'Define the Mouse cursor
ShowCursor                              'Show it


'----- Display our data entry form from the library
LibName$ = "CUSTOMER.QSL"               'Specify a Screen Library file name.
IF NOT Exist(LibName$) THEN             'See if it is available
   PRINT LibName$; " not found!"
   END
END IF

FormName$ = "CUSTOMER"                  'Specify the screen to display


'----- Sizes the arrays defined above, loads the field definitions, and
'      displays the form.
LibLoadDisplayForm LibName$, FormName$, Form$(), Fld()


'----- Open/Field the data file as well as the notes file.
OpenFiles FormName$, Form$(), Fld()
                                        'Find the last record number
LastRec& = LOF(Fld(0).RelHandle) \ (Fld(0).Row)


'----- Clears the form, puts date in field, and sets record pointer
GOSUB ResetForm


'----- Set the "Action" flag to force the editor to initialize itself and
'      display the data on the form.
Action = 1

'----- The following is our form editing loop
DO
   EditForm Form$(), Fld(), Frm, Cnf, Action

   SELECT CASE Frm.KeyCode              'Check the user's key presses
      CASE F2Key
         GOSUB ResetForm                'Clear the form

      CASE F3Key
         GOSUB StoreRec                 'Save the form

      CASE F4KEY
         GOSUB RetrieveRec              'Get a previously saved record

      CASE ELSE                         'Ignor all other keys including ""
   END SELECT

LOOP UNTIL Frm.KeyCode = ESC            'Keep editing until the user presses
                                        '  the Escape key.

HideCursor                              'Turn the mouse cursor Off
END


'****************************************************************************
'************ Subroutines used to save and get BASIC records ****************
'****************************************************************************

'----- Clears the form and sets the record pointer to the end of the file.
ResetForm:

   GOSUB CheckSave                      'Prompt user to save if record changed

   FOR N = 1 TO Fld(0).Fields           'Clear all fields
      Form$(N, 0) = ""
   NEXT
   Form$(2, 0) = DATE$                  'Put today's date on the form
   RecNo& = LastRec& + 1                'Set current record # to end of file

   Action = 1                           'Force a redisplay of the form

RETURN


'----- Prompts the user to save the current record if it has changed
CheckSave:

   IF Frm.Edited THEN                   'If the form has changed, prompt save.
      IF MsgBox(LibName$, "SAVENOW") = 1 THEN
         GOSUB StoreRec                 'Save if "Y" was pressed "Y" = 1,
      END IF                            '  "N" = 2, and Esc = 0
      Action = 1
   END IF

RETURN


'----- Saves the current record
StoreRec:

   SaveRec RecNo&, Form$(), Fld()       'Save the currently displayed record
   IF RecNo& > LastRec& THEN LastRec& = RecNo&
   Frm.Edited = 0                       'Reset "Edited" flag

RETURN


'----- Get a previously saved record
RetrieveRec:

    GOSUB CheckSave                     'Prompt user to save if record changed

    IF LastRec& THEN                    'If there are records to retrieve,
                                        '  ask the user for the record number
       DO
          RecNo& = VAL(InputBox$(LibName$, "RETRIEVE"))

          IF RecNo& > LastRec& THEN     'Is record past the end?
             Temp = MsgBox(LibName$, "NOREC")
          ELSE
             EXIT DO
          END IF
       LOOP

       IF RecNo& THEN                   'If a record was specified,

          GetRec RecNo&, Form$(), Fld() 'Retrieve the record from disk

          '----- The following line of code can be used to copy the
          '      Form$(0, 0) file buffer to our TYPE record "Customer".
          '      This is completly optional.
          'BCopy VARSEG(Form$(0, 0)), SADD(Form$(0, 0)), VARSEG(Customer), VARPTR(Customer), LEN(Customer), 0
                 '^
                 'ÀÄSSEG for BASIC 7.x

                                        'Copy "Form" buffer to individual
                                        '  field elements
          UnPackBuffer 1, Fld(0).Fields, Form$(), Fld()
       END IF
    ELSE
                                        'Tell the user that there are no records
       Temp = MsgBox(LibName$, "NORECS")
    END IF
  
    Action = 1                          'Since the calls to InputBox$ &
                                        '  MsgBox$ above used EditForm, we
                                        '  need to re-initialize the customer
                                        '  form.
RETURN

'****************************************************************************
'Allows the user to enter a string of text into a field of a specified form.
'
'Inputs:
'  LibName$ - Name of the screen/form library file containing the input form.
'  FormName$ - Name of the input form.
'
'Returns:
'  The text of the field if enter was pressed, otherwise returns a nul string.
'
'Notes:
'  The underlying screen is automaticaly saved on entry and restored on exit.
'  The input form should have one entry field (any type) and two button
'  fields.  The first button should return an Enter key press (13) and the
'  second should return an Escape key (27).
'****************************************************************************
'
FUNCTION InputBox$ (LibName$, FormName$)

   SHARED Cnf AS Config
   REDIM Form$(0, 0)
   REDIM Fld(0) AS FieldInfo
   REDIM Choice$(0, 0)
   REDIM ScrBuf(2000)
 
   MScrnSave 1, 1, 25, 80, ScrBuf(0)       'Save the existing screen
                                         
                                           'Display the form
   LibLoadDisplayForm LibName$, FormName$, Form$(), Fld()

   DIM Frm AS FormInfo
   Action = 1

   DO                                      'Allow user to edit the form
      EditForm Form$(), Fld(), Frm, Cnf, Action
   LOOP UNTIL Frm.KeyCode = 13 OR Frm.KeyCode = 27

   MScrnRest 1, 1, 25, 80, ScrBuf(0)       'Restore the original screen

   IF Frm.KeyCode = 13 THEN                'If user pressed Enter,
      InputBox$ = Form$(1, 0)              '  return field 1's contents
   ELSE                                    'otherwise,
      InputBox$ = ""                       '  return nothing.
   END IF

END FUNCTION

'****************************************************************************
'This procedure simplifies loading and displaying data entry forms.  It will
'size the field information and definition arrays, loads them from a forms
'library (.QFL) and displays their screen from the screen library (.QSL).
'
'Inputs:
'  LibName$ - name of the screen/form library containing the form.
'  FormName$ - name of the form to load and display.
'
'Returns:
'  Form$() - field data array sized to the number of fields in the form and
'          filled with formulas and help messages.
'  Fld() - field information array sized to the number of fields in the form
'          and filled with their information.
'Actions:
'  Displays the specified form.
'****************************************************************************
'
SUB LibLoadDisplayForm (LibName$, FormName$, Form$(), Fld() AS FieldInfo)
 
   '----- Flag - Set to 3 if using a monochrome monitor or the program was
   '      started with "/b" in the command line.
   DEF SEG = 0
   MonoCode = ABS(PEEK(&H463) = &HB4 OR INSTR(COMMAND$, "/B")) * 3

   '----- Display the screen
   LibFile2Scrn LibName$, FormName$, MonoCode, 0, ErrorCode

   '----- Load the field definition file
                                        'Get the number of fields in form
   NumFlds = LibNumberOfFields(LibName$, FormName$)

   REDIM Form$(NumFlds, 2)              'DIM the form data array

   REDIM Fld(NumFlds) AS FieldInfo      'DIM the field information array. see
                                        '"FieldInf.BI" for structure
                                        'Load the field information
   LibGetFldDef LibName$, FormName$, 0, Fld(), Form$(), ErrCode

END SUB

'****************************************************************************
'Displays a form containing a message and waits for the user to press a key
'or button to acknowledge the message.
'
'Inputs:
'  LibName$ - Name of the screen/form library file containing the input form.
'  FormName$ - Name of the message form.
'
'Returns:
'  The number of the button used to acknowledge the message.
'
'Notes:
'  The underlying screen is automaticaly saved on entry and restored on exit.
'  The Message form should contain button fields only.  If you defined 3
'  buttons, where the first returned 65 ("A"bort), the second 82 ("R"etry)
'  and the third 73 ("I"gnore), and the user pressed "R" or clicked on the
'  second button, this function would return 2.
'****************************************************************************
'
FUNCTION MsgBox% (LibName$, FormName$)

   SHARED Cnf AS Config
   REDIM Form$(0, 0)
   REDIM Fld(0) AS FieldInfo
   REDIM Choice$(0, 0)
   REDIM ScrBuf(2000)

   MScrnSave 1, 1, 25, 80, ScrBuf(0)       'Save the existing screen
                                        
                                           'Display the form
   LibLoadDisplayForm LibName$, FormName$, Form$(), Fld()

   DIM Frm AS FormInfo
   Action = 1

   DO
      EditForm Form$(), Fld(), Frm, Cnf, Action

      IF Frm.KeyCode THEN                  'If a key was pressed,
                                           'Make it upper case
         IF Frm.KeyCode >= 97 AND Frm.KeyCode <= 122 THEN
            Frm.KeyCode = Frm.KeyCode - 32
         END IF

         FOR B = 1 TO Fld(0).Fields        'See if its one of the buttons
            IF Fld(B).ScratchI = Frm.KeyCode THEN EXIT DO
         NEXT
      END IF
   LOOP

   MScrnRest 1, 1, 25, 80, ScrBuf(0)       'Restore the original screen
     
   MsgBox% = B

END FUNCTION

