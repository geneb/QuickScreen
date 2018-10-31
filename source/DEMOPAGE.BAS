'***** DEMOPAGE.BAS *********************************************************
'Illustrates using "FormEdit" to allow data entry in a multi page form.
'Also shows techniques for displaying screens from a screen library (.QSL)
'and loading field definitions from a forms library (.QFL)

'Copyright (c) 1989, 1990 Don Malin

'Overview:
'  Loads a screen library containing the pages (screens) of the form.
'  Loads and combines the form definitions from each page into the field
'    information arrays.
'  Displays the first page
'  Polls the editing sub program and traps events that cause a page change.

'NOTE: Modules - "VertMenu.bas" and "QEditS.bas" from "QuickPak Professional"
'      are required for this demo.  If you don't have these modules, unload
'      the above modules and then load the following "NO" files instead.
'      "NoMult.Bas" - Replaces "VertMenu.Bas" when not using Multiple Choice
'                     Fields.
'      "NoNotes.Bas" - Replaces "QEdit.Bas" when not using Notes fields.
'****************************************************************************

DEFINT A-Z

'$INCLUDE: 'DefCnf.bi'          'Defines a TYPE for monitor and color info..
'$INCLUDE: 'FieldInf.bi'        '        "          field information
'$INCLUDE: 'FormEdit.bi'        '        "          form editing information

'----- ASM Functions
DECLARE FUNCTION Monitor% ()

'----- ASM Subs
DECLARE SUB HideCursor ()
DECLARE SUB TextCursor (Fg%, Bg%)
DECLARE SUB ShowCursor ()

'----- BASIC Funstions and Subs
DECLARE FUNCTION LibNumberOfFields% (LibName$, FrmName$)
DECLARE FUNCTION LibScrName$ (Scr, ScrnLib())
DECLARE FUNCTION ScrnLibSize% (LibName$)
DECLARE SUB EditForm (Form$(), Fld() AS FieldInfo, Frm AS FormInfo, Cnf AS Config, Action%)
DECLARE SUB Lib2Scrn (NameInLib$, ScrnLib%(), MonoCode%, Attribute%, ErrorCode%)
DECLARE SUB LibGetFldDef (LibName$, FrmName$, StartEl%, Fld() AS ANY, Form$(), ErrCode%)
DECLARE SUB LoadScrnLib (LibName$, ScrnLib%(), ErrorCode%)
DECLARE SUB PrintArray (FirstFld%, LastFld%, Form$(), Fld() AS FieldInfo)
DECLARE SUB UnPackBuffer (FirstFld%, LastFld%, Form$(), Fld() AS ANY)

'$INCLUDE: 'SetCnf.bi'                  'Sets monitor mouse and color info.



'----- The following arrays are mandatory
DIM Frm AS FormInfo                     'TYPE used for editing information.
                                        '  See "FormEdit.bi" for more info.

REDIM Fld(0) AS FieldInfo               'Holds field information.  See
                                        '  "FieldInf.Bi" for more information
                                        '  REDIMed later

REDIM Form$(0, 0)                       'Holds form data, formulas and help
                                        '  messages.  REDIMed later

REDIM Choice$(0, 0)                     'Holds Multiple choice selections



'----- Load a Screen library containing both pages of the form.
LibName$ = "Invoice.QSL"                'Specify a Screen Library file name.

LibSize = ScrnLibSize%(LibName$)        'Get required size for the array that
                                        '  will hold the screen library.
REDIM ScrnLib(LibSize%)                 'Create the array
                                        'Load the screen library into it
LoadScrnLib LibName$, ScrnLib(), ErrorCode

IF ErrorCode = -1 THEN                  'Check for errors
   PRINT LibName$ + " is not a QuickScreen compatible library."
   END
ELSEIF ErrorCode = -2 THEN
   PRINT LibName$ + " not found."
   END
END IF



'----- Find the number of fields from both pages (Forms)
NumFlds = -1
FOR Scr = 1 TO 2                        'Get form name from screen number
   FormName$ = LibScrName$(Scr, ScrnLib())
                                       
                                        'Add number of fields to total
   NumFlds = NumFlds + LibNumberOfFields%(LibName$, FormName$) + 1
NEXT

REDIM Fld(NumFlds) AS FieldInfo         'DIM the field information array to
                                        '  combined size of both forms

'---- Create the array used for form data, help messages and formulas
'     Form$(0, 0) holds all data from fields as a contiguous string with
'                 numbers stored as IEEE formatted strings.  (Data Buffer)
'     Form$(FieldNum, 0) holds data (formatted numeric string for numbers)
'     Form$(FieldNum, 1) holds help message string
'     Form$(FieldNum, 2) holds formulas for calculated fields
REDIM Form$(NumFlds, 2)                 'DIM the form data array


'----- Load the form definitions for both screens
StartEl = 0                             'Load first form to beginning of array
FOR Scr = 1 TO 2                        'Get form name from screen number
   FormName$ = LibScrName$(Scr, ScrnLib())
                                        'Load field information @ "StartEl"
   LibGetFldDef LibName$, FormName$, StartEl, Fld(), Form$(), ErrCode
                                        'Bump "StartEl" to next available
   StartEl = StartEl + Fld(StartEl).Fields + 1
   
NEXT



'----- Presets data in fields for this demo
GOSUB InvoiceSetup



'----- The following Sets the "MonoCode" flag to 3 if using a monochrome
'      monitor or the program was started with "/b" on the command line.
DEF SEG = 0
MonoCode = ABS(PEEK(&H463) = &HB4 OR INSTR(COMMAND$, "/B")) * 3



'----- Display the first page (screen) from the library
CLS
Scr = 1                                 'Set screen number to one
FormName$ = LibScrName$(Scr, ScrnLib()) 'Get the name of the first screen
                                        'Display it
Lib2Scrn FormName$, ScrnLib(), MonoCode, -1, ErrCode

                                        'This is optional if you know the
IF ErrCode = 1 THEN                     '  screen is in library and will fit
   PRINT FormName$ + " is not in the library."
   END
ELSEIF ErrCode = 2 THEN
   PRINT FormName$ + " cannot be displayed on this monitor."
   END
END IF



'----- Turn the mouse cursor on
TextCursor -2, -2                       'define the Mouse cursor
ShowCursor                              'Turn it on



'----- Print instructions on bottom of screen
LOCATE 1, 1
COLOR 0, 3
PRINT " This could be a menu or title line.                                            ";



'----- Setup TYPE for setting and reading form editing information.
Frm.FldNo = 1                           'Start editing on field #1
Frm.InsStat = 0                         'Set the insert state (-1 = Insert on)
Frm.StartEl = 0                         'Set form starting element to 0 and
LastStartEl = Frm.StartEl               '  save value for later comparisons
'----- See "FormEdit.bi" for other elements.


'****************************************************************************
'Open the database file and field "Form$(0, 0)" (the whole record buffer) to
'the length specified by "Fld(0).Row".  Fld(0).Row shows the record length
'for both forms combined.
'----------------------------------------------------------------------------
OPEN FormName$ FOR RANDOM AS #1 LEN = Fld(0).Row
FIELD #1, Fld(0).Row AS Form$(0, 0)
'****************************************************************************


'----- Set the "Action" flag to force the editor to initialize itself and
'      display the data on the form.
Action = 1


DO                                      'Pole the editing procedure
   EditForm Form$(), Fld(), Frm, Cnf, Action

   '----- If the user pressed PgUp or PgDn or moved off the top or bottom of
   '----- the form, "StartEl" will be updated by "EditForm" so we need to
   '----- check it.
   IF Frm.StartEl <> LastStartEl THEN
      IF Frm.StartEl < LastStartEl THEN         'Previous page?
         Scr = Scr - 1                          'Yes set previous page number
      ELSEIF Frm.StartEl > LastStartEl THEN     'Next page?
         Scr = Scr + 1                          'Yes set next page number
      END IF                                    'Display the screen
      Lib2Scrn LibScrName$(Scr, ScrnLib()), ScrnLib(), MonoCode, -2, ErrCode
      LastStartEl = Frm.StartEl                 'Save the new "StartEl"
   END IF

   '*************************************************************************
   'Check for "Save"-F2 and Retrieve-F3 keys
   '-------------------------------------------------------------------------
   IF Frm.KeyCode = -60 THEN                    'F2 = Save
      PUT #1, 1                                 'Save both pages of the form
      FOR N = 1 TO NumFlds                      'For all fields on both pages
         LSET Form$(N, 0) = ""                  'Clear each field
      NEXT
      Action = 1                                'Set to 1 so form is redisplayed
  
   ELSEIF Frm.KeyCode = -61 THEN                'F3 = Retrieve
      IF LOF(1) THEN
         GET #1, 1                              'Retrieve record #1
         UnPackBuffer 0, 0, Form$(), Fld()      'Unpack Form$(0, 0) into each
                                                'field on the form
         Action = 1                             'Set to 1 so form is redisplayed
      END IF
   END IF
   '*************************************************************************
                                        'Keep editing until the user presses
LOOP UNTIL Frm.KeyCode = 27             '  the Escape key.
                                        

HideCursor                              'Turn the mouse cursor Off
COLOR 7, 0
END





'****************************************************************************
'*** The following subroutine does setup specificaly for the Invoice form ***
'****************************************************************************

InvoiceSetup:
    '----- Here's how you set up multiple choice fields
    REDIM Choice$(7, 1)                 '7 choices in 2 menus (menu 0 is used)

    Choice$(0, 0) = "15"                'these choices go with field 15
    Choice$(1, 0) = "CASH  "
    Choice$(2, 0) = "COD   "
    Choice$(3, 0) = "CREDIT CARD"
    Choice$(4, 0) = "NET-10"
    Choice$(5, 0) = "NET-30"

    Choice$(0, 1) = "16"                'these choices go with field 16
    Choice$(1, 1) = "Don Malin"
    Choice$(2, 1) = "Ethan Winer"
    Choice$(3, 1) = "Nash Bly"
    Choice$(4, 1) = "Paul Passarelli"
    Choice$(5, 1) = "Jay Munro"
    Choice$(6, 1) = "Brian Giedt"
    Choice$(7, 1) = "John Waldman"


    '----- Preset some fields data
    Form$(13, 0) = DATE$                'Invoice date field
    Form$(94, 0) = "8.0"                'Tax rate field

    '----- The following statements create a single long string which will be
    '      formatted into the notes section of the form.  Single strings like
    '      this can be saved as notes fields (variable length fields) in
    '      sequential, "DBase" (db/LIB), "Btrieve" or "ISAM" files.  Note the
    '      CHR$(20) paragraph markers used to specify the end of paragraphs
    '      and blank lines (they will not be displayed on the form).
    '
    Form$(98, 0) = "This is a test of the Notes field type.  We are pre-setting "
    Form$(98, 0) = Form$(98, 0) + "some text in here to show that it will be un-wrapped "
    Form$(98, 0) = Form$(98, 0) + "and displayed properly when the form is first entered."
    Form$(98, 0) = Form$(98, 0) + CHR$(20) + CHR$(20) + "This is the start of another "
    Form$(98, 0) = Form$(98, 0) + "paragraph which should be on the second line after "
    Form$(98, 0) = Form$(98, 0) + "the above paragraph." + CHR$(20)


RETURN



