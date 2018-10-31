'***** DEMOINV.BAS **********************************************************
'Illustrates using "FormEdit" with a complex data entry form containing
'"Multiple Choice", "Calculated" and "Notes" fields.  Also shows how to
'modify the contents of fields on the fly based on the user's input.  See
'"CalcDiscountRate" and "FillInLineItem".  Also see "PaintField" to see how
'to implement a moving hilight bar.

'Copyright (c) 1989 - 1991 Crescent Software, Inc.
'by Don Malin

'NOTE: Modules - "VertMenu.bas", "Evaluate.bas" and "QEditS.bas" from
'      "QuickPak Professional" are required for this demo.
'      If you don't have these modules or your form doesn't require these
'      features,  load any of the following "NO" files instead.
'      "NoMult.Bas" - Replaces "VertMenu.Bas" when not using Multiple Choice
'                     Fields.
'      "NoCalc.Bas" - Replaces "Calc.Bas" and "Evaluate.Bas" when not using
'                     Calculated fields.
'      "NoNotes.Bas" - Replaces "QEditS.Bas" when not using Notes fields.
'****************************************************************************

DEFINT A-Z

'$INCLUDE: 'DefCnf.bi'          'Defines a TYPE for monitor and color info..
'$INCLUDE: 'FieldInf.bi'        '        "          field information
'$INCLUDE: 'FormEdit.bi'        '        "          form editing information

'----- ASM Functions
DECLARE FUNCTION DosError% ()
DECLARE FUNCTION Monitor% ()
DECLARE FUNCTION WhichError% ()

'----- ASM Subs
DECLARE SUB HideCursor ()
DECLARE SUB MPaintBox (UlRow, UlCol, LRRow, LRCol, Colr)
DECLARE SUB TextCursor (Fg%, Bg%)
DECLARE SUB ShowCursor ()

'----- BASIC Functions and Subs
DECLARE FUNCTION FldNum% (FldName$, Fld() AS ANY)
DECLARE FUNCTION NumberOfFields (ScrName$)
DECLARE SUB CalcDiscountRate (Form$(), Fld() AS ANY)
DECLARE SUB CalcFields (StartOfForm%, FldNo%, Form$(), Fld() AS ANY)
DECLARE SUB DisplayScrn (BTmp%(), Element%, MonoCode%, WipeType%)
DECLARE SUB EditForm (Form$(), Fld() AS FieldInfo, Frm AS FormInfo, Cnf AS Config, Action%)
DECLARE SUB FillInLineItem (FldNo%, Form$(), Fld() AS ANY)
DECLARE SUB GetFldDef (ScrName$, StartEl, Fld() AS FieldInfo, Form$())
DECLARE SUB LoadFormDef (FormName$, Form$(), Fld() AS ANY)
DECLARE SUB LoadScreen (LName$, MonoCode%, Attribute%, ErrorCode%)
DECLARE SUB PaintField (Fld AS ANY, Clr%)
DECLARE SUB PrintArray (FirstFld%, LastFld%, Form$(), Fld() AS ANY)
DECLARE SUB SaveField (FldNo%, Form$(), Fld() AS ANY, BadFld%)

'----- Reserve stack space for recursive subs (calculated fields).
'      Note: If you get an "Out of stack space" error, first check that your
'            form doesn't have any "Circular" references in any calculated
'            fields.  If it does not then un-rem and/or increase the value
'            below.
'CLEAR , , 2000


'$INCLUDE: 'SetCnf.bi'                  'Sets monitor and color information


'----- This demo makes references to certain fields for calculations and
'      since we might add, delete, or move fields around, it is a good
'      idea to use field number variables to refer to them.  See the calls
'      to "FldNum%" in the "InvoiceSetup:" subroutine to see how these are
'      assigned.
DIM SHARED CurDate                      'Field # of the date field
DIM SHARED Descr1                       'Field # of Description of 1st item
DIM SHARED DescrDif                     '# of fields between description fields
DIM SHARED DiscountRate                 'Field # of Discount rate field
DIM SHARED TaxRate                      'Field # of the tax rate field
DIM SHARED Notes                        'Field # of the notes field.

'----- The following arrays are mandatory
DIM Frm AS FormInfo                     'TYPE used for editing information.
                                        '  See "FormEdit.bi" for more info.

REDIM Fld(0) AS FieldInfo               'Holds field information.  See
                                        '  "FieldInf.Bi" for more information
                                        '  REDIMed later in "GetForm" subroutine

REDIM Form$(0, 0)                       'Holds form data, formulas and help
                                        '  messages.  REDIMed later in
                                        '  "GetForm" subroutine

REDIM Choice$(0, 0)                     'Holds Multiple choice selections
                                        '  must dim, even if not using choices




'----- Load the field definition file
FormName$ = "Invoice"                   'Specify the form and screen to use

LoadFormDef FormName$, Form$(), Fld()   'Load the form definition file
IF DosError THEN
   ERROR WhichError%
   END
END IF

GOSUB InvoiceSetup                      'Do setup specific to the "Invoice"
                                        '  form

CLS                                     'Clear the screen
'----- Turn the mouse cursor on
TextCursor -2, -2                       'Define the Mouse cursor
ShowCursor                              'Show it

'----- Flag - Set to 3 if using a monochrome monitor or the program was
'      started with "/b" in the command line.
DEF SEG = 0
MonoCode = ABS(PEEK(&H463) = &HB4 OR INSTR(COMMAND$, "/B")) * 3

'----- Display the screen
LoadScreen FormName$, MonoCode, 0, ErrorCode

SELECT CASE ErrorCode                   'Check for errors displaying screen
   CASE 1
      PRINT "Can't find " + FormName$ + ".Scr"
   CASE 2
      PRINT "Screen cannot fit on this monitor."
   CASE 3
      PRINT FormName$ + " is not a compatible screen."
   CASE ELSE
END SELECT
IF ErrorCode THEN END



'----- Print instructions
LOCATE 43, 1
COLOR 15, 0
PRINT "Press [F1] for help, [Esc] to quit";
COLOR 0, 7

ShowCursor                              'Turn the mouse cursor on


'----- Set up "Frm" structure variables
Frm.FldNo = 1                           'Start editing field #1
Frm.InsStat = 0                         'Set the insert state (-1 = Insert on)

'----- Save the first field's color and then hilight it.
ClrWas = SCREEN(Fld(Frm.FldNo).Row, Fld(Frm.FldNo).LCol, 1)
PaintField Fld(Frm.FldNo), 15           'Paint the new field hi-white on black

'----- Set the "Action" flag to force the editor to initialize itself and
'      display the data on the form.
Action = 1

'----- This is the main processing loop where we are polling the "EditForm"
'      procedure and testing for special user actions.
DO
   EditForm Form$(), Fld(), Frm, Cnf, Action

   '----- Are we moving to a new field?
   IF Frm.FldNo <> Frm.PrevFld THEN
     
      '----- Did they enter/edit anything?
      IF Frm.FldEdited THEN

         '----- Where we within the range of line items?
         IF Frm.PrevFld >= Descr1 AND Frm.PrevFld <= LastDescr THEN
        
            '----- See which field column we where on
            SELECT CASE (Frm.PrevFld - Descr1) MOD DescrDif
           
               '----- Was it a description field?
               CASE 0
                  '----- Fill in the rest of the line (Units, Quantity, Price)
                  FillInLineItem Frm.PrevFld, Form$(), Fld()
                  '----- Calculate the discount rate if it applies
                  CalcDiscountRate Form$(), Fld()
                  '----- Put the cursor on the next line item description
                  Frm.FldNo = Frm.PrevFld + DescrDif

               '----- Was it a quantity field?
               CASE 2
                  '----- Calculate the discount rate if it applies
                  CalcDiscountRate Form$(), Fld()

               CASE ELSE
            END SELECT
         END IF
      END IF

      '----- Hilight fields as we go
      PaintField Fld(Frm.PrevFld), ClrWas 'Unpaint the previous field
                                        'Save current color of new field
      ClrWas = SCREEN(Fld(Frm.FldNo).Row, Fld(Frm.FldNo).LCol, 1)
      PaintField Fld(Frm.FldNo), 15     'Paint the new field hi-white on black
   END IF

LOOP UNTIL Frm.KeyCode = 27             'Keep editing until the user presses
                                        '  the Escape key.

HideCursor                              'Turn the mouse cursor off
COLOR 7, 0
END



'****************************************************************************
'*** The following subroutine does setup specificaly for the Invoice form ***
'****************************************************************************

InvoiceSetup:

    '----- Find field numbers from their names
    CurDate = FldNum%("INVDATE", Fld())
    Descr1 = FldNum%("DESCR1", Fld())
    DescrDif = FldNum%("DESCR2", Fld()) - Descr1
    DiscountRate = FldNum%("DISCRATE", Fld())
    TaxRate = FldNum%("TAXRATE", Fld())
    Notes = FldNum%("NOTES", Fld())

    LastDescr = 10 * DescrDif + Descr1


    '----- Here's how you set up multiple choice fields
    REDIM Choice$(13, 2)                '13 choices in 3 menus (menu 0 is used)

    Choice$(0, 0) = STR$(FldNum%("TERMS", Fld()))
    Choice$(1, 0) = "CASH"
    Choice$(1, 0) = "CHECK"
    Choice$(2, 0) = "COD"
    Choice$(3, 0) = "AMEX"
    Choice$(4, 0) = "VISA"
    Choice$(5, 0) = "Master Card"

    Choice$(0, 1) = STR$(FldNum%("SALESMAN", Fld()))
    Choice$(1, 1) = "Don Malin"
    Choice$(2, 1) = "Ethan Winer"
    Choice$(3, 1) = "Tami Osborn"
    Choice$(4, 1) = "Paul Passarelli"
    Choice$(5, 1) = "Jay Munro"
    Choice$(6, 1) = "Brian Giedt"
    Choice$(7, 1) = "Jon Waldman"
    Choice$(8, 1) = "Phil Cramer"
    Choice$(9, 1) = "Nash Bly"

    '----- Define the stock items.
    Choice$(1, 2) = "0110   QuickPak Professional"
    Choice$(2, 2) = "1000   P.D.Q."
    Choice$(3, 2) = "0270   Quick Screen"
    Choice$(4, 2) = "0410   GraphPak Professional"
    Choice$(5, 2) = "1200   Graphics Workshop"
    Choice$(6, 2) = "0600   LaserPak"
    Choice$(7, 2) = "0700   QuickPak Scientific"
    Choice$(8, 2) = "1001   P.D.Q. COM"
    Choice$(9, 2) = "1100   XREF Cross Reference"
    Choice$(10, 2) = "0500   QuickMenu"
    Choice$(11, 2) = "2001   Auto-Upgrade plan for QuickPak"
    Choice$(12, 2) = "2000   Auto-Upgrade plan for P.D.Q."
    Choice$(13, 0) = "X                          "
    '----- The above choices go with each of the 10 description fields so we
    '      need to put the numbers of all ten in the first element seperated
    '      by comas.
    Choice$(0, 2) = STR$(Descr1)
    FOR N = 1 TO 9
       FldNo$ = STR$(N * DescrDif + Descr1)
       Choice$(0, 2) = Choice$(0, 2) + "," + FldNo$
    NEXT


    '----- Preset some fields data
    Form$(CurDate, 0) = DATE$           'Invoice date field
    Form$(TaxRate, 0) = "8"             'Tax rate field

    '----- The following statements create a single long string which will be
    '      formatted into the notes section of the form.  Single strings like
    '      this can be saved as notes fields (variable length fields) in
    '      sequential, "DBase" (db/LIB), "Btrieve" or "ISAM" files.  Note the
    '      CHR$(20) paragraph markers used to specify the end of paragraphs
    '      and blank lines.
    '
    Form$(Notes, 0) = "This is a test of the Notes field type.  We are presetting "
    Form$(Notes, 0) = Form$(Notes, 0) + "some text in here to show that it will be unwrapped "
    Form$(Notes, 0) = Form$(Notes, 0) + "and displayed properly when the form is first entered."
    Form$(Notes, 0) = Form$(Notes, 0) + CHR$(20) + CHR$(20) + "This is the start of another "
    Form$(Notes, 0) = Form$(Notes, 0) + "paragraph which should be on the second line after "
    Form$(Notes, 0) = Form$(Notes, 0) + "the above paragraph." + CHR$(20)

RETURN

'****************************************************************************
'Calculates a discount rate based on the number of products purchased.
'While this procedure is only for use with this demo., it shows how a
'program can modify the contents of fields and force calculations.
'****************************************************************************
'
SUB CalcDiscountRate (Form$(), Fld() AS FieldInfo)

   FOR L = 0 TO 9
      QuantFld = L * DescrDif + Descr1 + 2      'Find Quantity field's number
      '----------------------------------------------------------------------
      'The following line retrieves the Quantity field's value from
      'the field buffer "Form$(0, 0)".  Since numeric fields are
      'stored here as IEEE strings, we can use CV? to quickly extract
      'a field's value.  While we could accomplish the same thing by
      'using a statement such as:
      '  Quant = VAL(Form$(QuantFld, 0))
      'or to guard against dollar signs and commas in number strings:
      '  Quant = Value#(Form$(QuantFld, 0))
      'While these two examples may seem simpler, they are
      'considerably slower.
      
      Quant = CVI(MID$(Form$(0, 0), Fld(QuantFld).Fields, 2))
      '                ^ buffer     ^ Offset into buffer  ^ 2 bytes for Int.s
      '----------------------------------------------------------------------

      '----- Blank numbers are stored as the lowest possible value for the
      '      type of number being represented.  ie. a blank integer field
      '      would hold the value: -32767.
      IF Quant > 0 THEN
         NumItems = NumItems + Quant
      END IF
   NEXT

   '----- Determine the discount rate based on the number of products.
   '      Notice that we use LSET to assign the Form$() array element.
   '      This insures that the length of the field does not change.
   SELECT CASE NumItems
      CASE 2
         LSET Form$(DiscountRate, 0) = "5"
      CASE 3
         LSET Form$(DiscountRate, 0) = "10"
      CASE IS >= 4
         LSET Form$(DiscountRate, 0) = "15"
      CASE ELSE
   END SELECT

   '----- Format and save the field's value into the Form$(0, 0) buffer
   SaveField DiscountRate, Form$(), Fld(), BadFld
  
   '----- Display the field's formatted contents on the screen.
   PrintArray DiscountRate, DiscountRate, Form$(), Fld()
  
   '----- Force the discount amount to be calculated from the
   '      discount rate field we just updated.
   CalcFields 0, DiscountRate, Form$(), Fld()
   '             ^ Point to the field we just changed
END SUB

'****************************************************************************
'Fills in line item fields (Units, Quantity, and Unit Price) based on the
'product selected from a Description field's menu.
'
'While this procedure is only for use with this demo., it shows how a
'program can modify the contents of fields and force calculations.
'****************************************************************************
'
SUB FillInLineItem (FldNo, Form$(), Fld() AS FieldInfo)
    
   Quant$ = "1"

   '----- Find the unit price based on the product number
   SELECT CASE LEFT$(Form$(FldNo, 0), 4)
      CASE "0110"
         Um$ = "EA"
         Unit$ = "199"
      CASE "1000"
         Um$ = "EA"
         Unit$ = "129"
      CASE "0270"
         Um$ = "EA"
         Unit$ = "149"
      CASE "0410"
         Um$ = "EA"
         Unit$ = "149"
      CASE "1200"
         Um$ = "EA"
         Unit$ = "149"
      CASE "0600"
         Um$ = "EA"
         Unit$ = "149"
      CASE "0700"
         Um$ = "EA"
         Unit$ = "149"
      CASE "1001"
         Um$ = "EA"
         Unit$ = "99"
      CASE "1100"
         Um$ = "EA"
         Unit$ = "59"
      CASE "0500"
         Um$ = "EA"
         Unit$ = "59"
      CASE "2000"
         Um$ = "YEAR"
         Unit$ = "69"
      CASE "2001"
         Um$ = "YEAR"
         Unit$ = "69"
      CASE ELSE
         Quant$ = ""
   END SELECT

   '----- Assign the Units of measure, Quantity, and Unit Price
   '      Notice that we use LSET to assign the Form$() array element.
   '      This insures that the length of the field does not change.
   LSET Form$(FldNo + 1, 0) = Um$
   LSET Form$(FldNo + 2, 0) = Quant$
   LSET Form$(FldNo + 3, 0) = Unit$
     
   '----- Format and save each field's value into the Form$(0, 0) buffer
   FOR N = FldNo + 1 TO FldNo + 3
      SaveField N, Form$(), Fld(), BadFld
   NEXT
  
   '----- Display the field's formatted contents on the screen.
   PrintArray FldNo + 1, FldNo + 3, Form$(), Fld()

   '----- Force the total item cost as well as the total fields to be
   '      updated.
   CalcFields 0, FldNo + 2, Form$(), Fld()
   '             ^ Point to the quantity field we just changed

END SUB

'****************************************************************************
'This procedure can be used to size and load the field data (Form$()) and
'information (Fld()) arrays from a form file (.FRM).
'****************************************************************************
'
SUB LoadFormDef (FormName$, Form$(), Fld() AS FieldInfo)

    NumFlds = NumberOfFields(FormName$) 'Get the number of fields in form
    IF DosError THEN EXIT SUB

    REDIM Fld(NumFlds) AS FieldInfo     'DIM the field information array

    '---- Create the array used for form data, help messages and formulas
    '     Form$(0, 0) holds all data from fields as a contiguous string with
    '                 numbers stored as IEEE formatted strings.
    '     Form$(FieldNum, 0) holds data (formatted numeric string for numbers)
    '     Form$(FieldNum, 1) holds help message string
    '     Form$(FieldNum, 2) holds formulas for calculated fields
    REDIM Form$(NumFlds, 2)             'DIM the form data array

    GetFldDef FormName$, 0, Fld(), Form$() 'Load the field information

END SUB

'****************************************************************************
'Paints a field a specified color.
'****************************************************************************
'
SUB PaintField (Fld AS FieldInfo, Clr) STATIC

   '----- Note fields are usually more than one line.
   IF Fld.FType = NotesFld THEN
      BotRow = Fld.ScratchI
   ELSE
      BotRow = Fld.Row
   END IF
  
   '----- Paint the field
   MPaintBox Fld.Row, Fld.LCol, BotRow, Fld.RCol, Clr

END SUB

