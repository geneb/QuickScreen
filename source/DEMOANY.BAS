'***************  DEMOANY.BAS  -  Demo Data Entry on ANY Form

'Copyright (c) 1990, 1991 Crescent Software
'Written by Nash Bly and Don Malin

'Use this demo with any form you have created using QuickScreen.
'The .SCR and .FRM files must be present when you run this demo.
'This is a bare bones example of loading and displaying screens,
'loading form definitions, and using the EditForm subprogram for data entry.

DEFINT A-Z

'-------- Declarations
DECLARE FUNCTION Monitor% ()
DECLARE FUNCTION NumberOfFields% (FormName$)
DECLARE SUB EditForm (Form$(), Fld() AS ANY, Frm AS ANY, Cnf AS ANY, Action)
DECLARE SUB GetFldDef (FormName$, StartEl, Fld() AS ANY, Form$())
DECLARE SUB LoadScreen (ScreenName$, MonoCode, Attribute, ErrorCode)

'-------- QuickScreen TYPE and constant definitions
'   $INCLUDE: 'FIELDINF.BI'                     'FieldInfo type and constants
'   $INCLUDE: 'FORMEDIT.BI'                     'FormInfo type and constants
'   $INCLUDE: 'DEFCNF.BI'                       'defines the Config TYPE
'   $INCLUDE: 'SETCNF.BI'                       'init configuration variables

'-------- Init Type Variables
    DIM Frm AS FormInfo                         'FormInfo TYPE variable

'-------- Start of Execution
    CLS                                         'clear screen, display prompt
    PRINT "Enter the Form Name (you may use a path but no extension)."
    PRINT "Press Escape to exit any time while the form is displayed."
    PRINT
    INPUT "Form Name: "; FormName$              'prompt and input form name

'-------- Display screen in black & white if monochrome monitor is installed
    DEF SEG = 0
    MonoCode = ABS(PEEK(&H463) = &HB4 OR INSTR(COMMAND$, "/B")) * 3

'-------- Enable the mouse (optional)
    CALL TextCursor(-2, -2)                     'Define the Mouse cursor
    CALL ShowCursor                             'turn on the cursor

'-------- Initialize arrays and load form definition file
    NumFlds = NumberOfFields%(FormName$)        'load number of fields
    IF NumFlds = 0 THEN                         'if no fields then print
       PRINT "Can't find "; FormName$; ".FRM !" '  a message and exit
       PRINT "Try again with the proper path."
       END
    END IF

    REDIM Fld(NumFlds) AS FieldInfo             'field info array
    REDIM Form$(NumFlds, 2)                     'field data array
    REDIM Choice$(0, 0)                         'multi-choice array

    GetFldDef FormName$, Zero, Fld(), Form$()   'load form definition file

'-------- Load and display form
    CLS                                                  'clear screen first
    LoadScreen FormName$, MonoCode, Attribute, ErrorCode 'display the form
    IF ErrorCode THEN                                    'if an error then
        PRINT "Unable to display "; FormName$; " .SCR !" 'message and exit
        PRINT "ErrorCode: "; ErrorCode
        END
    END IF

'-------- Edit the form
    Action = 1

    DO                                                'inside a loop
        EditForm Form$(), Fld(), Frm, Cnf, Action     'check for keypresses
    LOOP UNTIL Frm.KeyCode = 27 OR Frm.KeyCode = -60  'loop until escape or
    END                                               '  F2 is pressed

