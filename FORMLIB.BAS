'****************************************************************************
'Module used to load Form Definition from a Form Definition library (.QFL).
'
'by Donald R. Malin,  Copyright (c) 1990 Crescent Software, Inc.
'
'This module should be used with programs that load field definitions from
'a forms library (.QFL file containing more than one forms definitions).
'There are three procedures contained in this module:
'
'  "LibNumberOfFields" - returns the number of fields contained in the form.
'                        Use this function to determine the size of the Field
'                        definition array (Fld()) and the form's data array
'                        (Form$()) prior to loading the file with "GetFldDef".
'
'  "LibGetFldDef"      - Loads field definitions, help strings and formulas
'                        into the arrays mentioned above.
'
'  "LibGetAddresses"   - This procedure is meant for internal use only.  It
'                        returns the starting and ending offsets of a given
'                        form's information within the form library.  It is
'                        used by the two procedures above.
'****************************************************************************

DEFINT A-Z

'$INCLUDE: 'FieldInf.bi'                'Defines TYPE for field information

'----- Define functions
DECLARE FUNCTION DosError% ()
DECLARE FUNCTION Exist% (FileName$)
DECLARE FUNCTION FLInput$ (Handle%, Buffer$)
DECLARE FUNCTION FLof& (Handle%)
DECLARE FUNCTION LibNumberOfFields% (LibName$, FrmName$)

'----- Define subs
DECLARE SUB FClose (Handle%)
DECLARE SUB FGet (Handle, Text$)
DECLARE SUB FGetA (Handle%, SEG Element AS ANY, NumBytes&)
DECLARE SUB FGetT (Handle, Addr AS ANY, Var AS ANY)
DECLARE SUB FOpen (FileName$, Handle%)
DECLARE SUB FSeek (Handle, Offset&)
DECLARE SUB LibGetAddresses (LibName$, FrmName$, Handle%, Offset&, EndOffset&, ErrCode%)
DECLARE SUB Tokenize (Calc$, Fld() AS ANY)

'****************************************************************************
'This is an internal routine used by "LibNumberOfFields%" and "LibGetFldDef".
'Returns the starting offset ("Offset&") and ending offset ("EndOffset&")
'into the "LibName$" library file of the form - "FrmName$".  "ErrCode"
'returns -1 if the library file is not found, 1 if the form name is not
'found or 0 if all is well.  This routine opens the library file and leaves
'it open for the calling routines.  It is up to the caller to close it when
'done with it.
'****************************************************************************
'
SUB LibGetAddresses (LibName$, FrmName$, Handle, Offset&, EndOffset&, ErrCode)

   STATIC PrevLibName$, PrevFrmName$, PrevOffset&, PrevEndOffset&
   ErrCode = 0                                  'No errors yet

   N = INSTR(LibName$, ".")                     'Look for a period in name
   IF N THEN                                    'If there is one,
      LibNameCopy$ = LEFT$(LibName$, N) + "QFL" '  use the name as is.
   ELSE                                         'Otherwise add the extension
      LibNameCopy$ = LibName$ + ".QFL"
   END IF

   Test$ = SPACE$(8)                            'Pad out a test variable for
                                                '  getting form names below.
   FrmNameCopy$ = Test$                         'Make a padded copy of the
   LSET FrmNameCopy$ = FrmName$                 '  form name.

   IF NOT Exist(LibNameCopy$) THEN              'See if the library file is
      ErrCode = -1                              '  there.  If not, set error
      EXIT SUB                                  '  and bail out.
   END IF

   FOpen LibNameCopy$, Handle                   'Open the library file
   IF DosError THEN                             'Was there an error?
      ErrCode = -1                              'Yes, set the error and
      EXIT SUB                                  '  bail out.
   END IF


   '----- If we have already gotten information for the same library and
   '      form, use the previous values and bail out.
   '
   IF LibNameCopy$ = PrevLibName$ AND FrmNameCopy$ = PrevFrmName$ THEN
      IF PrevOffset& THEN
         Offset& = PrevOffset&
         EndOffset& = PrevEndOffset&
         EXIT SUB
      END IF
   END IF
  
   FGet Handle, Test$                           'Read the first form name,
   FGetT Handle, Offset&, 4                     '  and offset.
   N = 1

   Last = Offset& \ 12                          'Calc. the # of form names

   IF Test$ <> FrmNameCopy$ THEN                'If the first name didn't
                                                '  match, search the rest.
      FOR N = 2 TO Last
         FGet Handle, Test$                     'Get next form name and,
         FGetT Handle, Offset&, 4               '  offset.

         IF Test$ = FrmNameCopy$ THEN EXIT FOR  'Bail out if we found a match
      NEXT

      IF Test$ <> FrmNameCopy$ THEN             'If not match from above,
         ErrCode = 1                            '  set the error and,
         EXIT SUB                               '  bail out.
      END IF
   END IF

   IF N < Last THEN                             'If the form wasn't the last,
      FGet Handle, Test$                        '  read over the next name
      FGetT Handle, EndOffset&, 4               '  and then read it's offset.
   ELSE                                         'Otherwise,
      EndOffset& = FLof&(Handle)                '  make it the file length.
   END IF

   PrevLibName$ = LibNameCopy$                  'Save the library name
   PrevFrmName$ = FrmNameCopy$                  '   "     form name
   PrevOffset& = Offset&                        '   "     offset
   PrevEndOffset& = EndOffset&                  '   "     ending offset

END SUB

'****************************************************************************
'Retrieves Form information from file and puts it in a structure for later
'reference by other routines.  Also loads formulas and help messages into
'the "Form$()" data array.
'
'LibName$ is the name of the Forms Library file.
'
'FrmName$ is the name of the form (Base name only, no path or extension)
'
'StartEl is the element of the arrays below to begin loading the file into.
'
'Fld() is the field information TYPE array that the file will be loaded into.
'  It must have been DIMensioned to at least the number of fields in the form.
'  See "LibNumberOfFields" function to determine the number of fields in a
'  form.
'
'Form$() is the field data array that the Help and Calc files will be loaded
'  into.  It must have been DIMensioned to the number of fields by 2 ie.
'  [REDIM Form$(NumFields, 2)].  The Help messages will be loaded into the
'  second column [Form$(1, 1)] and the formulas will be loaded into the third
'  [Form$(1, 2).
'****************************************************************************
'
SUB LibGetFldDef (LibName$, FrmName$, StartEl, Fld() AS FieldInfo, form$(), ErrCode)

   '----- Get the offset of the form's data in the library
   LibGetAddresses LibName$, FrmName$, Handle, Offset&, EndOffset&, ErrCode
  
   IF ErrCode THEN                              'If there was a problem,
      FClose Handle                             '  close the library and
      EXIT SUB                                  '  bail out
   END IF

   FSeek Handle, Offset& - 1                    'Point to the forms data
   FGetT Handle, NumFlds, 2                     'Read the number of fields

   FSeek Handle, Offset& - 1                    'Reposition the file pointer
                                               
   '----- Load the field definitions into the "Fld()" array
   FGetA Handle, Fld(StartEl), (NumFlds + 1) * FrmWdth
                                                
   Offset& = Offset& + (NumFlds + 1) * FrmWdth  'Keep track of the offset

   Byte$ = " "                                  'Read a byte of data looking
   FGet Handle, Byte$                           '  for a flag.
   Offset& = Offset& + 1

   Buffer$ = SPACE$(194)                        'Make a buffer for "FLinput"
 
   '----- Load field help and formulas into the "Form$()" array
   DO UNTIL Offset& >= EndOffset& OR DosError
      IF Byte$ = "H" THEN                       'Is this Help data?
         FOR N = StartEl + 1 TO StartEl + NumFlds
            form$(N, 1) = FLInput$(Handle, Buffer$)
            Offset& = Offset& + LEN(form$(N, 1)) + 2
         NEXT
      ELSE                                      'Must be formulas
         FOR N = StartEl + 1 TO StartEl + NumFlds
            form$(N, 2) = FLInput$(Handle, Buffer$)
            Offset& = Offset& + LEN(form$(N, 2)) + 2
            IF LEN(form$(N, 2)) THEN Tokenize form$(N, 2), Fld()
         NEXT
      END IF

      FGet Handle, Byte$                        'Read another single byte for
      Offset& = Offset& + 1                     '  testing above.
   LOOP
   FClose Handle                                'Close the library file.
 
   '----- Calculate pointers into whole record buffer
   IF StartEl = 0 THEN
      Fld(StartEl + 1).Fields = 1
   ELSE
      Fld(StartEl + 1).Fields = Fld(StartEl - 1).Fields + Fld(StartEl - 1).StorLen
   END IF
                                              
   FOR N = StartEl + 2 TO StartEl + Fld(StartEl).Fields
      Fld(N).Fields = Fld(N - 1).Fields + Fld(N - 1).StorLen
   NEXT

   Fld(0).Row = Fld(N - 1).Fields + Fld(N - 1).StorLen - 1

END SUB

'****************************************************************************
'Function returns the number of fields contained in the form "FrmName$"
'contained in a Form Library "LibName$".
'Use this routine to determine the size of the arrays used to hold field
'information prior to loading them with the SUB - "LibGetFldDef".
'****************************************************************************
'
FUNCTION LibNumberOfFields% (LibName$, FrmName$)

   '----- Get the offset of the form's data in the library
   LibGetAddresses LibName$, FrmName$, Handle, Offset&, EndOffset&, ErrCode

   IF ErrCode = 0 THEN                          'If no errors occurred,
      FSeek Handle, Offset& - 1                 '  point to the data and
      FGetT Handle, NumFlds, 2                  '  read the number of fields.
   END IF

   FClose Handle                                'Close the library file

   LibNumberOfFields% = NumFlds                 'Assign the function

END FUNCTION

