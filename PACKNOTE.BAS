'*** PACKNOTE.BAS ***********************************************************
'Note File Compaction Utility
'
'Copyright (c) 1991, Crescent Software, Inc.
'by Don Malin
'
'This utility can be used to compact abandoned space in "Notes" database
'files that were created using the routines contained in the RANDOMIO.BAS
'module.  Portions of a file are abandoned when individual Notes records are
'shortened or lengthened after they were first saved.  While this should
'never cause any speed problems, you may want to compact a file from time to
'time inorder to conserve disk space.
'
'When run, the program will prompt for the name of a Field Definition file.
'You can enter the name of either a .FRM file, or a .QFL Field Definition
'Library file.  If a .QFL extension is specified, the program will
'additionally prompt for the name of the form within that Library.  Enter
'this with no extension.
'
'After prompting for the name of the Field Definition file, the program will
'then create a temporary file named "TEMP.NOT" where it will build the
'compacted version of the Notes file.  Be sure there is enough room on your
'drive as the program will not be able to complete the compaction if there
'isn't.  After transfering all the notes from the original file to the
'temporary one, the original file will be deleted and the temporary file will
'be renamed to the original file's name.
'****************************************************************************
DEFINT A-Z

'$INCLUDE: 'FieldInf.bi'

DECLARE FUNCTION NumberOfFields (FrmName$)
DECLARE FUNCTION LibNumberOfFields% (LibName$, FrmName$)

DECLARE SUB GetFldDef (FrmName$, StartEl%, Fld() AS FieldInfo, Form$())
DECLARE SUB LibGetFldDef (LibName$, FrmName$, StartEl%, Fld() AS FieldInfo, Form$(), ErrCode%)
DECLARE SUB GetRec (RecNo&, Form$(), Fld() AS FieldInfo)
DECLARE SUB OpenFiles (FormName$, Form$(), Fld() AS FieldInfo)
DECLARE SUB SaveRec (RecNo&, Form$(), Fld() AS FieldInfo)

NoFileMsg$ = "Form not found!"

PRINT "Note File Compaction Utility"
PRINT "Copyright (c) 1991, Crescent Software, Inc."
PRINT
PRINT "Enter the name of the Form Definition file (.FRM) or library (.QFL)."
INPUT "File Name: ", FileName$
FileName$ = UCASE$(FileName$)

IF LEN(FileName$) = 0 THEN END                  'No file given, end

'----- Find the file's path and extension
FOR N = LEN(FileName$) TO 1 STEP -1             'Look backwards through name
   C$ = MID$(FileName$, N, 1)                   'Examine a character

   IF INSTR(":\", C$) THEN                      'Is it a path character?
      EXIT FOR                                  'Yes, all done
   ELSEIF C$ = "." THEN                         'Is it the extension?
      Extn$ = MID$(FileName$, N)                'Yes, save extension
      FileName$ = LEFT$(FileName$, N - 1)       'Cut extension from name
   END IF
NEXT
Path$ = LEFT$(FileName$, N)                     'Save the path portion

IF LEN(Extn$) = 0 THEN Extn$ = ".FRM"           'Extension not given, default
                                                '  to .FRM files.
'----- See what type of file we are dealing with.
IF Extn$ = ".QFL" THEN
   '----- Field Definition Library files.
   QFLName$ = FileName$ + Extn$                 'Put extension back on name

   OPEN QFLName$ FOR BINARY AS #1               'Open it for below and to
   IF LOF(1) < 12 THEN                          '  see if its there.
      PRINT NoFileMsg$
      END
   END IF

   '----- Print the form names contained in the library.
   FileName$ = SPACE$(8)                        'Make a buffer for form names
   DO                                           'Read each form name
      N = N + 1                                 'Keep a count
      GET #1, , FileName$                       'Read a form name
      PRINT FileName$                           'display it

      GET #1, , Offset&                         'Read the offset part
      IF N = 1 THEN NumFrms = Offset& \ 12      'Find number left to read
   LOOP WHILE N < NumFrms
   PRINT
   CLOSE                                        'Close the .QFL

   INPUT "Form Name to Compact: ", FileName$    'Prompt user for form name
   IF LEN(FileName$) = 0 THEN END               'Nothing given, bail out
                                                'Find the number of fields
   NumFlds = LibNumberOfFields%(QFLName$, FileName$)
   IF NumFlds = 0 THEN                          '0 means form wasn't found
      PRINT NoFileMsg$                          '  in the library.
      END
   END IF
   REDIM Fld(NumFlds) AS FieldInfo              'Make a field definition array
   REDIM Form$(NumFlds, 2)                      'Array for data
                                                'Load the field definitions
   LibGetFldDef QFLName$, FileName$, StartEl, Fld(), Form$(), ErrCode
   IF ErrCode THEN END                          'Check for an error
ELSE
   NumFlds = NumberOfFields(FileName$)          'Find the number of fields
   IF NumFlds = 0 THEN                          '0 means file wasn't found
      PRINT NoFileMsg$                          'Bail out.
      END
   END IF
   REDIM Fld(NumFlds) AS FieldInfo              'Make a field definition array
   REDIM Form$(NumFlds, 2)                      'Array for data

   GetFldDef FileName$, StartEl, Fld(), Form$()
END IF

OpenFiles FileName$, Form$(), Fld()             'Open the main database and
                                                '  Note file (.NOT).
IF LOF(Fld(0).RelHandle) = 0 THEN               'Make sure the database was
   PRINT FileName$; ".DAT "; NoFileMsg$         '  found.
   END
END IF

REDIM Fld2(NumFlds) AS FieldInfo                'Make a copy of the field
FOR N = 0 TO Fld(0).Fields                      '  definition array for the
   Fld2(N) = Fld(N)                             '  destination (tem.) file.
NEXT

TempName$ = Path$ + "TEMP.NOT"                  'Make the temp. file's name.

Fld2(0).ScratchI = FREEFILE                     'Get a handle
OPEN TempName$ FOR OUTPUT AS Fld2(0).ScratchI   'Truncate any existing file.
CLOSE Fld2(0).ScratchI
OPEN TempName$ FOR BINARY AS Fld2(0).ScratchI   'Open it for binary

'----- Process all records in the main database.
FOR R& = 1 TO LOF(Fld(0).RelHandle) \ Fld(0).Row

   GetRec R&, Form$(), Fld()                    'Read main database record
                                                '  and all notes
   FOR N = 1 TO Fld(0).Fields                   'Reset all note field's record
      IF Fld(N).FType = NotesFld THEN           '  pointers to 0 so they will
         MID$(Form$(0, 0), Fld(N).Fields) = MKL$(0)'be realocated below.
      END IF
   NEXT

   SaveRec R&, Form$(), Fld2()                  'Save main database record
NEXT                                            '  and all notes to the temp.
                                                '  file.
CLOSE

FileName$ = FileName$ + ".NOT"                  'Compose note file's name
KILL FileName$                                  'Delete the original file
NAME TempName$ AS FileName$                     'Rename the temp. file to the
                                                '  original name.


