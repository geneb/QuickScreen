'***** dbLIBMod.Bas - db/LIB support module *********************************
' by Donald R. Malin,  Copyright (c) 1989 Crescent Software, Inc.

' The following services are provided for interfacing QuickScreen form
' definitions with db/LIB files:

' [db2Form] -      Transfers and converts data from a db/LIB record to the
'                  "Form$()" array for editing.
' [dbDefineRec] -  Defines a db/LIB record structure from a QuickScreen
'                  Form Definition.
' [dbNumericStr] - Converts a formated numeric string to a form compatable
'                  with db/LIB "N" field type.
' [Form2db] -      Transfers and converts data from the "Form$()" array to
'                  a db/LIB record.
'****************************************************************************

DEFINT A-Z

'$INCLUDE: 'FieldInf.bi'

DECLARE FUNCTION dbNumericStr$ (Number$)

DECLARE SUB ReturnSTR (DatFile%, Status, FldNum, FldName$, FldType$, FldLen, Decimal)
DECLARE SUB DefineSTR (DatFile%, Status, FldNum, FldName$, FldType$, FldLen, Decimal)
DECLARE SUB GetFLD (DatFile, Status, FldNum, FldName$, FldData$, RecData$)
DECLARE SUB PutFLD (DatFile, Status, FldNum, FldName$, FldData$, RecData$)
DECLARE SUB GetMEMO (MemFile, Status, Sector$, MemoData$)
DECLARE SUB PutMEMO (MemFile, Status, Sector$, MemoData$)

'****************************************************************************
'Transfers and converts data from a db/LIB record to the "Form$()" array for
'editing.
'****************************************************************************
SUB db2Form (First, Last, DatFile, RecData$, Fld() AS FieldInfo, Form$(), Status) STATIC

    FOR N = First TO Last                       'Work on the range specified      

        FType = Fld(N).FType                    'Get the fields type
        IF FType THEN
           IF FType = Relational THEN FType = Fld(N).ScratchI

           FldNum = 0                           'Get the field's data
           GetFLD DatFile, Status, FldNum, Fld(N).FldName, FldData$, RecData$
           IF Status THEN EXIT SUB              'Bail out if error
                                                'Make sure the field is the
                                                '  right length
           IF LEN(Form$(N, 0)) <> Fld(N).RCol - Fld(N).LCol + 1 THEN
              Form$(N, 0) = SPACE$(Fld(N).RCol - Fld(N).LCol + 1)
           END IF

           SELECT CASE FType                    'Branch according to type

           CASE DateFld                         'Convert dbLIB date to US
              LSET Form$(N, 0) = "  -  -"       '  format (MM-DD-YYYY)
              MID$(Form$(N, 0), 1) = MID$(FldData$, 5, 2)
              MID$(Form$(N, 0), 4) = MID$(FldData$, 7, 2)
              MID$(Form$(N, 0), 7) = MID$(FldData$, 1, 4)

           CASE EuroDateFld                     'Convert dbLIB date to European
              LSET Form$(N, 0) = "  -  -"       '  format (DD-MM-YYYY)
              MID$(Form$(N, 0), 1) = MID$(FldData$, 7, 2)
              MID$(Form$(N, 0), 4) = MID$(FldData$, 5, 2)
              MID$(Form$(N, 0), 7) = MID$(FldData$, 1, 4)

           CASE PhoneFld                        'Format Phone numbers
              LSET Form$(N, 0) = "(   )    -"
              MID$(Form$(N, 0), 2) = MID$(FldData$, 1, 3)
              MID$(Form$(N, 0), 7) = MID$(FldData$, 4, 3)
              MID$(Form$(N, 0), 11) = MID$(FldData$, 7, 4)

           CASE SoSecFld                        'Format Social Security number
              LSET Form$(N, 0) = "   -  -"
              MID$(Form$(N, 0), 1) = MID$(FldData$, 1, 3)
              MID$(Form$(N, 0), 5) = MID$(FldData$, 4, 2)
              MID$(Form$(N, 0), 8) = MID$(FldData$, 6, 4)

           CASE ZipFld                          'Format Zip Code
              LSET Form$(N, 0) = "     -"
              MID$(Form$(N, 0), 1) = MID$(FldData$, 1, 5)
              MID$(Form$(N, 0), 7) = MID$(FldData$, 6, 4)

           CASE NotesFld                        'Retrieve Notes data from DBT
              MemFile = Fld(N).RelHandle        '  file.
              GetMEMO MemFile, Status, FldData$, Form$(N, 0)
              IF Status THEN EXIT SUB

           CASE ELSE                            'All other field types require
              LSET Form$(N, 0) = FldData$       '  no translation

           END SELECT
        END IF
    NEXT

END SUB

'****************************************************************************
'Defines a db/LIB record structure from a QuickScreen Form Definition.
'****************************************************************************
SUB dbDefineRec (First, Last, DatFile, Fld() AS FieldInfo, Status) STATIC

    FOR N = First TO Last                       'Use the range specified

        FldName$ = Fld(N).FldName               'Make a copy of field name
        Decimal = 0                             'No decimal places yet

        SELECT CASE Fld(N).FType                'Branch acording to type
                                                'Numeric fields
           CASE IntFld, LongIntFld, SngFld, DblFld, MoneyFld
              FldType$ = "N"                    'Set dbLIB type flag
                                                'Set decimal places
              IF Fld(N).Decimals > -1 THEN Decimal = Fld(N).Decimals
                                                'Set storage length
              FldLen = Fld(N).RCol - Fld(N).LCol + 1

           CASE DateFld, EuroDateFld            'Date fields
              FldType$ = "D"                    'Set dbLIB type flag
              FldLen = 8                        'Store 8 characters

           CASE LogicalFld                      'Logical fields
                                                'Is it T/F or Y/N?
              IF INSTR("TFYN", UCASE$(MID$(Fld(FldNo).RelFile, 1, 1))) THEN
                 FldType$ = "L"                 'Set dbLIB logical type
              ELSE
                 FldType$ = "C"                 'Set dbLIB character type
              END IF
              FldLen = 1                        'Store 1 character

           CASE NotesFld                        'Notes fields (multi-line text)
              FldType$ = "M"                    'Set dbLIB type flag
              FldLen = 10                       'Store 10 characters

           CASE ELSE                            'All others are character data
              FldType$ = "C"
              FldLen = Fld(N).StorLen           'Use storage length from "Fld"

        END SELECT

        IF Fld(N).FType THEN
           FldNum = 0                           'Set the definition
           DefineSTR DatFile, Status, FldNum, FldName$, FldType$, FldLen, Decimal
           IF Status THEN EXIT SUB              'Bail out if there was an error
        END IF
    NEXT

END SUB

'****************************************************************************
'Converts a formated numeric string to a form compatable with db/LIB "N"
'field type.  ie. strips all non numeric characters
'****************************************************************************
FUNCTION dbNumericStr$ (Number$) STATIC

   Num$ = RTRIM$(Number$)               'Make a copy of the number$
   LN = LEN(Num$)                       'Save it's length
                                        'Make a table of legal characters if
                                        '  it hasn't already been done.
   IF LEN(Tbl$) = 0 THEN Tbl$ = "-+.eEdD0123456789"

   X = 1                                'Set string pointer position to 1

   DO UNTIL X > LN                      'examine characters to the end
                                        'Is character in the table?
      IF INSTR(Tbl$, MID$(Num$, X, 1)) = 0 THEN
                                        'No, delete the character by shifting
         LSET Num$ = LEFT$(Num$, X - 1) + MID$(Num$, X + 1)
         LN = LN - 1                    'Show length as 1 less
         X = X - 1                      'Set pointer to examine the shifted
      END IF                            '  character

      X = X + 1                         'Examine next character
   LOOP

   dbNumericStr$ = LEFT$(Num$, LN)      'Assign the function

END FUNCTION

'****************************************************************************
'Transfers and converts data from the "Form$()" array to a db/LIB record.
'****************************************************************************
SUB Form2db (First, Last, DatFile, RecData$, Fld() AS FieldInfo, Form$(), Status) STATIC

    FOR N = First TO Last                       'Assign all fields in range

        FType = Fld(N).FType                    'Get the field's type

        IF FType THEN
           IF FType = Relational THEN FType = Fld(N).ScratchI

           FldNum = 0                           'Get the fields length
           ReturnSTR DatFile, Status, FldNum, Fld(N).FldName, FldType$, FldLen, Decimal
           IF Status THEN EXIT SUB              'Bail out on an error

           FldData$ = SPACE$(FldLen)            'Make a buffer$ to hold data

           SELECT CASE FType                    'Branch acording to field type
                                                'Numeric fields
           CASE IntFld, LongIntFld, SngFld, DblFld, MoneyFld
                                                'Convert formatted number
              RSET FldData$ = dbNumericStr$(Form$(N, 0))

           CASE DateFld                         'US Date, make it "YYYYMMDD"
              MID$(FldData$, 1) = MID$(Form$(N, 0), 7, 4)
              MID$(FldData$, 5) = MID$(Form$(N, 0), 1, 2)
              MID$(FldData$, 7) = MID$(Form$(N, 0), 4, 2)

           CASE EuroDateFld                     'European Date, same as above
              MID$(FldData$, 1) = MID$(Form$(N, 0), 7, 4)
              MID$(FldData$, 5) = MID$(Form$(N, 0), 4, 2)
              MID$(FldData$, 7) = MID$(Form$(N, 0), 1, 2)

           CASE NotesFld                        'Save Notes field to DBT file
              MemFile = Fld(N).RelHandle
              IF LEN(Form$(N, 0)) = 0 THEN Form$(N, 0) = " "
              PutMEMO MemFile, Status, FldData$, Form$(N, 0)
              IF Status THEN EXIT SUB

           CASE ELSE                            'All others. Get the data from
                                                '  the Form$(N, 0) buffer
              LSET FldData$ = MID$(Form$(0, 0), Fld(N).Fields, Fld(N).StorLen)

           END SELECT
                                                'Save the field to "RecData$"
           PutFLD DatFile, Status, FldNum, FldName$, FldData$, RecData$
           IF Status THEN EXIT SUB              'Bail out on an error
        END IF
    NEXT

END SUB

