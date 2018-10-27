'********** NoNotes.Bas - Used to link out code for "Notes" fields

'Copyright (c) 1988 Crescent Software


DEFINT A-Z

'$INCLUDE: 'QEditype.BI'

SUB QEdit (Array$(), X$, Action%, Ed AS EditInfo) STATIC
    LOCATE , , 1
    X$ = INKEY$
    Ed.CurLine = 1
END SUB

