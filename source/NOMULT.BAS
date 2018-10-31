'****** NoMult.Bas - Module used to link out code for multiple choice
'                    fields.

DEFINT A-Z

'$INCLUDE: 'DefCnf.BI'

SUB VertMenu (Item$(), Choice, MaxLen, BoxBot, Ky$, Action, Cnf AS Config) STATIC
    Ky$ = CHR$(13)
    Action = 4
    Choice = 0
END SUB

