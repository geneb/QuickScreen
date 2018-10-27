CONST UCase = 1
CONST Intgr = 2
CONST FloatP = 3
CONST Money = 4
CONST MultFld = 5
CONST Propr = 6
CONST ButtonFld = 7

TYPE FormInfo
   StartEl AS INTEGER           'Starting (base) element of the current form
   FldNo AS INTEGER             'Current field number
   PrevFld AS INTEGER           'Previous field (different from above only
                                '  when first moving to a new field.
   FldEdited AS INTEGER         'Specifies that a field has been changed (-1)
   KeyCode AS INTEGER           'ASCII value of the last key pressed. Negative
                                '  for extended keys ie. F1 = -59
   TxtPos AS INTEGER            'Cursor position relative to current field.
   InsStat AS INTEGER           'Current insertion mode status (-1) insert ON)
   Presses AS INTEGER           'Number of mouse presses since last press.
   MRow AS INTEGER              'Mouse row # at last press
   MCol AS INTEGER              'Mouse column # at last press
   DoingMult AS INTEGER         'True if handling a multiple choice field
   Edited AS INTEGER            'True if anything on the form has changed.
END TYPE

COMMON Choice$()

