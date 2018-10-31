'***** ScrnFile.Bas *********************************************************
'Module used for loading and displaying a screen (.SCR) file.
'
'Copyright (c) 1989, 1990 Crescent Software Inc.
'By Don Malin
'
'The following routine can be called from your program:
'
'  LoadScreen - Loads and displays a screen file (.SCR) from disk.
'****************************************************************************
'
DEFINT A-Z

DECLARE FUNCTION Exist% (NewSpec$)
DECLARE FUNCTION FileSize& (FileName$)
DECLARE FUNCTION MinInt% (Var1%, Var2%)
DECLARE FUNCTION Monitor% ()

DECLARE SUB DisplayScrn (BTmp%(), Element%, MonoCode%, WipeType%)
DECLARE SUB GetVMode (Mode, Page, PageSize, Rows, Columns)
DECLARE SUB HideCursor ()
DECLARE SUB MouseTrap (UlRow, UlCol, BrRow, BrCol)
DECLARE SUB MPaintBox (UlRow, UlCol, LrRow, LrCol, Colr)
DECLARE SUB QBLoad (FileName$, SEG Element AS ANY)
DECLARE SUB ShowCursor ()

'****************************************************************************
'Loads and displays a screen file using whatever wipe type was specified when
'the file was saved.
'
'ScrName$ is the name of the screen to load.  An extension is optional and
'  will default to ".SCR" if omitted.
'
'MonoCode can be set to -3 on entry to force the screen to be displayed
'  using monochrome colors only (black and white).
'
'Attribute can be set to -1 on entry to force a screen mode change when
'  going from a large screen (ie.>25) to a smaller one.  -2 supresses
'  drop shadows.  Attribute will be returned holding the following
'  information:
'            Byte 1 (Attribute MOD 256) = Required screen lines for display
'            Byte 2 (Attribute \ 256)   = Wipe type code
'
'Error Codes: 0 - No Error
'             1 - Screen name was not found on disk
'             2 - Screen is to big for monitor
'             3 - Not a QuickScreen compatable screen
'****************************************************************************
'
SUB LoadScreen (ScrName$, MonoCode, Attribute, ErrorCode) STATIC

    ErrorCode = 0                               'No error yet

    LName$ = ScrName$                           'Fix screen name
    IF INSTR(LName$, ".") = 0 THEN LName$ = LName$ + ".SCR"


    IF Exist%(LName$) THEN                      'Does the file exist?
       DimSize& = (FileSize&(LName$) - 8) \ 2   'Yes, get size for array to
                                                '  hold file
       IF DimSize& <= 4808 THEN                 'If its not too big,
          REDIM BTmp(DimSize&)                  '  make an array to hold it
          QBLoad LName$, BTmp(0)                'Load it in
       END IF
                                                'Is it a legitimate file?
       IF BTmp(0) MOD 256 <> 26 OR DimSize& > 4808 THEN
          ErrorCode = 3                         'No, set error and bail out
          EXIT SUB
       END IF

       '----- Find the screen coodinates
       UlRow = BTmp(2) MOD 256
       UlCol = BTmp(2) \ 256
       BrRow = BTmp(3) MOD 256
       BrCol = BTmp(3) \ 256

       ReqLines = BTmp(1) \ 256                 'See how many screen lines
                                                '  are required for display
       SELECT CASE Monitor%                     'See how many lines monitor
                                                '  can display
          CASE 6, 7, 11                         'VGA and 8514a adapters
             Rows = MinInt(ReqLines, 50)        '50 lines
          CASE 4, 5                             'EGA adapter
             Rows = MinInt(ReqLines, 43)        '43 lines
          CASE ELSE                             'All others
             Rows = 25                          '25 lines
       END SELECT

       IF BrRow > Rows THEN                     'Will the screen fit?
          ErrorCode = 2                         'No, set error and bail out
          EXIT SUB
       END IF

       GetVMode 0, 0, 0, ScrRows, 0             'Get current screen lines
                                                'Do we need to change mode?
       IF Rows > ScrRows OR Attribute = -1 OR Attribute = -3 THEN
          HideCursor
          WIDTH , Rows                          'Yes, change screen mode
          MouseTrap 1, 1, Rows, 80              'Let the mouse use all lines
          ShowCursor
       END IF

       '----- Display the screen using the specified wipe type
       DisplayScrn BTmp(), 2, MonoCode, BTmp(1) MOD 256

       '----- Paint "Drop Shadows"
       IF Attribute > -2 THEN
          IF BrCol < 79 THEN                    'Paint right side
             MPaintBox UlRow + 1, BrCol + 1, MinInt(BrRow + 1, ScrRows), BrCol + 2, 8
          END IF
          IF BrRow < ScrRows THEN               'Paint bottom line
             MPaintBox BrRow + 1, UlCol + 2, BrRow + 1, BrCol, 8
          END IF
       END IF

       Attribute = BTmp(1)

       ERASE BTmp

    ELSE
       ErrorCode = 1
    END IF

END SUB

