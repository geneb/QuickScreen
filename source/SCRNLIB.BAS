'***** ScrnLib.Bas **********************************************************
'Module used for loading and or displaying screens from a screen library
'(.QSL) file.
'
'Copyright (c) 1989, 1990 Crescent Software Inc.
'By Don Malin
'
'The following routines can be called from your program:
'
'    Lib2Scrn - Displays a screen from a screen library.
'
'      LibNo% - Returns a screen number from a "library" screen name.
'               Use this routine the screen library has been loaded.
'
'LibFile2Scrn - Loads and displays an individual screen from a screen
'               library file without having to have the library in memory.
'
' LibScrName$ - Returns a "library" screen name from a screen number.
'               Use this routine the screen library has been loaded.
'
' LoadScrnLib - Loads a Screen library into an array
'
' ScrnLibSize - Returns the size of an integer array required to hold a
'               Screen Library
'****************************************************************************

DEFINT A-Z

DECLARE FUNCTION Exist% (NewSpec$)
DECLARE FUNCTION FileSize& (FileName$)
DECLARE FUNCTION LibNo% (NameInLib$, Lib%())
DECLARE FUNCTION LibScrName$ (ScrnNo%, ScrnLib%())
DECLARE FUNCTION MinInt% (Var1%, Var2%)
DECLARE FUNCTION Monitor% ()
DECLARE FUNCTION Peek1% (Segment, Address)
DECLARE FUNCTION ScrnLibSize% (LibName$)

DECLARE SUB DisplayScrn (BTmp%(), Element%, MonoCode%, WipeType%)
DECLARE SUB FClose (Handle%)
DECLARE SUB FGet (Handle, Text$)
DECLARE SUB FGetA (Handle%, SEG Element AS ANY, NumBytes&)
DECLARE SUB FGetT (Handle, Addr AS ANY, NumBytes&)
DECLARE SUB FOpen (FileName$, Handle%)
DECLARE SUB FSeek (Handle, Offset&)
DECLARE SUB GetVMode (Mode, Page, PageSize, Rows, Columns)
DECLARE SUB HideCursor ()
DECLARE SUB MouseTrap (UlRow, UlCol, BrRow, BrCol)
DECLARE SUB MPaintBox (UlRow, UlCol, LrRow, LrCol, Colr)
DECLARE SUB QBLoad (FileName$, SEG Element AS ANY)
DECLARE SUB ShowCursor ()

'****************************************************************************
'Displays a screen from a Screen Library using whatever wipe type was
'specified when saving the screen.
'
'NameInLib$ is the name of the screen in the library (8 characters or less).
'
'ScrnLib() is the screen library array.
'
'MonoCode can be set to 3 on entry to force the screen to be displayed
'  using monochrome colors only (black and white).
'
'Attribute can be set to -1 on entry to force a screen mode change when
'  going from a large screen (ie.>25) to a smaller one.  -2 supresses
'  drop shadows.  Attribute will be returned holding the following
'  information:
'            Byte 1 (Attribute MOD 256) = Required screen lines for display
'            Byte 2 (Attribute \ 256)   = Wipe type code
'
'Error Codes: 0 = No Error
'             1 = Screen name is not in the library
'             2 = Screen is to big for monitor
'****************************************************************************
'
SUB Lib2Scrn (NameInLib$, ScrnLib(), MonoCode, Attribute, ErrorCode) STATIC

    ErrorCode = 0                               'No error yet
                                                'Fix the library name
    NameInLib$ = UCASE$(LTRIM$(RTRIM$(NameInLib$)))
    N = LibNo(NameInLib$, ScrnLib())            'Get the screen number
    IF N = 0 THEN                               'If its 0, set error and
       ErrorCode = 1                            '  bail out
       EXIT SUB
    END IF

    DicOff = N * 7                              'Offset into dictionary
    ScrOff = ScrnLib(DicOff)                    'Offset of screen

    '----- Find the screen coodinates
    UlRow = ScrnLib(ScrOff) MOD 256             'Upper left row
    UlCol = ScrnLib(ScrOff) \ 256               'Upper left column
    BrRow = ScrnLib(ScrOff + 1) MOD 256         'Bottom right row
    BrCol = ScrnLib(ScrOff + 1) \ 256           'Bottom right column


    ReqLines = ScrnLib(DicOff + 2) \ 256        'See how many screen lines
                                                '  are required for display
    SELECT CASE Monitor%                        'See how many lines monitor
                                                '  can display
       CASE 6, 7, 11                            'VGA and 8514a adapters
          Rows = MinInt(ReqLines, 50)           '50 lines
       CASE 4, 5                                'EGA adapter
          Rows = MinInt(ReqLines, 43)           '43 lines
       CASE ELSE                                'All others
          Rows = 25                             '25 lines
    END SELECT

    IF BrRow > Rows THEN                        'Will the screen fit?
       ErrorCode = 2                            'No, set error and bail out
       EXIT SUB
    END IF

    GetVMode 0, 0, 0, ScrRows, 0                'Get current screen lines
                                                'Do we need to change mode?
    IF Rows > ScrRows OR Attribute = -1 OR Attribute = -3 THEN
       HideCursor
       WIDTH , Rows                             'Yes, change screen mode
       MouseTrap 1, 1, Rows, 80                 'Let the mouse use all lines
       ShowCursor
    END IF

    '----- Display the screen using the specified wipe type
    DisplayScrn ScrnLib(), ScrOff, MonoCode, ScrnLib(DicOff + 2) MOD 256

    '----- Paint "Drop Shadows"
    IF Attribute > -2 THEN
       IF BrCol < 79 THEN                       'Paint right side
          MPaintBox UlRow + 1, BrCol + 1, MinInt(BrRow + 1, ScrRows), BrCol + 2, 8
       END IF
       IF BrRow < ScrRows THEN                  'Paint bottom line
          MPaintBox BrRow + 1, UlCol + 2, BrRow + 1, BrCol, 8
       END IF
    END IF

    Attribute = ScrnLib(DicOff + 2)

END SUB

'****************************************************************************
'Displays a screen from a Screen Library file using whatever wipe type was
'specified when saving the screen.
'
'LibName$ is the name of the screen library file.
'
'ScrnName$ is the name of the screen in the library (8 characters or less).
'
'MonoCode can be set to 3 on entry to force the screen to be displayed
'  using monochrome colors only (black and white).
'
'Attribute can be set to -1 on entry to force a screen mode change when
'  going from a large screen (ie.>25) to a smaller one.  -2 supresses
'  drop shadows.  Attribute will be returned holding the following
'  information:
'            Byte 1 (Attribute MOD 256) = Required screen lines for display
'            Byte 2 (Attribute \ 256)   = Wipe type code
'
'Error Codes: 0 = No Error
'            -1 = Library is not on disk or is incompatable
'             1 = Screen name is not in the library
'             2 = Screen is to big for monitor
'****************************************************************************
'
SUB LibFile2Scrn (LibName$, ScrnName$, MonoCode, Attribute, ErrorCode) STATIC

    ErrorCode = -1                              'Guilty until proven otherwise

    DispMode = Attribute                        'Save incomming Attribute

                                                'Add file extension if omited
    IF INSTR(LibName$, ".") = 0 THEN LibName$ = LibName$ + ".QSL"

    IF NOT Exist(LibName$) THEN EXIT SUB        'Bail out if file isn't there

    FOpen LibName$, Handle                      'Open the library file
   
    FSeek Handle, 7                             'Skip past BSAVE header
    FGetT Handle, NumScrns, 2                   'Read number of screens
    FGetT Handle, Code, 2                       'Read identifier code
  
    IF Code <> 26 THEN                          'Check file for validity
       FClose Handle
       EXIT SUB
    END IF
  
    NameInLib$ = STRING$(8, 0)                  'Fix the screen name
    MID$(NameInLib$, 1) = UCASE$(LTRIM$(RTRIM$(ScrnName$)))

    Test$ = SPACE$(8)                           'Make buffer for screen names
    FOR N = 1 TO NumScrns
        FSeek Handle, N * 14 + 13               'Read each screen name
        FGet Handle, Test$
        IF Test$ = NameInLib$ THEN EXIT FOR     'Bail out if we find a match
    NEXT

    IF N > NumScrns THEN                        'If we looped past last name
       ErrorCode = 1                            '  bail out
       FClose Handle
       EXIT SUB
    END IF

    DicOff& = N * 14 + 7&                       'Calculate Offset into
                                                '  dictionary for this screen
    FSeek Handle, DicOff&                       '  and seek there.
    FGetT Handle, X, 2                          'Read word offset of screen
    ScrOff& = X * 2& + 7                        'Make it a byte offset

    FGetT Handle, ScrLen, 2                     'Read screen length in bytes
    FGetT Handle, Attribute, 2                  'Read screen attributes

    REDIM Temp(ScrLen \ 2 + 1)                  'Make an array to hold screen

    FSeek Handle, ScrOff&                       'Seek to beginning of screen
    FGetA Handle, Temp(0), CLNG(ScrLen)         'Load it into the array
    FClose Handle                               'Close the library file

    '----- Find the screen coodinates
    UlRow = Temp(0) MOD 256                     'Upper left row
    UlCol = Temp(0) \ 256                       'Upper left column
    BrRow = Temp(1) MOD 256                     'Bottom right row
    BrCol = Temp(1) \ 256                       'Bottom right column

    ReqLines = Attribute \ 256                  'See how many screen lines
                                                '  are required for display
    SELECT CASE Monitor%                        'See how many lines monitor
                                                '  can display
       CASE 6, 7, 11                            'VGA and 8514a adapters
          Rows = MinInt(ReqLines, 50)           '50 lines
       CASE 4, 5                                'EGA adapter
          Rows = MinInt(ReqLines, 43)           '43 lines
       CASE ELSE                                'All others
          Rows = 25                             '25 lines
    END SELECT

    IF BrRow > Rows THEN                        'Will the screen fit?
       ErrorCode = 2                            'No, set error and bail out
       ERASE Temp
       EXIT SUB
    END IF

    GetVMode 0, 0, 0, ScrRows, 0                'Get current screen lines
                                                'Do we need to change mode?
    IF Rows > ScrRows OR DispMode = -1 OR DispMode = -3 THEN
       HideCursor
       WIDTH , Rows                             'Yes, change screen mode
       MouseTrap 1, 1, Rows, 80                 'Let the mouse use all lines
       ShowCursor
    END IF

    '----- Display the screen using the specified wipe type
    DisplayScrn Temp(), 0, MonoCode, Attribute MOD 256

    '----- Paint "Drop Shadows"
    IF DispMode > -2 THEN
       IF BrCol < 79 THEN                       'Paint right side
          MPaintBox UlRow + 1, BrCol + 1, MinInt(BrRow + 1, ScrRows), BrCol + 2, 8
       END IF
       IF BrRow < ScrRows THEN                  'Paint bottom line
          MPaintBox BrRow + 1, UlCol + 2, BrRow + 1, BrCol, 8
       END IF
    END IF

    '----- Clean up memory
    ERASE Temp
    NameInLib$ = ""
    Test$ = ""

    ErrorCode = 0

END SUB

'****************************************************************************
'Returns a library screen number from screen name
'
'NameInLib$ is the name of the screen in the library (8 characters or less).
'
'ScrnLib() is the screen library array.
'****************************************************************************
'
FUNCTION LibNo (NameInLib$, ScrnLib()) STATIC

    LibNo = 0

    NamLen = LEN(NameInLib$)
    LibSeg = VARSEG(ScrnLib(0))

    FOR N = 1 TO ScrnLib(0)

        Match = -1
        Offset = VARPTR(ScrnLib(N * 7 + 3))

        FOR P = 1 TO 8

            PeekChar = Peek1%(LibSeg, Offset + P - 1)
            IF P > NamLen THEN
               MidChar = 0
            ELSE
               MidChar = ASC(MID$(NameInLib$, P, 1))
            END IF

            IF PeekChar = 0 AND MidChar = 32 THEN EXIT FOR

            IF PeekChar <> MidChar THEN
               Match = 0
               EXIT FOR
            END IF

        NEXT

        IF Match THEN
           LibNo = N
           EXIT FOR
        END IF

    NEXT

END FUNCTION

'****************************************************************************
'Returns a "Library" screen name given a screen number.
'
'ScrnNo - Number of the screen in the library.
'
'ScrnLib() - The Screen Library array.
'
'Example: To load a string array with the names of all the screens in a
'         library:
'
'         REDIM Array$(ScrnLib(0))      'ScrnLib(0) gives the last screen #
'         FOR N = 1 TO ScrnLib(0)
'             Array$(N) = LibScrName$(N, ScrnLib())
'         NEXT
'****************************************************************************
FUNCTION LibScrName$ (ScrnNo, ScrnLib()) STATIC

    IF ScrnNo > ScrnLib(0) THEN
       LibScrName$ = ""
       EXIT FUNCTION
    END IF

    LibSeg = VARSEG(ScrnLib(0))
    Offset = VARPTR(ScrnLib(ScrnNo * 7 + 3))
    SName$ = SPACE$(8)

    FOR P = 1 TO 8
        Char = Peek1(LibSeg, Offset + P - 1)
        IF Char = 0 THEN EXIT FOR
        MID$(SName$, P) = CHR$(Char)
    NEXT

    LibScrName$ = RTRIM$(SName$)

END FUNCTION

'***************************************************************************
'Loads a Screen library into an integer array.
'
'LibName$  Specifies the screen library file to load.  ".QSL" extension
'          is optional.
'ScrnLib() is the array that will hold the library.  It must have previously
'          been dimensioned to a size large enough to hold the library.
'          See the "ScrnLibSize" function for determining the correct size.
'ErrorCode will be returned holding either 0 if the library was loaded
'          succesfully or -1 if the file did not exist or was incompatable.
'***************************************************************************
'
SUB LoadScrnLib (LibName$, ScrnLib(), ErrorCode) STATIC

    ErrorCode = -2                      'Guilty until proven otherwise

    IF INSTR(LibName$, ".") = 0 THEN LibName$ = LibName$ + ".QSL"

    IF NOT Exist(LibName$) THEN EXIT SUB

    H = FREEFILE
    OPEN LibName$ FOR BINARY AS #H      'Open the file in binary mode
    DimSize& = (LOF(H) - 8) \ 2         'Calculate the size of an integer
                                        '  array used to hold the library
    ErrorCode = -1
    IF DimSize& < 2 THEN EXIT SUB       'Is the file big enough?
                                        'Is it to big?
    IF DimSize& > UBOUND(ScrnLib) THEN EXIT SUB
                                           
    SEEK #H, 10                         'Skip past BASVE header
    GET #H, , Code                      'Read first 2 bytes from library
    CLOSE #H                            'Close the file
                                       
    IF Code <> 26 THEN EXIT SUB         'Check file for validity

    QBLoad LibName$, ScrnLib(0)         'Load it in

    ErrorCode = 0                       'No errors so set flag

END SUB

'***************************************************************************
'Returns the size of an integer array required to hold a Screen Library
'
'LibName$ - Specifies the Sreen Library file name to check.
'
'Function returns the size in words required to hold the screen library.
'***************************************************************************
'
FUNCTION ScrnLibSize% (LibName$) STATIC

    ScrnLibSize% = 0

    IF INSTR(LibName$, ".") = 0 THEN LibName$ = LibName$ + ".QSL"

    IF Exist(LibName$) THEN
       Test& = (FileSize&(LibName$) - 8) \ 2
       IF Test& > 0 THEN
          ScrnLibSize% = Test&
       END IF
    END IF

END FUNCTION

