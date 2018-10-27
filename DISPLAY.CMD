'***** Display.Cmd - Display command file

LoadLib Show.qsl                'Load the "Show.qsl" screen library

Color 14, 1                     'Set color for messages
Cls                             'Clear the screen

Locate 5, 17                    'Locate the cursor for the message below
Message "QuickSCREEN    D I S P L A Y    Demonstration"

Locate 8, 27
Message "By Crescent Software Inc."

Locate 25, 28
Message "Press any key to begin..."

KeyPress                        'Wait for a key press


Color 7, 0                      'Reset color for rest of demo

Main:
    '----- Show the openning screen
    Display CSDemo              '"CSDemo" is the name of the screen to display
    Pause 3                     'Pause for 3 seconds

    '----- Show QuickScreens banner screen
    Display Banner
    Pause 3

    '----- Show "File" menu and Dialog box screens
    Display FMenu -2            '-2 suppresses drop shadows on partial screens
    Pause 1

    Display FMenu2 -2
    Display NewHelp
    Pause 1

    Display FMenu3 -2
    Display OpenHelp
    Pause .7

    Display FDial -2
    Pause 2

    Display FDial1 -2
    Pause 1.5

    '----- Show the working screen
    Cls
    Display Employee
    Pause 4

    '----- Show the Help screens
    Display F1Help -2
    Display F1HlpLin
    Pause 1.5

    Display H1
    Pause 2

    Display BlockOps
    Pause 3

    Display EndHelp
    Pause 2

    '----- Simulate a block move
    Display BlkM1 -2
    Pause .1

    Display BlkM2 -2
    Pause .1

    Display BlkM3 -2
    Pause .1

    Display BlkM4 -2
    Pause 2

    Display BlkMen1 -2
    Pause .5

    Display BlkMen2 -2
    Pause 2

    Display BlkMov1 -2
    Pause .1

    Display BlkMov2 -2
    Pause .1

    Display BlkMov3 -2
    Pause .1

    Display BlkMov4 -2
    Pause 3

    '----- Simulate drawing a new data entry field
    Display Billrat1
    Pause .1

    Display Billrat2 -2
    Pause 1

    Display Billrat3 -2
    Pause .1

    Display Billrat4 -2
    Pause .1

    Display Billrat5 -2
    Pause .1

    Display Billrat6 -2
    Pause 1.5

    Display Billrat7 -2
    Pause .1

    Display Billrat8 -2
    Pause .1

    Display Billrat9 -2
    Pause .1

    Display Billra10 -2
    Pause 3

    '----- Simulate picking a color
    Display SetClr1 -2
    Pause 1

    Display SetClr2 -2
    Pause 1

    Display SetClr3 -2
    Pause 1.5

    '----- Simulate painting the address field
    Display Paint1
    Pause 2

    Display Paint2 -2
    Pause .1

    Display Paint3 -2
    Pause .1

    Display Paint4 -2
    Pause .1

    Display Paint5 -2
    Pause .5

    Display Paint6 -2
    Pause 3

    '----- Simulate defining fields
    Display DefMen1
    Pause 3

    Display Def1
    Pause 3

    Display Def2
    Pause 4

    Display Def3
    Pause 3

    Display Def4
    Pause 5

    Display Def6
    Pause 2

    Display Def7
    Pause 3

    Display Def8
    Pause 3

    Display Def9
    Pause 6

    Display Def10
    Pause 1.5

    Display Paint6
    Pause 3

    '----- Simulate saving the form
    Display Sv1
    Pause 2

    Display Sv2
    Pause 2

    Display Sv3
    Pause 5

    Display Paint6
    Pause 4

    '----- Simulate quitting the screen builder
    Display Quit1
    Pause 1

    Display Quit2
    Pause 2

    '----- Simulate starting QuickBASIC
    Cls
    Message "C:\QB>"
    Pause 1     
    Message "QB Demo /l Forms"
    Pause 3

    '----- Simulate running a demo program
    Display QB1
    Pause 4

    Display QB2
    Pause 1.5

    Cls
    Pause 1

    Display Run1
    Pause 3

    '----- Simulate filling in the form
    Display Run2 -2
    Pause 1.5

    Display Run3 -2
    Pause 1.5

    Display Run4 -2
    Pause 1.5

    Display Run5 -2
    Pause 1.5

    Display Run6 -2
    Pause 10

    Cls

GOTO Main               'Do it again
