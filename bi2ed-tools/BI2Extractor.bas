'Extracts files from a BI2 LIB file
#CONSOLE OFF
#DIM ALL

%CCWIN = 1  ' Include GUI API calls
#INCLUDE "WIN32API.INC"

$PALETTEFILE = "GAME.PAL"

'Fonts
%FONT_ARIAL = 0
%FONT_ARIALBOLD = 1
%FONT_COURIER = 2
%FONT_FIXEDSYS = 3
%FORMBGCOLOR = %COLOR_BTNFACE  '&HE0E0E0

'System-Variablen
GLOBAL hWndMain&          'Window-Handle
GLOBAL hInstance&         'Program-Handle
GLOBAL nComponents&       'Number of window controls
GLOBAL compnames$()       'Name of window controls
GLOBAL hComp&()           'Window handles of all controls
GLOBAL orgWndProc&()      'Original WindowProc of all controls
GLOBAL hGlobalBGBrush&    'Brush for window background
GLOBAL bgcolor&           'Background color for controls
GLOBAL hFonts&()          'Font handles
GLOBAL TERMINATED&        'Program exit flag
GLOBAL EXEPATH$           'Program folder
GLOBAL isBI3&             '0 = Battle Isle 2 , 1 = Battle Isle 3

'Data extraction
GLOBAL srcfile$
GLOBAL destpath$
GLOBAL archiveinfo$
GLOBAL skipempty&
GLOBAL paletteloaded&
GLOBAL pal?()



'Checks if running for Battle Isle 2 or 3
SUB CheckBIVersion
  LOCAL a$

  isBI3& = 0
  IF INSTR(EXEPATH$, "BI3") > 0 OR INSTR(EXEPATH$, "Battle Isle 3") > 0 OR INSTR(EXEPATH$, "SDI") > 0 THEN isBI3& = 1
END SUB



'Evaluates a string with a constant
FUNCTION EVALCONST&(A$)
  LOCAL P&, Q&, B$, C&, R&

  'Evaluate input
  R& = 0
  P& = 1
  WHILE P& <= LEN(A$)
    Q&=INSTR(P&, A$, ANY " +")
    IF Q& = 0 THEN Q& = LEN(A$)+1
    B$ = UCASE$(TRIM$(MID$(A$, P&, Q&-P&)))
    C& = 0
    IF LEFT$(B$, 1) = "%" THEN B$ = MID$(B$, 2)
    IF VERIFY(B$, "-0123456789") = 0 THEN
      C& = VAL(B$)
    ELSE
      IF LEFT$(B$, 2) = "&H" THEN
        C& = VAL(B$)
      ELSE
        SELECT CASE B$
        CASE "FONT_ARIAL": C& = %FONT_ARIAL
        CASE "FONT_ARIALBOLD": C& = %FONT_ARIALBOLD
        CASE "FONT_COURIER": C& = %FONT_COURIER
        CASE "FONT_FIXEDSYS": C& = %FONT_FIXEDSYS
        CASE "WS_BORDER": C& = %WS_BORDER
        CASE "WS_VSCROLL": C& = %WS_VSCROLL
        CASE "WS_HSCROLL": C& = %WS_HSCROLL
        CASE "WS_DISABLED": C& = %WS_DISABLED
        CASE "BS_GROUPBOX": C& = %BS_GROUPBOX
        CASE "BS_CHECKBOX": C& = %BS_CHECKBOX
        CASE "LVS_REPORT": C& = %LVS_REPORT
        CASE "LVS_SHOWSELALWAYS": C& = %LVS_SHOWSELALWAYS
        CASE "LVS_SINGLESEL": C& = %LVS_SINGLESEL
        CASE "ES_MULTILINE": C& = %ES_MULTILINE
        CASE "ES_WANTRETURN": C& = %ES_WANTRETURN
        CASE "ES_AUTOHSCROLL": C& = %ES_AUTOHSCROLL
        CASE "ES_READONLY": C& = %ES_READONLY
        CASE "CBS_DROPDOWN": C&= %CBS_DROPDOWN
        CASE "CBS_AUTOHSCROLL": C& = %CBS_AUTOHSCROLL
        CASE "CBS_DROPDOWNLIST": C& = %CBS_DROPDOWNLIST
        CASE "TVS_HASLINES": C& = %TVS_HASLINES
        CASE "TVS_HASBUTTONS": C& = %TVS_HASBUTTONS
        CASE " TVS_LINESATROOT": C& = %TVS_LINESATROOT
        END SELECT
      END IF
    END IF
    R& = R& OR C&
    P& = Q&+1
  WEND

  EVALCONST& = R&
END FUNCTION



'Returns the control number for a window handle
FUNCTION FindCompByHWND&(hWnd&)
  LOCAL I&, CNR&

  'Check if handle belongs to main window
  CNR& = -1
  IF hComp&(0) = 0 THEN
    CNR& = 0
  ELSE
    FOR I& = 0 TO nComponents&-1
      IF hComp&(I&) = hWnd& THEN
        CNR& = I&
        EXIT FOR
      END IF
    NEXT I&
  END IF

  FindCompByHWND& = CNR&
END FUNCTION



'Returns the control number for a control name
FUNCTION FindCompByName&(N$)
  LOCAL CNR&

  'Find control
  ARRAY SCAN compnames$(0) FOR nComponents&, COLLATE UCASE, =N$, TO CNR&

  FindCompByName& = CNR&-1
END FUNCTION



'Create window controls
SUB CreateForm(hWnd&)
  LOCAL N&, I&, X&, Y&, WD&, HG&, PARENTNAME$, PARENT&, STYLE&, STYLEX&, FNT&, hParent&, VIS&
  LOCAL szClassName AS ASCIIZ * 64
  LOCAL szText AS ASCIIZ * 256
  LOCAL hChild&, R&

  'Insert main window handle at first position in control array
  compnames$(0) = "FMain"
  hComp&(0) = hWnd&

  'Brush for window background
  hGlobalBGBrush& = CreateSolidBrush(bgcolor&)

  'Create fonts
  hFonts&(%FONT_ARIAL) = CreateFont(-12, 0, 0, 0, 0, %FALSE, %FALSE, %FALSE, %ANSI_CHARSET, %OUT_DEFAULT_PRECIS, %CLIP_DEFAULT_PRECIS, %DEFAULT_QUALITY, %DEFAULT_PITCH OR %FF_SWISS, "Arial")
  hFonts&(%FONT_ARIALBOLD) = CreateFont(-12, 0, 0, 0, %FW_BOLD, %FALSE, %FALSE, %FALSE, %ANSI_CHARSET, %OUT_DEFAULT_PRECIS, %CLIP_DEFAULT_PRECIS, %DEFAULT_QUALITY, %DEFAULT_PITCH OR %FF_SWISS, "Arial")
  hFonts&(%FONT_COURIER) = CreateFont(-11, 0, 0, 0, 0, %FALSE, %FALSE, %FALSE, %ANSI_CHARSET, %OUT_DEFAULT_PRECIS, %CLIP_DEFAULT_PRECIS, %DEFAULT_QUALITY, %DEFAULT_PITCH OR %FF_SWISS, "Courier New")
  hFonts&(%FONT_FIXEDSYS) = CreateFont(-12, 0, 0, 0, 0, %FALSE, %FALSE, %FALSE, %ANSI_CHARSET, %OUT_DEFAULT_PRECIS, %CLIP_DEFAULT_PRECIS, %DEFAULT_QUALITY, %DEFAULT_PITCH OR %FF_SWISS, "Fixedsys")

  'Create controls
  N&=DATACOUNT/11
  FOR I& = 0 TO N&-1
    'Read attributes
    compnames$(I&+1) = READ$(I&*11+1)
    szClassName = READ$(I&*11+2)
    X& = VAL(READ$(I&*11+3))
    Y& = VAL(READ$(I&*11+4))
    WD& = VAL(READ$(I&*11+5))
    HG& = VAL(READ$(I&*11+6))
    PARENTNAME$ = READ$(I&*11+7)
    STYLE& = EVALCONST&(READ$(I&*11+8))
    szText = READ$(I&*11+9)
    FNT& = EVALCONST&(READ$(I&*11+10))
    VIS& = VAL(READ$(I&*11+11))
    IF PARENTNAME$ = "" THEN PARENT& = 0 ELSE PARENT& = FindCompByName&(PARENTNAME$)
    hParent& = hComp&(PARENT&)
    STYLEX& = 0
    STYLE& = STYLE& OR %WS_CHILD
    IF VIS& = 1 THEN STYLE& = STYLE& OR %WS_VISIBLE
    SELECT CASE szClassName
    CASE "EDIT", "BUTTON", "COMBOBOX", "SysListView32":
      IF (STYLE& AND %ES_MULTILINE) = 0 THEN STYLE& = STYLE& OR %WS_TABSTOP
    END SELECT
    IF szClassName = "PBTabpage" OR LEFT$(compnames$(I&+1), 2) = "GB" THEN STYLEX& = STYLEX& OR %WS_EX_CONTROLPARENT
    'Create control
    hChild& = CreateWindowEx(STYLEX&, szClassName, BYVAL %NULL, STYLE&, X&, Y&, WD&, HG&, hParent&, I&+1, hInstance&, BYVAL %NULL)
    hComp&(I&+1) = hChild&
    nComponents& = I&+2
    'Set additional control properties
    IF szText <> "" THEN SendMessage hChild&, %WM_SETTEXT, 0, VARPTR(szText)
    SendMessage hChild&, %WM_SETFONT, hFonts&(FNT&), 1
    'set new WindowProc if required
    IF PARENT& > 0 AND orgWndProc&(PARENT&) = 0 AND LEFT$(compnames$(PARENT&), 2) <> "TP" THEN
      orgWndProc&(PARENT&) = SetWindowLong(hParent&, %GWL_WNDPROC, CODEPTR(WndProc))
    END IF
    IF (szClassName = "EDIT" OR szClassName = "RichEdit" OR szClassName = "COMBOBOX") AND orgWndProc&(I&+1) = 0 THEN
      orgWndProc&(I&+1) = SetWindowLong(hChild&, %GWL_WNDPROC, CODEPTR(WndProc))
    END IF
  NEXT I&

  SendMessage hComp&(FindCompByName&("CBSkipEmpty")), %BM_SETCHECK, %BST_CHECKED, 0

  DragAcceptFiles hWnd&, %TRUE

  DATA "GBFiles", "BUTTON", 6, 4, 1004, 436, "", %BS_GROUPBOX, "Drop files to extract into this area", %FONT_ARIAL, 1
  DATA "LFiles", "STATIC", 10, 16, 984, 416, "GBFiles", 0, "", %FONT_COURIER, 1
  DATA "LInfo", "STATIC", 10, 444, 600, 20, "", 0, "", %FONT_ARIALBOLD, 1
  DATA "LError", "STATIC", 10, 464, 600, 20, "", 0, "", %FONT_ARIALBOLD, 1
  DATA "LSkipEmpty", "STATIC", 616, 454, 120, 20, "", 0, "Skip empty content", %FONT_ARIALBOLD, 1
  DATA "CBSkipEmpty", "BUTTON", 732, 454, 16, 16, "", %BS_CHECKBOX, "", %FONT_ARIALBOLD, 1
  DATA "BExtract", "BUTTON", 760, 444, 120, 32, "", 0, "Extract", %FONT_ARIAL, 1
  DATA "BQuit", "BUTTON", 888, 444, 120, 32, "", 0, "Quit", %FONT_ARIAL, 1
END SUB



'Create main window
FUNCTION MakeWindow&
  LOCAL wce AS WndClassEx
  LOCAL szClassName AS ASCIIZ * 64
  LOCAL hWnd&, WD&, HG&, X&, Y&, DESKWD&, DESKHG&
  LOCAL RC AS RECT
  STATIC registered&

  'Get Desktop size (without Taskbar) to center window
  CALL SystemParametersInfo(%SPI_GETWORKAREA, 0, RC, 0)
  DESKWD& = RC.nRight-RC.nLeft
  DESKHG& = RC.nBottom-RC.nTop
  IF DESKWD& <= 0 THEN DESKWD& = 640
  IF DESKHG& <= 0 THEN DESKHG& = 480
  WD& = 1024
  HG& = 512
  X& = (DESKWD&-WD&)/2
  Y& = (DESKHG&-HG&)/2

  hInstance& = GetModuleHandle(BYVAL %NULL)

  IF ISFALSE registered& THEN
    szClassName          = "MAKEMSI"
    wce.cbSize           = SIZEOF(wce)
    wce.style            = %CS_HREDRAW OR %CS_VREDRAW
    wce.lpfnWndProc      = CODEPTR(WndProc)
    wce.cbClsExtra       = 0
    wce.cbWndExtra       = 0
    wce.hInstance        = hInstance&
    wce.hIcon            = %NULL
    wce.hCursor          = %NULL
    wce.hbrBackground    = %NULL
    wce.lpszMenuName     = %NULL
    wce.lpszClassName    = VARPTR(szClassName)
    wce.hIconSm          = %NULL
    RegisterClassEx wce
    registered& = %TRUE
  END IF

  hWnd& = CreateWindowEx(%WS_EX_CONTROLPARENT, szClassName, "Battle Isle File Extractor", %WS_OVERLAPPED OR %WS_CAPTION OR %WS_SYSMENU, X&, Y&, WD&, HG&, %NULL, %NULL, hInstance&, BYVAL %NULL)

  IF ISFALSE hWnd& THEN
    hWnd = GetLastError
  ELSE
    ShowWindow hWnd&, %SW_SHOWNORMAL
    UpdateWindow hWnd&
  END IF

  MakeWindow& = hWnd&
END FUNCTION



'Clean-up
SUB DestroyForm(hWnd&)
  DeleteObject hFonts&(%FONT_COURIER)
  DeleteObject hFonts&(%FONT_ARIALBOLD)
  DeleteObject hFonts&(%FONT_ARIAL)
  DeleteObject hGlobalBGBrush&
END SUB



'Draw window
SUB PaintWin(hWnd&)
  LOCAL hDC&, hOldBrush&, hPen&, hOldPen&
  LOCAL C&, R&
  LOCAL X0&, Y0&, X1&, Y1&
  LOCAL P AS PAINTSTRUCT

  'Start drawing
  hDC&=BeginPaint(hWnd&, P)
  IF hDC&=0 THEN EXIT SUB

  'Get clipping rectangle
  X0&=P.rcPaint.nLeft
  Y0&=P.rcPaint.nTop
  X1&=P.rcPaint.nRight
  Y1&=P.rcPaint.nBottom

  'Draw background
  hPen& = CreatePen(%PS_SOLID, 0, bgcolor&)
  hOldBrush& = SelectObject(hDC&, hGlobalBGBrush&)
  hOldPen& = SelectObject(hDC&, hPen&)
  R&=Rectangle(hDC&, X0&, Y0&, X1&, Y1&)
  R&=SelectObject(hDC&, hOldBrush&)
  R&=SelectObject(hDC&, hOldPen&)
  R&=DeleteObject(hPen&)

  'End drawing
  R&=EndPaint(hWnd&, P)
END SUB



'Window Proc
FUNCTION WndProc(BYVAL hWnd&, BYVAL wMsg&, BYVAL wParam&, BYVAL lParam&) EXPORT AS LONG
  LOCAL hdc&, childnr&, cnr&, r&, i&
  LOCAL notifymsg AS NMHDR PTR
  LOCAL tmsg AS tagMSG PTR

  'Get message receiver
  cnr& = FindCompByHWND&(hWnd&)
  IF cnr& < 0 THEN
    FUNCTION = DefWindowProc(hWnd&, wMsg&, wParam&, lParam&)
    EXIT FUNCTION
  END IF

  'Process message
  SELECT CASE wMsg&
  CASE %WM_CREATE:  'Create form
    IF cnr& = 0 THEN CALL CreateForm(hWnd&)

  CASE %WM_DESTROY:  'Terminate application
    IF cnr& = 0 THEN
      CALL DestroyForm(hWnd&)
      TERMINATED& = 1
    END IF

  CASE %WM_SHOWWINDOW:  'Show window
    IF wParam& <> 0 THEN
    END IF

  CASE %WM_PAINT:  'Repaint window
    IF cnr& = 0 THEN CALL PaintWin(hWnd&)

  CASE %WM_COMMAND:  'Child window (control) event
    childnr& = FindCompByHWND&(lParam&)
    IF childnr& > 0 THEN
      CALL CHILDEVENT(childnr&, HIWRD(wParam&), lParam&, lParam&)
    END IF

  CASE %WM_NOTIFY:  'Child window (control) event
    notifymsg = lParam&
    childnr& = FindCompByHWND&(@notifymsg.hwndFrom)
    IF childnr& > 0 THEN CALL CHILDEVENT(childnr&, @notifymsg.code, @notifymsg.hwndFrom, lParam&)

  CASE %WM_CTLCOLORSTATIC:  'Set background color for control
    hdc& = wParam&
    IF lParam& = hComp&(FindCompByName&("LError")) THEN
      SetTextColor hdc&, RGB(255, 0, 0)
    ELSE
      SetTextColor hdc&, RGB(0, 0, 0)
    END IF
    SetBkMode hdc&, %TRANSPARENT
    FUNCTION = hGlobalBGBrush&
    EXIT FUNCTION

  CASE %WM_DROPFILES:  'Drag&Drop files
    CALL GETDROPPEDFILES(wParam&)

  END SELECT

  'Call default WindowProc of the control
  IF cnr& = 0 OR orgWndProc&(cnr&) = 0 THEN
    FUNCTION = DefWindowProc(hWnd&, wMsg&, wParam&, lParam&)
  ELSE
    CALL DWORD orgWndProc&(cnr&) USING WndProc(BYVAL hWnd&, BYVAL wMsg&, BYVAL wParam&, BYVAL lParam&) TO r&
    FUNCTION = r&
  END IF
END FUNCTION



'Handle child window (control) events
SUB CHILDEVENT(CHNR&, EVENTNR&, hChild&, lParam&)
  LOCAL childname$
  LOCAL e$

  childname$ = UCASE$(compnames$(CHNR&))

  'Checkbox
  IF childname$ = "CBSKIPEMPTY" THEN
    IF skipempty& = 0 THEN
      skipempty& = 10
      SendMessage hComp&(FindCompByName&("CBSkipEmpty")), %BM_SETCHECK, %BST_CHECKED, 0
    ELSE
      skipempty& = 0
      SendMessage hComp&(FindCompByName&("CBSkipEmpty")), %BM_SETCHECK, %BST_UNCHECKED, 0
    END IF
  END IF

  'Button "Extract" has been pressed
  IF childname$ = "BEXTRACT" THEN
    IF srcfile$ = "" THEN
      CALL SHOWERROR("Please drop the file to extract on above area first")
    ELSE
      IF paletteloaded& = -1 THEN paletteloaded& = 0
      CALL EXTRACTFILES(srcfile$, destpath$)
    END IF
  END IF

  'Button "Quit" has been pressed
  IF childname$ = "BQUIT" THEN
    CALL ExitApplication
  END IF
END SUB



'Button "Quit" has been pressed
SUB ExitApplication
  TERMINATED& = 1
END SUB



'Get dropped files from Drag&Drop operation
SUB GETDROPPEDFILES(hDrop&)
  LOCAL szFilename AS ASCIIZ*65536
  LOCAL n&

  'Get number of files
  n& = DragQueryFile(hDrop&, -1, szFilename, 65535)
  IF n& > 1 THEN
    CALL SHOWERROR("Please drop only one file")
    EXIT SUB
  END IF

  'Get filename
  DragQueryFile(hDrop&, 0, szFilename, 65535)
  srcfile$ = szFilename
  destpath$ = EXEPATH$+PATHNAME$(NAME, srcfile$)
  CALL SHOWINFO("Files will be extracted to: "+destpath$)
  CALL SHOWERROR("")
  CALL EXTRACTFILES(srcfile$, "")

  DragFinish hDrop&
END SUB



'Shows an information text
SUB SHOWINFO(A$)
  LOCAL h&
  LOCAL szText AS ASCIIZ*65536

  h& = hComp&(FindCompByName&("LInfo"))
  szText = A$
  SendMessage h&, %WM_SETTEXT, 65535, VARPTR(szText)
END SUB



'Shows an error message
SUB SHOWERROR(A$)
  LOCAL h&
  LOCAL szText AS ASCIIZ*65536

  h& = hComp&(FindCompByName&("LError"))
  szText = A$
  SendMessage h&, %WM_SETTEXT, 65535, VARPTR(szText)
END SUB



'Shows the file list
SUB SHOWFILES(f$())
  LOCAL h&, i&, n&, a$, b$
  LOCAL szText AS ASCIIZ*65536

  n& = UBOUND(f$())
  a$ = archiveinfo$+CHR$(13,10)
  FOR i& = 0 TO n&
    b$ = LEFT$(f$(i&), 34)
    a$ = a$+b$+SPACE$(34-LEN(b$))
    IF (i& AND 3) = 3 THEN a$ = a$+CHR$(13,10)
  NEXT i&

  h& = hComp&(FindCompByName&("LFiles"))
  szText = a$
  SendMessage h&, %WM_SETTEXT, 65535, VARPTR(szText)
END SUB



'Extract files from a LIB archive
SUB EXTRACTFILES(f$, destpathname$)
  LOCAL a$, b$, e$, g$, filelen&, pindex&, nrecords&, i&, p&, q&, nextracted&, framenr&, framecount&, framewidth&, frameheight&
  LOCAL frames$(), filelist$()

  'Create destination folder
  IF destpathname$ <> "" AND NOT ISFOLDER(destpathname$) THEN MKDIR destpathname$

  'Read file content
  OPEN f$ FOR BINARY AS #1
  filelen& = LOF(#1)
  GET$ #1, filelen&, a$
  CLOSE #1

  'Check if file is an archive or not
  IF LEFT$(a$, 4) = "TPWM" THEN
    b$ = DECOMPRESSTPWM$(a$)
    'Extact file content
    DIM filelist$(0)
    archiveinfo$ = f$+" ("+FORMAT$(filelen&)+" bytes)"
    p& = INSTR(-1, f$, "\")
    g$ = MID$(f$, p&+1)
    filelist$(0) = g$+" ("+FORMAT$(LEN(b$))+" bytes)"

    'Save extracted content to file
    IF destpathname$ <> "" THEN
      OPEN destpathname$+"\"+g$ FOR OUTPUT AS #1
      PRINT #1, b$;
      CLOSE #1
      nextracted& = nextracted&+1
    END IF
  ELSE

    'Get number of records in the archive
    pindex& = CVL(a$)
    IF pindex& < 4 OR pindex& > filelen& THEN
      CALL SHOWERROR(f$+" ("+FORMAT$(filelen&)+" bytes, no directory!)")
      srcfile$ = ""
      EXIT SUB
    END IF
    nrecords& = INT((filelen&-pindex&)/12)
    DIM filelist$(nrecords&-1)
    archiveinfo$ = f$+" ("+FORMAT$(filelen&)+" bytes, "+FORMAT$(nrecords&)+" entries)"

    'Extract records
    FOR i& = 0 TO nrecords&-1
      g$ = TRIM$(MID$(a$, pindex&+i&*12+1, 8), ANY CHR$(0, 32))
      p& = CVL(a$, pindex&+i&*12+9)+1
      IF i& = nrecords&-1 THEN q& = pindex&+1 ELSE q& = CVL(a$, pindex&+i&*12+21)+1
      b$ = MID$(a$, p&, q&-p&)
      filelist$(i&) = g$+" ("+FORMAT$(LEN(b$))+" bytes)"

      IF LEFT$(b$, 4) = "BBHD" THEN
        CALL EXTRACTFRAMES(b$, destpathname$ = "", framewidth&, frameheight&, frames$())
        framecount& = UBOUND(frames$())+1
        filelist$(i&) = g$+" ("+FORMAT$(framecount&)+" frames "+FORMAT$(framewidth&)+"x"+FORMAT$(frameheight&)+")"
        IF destpathname$ <> "" THEN
          FOR framenr& = 0 TO framecount&-1
            OPEN destpathname$+"\"+g$+"_"+FORMAT$(framenr&)+".bmp" FOR OUTPUT AS #1
            PRINT #1, frames$(framenr&)
            CLOSE #1
            nextracted& = nextracted&+1
          NEXT framenr&
        END IF
      ELSE
        IF LEFT$(b$, 4) = "TPWM" THEN b$ = DECOMPRESSTPWM$(b$)
        'Save extracted content to file
        IF LEN(b$) > skipempty& AND destpathname$ <> "" THEN
          e$ = GETFILEEXTENSION$(b$, PATHNAME$(NAME, f$))
          IF e$ = ".bmp" THEN b$ = CONVERTTOPICTURE$(b$, e$)
          OPEN destpathname$+"\"+g$+e$ FOR OUTPUT AS #1
          PRINT #1, b$;
          CLOSE #1
          nextracted& = nextracted&+1
        END IF
      END IF
    NEXT i&

  END IF

  CALL SHOWFILES(filelist$())
  IF destpathname$ <> "" THEN CALL SHOWINFO(FORMAT$(nextracted&)+" files extracted")
END SUB



'Determines the file extention from the file content
FUNCTION GETFILEEXTENSION$(content$, f$)
  LOCAL e$, i&, n&, ntext&, nctrl&, nother&

  n& = LEN(content$)
  FOR i& = 1 TO n&
    SELECT CASE ASC(content$, i&)
    CASE 0 TO 8,11,12,14 TO 31: nctrl& = nctrl&+1
    CASE 9,10,13, 32 TO 127: ntext& = ntext&+1
    CASE ELSE: nother& = nother&+1
    END SELECT
  NEXT i&

  e$ = ".dat"
  IF nctrl& = < ntext&/20 AND nother& < ntext&/10 THEN
    e$ = ".txt"
  ELSE
    IF LEFT$(f$, 3) = "UIA" THEN e$ = ".bmp"
    IF LEN(content$) = 576 THEN e$ = ".bmp"
    IF LEN(content$) = 768 AND INSTR(f$, "PAL") > 0 THEN e$ = ".pal"
  END IF

  GETFILEEXTENSION$ = e$
END FUNCTION



'Show "File open" dialogue
FUNCTION SelectFile$(filepreset$, filter$, filterdescription$)
  LOCAL ofn AS OPENFILENAME
  LOCAL szFile AS ASCIIZ*1024
  LOCAL szFilter AS STRING*1024
  LOCAL szDir AS STRING*1024
  LOCAL f$

  szFile = filepreset$
  szDir = EXEPATH$+CHR$(0)
  szFilter = filterdescription$+CHR$(0)+filter$+CHR$(0)+CHR$(0,0)

  ofn.lStructSize = SIZEOF(ofn)
  ofn.hwndOwner = hWndMain&
  ofn.lpstrFile = VARPTR(szFile)
  ofn.nMaxFile = 1023
  ofn.lpstrFilter = VARPTR(szFilter)
  ofn.nFilterIndex = 1
  ofn.lpstrFileTitle = %NULL
  ofn.nMaxFileTitle = 0
  ofn.lpstrInitialDir = VARPTR(szDir)
  ofn.Flags = %OFN_PATHMUSTEXIST OR %OFN_FILEMUSTEXIST

  IF GetOpenFileName(ofn) <> 0 THEN
    SelectFile$ = szFile
  ELSE
    SelectFile$ = ""
  END IF
END FUNCTION



'Load color palette
SUB LOADPALETTE
  LOCAL f$, a$, filelen&

  f$ = $PALETTEFILE
  IF ISFILE(f$) = 0 THEN
    f$ = EXEPATH$+"LBM\"+$PALETTEFILE
    IF ISFILE(f$) = 0 THEN
      f$ = SelectFile$($PALETTEFILE, "*.pal", "Color palette (*.pal)")
      IF f$ = "" THEN
        CALL SHOWERROR("No color palette loaded")
        paletteloaded& = -1
        EXIT SUB
      END IF
    END IF
  END IF

  OPEN f$ FOR BINARY AS #1
  filelen& = LOF(#1)
  GET$ #1, filelen&, a$
  CLOSE #1

  DIM pal?(767)
  POKE$ VARPTR(pal?(0)), LEFT$(a$, 768)

  paletteloaded& = 1
END SUB



'Interprete data as picture
FUNCTION CONVERTTOPICTURE$(a$, e$)
  LOCAL wd&, hg&, x&, y&, p&, q&, blockx&, blocky&, xoff&, yoff&
  LOCAL bitspercolor&, ppixeldata&, transparent&, b$, d$
  LOCAL pic??()

  'Check if loading color palette has been cancelled
  IF paletteloaded& = -1 THEN
    CONVERTTOPICTURE$ = a$
    EXIT FUNCTION
  END IF

  'Check for standard sprite
  IF LEN(a$) = 576 THEN
    wd& = 24
    hg& = 24
    ppixeldata& = 1
    'Convert sprite from LIB to PB
    IF isBI3& = 0 THEN
      d$ = STRING$(wd&*hg&, 0)
      p& = 0
      FOR y& = 0 TO hg&-1
        blocky& = INT(y&/6)
        yoff& = y& MOD 6
        FOR x& = 0 TO wd&-1
          blockx& = INT(x&/6)  '0..3
          xoff& = x& MOD 6     '0..5
          q& = (xoff&*4+blocky&)+(yoff&*4+blockx&)*wd&+1
          p& = p&+1
          ASC(d$, q&) = ASC(a$, p&)
        NEXT x&
      NEXT y&
      a$ = d$
    END IF
  ELSE
    'Check header
    SELECT CASE LEFT$(a$, 4)
    CASE "INFO"
      bitspercolor& = ASC(a$, 14)
      wd& = CVI(a$, 19)
      hg& = CVI(a$, 21)
      ppixeldata& = 27
    CASE CHR$(0,0,0,0)
      wd& = CVI(a$, 15)
      hg& = CVI(a$, 17)
      ppixeldata& = 19
    CASE ELSE
      CALL SHOWERROR("No valid picture data found.")
      EXIT FUNCTION
    END SELECT
  END IF

  'Load color palette
  IF paletteloaded& = 0 THEN CALL LOADPALETTE
  IF paletteloaded& = -1 THEN
    CONVERTTOPICTURE$ = a$
    EXIT FUNCTION
  END IF

  'Create picture
  DIM pic??(wd&*hg&/2+1)
  pic??(0) = wd&
  pic??(1) = hg&
  POKE$ VARPTR(pic??(2)), MID$(a$, ppixeldata&, wd&*hg&)
  transparent& = -1
  IF e$ = ".gif" THEN
'    b$ = SAVEGIF$(pic??(), pal?(), transparent&)
  ELSE
    b$ = SAVEBMP$(pic??(), pal?())
  END IF

  CONVERTTOPICTURE$ = b$
END FUNCTION



'Convert animation frame to picture
FUNCTION FRAMETOPICTURE$(a$, wd&, hg&)
  LOCAL transparent&, b$
  LOCAL pic??()

  'Load color palette
  IF paletteloaded& = 0 THEN CALL LOADPALETTE
  IF paletteloaded& = -1 THEN
    FRAMETOPICTURE$ = a$
    EXIT FUNCTION
  END IF

  'Create picture
  DIM pic??(wd&*hg&/2+1)
  pic??(0) = wd&
  pic??(1) = hg&
  POKE$ VARPTR(pic??(2)), LEFT$(a$, wd&*hg&)
  transparent& = -1
'  b$ = SAVEGIF$(pic??(), pal?(), transparent&)
  b$ = SAVEBMP$(pic??(), pal?())

  FRAMETOPICTURE$ = b$
END FUNCTION



'Extract BBHD encoded data
SUB EXTRACTFRAMES(a$, infoonly&, framewidth&, frameheight&, frames$())
  LOCAL framenr&, framecount&, framestart&, lastframe&, totalsize&, framelen&, fdata$

  'Read header data
  framestart& = CVL(a$, 5)
  lastframe& = CVL(a$, 9)
  totalsize& = CVL(a$, 13)
  framecount& = CVL(a$, 17)
  framewidth& = CVI(a$, 27)
  frameheight& = CVI(a$, 29)
  REDIM frames$(framecount&-1)
  IF infoonly& <> 0 THEN EXIT SUB

  'Extract frames
  FOR framenr& = 0 TO framecount&-1
    IF MID$(a$, framestart&+1, 4) <> "BBFR" THEN EXIT FOR
    framelen& = CVL(a$, framestart&+7)
    fdata$ = MID$(a$, framestart&+29, framelen&-28)
    IF LEFT$(fdata$, 4) = "TPWM" THEN fdata$ = DECOMPRESSTPWM$(fdata$)
    frames$(framenr&) = FRAMETOPICTURE$(fdata$, framewidth&, frameheight&)
    framestart& = framestart&+framelen&
  NEXT framenr&
END SUB



'Decode XOR encoded string with static key
FUNCTION DecodeXOR$(a$)
  LOCAL k$, r$, i&, keypos&, keylen&

  k$ = "FUCKBAUEREWALD"
  r$ = a$
  keypos& = 1
  keylen& = LEN(k$)
  FOR i& = 1 TO LEN(a$)
    ASC(r$, i&) = ASC(r$, i&) XOR ASC(k$, keypos&)
    keypos& = keypos&+1
    IF keypos& > keylen& THEN keypos& = 1
  NEXT i&

  DecodeXOR$ = r$
END FUNCTION



'Extact TPWM compressed data
FUNCTION DECOMPRESSTPWM$(a$)
  LOCAL id$, rep$, decoded$
  LOCAL orglen&, p&, inputlen&, c&, n&, q&, plaintextmask&, b&

  'Read header data
  id$ = LEFT$(a$, 4)
  IF id$ <> "TPWM" THEN
    DECOMPRESSTPWM$ = a$
    EXIT FUNCTION
  END IF
  orglen& = CVL(a$, 5)
  inputlen& = LEN(a$)
  IF orglen& > inputlen&*10 THEN
    DECOMPRESSTPWM$ = DecodeXOR$(MID$(a$, 5))
    EXIT FUNCTION
  END IF


  'Decode character
  p& = 9
  WHILE p& <= inputlen&
    plaintextmask& = ASC(a$, p&)
    b& = 256
    p& = p&+1
    WHILE p& <= inputlen& AND b& > 1
      b& = b&/2
      IF (plaintextmask& AND b&) = 0 THEN
        'Plain text
        c& = ASC(a$, p&)
        decoded$ = decoded$+CHR$(c&)
        p& = p&+1
      ELSE
        'Repeated data
        c& = ASC(a$, p&)
        n& = c&  'lower 4 bits determine the number of copies (+3)
        p& = p&+1
        q& = LEN(decoded$)+1-ASC(a$, p&)
        IF n& < 16 THEN
          n& = n&+3
        ELSE
          q& = q&-256*INT(n&/16)
          n& = (n& AND 15)+3
        END IF
        IF q& > 0 THEN
          rep$ = MID$(decoded$, q&, n&)
          IF LEN(rep$) < n& THEN rep$ = LEFT$(REPEAT$(n&, rep$), n&)
          decoded$ = decoded$+rep$
          p& = p&+1
        ELSE
'          PRINT "Warning: Invalid repeat position "+FORMAT$(q&)
        END IF
      END IF
    WEND
  WEND

  DECOMPRESSTPWM$ = LEFT$(decoded$, orglen&)
END FUNCTION



'Save a picture in BMP format
FUNCTION SAVEBMP$(pic??(),pal?())
  LOCAL a$
  LOCAL xgr&,ygr&,zgr&
  LOCAL i&,p&,ndim&

  'Create BMP header
  xgr&=pic??(0)
  zgr&=INT((xgr&+3)/4)*4
  ygr&=pic??(1)
  a$="BM"+MKL$(54+1024+zgr&*ygr&)+MKL$(0)+MKL$(54+1024) _
    +MKL$(40)+MKL$(xgr&)+MKL$(ygr&)+MKI$(1)+MKI$(8)+MKL$(0)+MKL$(zgr&*ygr&) _
    +MKL$(15120)+MKL$(15120)+MKL$(0)+MKL$(0)

  'Create palette
  ndim&=ARRAYATTR(pal?(),3)
  IF ndim&=1 THEN
    FOR i&=0 TO 255
      a$=a$+CHR$(pal?(i&*3+2))+CHR$(pal?(i&*3+1))+CHR$(pal?(i&*3))+CHR$(0)
    NEXT i&
  ELSE
    FOR i&=0 TO 255
      a$=a$+CHR$(pal?(2,i&))+CHR$(pal?(1,i&))+CHR$(pal?(0,i&))+CHR$(0)
    NEXT i&
  END IF

  'Create pixel data
  p&=VARPTR(pic??(2))+xgr&*ygr&-xgr&
  FOR i&=1 TO ygr&
    a$=a$+PEEK$(p&,xgr&)+STRING$(zgr&-xgr&,0)
    p&=p&-xgr&
  NEXT i&

  SAVEBMP$=a$
END FUNCTION



'Main function
FUNCTION PBMAIN&
  LOCAL Msg AS tagMsg
  DIM compnames$(255), hComp&(255), orgWndProc&(255), hFonts&(15)

  EXEPATH$ = EXE.PATH$
  CALL CheckBIVersion
  skipempty& = 10
  IF %FORMBGCOLOR <= 30 THEN bgcolor& = GetSysColor(%FORMBGCOLOR) ELSE bgcolor& = %FORMBGCOLOR
  hwndMain& = MakeWindow&
  SetForegroundWindow hWndMain&
  SetFocus hWndMain&

  DO WHILE IsWindow(hwndMain&) AND GetMessage(Msg, %NULL, 0, 0) AND TERMINATED& = 0
    TranslateMessage Msg
    DispatchMessage Msg
  LOOP
END FUNCTION
