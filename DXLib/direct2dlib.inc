#INCLUDE ONCE "CWindow.inc"
#INCLUDE ONCE "D2D1.INC"
#INCLUDE ONCE "d2d1Helper.inc"
#INCLUDE ONCE "D2DUtils.inc"

%MAXBRUSHES = 256
%MAXFONTS = 256
%INITIALCONTROLSIZE = 256

%CTYPE_EDIT = 1
%CTYPE_LISTBOX = 2
%CTYPE_BUTTON = 3
%CTYPE_RADIOGROUP = 4
%CTYPE_CHECKBOX = 5
%CTYPE_BITMAPBUTTON = 6
%CTYPE_PROGRESSBAR = 7
%CTYPE_SCROLLBAR = 8

%LISTBOX_MAXOPENSIZE = 10



'Prototype for render function
SUB RenderFunctionPrototype(RenderTarget AS ID2D1HwndRenderTarget)
END SUB

'Prototype for button-pressed function
SUB ButtonPressedPrototype(buttonId&)
END SUB

'Prototype for item-changed function
SUB ItemChangedPrototype(controlId&)
END SUB



'==================================================================================================
'Direct2D class
'==================================================================================================
CLASS CDIRECT2D

INSTANCE i_hWnd AS LONG
INSTANCE i_pID2D1Helper AS ID2D1Helper
INSTANCE i_pD2DFactory AS ID2D1Factory
INSTANCE i_pWICFactory AS IWICImagingFactory
INSTANCE i_pDWriteFactory AS IDWriteFactory
INSTANCE i_pRenderTarget AS ID2D1HwndRenderTarget
INSTANCE i_pClippingRegion AS ID2D1Layer
INSTANCE i_pBrush() AS ID2D1Brush
INSTANCE i_BrushCount AS LONG
INSTANCE i_pFont() AS IDWriteTextFormat
INSTANCE i_fontsize() AS SINGLE
INSTANCE i_FontCount AS LONG
INSTANCE i_pBitmap() AS ID2D1Bitmap
INSTANCE i_BitmapCount AS LONG
INSTANCE i_InitialBitmapCount AS LONG
INSTANCE i_pRenderFunction AS DWORD
INSTANCE i_Controls() AS IDXCONTROL
INSTANCE i_ControlCount AS LONG
INSTANCE i_BitmapButtonTexture AS LONG



'Constructor
CLASS METHOD CREATE
END METHOD



' ========================================================================================
' Create resources which are not bound to any device. Their lifetime effectively extends
' for the duration of the app. These resources include the Direct2D, DirectWrite, and
' WIC factories; and a DirectWrite Text Format object (used for identifying particular
' font characteristics) and a Direct2D geometry.
' ========================================================================================
CLASS METHOD CreateDeviceIndependentResources() AS LONG

   LOCAL hr AS LONG
   LOCAL pSink AS ID2D1GeometrySink

   '// Create a Direct2D factory.
   hr = D2D1CreateFactory2(%D2D1_FACTORY_TYPE_SINGLE_THREADED, i_pD2DFactory)
   IF hr <> %S_OK THEN EXIT METHOD

   '// Create WIC factory.
   i_pWICFactory = NEWCOM CLSID $CLSID_WICImagingFactory
   IF ISNOTHING(i_pWICFactory) THEN EXIT METHOD

   '// Create a DirectWrite factory.
   hr = DWriteCreateFactory(%DWRITE_FACTORY_TYPE_SHARED, $IID_IDWriteFactory, i_pDWriteFactory)
   IF hr <> %S_OK THEN EXIT METHOD

   ' // We need to use an string here because using "" will pass a null pointer
   ' // instead of a pointer to an empty string.
'   LOCAL wszLocaleName AS WSTRINGZ * 260
   '// Create a DirectWrite text format object.
'   hr = g_pDWriteFactory.CreateTextFormat("Verdana", NOTHING, %DWRITE_FONT_WEIGHT_NORMAL, _
'        %DWRITE_FONT_STYLE_NORMAL, %DWRITE_FONT_STRETCH_NORMAL, 50, wszLocaleName, g_pTextFormat)
'   IF hr <> %S_OK THEN EXIT FUNCTION

   ' // Center the text horizontally and vertically.
'   g_pTextFormat.SetTextAlignment(%DWRITE_TEXT_ALIGNMENT_CENTER)
'   g_pTextFormat.SetParagraphAlignment(%DWRITE_PARAGRAPH_ALIGNMENT_CENTER)

   ' // Create a path geometry.
'   hr = i_pD2DFactory.CreatePathGeometry(g_pPathGeometry)
'   IF hr <> %S_OK THEN EXIT FUNCTION

   ' // Use the geometry sink to write to the path geometry.
'   hr = g_pPathGeometry.Open(pSink)
'   IF hr <> %S_OK THEN EXIT FUNCTION

'   pSink.SetFillMode(%D2D1_FILL_MODE_ALTERNATE)

   METHOD = %TRUE

END METHOD



' ========================================================================================
' This method creates resources which are bound to a particular Direct3D device. It's all
' centralized here, in case the resources need to be recreated in case of Direct3D device
' loss (eg. display change, remoting, removal of video card, etc).
' ========================================================================================
CLASS METHOD CreateDeviceResources(BYVAL hWnd&) AS LONG
  LOCAL k&, hr&, nType AS DWORD, pixelFormat AS D2D1_PIXEL_FORMAT, dpiX, dpiY AS SINGLE
  LOCAL usage, minlevel AS DWORD, dxgiFormat, alphaMode AS DWORD
  LOCAL rtp AS D2D1_RENDER_TARGET_PROPERTIES
  LOCAL rc AS RECT
  LOCAL a$, b$

  IF ISNOTHING(i_pRenderTarget) THEN
    'create rendertarget for full client size
    GetClientRect(hWnd&, rc)

    rtp.type = 0
    rtp.pixelFormat = i_pID2D1Helper.PixelFormat(0, 1)  'Format.Unknown, AlphaMode.Premultiplied
    rtp.dpiX = 96
    rtp.dpiY = 96
    rtp.usage = 0  'None
    rtp.minLevel = 0  'FeatureLevel.Level_DEFAULT
    hr& = i_pD2DFactory.CreateHwndRenderTarget(rtp, i_pID2D1Helper.HwndRenderTargetProperties(hWnd&, i_pID2D1Helper.SizeU(rc.Right - rc.Left, rc.Bottom - rc.Top)), i_pRenderTarget)
    IF hr& <> %S_OK THEN EXIT METHOD

    'load a bitmap from a file
'    hr& = D2D_LoadBitmapFromFile(i_pRenderTarget, i_pWICFactory, "F:\PBCC\DirectX\kitana-avatar184.jpg", 0, 0, i_pBitmap(0))
'    IF hr& <> %S_OK THEN EXIT METHOD
'    i_BitmapCount = 1
  END IF

  METHOD = %TRUE
END METHOD



' ========================================================================================
' Discard device-specific resources which need to be recreated when a Direct3D device is lost
' ========================================================================================
CLASS METHOD DiscardDeviceResources
  LOCAL i&

  i_pRenderTarget = NOTHING
  FOR i& = 0 TO i_BrushCount-1
    i_pBrush(i&) = NOTHING
  NEXT i&
  FOR i& = 0 TO i_BitmapCount-1
    i_pBitmap(i&) = NOTHING
  NEXT i&
END METHOD



' ========================================================================================
' Decompress LZW data with variable bit width
' ========================================================================================
CLASS METHOD LZWDecompress_ASM(src$, b&, destsize&) AS STRING
  #REGISTER NONE

  LOCAL dest$
  LOCAL startbitwidth&, cc&, eol&, dictfirstcustom&, bitwidth&, mask&, currentword&, lastword&, bitpos&
  LOCAL pdict&, psrc&, pdest&, psrcend&, pdestend&
  LOCAL dict&()  'bits 0-7 = suffix , bits 8-19 = prefix , bits 20-31 = length
  DIM dict&(4095)

  'allocate memory for the decompressed pixel data
  dest$ = STRING$(destsize&, 0)
  pdest& = STRPTR(dest$)
  pdict& = VARPTR(dict&(0))
  psrc& = STRPTR(src$)
  psrcend& = psrc&+LEN(src$)
  pdestend& = pdest&+destsize&
  startbitwidth& = b&

  'save CPU registers
  ! PUSH EAX
  ! PUSH EBX
  ! PUSH ECX
  ! PUSH EDX
  ! PUSH ESI
  ! PUSH EDI

  'initialize roots
  ! MOV EDI, pdict&
  ! MOV ECX, startbitwidth&
  ! MOV EBX, 1
  ! SHL EBX, CL
  ! MOV EDX, EBX
  ! MOV EAX, &H001FFF00  ; prefix = -1 , suffix = 0 , length = 1
  LZWDec_InitRoots:
  ! STOSD
  ! INC AL               ; next suffix
  ! DEC EBX
  ! JNZ LZWDec_InitRoots

  'initialize special codes
  ! MOV cc&, EDX               ; clear-code
  ! INC EDX
  ! MOV eol&, EDX              ; end-of-line
  ! INC EDX
  ! MOV dictfirstcustom&, EDX  ; number of the first custom code

  'set current bit-width to start bit-width +1
  ! MOV ECX, startbitwidth&
  ! INC ECX
  ! MOV bitwidth&, ECX

  'set current bit-mask
  ! MOV EAX, 1
  ! SHL EAX, CL
  ! DEC EAX
  ! MOV mask&, EAX

  'start decompression
  'ESI = read position in source string
  'EDI = write position in destination string
  'EAX = current word
  'EBX = position of next free dictionary record
  ! MOV lastword&, 4095
  ! MOV bitpos&, 0
  ! MOV EBX, dictfirstcustom&
  ! MOV ESI, psrc&
  ! MOV EDI, pdest&
  LZWDec_MainLoop:
  ! CMP ESI, psrcend&
  ! JAE LZWDec_End

  'get next word
  ! MOV EAX, [ESI]
  ! MOV ECX, bitpos&
  ! SHR EAX, CL
  ! AND EAX, mask&
  ! MOV currentword&, EAX

  'move read position by current bit width
  ! ADD ECX, bitwidth&
  ! MOV EDX, ECX
  ! SHR EDX, 3
  ! ADD ESI, EDX
  ! AND ECX, 7
  ! MOV bitpos&, ECX

  'check source character for end-of-line
  ! CMP EAX, eol&
  ! JE LZWDec_End

  'check source character for code table reset
  ! CMP EAX, cc&
  ! JNE LZWDec_Append
  'discard all dictionary records except for roots and special codes
  ! MOV EBX, dictfirstcustom&
  'set current bit-width to start bit-width +1
  ! MOV ECX, startbitwidth&
  ! INC ECX
  ! MOV bitwidth&, ECX
  'set current bit-mask
  ! MOV EAX, 1
  ! SHL EAX, CL
  ! DEC EAX
  ! MOV mask&, EAX
  'reset last word
  ! MOV lastword&, 4095
  ! JMP LZWDec_MainLoop

  'append word to output string
  LZWDec_Append:
  ! CMP EDI, pdestend&
  ! JAE LZWDec_End
  ! CMP EAX, EBX
  ! JB LZWDec_Existing_Code

  'code is (yet) unknown -> append last word to the output and the first character of that last word
  ! MOV EAX, lastword&
  ! SHL EAX, 2
  ! ADD EAX, pdict&
  ! MOV EAX, [EAX]  ; dictionary record of the last word
  ! MOV ECX, EAX
  ! SHR ECX, 20     ; length of the record
  ! PUSH ECX
  'add last word to the output
  LZWDec_Append_Unknown:
  ! MOV [EDI+ECX-1], AL   ; append suffix
  ! SHR EAX, 8
  ! AND EAX, 4095
  ! CMP EAX, 4095
  ! JE LZWDec_Unknown_Done
  ! SHL EAX, 2
  ! ADD EAX, pdict&
  ! MOV EAX, [EAX]  ; dictionary record of the prefix
  ! DEC ECX
  ! JNZ LZWDec_Append_Unknown
  LZWDec_Unknown_Done:
  'append first character of that last word
  ! POP ECX
  ! MOV AL, [EDI]
  ! ADD EDI, ECX
  ! MOV [EDI], AL
  ! INC EDI
  ! JMP LZWDec_Extend_Dictionary

  'existing code -> append this word to the output
  LZWDec_Existing_Code:
  ! SHL EAX, 2
  ! ADD EAX, pdict&
  ! MOV EAX, [EAX]  ; dictionary record of this word
  ! MOV ECX, EAX
  ! SHR ECX, 20     ; length of the record
  ! PUSH ECX
  'add word to the output
  LZWDec_Append_Existing:
  ! MOV [EDI+ECX-1], AL   ; append suffix
  ! SHR EAX, 8
  ! AND EAX, 4095
  ! CMP EAX, 4095
  ! JE LZWDec_Existing_Done
  ! SHL EAX, 2
  ! ADD EAX, pdict&
  ! MOV EAX, [EAX]  ; dictionary record of the prefix
  ! DEC ECX
  ! JNZ LZWDec_Append_Existing
  LZWDec_Existing_Done:
  ! POP ECX
  ! MOV AL, [EDI]
  ! ADD EDI, ECX

  'create new dictionary record
  LZWDec_Extend_Dictionary:
  ! CMP EBX, 4095
  ! JAE LZWDec_MainLoop
  ! CMP EBX, mask&
  ! JB LZWDec_Dictionary_Keep_Bitwidth
  'final record for this bit-width reached -> increase bit-width
  ! INC bitwidth&
  ! SHL mask&, 1
  ! INC mask&
  LZWDec_Dictionary_Keep_Bitwidth:
  ! MOV ECX, lastword&
  ! CMP ECX, 4095
  ! JE LZWDec_Dictionary_Skip
  'add dictionary record
  ! MOV EDX, ECX
  ! SHL EDX, 2
  ! ADD EDX, pdict&
  ! SHL ECX, 8     ; last word becomes prefix of the new record
  ! MOV CL, AL     ; store suffix (last written character)
  ! MOV EDX, [EDX]
  ! SHR EDX, 20    ; length of the last word
  ! INC EDX        ; new word is 1 character longer
  ! SHL EDX, 20
  ! OR ECX, EDX    ; store length
  ! MOV EAX, EBX
  ! SHL EAX, 2
  ! ADD EAX, pdict&
  ! MOV [EAX], ECX
  ! INC EBX
  'save last word
  LZWDec_Dictionary_Skip:
  ! MOV EAX, currentword&
  ! MOV lastword&, EAX
  ! JMP LZWDec_MainLoop

  'done
  LZWDec_End:
  ! POP EDI
  ! POP ESI
  ! POP EDX
  ! POP ECX
  ! POP EBX
  ! POP EAX

  METHOD = dest$
END METHOD



' ========================================================================================
' Decompress LZW data with variable bit width
' ========================================================================================
CLASS METHOD LZWDecompress(src$, b&, destsize&) AS STRING
  LOCAL dictpos&, n&, cc&, eol&
  LOCAL i&, p&, q&, v&, r&, d&, a&, lasta&, suffix&
  LOCAL qbits&, nbits&
  LOCAL dest$, destpos&
  LOCAL pre&(), suf&(), wordlen&()
  DIM pre&(4095), suf&(4095), wordlen&(4095)

  'allocate memory for the decompressed pixel data
  dest$ = STRING$(destsize&, 0)
  destpos& = 1

  'initialize roots
  n& = 2^b&
  FOR i& = 0 TO n&-1
    pre&(i&) = -1
    suf&(i&) = i&
    wordlen&(i&) = 1
  NEXT i&

  'initialize special codes
  cc& = n&
  eol& = cc&+1
  dictpos& = n&+2

  'decompress
  nbits& = b&+1  'current bit-width
  q& = 1         'start position
  v& = 0         'current code character
  qbits& = 0     'number of bits in the current character
  r& = 2^nbits&  'divisor to split a character
  d& = r&-1      'mask for current bit width
  lasta& = -1    'previous source character
  WHILE q& <= LEN(src$)

    'stuff source bits to minimum width
    WHILE qbits& < nbits&
      a& = ASC(src$, q&)
      v& = v& OR a&*2^qbits&
      qbits& = qbits&+8
      q& = q&+1
      IF q& > LEN(src$) THEN EXIT LOOP
    WEND

    'extract next source character
    a& = v& AND d&
    v& = INT(v&/r&)
    qbits& = qbits&-nbits&

    'evaluate source character
    IF a& = eol& THEN EXIT LOOP
    IF a& = cc& THEN  'reset code table
      dictpos& = n&+2
      nbits& = b&+1
      r& = 2^nbits&
      d& = r&-1
      lasta& = -1
    ELSE  'append character to output string
      IF a& >= dictpos& THEN       '(yet) unknown code
        'add last word to the output
        p& = lasta&
        FOR i& = wordlen&(lasta&)-1 TO 0 STEP -1
          ASC(dest$, destpos&+i&) = suf&(p&)
          p& = pre&(p&)
        NEXT i&
        suffix& = ASC(dest$, destpos&)
        destpos& = destpos&+wordlen&(lasta&)
        'then append first character of that last word
        ASC(dest$, destpos&) = suffix&
        destpos& = destpos&+1
      ELSE
        'add found word to the ouput
        p& = a&
        FOR i& = wordlen&(a&)-1 TO 0 STEP -1
          suffix& = suf&(p&)
          ASC(dest$, destpos&+i&) = suffix&
          p& = pre&(p&)
        NEXT i&
        destpos& = destpos&+wordlen&(a&)
      END IF
      IF dictpos& < 4095 THEN
        IF dictpos& = d& THEN  'final record for this bit-width reached
          nbits& = nbits&+1
          r& = r&*2
          d& = r&-1
        END IF
        IF lasta& > -1 THEN
          pre&(dictpos&) = lasta&
          suf&(dictpos&) = suffix&
          wordlen&(dictpos&) = wordlen&(lasta&)+1
          dictpos& = dictpos&+1
        END IF
        lasta& = a&
      END IF
    END IF

  WEND

  METHOD = dest$
END METHOD



' ========================================================================================
' Sorts interlaced rows to the correct order
' ========================================================================================
CLASS METHOD DeinterlaceGIF(src$, xsize&) AS STRING
  LOCAL ysize&
  LOCAL i&, n&, y&, md&
  LOCAL dest$

  ysize& = INT(LEN(src$)/xsize&)
  n& = 8   'step size
  y& = -8  'start row
  md& = 0  'group number
  dest$ = src$

  'reorganize rows
  FOR i& = 0 TO ysize&-1
    'move destination position
    DO
      y& = y&+n&
      IF y& >= ysize& THEN
        md& = md&+1
        SELECT CASE md&
        CASE 1
          n& = 8
          y& = -4
        CASE 2
          n& = 4
          y& = -2
        CASE 3
          n& = 2
          y&= -1
        CASE 4
          EXIT FOR
        END SELECT
      END IF
    LOOP UNTIL y& >= 0

    'move row to correct position
    MID$(dest$, y&*xsize&+1, xsize&) = MID$(src$, i&*xsize&+1, xsize&)
  NEXT i&

  METHOD = dest$
END METHOD



' ========================================================================================
' Converts indexed pixel data to BGRA pixel data
' ========================================================================================
CLASS METHOD ConvertIndexedColorsToBGRA(pixeldata$, pal$, transparentcolor&) AS STRING
  LOCAL i&, n&, p&, c&, dest$

  n& = LEN(pixeldata$)
  dest$ = STRING$(n&*4, 0)
  p& = 1
  FOR i& = 1 TO n&
    c& = ASC(pixeldata$, i&)
    IF c& <> transparentcolor& THEN
      ASC(dest$, p&) = ASC(pal$, c&*3+3)
      ASC(dest$, p&+1) = ASC(pal$, c&*3+2)
      ASC(dest$, p&+2) = ASC(pal$, c&*3+1)
      ASC(dest$, p&+3) = 255
    END IF
    p& = p&+4
  NEXT i&

  METHOD = dest$
END METHOD



INTERFACE IDIRECT2D : INHERIT IUNKNOWN


PROPERTY GET Helper AS ID2D1Helper
  PROPERTY = i_pID2D1Helper
END PROPERTY

PROPERTY GET Brushes(nr&) AS ID2D1SolidColorBrush
  PROPERTY = i_pBrush(nr&)
END PROPERTY

PROPERTY GET Fonts(nr&) AS IDWriteTextFormat
  PROPERTY = i_pFont(nr&)
END PROPERTY

PROPERTY GET FontSize(nr&) AS SINGLE
  PROPERTY = i_fontsize(nr&)
END PROPERTY

PROPERTY GET Sprites(nr&) AS ID2D1Bitmap
  PROPERTY = i_pBitmap(nr&)
END PROPERTY

PROPERTY GET BitmapButtonTexture AS LONG
  PROPERTY = i_BitmapButtonTexture
END PROPERTY



'GRAPHIC BOX (left, top) - (right, bottom), corner, bordercolor, fillcolor, fillstyle
'Parameter "corner" is not the same as in PB; here it is the radius of the corners
METHOD GraphicBox(BYVAL x0!, BYVAL y0!, BYVAL x1!, BYVAL y1!, bordercolor&, fillcolor&, OPTIONAL BYVAL corner&, OPTIONAL BYVAL borderwidth!)
  IF bordercolor& < 0 OR bordercolor& >= i_BrushCount THEN EXIT METHOD

  IF borderwidth! <= 0 THEN borderwidth! = 1

  IF bordercolor& = fillcolor& THEN
    IF corner& = 0 THEN
      i_pRenderTarget.FillRectangle(i_pID2D1Helper.RectF(x0!, y0!, x1!, y1!), i_pBrush(fillcolor&))
    ELSE
      i_pRenderTarget.FillRoundedRectangle(i_pID2D1Helper.RoundedRect(i_pID2D1Helper.RectF(x0!, y0!, x1!, y1!), corner&, corner&), i_pBrush(fillcolor&))
    END IF
  ELSE
    'add 0.5 because pixels are centered on the coordinates (pos 1 in PBCC occupies pos 0.5 to 1.5 in DX but to show the same result it must occupy pos 1.0 to 2.0)
    IF fillcolor& >= 0 THEN
      IF corner& = 0 THEN
        i_pRenderTarget.FillRectangle(i_pID2D1Helper.RectF(x0!+0.5, y0!+0.5, x1!+0.5, y1!+0.5), i_pBrush(fillcolor&))
      ELSE
        i_pRenderTarget.FillRoundedRectangle(i_pID2D1Helper.RoundedRect(i_pID2D1Helper.RectF(x0!+0.5, y0!+0.5, x1!+0.5, y1!+0.5), corner&, corner&), i_pBrush(fillcolor&))
      END IF
    END IF
    IF corner& = 0 THEN
      i_pRenderTarget.DrawRectangle(i_pID2D1Helper.RectF(x0!+0.5, y0!+0.5, x1!+0.5, y1!+0.5), i_pBrush(bordercolor&), borderwidth!)
    ELSE
      i_pRenderTarget.DrawRoundedRectangle(i_pID2D1Helper.RoundedRect(i_pID2D1Helper.RectF(x0!+0.5, y0!+0.5, x1!+0.5, y1!+0.5), corner&, corner&), i_pBrush(bordercolor&), borderwidth!)
    END IF
  END IF
END METHOD



'GRAPHIC LINE (left, top) - (right, bottom), color
METHOD GraphicLine(BYVAL x0!, BYVAL y0!, BYVAL x1!, BYVAL y1!, lcolor&, OPTIONAL BYVAL linewidth!)
  LOCAL p0, p1 AS D2D1_POINT_2F
  LOCAL offset!

  IF lcolor& < 0 OR lcolor& >= i_BrushCount THEN EXIT METHOD

  IF linewidth! <= 0 THEN linewidth! = 1
  'add 0.5 because pixels are centered on the coordinates (pos 1 in PBCC occupies pos 0.5 to 1.5 in DX but to show the same result it must occupy pos 1.0 to 2.0)
  IF INT(linewidth!) = linewidth! AND (linewidth! AND 1) = 1 THEN offset = 0.5
  p0 = i_pID2D1Helper.Point2F(x0!+offset, y0!+offset)
  p1 = i_pID2D1Helper.Point2F(x1!+offset, y1!+offset)
  i_pRenderTarget.DrawLine(p0, p1, i_pBrush(lcolor&), linewidth!)
END METHOD



'GRAPHIC ELLIPSE (left, top) - (right, bottom), colorborder&, colorfill&
METHOD GraphicEllipse(BYVAL x0!, BYVAL y0!, BYVAL x1!, BYVAL y1!, bordercolor&, fillcolor&, OPTIONAL BYVAL borderwidth!)
  LOCAL c AS D2D1_POINT_2F
  LOCAL e AS D2D1_ELLIPSE

  IF bordercolor& < 0 OR bordercolor& >= i_BrushCount THEN EXIT METHOD
  c = i_pID2D1Helper.Point2F((x0!+x1!)/2, (y0!+y1!)/2)
  e = i_pID2D1Helper.Ellipse(c, ABS((x1!-x0!)/2), ABS((y1!-y0!)/2))

  IF bordercolor& = fillcolor& THEN
    i_pRenderTarget.FillEllipse(e, i_pBrush(fillcolor&))
  ELSE
    IF fillcolor& >= 0 THEN i_pRenderTarget.FillEllipse(e, i_pBrush(fillcolor&))
    IF borderwidth! <= 0 THEN borderwidth! = 1
    i_pRenderTarget.DrawEllipse(e, i_pBrush(bordercolor&), borderwidth!)
  END IF
END METHOD



'GRAPHIC POLYGON points, colorborder&, colorfill&
METHOD GraphicPolygon(geometry AS ID2D1Geometry, bordercolor&, fillcolor&)
  IF bordercolor& < 0 OR bordercolor& >= i_BrushCount THEN EXIT METHOD

  IF bordercolor& = fillcolor& THEN
    i_pRenderTarget.FillGeometry(geometry, i_pBrush(fillcolor&))
  ELSE
    IF fillcolor& >= 0 THEN i_pRenderTarget.FillGeometry(geometry, i_pBrush(fillcolor&))
    i_pRenderTarget.DrawGeometry(geometry, i_pBrush(bordercolor&), 1)
  END IF
END METHOD



'GRAPHIC COPY hBmp&, 0 TO (left, top)
METHOD GraphicCopy(sprnr&, x&, y&)
  LOCAL BitmapSize AS D2D1_SIZE_F

  IF sprnr& < 0 OR sprnr& >= i_BitmapCount THEN EXIT METHOD

  BitmapSize = i_pBitmap(sprnr&).GetSize()
  i_pRenderTarget.DrawBitmap(i_pBitmap(sprnr&), i_pID2D1Helper.RectF(x&, y&, x&+BitmapSize.width, y&+BitmapSize.height), 1.0, %D2D1_BITMAP_INTERPOLATION_MODE_LINEAR)
END METHOD



'GRAPHIC STRETCH hBmp, 0, (srcleft, srctop)-(srcright, srcbottom) TO (destleft, desttop)-(destright, destbottom), %MIX_COPYSRC, %COLORONCOLOR
METHOD GraphicStretch(sprnr&, BYVAL srcleft!, BYVAL srctop!, BYVAL srcright!, BYVAL srcbottom!, BYVAL destleft!, BYVAL desttop!, BYVAL destright!, BYVAL destbottom!, OPTIONAL BYVAL opacity!)
  IF sprnr& < 0 OR sprnr& >= i_BitmapCount THEN EXIT METHOD

  IF opacity! = 0 THEN opacity! = 1
  i_pRenderTarget.DrawBitmap(i_pBitmap(sprnr&), i_pID2D1Helper.RectF(destleft!, desttop!, destright!, destbottom!), opacity!, %D2D1_BITMAP_INTERPOLATION_MODE_LINEAR, i_pID2D1Helper.RectF(srcleft!, srctop!, srcright!, srcbottom!))
END METHOD



METHOD GraphicCopyFromRenderTarget(sprnr&, BYVAL srcleft!, BYVAL srctop!, BYVAL srcright!, BYVAL srcbottom!)
  LOCAL src AS D2D1_RECT_U
  LOCAL dest AS D2D1_POINT_2U

  IF sprnr& < 0 OR sprnr& >= i_BitmapCount THEN EXIT METHOD

  'copy the selected area from the render target (screen) to the specified bitmap
  src.left = srcleft!
  src.top = srctop!
  src.right = srcright!
  src.bottom = srcbottom!
  dest.x = 0
  dest.y = 0
  i_pBitmap(sprnr&).CopyFromRenderTarget(dest, i_pRenderTarget, src)
END METHOD



'GRAPHIC SET POS (x&, y&)
'GRAPHIC COLOR cl&, -2
'GRAPHIC SET FONT font&
'GRAPHIC PRINT text$, x&, y&, cl&, font&
METHOD GraphicPrint(t$, BYVAL x!, BYVAL y!, cl&, fnt&)
  LOCAL tx AS WSTRINGZ*1024
  LOCAL renderTargetSize AS D2D1_SIZE_F

  IF cl& < 0 OR cl& >= i_BrushCount THEN EXIT METHOD
  IF fnt& < 0 OR fnt& >= i_FontCount THEN EXIT METHOD

  tx = t$
  renderTargetSize = i_pRenderTarget.GetSize()
  i_pRenderTarget.DrawText(tx, LEN(tx), i_pFont(fnt&), i_pID2D1Helper.RectF(x!, y!, renderTargetSize.width, renderTargetSize.height), i_pBrush(cl&), %D2D1_DRAW_TEXT_OPTIONS_NONE, %DWRITE_MEASURING_MODE_NATURAL)
END METHOD



'GRAPHIC TEXT SIZE t$ TO width&, height&
METHOD GraphicTextSize(BYVAL t$, BYVAL fnt&, BYREF textwidth&, BYREF textheight&)
  LOCAL tx AS WSTRINGZ*1024
  LOCAL textLayout AS IDWriteTextLayout
  LOCAL textMetrics AS DWRITE_TEXT_METRICS

  tx = t$
  i_pDWriteFactory.CreateTextLayout(tx, LEN(tx), i_pFont(fnt&), 4000, 100, textLayout)
  textLayout.GetMetrics(textMetrics)
  textwidth& = textMetrics.WidthIncludingTrailingWhitespace
  textheight& = textMetrics.height
  textLayout = NOTHING
END METHOD



'GRAPHIC GET SIZE TO width&, height&
METHOD GraphicGetSize(sprnr&, bitmapWidth&, bitmapHeight&)
  LOCAL sz AS D2D1_SIZE_U

  IF sprnr& >= 0 AND sprnr& < i_BitmapCount THEN sz = i_pBitmap(sprnr&).GetPixelSize()
  bitmapWidth& = sz.width
  bitmapHeight& = sz.height
END METHOD



'GRAPHIC BITMAP LOAD filename$, width&, height& TO handle&
METHOD GraphicBitmapLoad(BYVAL f$, destWidth&, destHeight&) AS LONG
  LOCAL hr&
  LOCAL hD2DBmp AS ID2D1Bitmap

  LOCAL sz AS WSTRINGZ*512

  sz = f$
  IF D2D_LoadBitmapFromFile(i_pRenderTarget, i_pWICFactory, sz, destWidth&, destHeight&, hD2DBmp) <> %S_OK THEN
    METHOD = -1
    EXIT METHOD
  END IF

  IF i_BitmapCount > UBOUND(i_pBitmap) THEN REDIM PRESERVE i_pBitmap(i_BitmapCount+i_InitialBitmapCount)
  i_pBitmap(i_BitmapCount) = hD2DBmp
  i_BitmapCount = i_BitmapCount+1

  METHOD = i_BitmapCount-1
END METHOD



'Load a GIF file and return its pixel data as string (converted to BGRA format)
'To be used to get the pixel data (for manipulation) and create a Direct2D bitmap with "CreateMemoryBitmap" afterwards
'Use "GraphicBitmapLoad" if there is no need to manipulate the pixel data
METHOD GraphicLoadGIF(BYVAL f$, BYREF bitmapwidth&, BYREF bitmapheight&, BYREF pixeldata$, OPTIONAL BYVAL erasebackground&) AS LONG
  LOCAL fnr&, filelen&
  LOCAL p&, q&, l&, par&, gct&, bpp&, gctsize&, tp&, xpos&, ypos&, xsize&, ysize&, lct&, il&, st&, lctsize&, cs&, dt&, tr&, ui&, dm&
  LOCAL bgcolor&, transparentcolor&, sorted&
  LOCAL a$, b$, pal$, sig$, ver$

  'read GIF file into memory
  IF ISFILE(f$) = 0 THEN EXIT METHOD
  fnr& = FREEFILE
  OPEN f$ FOR BINARY LOCK SHARED AS fnr&
  filelen& = LOF(fnr&)
  GET$ fnr&, filelen&, a$
  CLOSE fnr&

  'header
  p& = 1
  sig$ = MID$(a$, p&,3)    'signatur (GIF-ID)
  ver$ = MID$(a$, p&+3, 3)  'version string
  IF sig$ <> "GIF" THEN EXIT METHOD
  p& = p&+6

  'logical screen descriptor
  bitmapwidth& = CVI(a$, p&)     'bitmap width
  bitmapheight& = CVI(a$, p&+2)  'bitmap height
  q& = ASC(a$, p&+4)
  bgcolor& = ASC(a$, p&+5)       'background color
  par& = ASC(a$, p&+6)           'pixel-aspect-ratio
  gct& = q&\128                  'global color table present
  bpp& = (q&\16 AND 7)+1         'color depth (bits per pixel)
  sorted& = q&\8 AND 1           'color table is sorted
  gctsize& = 3*2^((q& AND 7)+1)  'global color table size
  p& = p&+7
  IF bpp& <> 8 THEN EXIT METHOD  'only 8 bit color supported

  'global color table
  IF gct& <> 0 THEN
    pal$ = MID$(a$, p&, gctsize&)  'color table
    p& = p&+gctsize&
  END IF

  'data blocks
  DO
    tp& = ASC(a$, p&)
    SELECT CASE tp&
    CASE &H21:  'extension block
      p& = p&+1

    CASE &H2C:  'image data
      'image descriptor
      xpos& = CVI(a$, p&+1)          'x-position
      ypos& = CVI(a$, p&+3)          'y-position
      xsize& = CVI(a$, p&+5)         'width
      ysize& = CVI(a$, p&+7)         'height
      q& = ASC(a$, p&+9)
      lct& = q&\128                  'local color table present
      il& = q&\64 AND 1              'interlace mode
      st& = q&\32 AND 1              'color table is sorted
      lctsize& = 3*2^((q& AND 7)+1)  'local color table size
      p& = p&+10
      'locale color table
      IF lct& <> 0 THEN
        pal$ = MID$(a$, p&, lctsize&)  'color table
        p& = p&+lctsize&
      END IF
      'table based image data
      cs& = ASC(a$, p&)              'starting bit size for LZW-compression
      p& = p&+1
      b$ = ""
      DO
        l& = ASC(a$, p&)             'block length
        p& = p&+1
        IF l& = 0 THEN EXIT LOOP
        b$ = b$+MID$(a$, p&, l&)     'LZW-compressed pixel data
        p& = p&+l&
      LOOP
      pixeldata$ = ME.LZWDecompress_ASM(b$, cs&, bitmapwidth&*bitmapheight&)
      IF il& <> 0 THEN pixeldata$ = ME.DeinterlaceGIF(pixeldata$, xsize&)
      EXIT LOOP

    CASE &H3B:  'trailer
      p& = p&+1
      EXIT LOOP

    CASE &HF9:  'graphic control extension
      l& = ASC(a$, p&+1)                 'block size
      q& = ASC(a$, p&+2)
      dt& = CVI(a$, p&+3)                'delay time
      transparentcolor& = ASC(a$, p&+5)  'index of the transparent color
      tr& = q& AND 1                     'transparent flag (set to 1 if transparent color is used)
      ui&= q&\2 AND 1                    'wait for user
      dm&= q&\4 AND 7                    'dispose mode
      IF tr& = 0 THEN transparentcolor& = -1
      p& = p&+l&+3

    CASE &HFF:  'application extension
      l& = ASC(a$, p&+1)  'block size
      p& = p&+l&+2
      'skip sub blocks
      WHILE p& <= LEN(A$)
        IF ASC(a$, p&) = 0 THEN EXIT LOOP
        l& = ASC(a$, p&+1)
        p& = p&+l&+3      'type(1) + length(1) + data(L) + terminator(1)
      WEND
      p& = p&+1

    CASE ELSE:  'unknown block
      l& = ASC(a$, p&+1)  'block size
      p& = p&+l&+3        'type(1) + length(1) + data(L) + terminator(1)
    END SELECT
  LOOP

  'stuff palette to 768 bytes
  pal$ = pal$+STRING$(768-LEN(pal$), 0)

  'convert pixel data from indexed colors to BGRA
  IF erasebackground& <> 0 THEN transparentcolor& = ASC(pixeldata$)  'make all pixels transparent that have the same color as the bitmap border
  pixeldata$ = ME.ConvertIndexedColorsToBGRA(pixeldata$, pal$, transparentcolor&)

  METHOD = 1
END METHOD



'Draw a triangle (special version of GraphicPolygon)
METHOD DrawTriangle(BYVAL x1!, BYVAL y1!, BYVAL x2!, BYVAL y2!, BYVAL x3!, BYVAL y3!, bcolor&, fcolor&)
  LOCAL pathgeometry AS ID2D1PathGeometry
  LOCAL sink AS ID2D1GeometrySink
  LOCAL p AS D2D1_POINT_2F

  'create path geometry from the 3 points
  i_pD2DFactory.CreatePathGeometry(pathgeometry)
  pathgeometry.Open(sink)
  sink.SetFillMode(%D2D1_FILL_MODE_ALTERNATE)
  p.x = x1!
  p.y = y1!
  sink.BeginFigure(p, %D2D1_FIGURE_BEGIN_FILLED)
  p.x = x2!
  p.y = y2!
  sink.AddLine(p)
  p.x = x3!
  p.y = y3!
  sink.AddLine(p)
  p.x = x1!
  p.y = y1!
  sink.AddLine(p)
  sink.EndFigure(%D2D1_FIGURE_END_CLOSED)
  sink.Close
  sink = NOTHING

  'draw triangle
  ME.GraphicPolygon(pathgeometry, bcolor&, fcolor&)
END METHOD



'Initialize Direct2D
METHOD InitD2D(BYVAL hwnd&, BYVAL bitmapArraySize&, BYVAL renderFunction AS DWORD) AS LONG
  LOCAL r&
  DIM i_pBitmap(bitmapArraySize&-1)
  DIM i_pBrush(%MAXBRUSHES-1)
  DIM i_pFont(%MAXFONTS-1), i_fontsize(%MAXFONTS-1)
  DIM i_Controls(%INITIALCONTROLSIZE-1)

  i_hWnd = hwnd&
  i_InitialBitmapCount = bitmapArraySize&
  i_pRenderFunction = renderFunction
  i_pClippingRegion = NOTHING

  i_pID2D1Helper = CLASS "CD2D1Helper"
  IF ISNOTHING(i_pID2D1Helper) THEN EXIT METHOD

  IF ISFALSE ME.CreateDeviceIndependentResources THEN EXIT METHOD
  IF ISFALSE ME.CreateDeviceResources(hwnd&) THEN EXIT METHOD

  METHOD = %TRUE
END METHOD



'Set anti-alias mode
'md&: %D2D1_ANTIALIAS_MODE_PER_PRIMITIVE or %D2D1_ANTIALIAS_MODE_ALIASED
METHOD SetAntialiasMode(md&)
  i_pRenderTarget.SetAntialiasMode(md&)
END METHOD



'Create a solid brush
METHOD CreateSolidBrush(r?, g?, b?, OPTIONAL BYVAL opacity?) AS LONG
  LOCAL brushProperties AS D2D1_BRUSH_PROPERTIES
  LOCAL c AS D2D1_COLOR_F

  IF i_BrushCount = %MAXBRUSHES THEN
    METHOD = -1
    EXIT METHOD
  END IF
  brushProperties.opacity = 1.0
  brushProperties.transform = i_pID2D1Helper.IdentityMatrix()
  IF opacity? = 0 THEN opacity? = 255
  c.r = r?/255
  c.g = g?/255
  c.b = b?/255
  c.a = opacity?/255
  i_pRenderTarget.CreateSolidColorBrush(c, brushProperties, i_pBrush(i_BrushCount))
  i_BrushCount = i_BrushCount+1

  METHOD = i_BrushCount-1
END METHOD



'Create a gradient brush
METHOD CreateGradientBrush(r1?, g1?, b1?, r2?, g2?, b2?, x0!, y0!, x1!, y1!) AS LONG
  LOCAL stops() AS D2D1_GRADIENT_STOP
  LOCAL c1 AS D2D1_COLOR_F, c2 AS D2D1_COLOR_F
  LOCAL gsc AS ID2D1GradientStopCollection
  LOCAL gradientBrushProperties AS D2D1_LINEAR_GRADIENT_BRUSH_PROPERTIES
  LOCAL brushProperties AS D2D1_BRUSH_PROPERTIES
  DIM stops(1)

  IF i_BrushCount = %MAXBRUSHES THEN
    METHOD = -1
    EXIT METHOD
  END IF

  'create gradient stops (colors)
  c1.r = r1?/255
  c1.g = g1?/255
  c1.b = b1?/255
  stops(0).position = 0
  stops(0).color = c1
  c2.r = r2?/255
  c2.g = g2?/255
  c2.b = b2?/255
  stops(1).position = 1
  stops(1).color = c2
  i_pRenderTarget.CreateGradientStopCollection(stops(0), 2, %D2D1_GAMMA_2_2, %D2D1_EXTEND_MODE_MIRROR, gsc)

  'create brush properties (fill area)
  gradientBrushProperties.startPoint = i_pID2D1Helper.Point2F(x0!, y0!)
  gradientBrushProperties.endPoint = i_pID2D1Helper.Point2F(x1!, y1!)
  brushProperties.opacity = 1.0
  brushProperties.transform = i_pID2D1Helper.IdentityMatrix()

  'create brush
  i_pRenderTarget.CreateLinearGradientBrush(gradientBrushProperties, brushProperties, gsc, i_pBrush(i_BrushCount))
  i_BrushCount = i_BrushCount+1

'PRINT "CreateGradientBrush: "; ISOBJECT(gsc); ISOBJECT(i_pBrush(i_BrushCount-1))

  METHOD = i_BrushCount-1
END METHOD



'Create a font
METHOD CreateFont(fontname$, isbold&, sz!) AS LONG
  LOCAL hr&, b&
  LOCAL nm AS WSTRINGZ*50, localname AS WSTRINGZ*260

  IF i_FontCount = %MAXFONTS THEN
    METHOD = -1
    EXIT METHOD
  END IF

  nm = fontname$
  b& = IIF&(isbold&, %DWRITE_FONT_WEIGHT_BOLD, %DWRITE_FONT_WEIGHT_NORMAL)
  'PBCC uses font size in points (1 point = 1/72 inch) but CreateTextFormat requires it in DIP (1 DIP = 1/96 inch)
  hr& = i_pDWriteFactory.CreateTextFormat(nm, NOTHING, b&, %DWRITE_FONT_STYLE_NORMAL, %DWRITE_FONT_STRETCH_NORMAL, sz!*96/72, localname, i_pFont(i_FontCount))
  i_fontsize(i_FontCount) = sz!
  i_FontCount = i_FontCount+1

  METHOD = i_FontCount-1
END METHOD



'Create a memory bitmap from a pixel data array
METHOD CreateMemoryBitmap(wd&, hg&, px$) AS LONG
  LOCAL hr&
  LOCAL hWicBmp AS IWICBitmap
  LOCAL hD2DBmp AS ID2D1Bitmap

  hr& = i_pWICFactory.CreateBitmapFromMemory(wd&, hg&, $GUID_WICPixelFormat32bppPBGRA, wd&*4, wd&*hg&*4, STRPTR(px$), hWicBmp)
  hr& = i_pRenderTarget.CreateBitmapFromWicBitmap(hWicBmp, BYVAL %NULL, hD2DBmp)
  IF i_BitmapCount > UBOUND(i_pBitmap) THEN REDIM PRESERVE i_pBitmap(i_BitmapCount+i_InitialBitmapCount)
  i_pBitmap(i_BitmapCount) = hD2DBmp
  i_BitmapCount = i_BitmapCount+1

  METHOD = i_BitmapCount-1
END METHOD



'Create a rectangle clipping region and activate it
METHOD CreateClippingRegion(BYVAL x0&, BYVAL y0&, BYVAL x1&, BYVAL y1&)
  LOCAL pathgeometry AS ID2D1PathGeometry
  LOCAL sink AS ID2D1GeometrySink
  LOCAL p AS D2D1_POINT_2F
  LOCAL i&

  'create path geometry from the points array
  i_pD2DFactory.CreatePathGeometry(pathgeometry)
  pathgeometry.Open(sink)
  sink.SetFillMode(%D2D1_FILL_MODE_WINDING)
  p.x = x0&
  p.y = y0&
  sink.BeginFigure(p, %D2D1_FIGURE_BEGIN_FILLED)
  p.x = x1&
  p.y = y0&
  sink.AddLine(p)
  p.x = x1&
  p.y = y1&
  sink.AddLine(p)
  p.x = x0&
  p.y = y1&
  sink.AddLine(p)
  sink.EndFigure(%D2D1_FIGURE_END_CLOSED)
  sink.Close
  sink = NOTHING

  'create a layer
  IF NOT ISNOTHING(i_pClippingRegion) THEN ME.ReleaseClippingRegion
  i_pRenderTarget.CreateLayer(BYVAL %NULL, i_pClippingRegion)

  'activate the layer
  i_pRenderTarget.PushLayer(i_pID2D1Helper.LayerParameters(i_pID2D1Helper.InfiniteRect, pathgeometry), i_pClippingRegion)
END METHOD



'Releases the clipping region
METHOD ReleaseClippingRegion
  IF NOT ISNOTHING(i_pClippingRegion) THEN
    i_pRenderTarget.PopLayer
  END IF
  i_pClippingRegion = NOTHING
END METHOD



'Resets the matrix to the identity matrix so all output is drawn normally again
METHOD ResetMatrix
  i_pRenderTarget.SetTransform(i_pID2D1Helper.IdentityMatrix())
END METHOD



'Rotates all output drawn after this call
'angle! is in degree
METHOD RotateOutput(angle!, x0!, y0!)
  i_pRenderTarget.SetTransform(i_pID2D1Helper.MatrixRotation(angle!, i_pID2D1Helper.Point2F(x0!, y0!)))
END METHOD



'Scales all output drawn after this call
METHOD ScaleOutput(xscale!, yscale!, OPTIONAL BYVAL xcenter!, OPTIONAL BYVAL ycenter!)
  i_pRenderTarget.SetTransform(i_pID2D1Helper.MatrixScale(xscale!, yscale!, i_pID2D1Helper.Point2F(xcenter!, ycenter!)))
END METHOD



'Add a DX-Control to the control array
'METHOD AddControl(cntr AS IDXCONTROL)
METHOD AddControl(cntr AS IDXCONTROL)
  IF i_ControlCount > UBOUND(i_Controls) THEN REDIM PRESERVE i_Controls(i_ControlCount+%INITIALCONTROLSIZE)
  i_Controls(i_ControlCount) = cntr
  i_ControlCount = i_ControlCount+1
  cntr.ID = i_ControlCount
END METHOD



'Defines the texture for all bitmap-buttons (texturenr is the result of "CreateMemoryBitmap")
METHOD SetBitmapButtonTexture(texturenr&)
  i_bitmapbuttontexture = texturenr&
END METHOD



'Returns the DX-Control which has the focus
METHOD GetFocusedControl AS IDXCONTROL
  LOCAL i&

  FOR i& = 0 TO i_ControlCount-1
    IF i_Controls(i&).Visible <> 0 AND i_Controls(i&).HasFocus = 1 THEN
      METHOD = i_Controls(i&)
      EXIT METHOD
    END IF
  NEXT i&

  METHOD = NOTHING
END METHOD



'Returns the DX-Control at a pixel position
METHOD GetControlAtPos(x&, y&) AS IDXCONTROL
  LOCAL i&

  FOR i& = 0 TO i_ControlCount-1
    IF i_Controls(i&).Visible <> 0 AND x& >= i_Controls(i&).XPos AND x& <= i_Controls(i&).XPos+i_Controls(i&).Width AND y& >= i_Controls(i&).YPos AND y& <= i_Controls(i&).YPos+i_Controls(i&).Height THEN
      METHOD = i_Controls(i&)
      EXIT METHOD
    END IF
  NEXT i&

  METHOD = NOTHING
END METHOD



'Hides all controls
METHOD HideAllControls
  LOCAL i&

  FOR i& = 0 TO i_ControlCount-1
    i_Controls(i&).Visible = 0
  NEXT i&
END METHOD



'Closes all comboboxes
METHOD CloseAllListboxes
LOCAL i&

  FOR i& = 0 TO i_ControlCount-1
    IF i_Controls(i&).ControlType = %CTYPE_LISTBOX THEN i_Controls(i&).ComboboxOpen = 0
  NEXT i&
END METHOD



'Render the scene using the installed render-callback function
METHOD OnRender(mx&, my&) AS LONG
  LOCAL hr&, i&

  hr& = ME.CreateDeviceResources(i_hWnd)
  IF hr& <> %TRUE THEN EXIT METHOD

  'if the window is occluded then do not render
  IF (i_pRenderTarget.CheckWindowState AND %D2D1_WINDOW_STATE_OCCLUDED) <> 0 THEN EXIT METHOD

  'begin draw and clear screen
  i_pRenderTarget.BeginDraw()
  i_pRenderTarget.SetTransform(i_pID2D1Helper.IdentityMatrix())
  i_pRenderTarget.Clear(i_pID2D1Helper.ColorF_2(%D2D1_White))

  'draw scene
  CALL DWORD i_pRenderFunction USING RenderFunctionPrototype(i_pRenderTarget)

  'draw controls on top of scene
  FOR i& = 0 TO i_ControlCount-1
    IF i_Controls(i&).Visible <> 0 THEN i_Controls(i&).OnRender(i_pRenderTarget, mx&, my&)
  NEXT i&

  'draw the dropped-down items of the open combobox (if any combobox is open)
  FOR i& = 0 TO i_ControlCount-1
    IF i_Controls(i&).Visible <> 0 AND i_Controls(i&).ControlType = %CTYPE_LISTBOX AND i_Controls(i&).ComboboxOpen <> 0 THEN
      i_Controls(i&).OnRenderListboxOpen(i_pRenderTarget, mx&, my&)
      EXIT FOR
    END IF
  NEXT i&

  'end draw
  hr& = i_pRenderTarget.EndDraw()
  IF hr& = %D2DERR_RECREATE_TARGET THEN
    ME.DiscardDeviceResources
  END IF

  METHOD = %TRUE
END METHOD



'Processes key events
METHOD OnKeyPress(k$)
  LOCAL i&

  'pass pressed key to focused control
  FOR i& = 0 TO i_ControlCount-1
    IF i_Controls(i&).Visible <> 0 AND i_Controls(i&).HasFocus <> 0 THEN
      i_Controls(i&).OnKeyPress(k$)
      EXIT FOR
    END IF
  NEXT i&
END METHOD



'Processes mouse click events
'btn = 1: left button down
'      2: left button up
'      3: right button down
'      4: right button up
METHOD OnClick(mx&, my&, btn&)
  LOCAL i&, x&, y&, wd&, hg&, maxitems&

  'remove focus from all controls
  FOR i& = 0 TO i_ControlCount-1
    i_Controls(i&).HasFocus = 0
  NEXT i&

  'check if mouse is over a dropped-down comboxbox
  FOR i& = 0 TO i_ControlCount-1
    IF i_Controls(i&).Visible <> 0 AND i_Controls(i&).ControlType = %CTYPE_LISTBOX AND i_Controls(i&).ComboboxOpen <> 0 THEN
      x& = i_Controls(i&).XPos
      y& = i_Controls(i&).YPos
      wd& = i_Controls(i&).Width
      hg& = i_Controls(i&).Height
      maxitems& = MIN&(%LISTBOX_MAXOPENSIZE, i_Controls(i&).ItemCount)
      IF mx& > x&+hg&/2+3 AND mx& < x&+wd&-hg&/2-3 AND my& > y&+hg& AND my& < y&+hg&+hg&*maxitems& THEN
        i_Controls(i&).HasFocus = 1
        i_Controls(i&).OnClick(mx&, my&, btn&)
        EXIT METHOD
      ELSE
        EXIT FOR
      END IF
    END IF
  NEXT i&

  'find visible control at this position and set focus to it
  FOR i& = 0 TO i_ControlCount-1
    IF i_Controls(i&).Visible <> 0 AND mx& >= i_Controls(i&).XPos AND mx& <= i_Controls(i&).XPos+i_Controls(i&).Width AND my& >= i_Controls(i&).YPos AND my& <= i_Controls(i&).YPos+i_Controls(i&).Height THEN
      i_Controls(i&).HasFocus = 1
      i_Controls(i&).OnClick(mx&, my&, btn&)
      EXIT FOR
    END IF
  NEXT i&
END METHOD



'If the application receives a WM_SIZE message, this method resize the render target appropriately.
METHOD OnResize(BYVAL nWidth AS LONG, BYVAL nHeight AS LONG)
  LOCAL su AS D2D1_SIZE_U

  IF ISOBJECT(i_pRenderTarget) THEN
    su.width = nWidth
    su.height = nHeight
    i_pRenderTarget.Resize(su)
  END IF
END METHOD



END INTERFACE

END CLASS



'==================================================================================================
'Control class
'==================================================================================================

CLASS DXCONTROL

INSTANCE i_d2d AS IDIRECT2D
INSTANCE i_controltype AS LONG          'control type (edit, listbox, etc.)
INSTANCE i_callback AS DWORD            'callback function for events
INSTANCE i_id AS LONG                   'unique ID (will be set automatically by CDIRECT2D.AddControl)
INSTANCE i_xpos AS LONG                 'x-position (left)
INSTANCE i_ypos AS LONG                 'y-position (top)
INSTANCE i_width AS LONG                'control width
INSTANCE i_height AS LONG               'control height
INSTANCE i_visible AS LONG              'flag if control should be rendered
INSTANCE i_enabled AS LONG              'flag if control is enabled
INSTANCE i_hasfocus AS LONG             'flag if control should receive input (must also be visible)
INSTANCE i_maxlength AS LONG            'max input length (edit only)
INSTANCE i_cursorpos AS LONG            'current cursor position (edit only)
INSTANCE i_multiline AS LONG            'set to 1 if multi-line edit (edit only)
INSTANCE i_value AS STRING              'value
INSTANCE i_maxprogress AS LONG          'maximum progressbar value
INSTANCE i_curprogress AS LONG          'current progressbar value
INSTANCE i_maxscroll AS LONG            'maximum scrollbar value
INSTANCE i_visiblerows AS LONG          'number of rows that fit into the display area (used to calculate the size of the scrollbar drawer)
INSTANCE i_scrollposition AS LONG       'current scrollbar value
INSTANCE i_caption AS STRING            'caption
INSTANCE i_font AS LONG                 'font
INSTANCE i_textcolor AS LONG            'text color
INSTANCE i_bordercolor AS LONG          'border color
INSTANCE i_shadowcolor AS LONG          'shadow color
INSTANCE i_backgroundcolor AS LONG      'background color
INSTANCE i_highlightcolor AS LONG       'highlight color
INSTANCE i_buttondown AS LONG           'set to 1 if the button is down (button + bitmapbutton only)
INSTANCE i_selecteditem AS LONG         'selected listbox index (listbox + radiogroup only)
INSTANCE i_itemcount AS LONG            'number of listbox items (listbox + radiogroup only) or number of text lines (multi-line edit)
INSTANCE i_items() AS STRING            'listbox items (listbox + radiogroup only) or text lines (multi-line edit)
INSTANCE i_bitmapindex AS LONG          'Y-position into the texture (bitmapbutton only)
INSTANCE i_iconwidth AS LONG            'icon width (bitmapbutton only)
INSTANCE i_iconheight AS LONG           'icon height (bitmapbutton only)
INSTANCE i_brushhightlight AS LONG      'gradient brush for highlight (progressbar only)
INSTANCE i_comboboxopen AS LONG         'set to 1 if the combobox is dropped down (listbox only)
INSTANCE i_vscrollpos AS LONG           'vertical scroll position (multi-line edit only)
INSTANCE i_editflags AS LONG            'edit flags (edit only)


INTERFACE IDXCONTROL : INHERIT IUNKNOWN

PROPERTY GET D2D AS IDIRECT2D
  PROPERTY = i_d2d
END PROPERTY
PROPERTY SET D2D(BYVAL v AS IDIRECT2D)
  i_d2d = v
END PROPERTY

PROPERTY GET ID AS LONG
  PROPERTY = i_id
END PROPERTY
PROPERTY SET ID(BYVAL v AS LONG)
  i_id = v
END PROPERTY

PROPERTY GET ControlType AS LONG
  PROPERTY = i_controltype
END PROPERTY

PROPERTY GET XPos AS LONG
  PROPERTY = i_xpos
END PROPERTY
PROPERTY SET XPos(BYVAL v AS LONG)
  i_xpos = v
END PROPERTY

PROPERTY GET YPos AS LONG
  PROPERTY = i_ypos
END PROPERTY
PROPERTY SET YPos(BYVAL v AS LONG)
  i_ypos = v
END PROPERTY

PROPERTY GET Width AS LONG
  PROPERTY = i_width
END PROPERTY
PROPERTY SET WIDTH(BYVAL v AS LONG)
  i_width = v
END PROPERTY

PROPERTY GET Height AS LONG
  PROPERTY = i_height
END PROPERTY
PROPERTY SET Height(BYVAL v AS LONG)
  i_height = v
END PROPERTY

PROPERTY GET Visible AS LONG
  PROPERTY = i_visible
END PROPERTY
PROPERTY SET Visible(BYVAL v AS LONG)
  i_visible = v
END PROPERTY

PROPERTY GET Enabled AS LONG
  PROPERTY = i_enabled
END PROPERTY
PROPERTY SET Enabled(BYVAL v AS LONG)
  i_enabled = v
END PROPERTY

PROPERTY GET HasFocus AS LONG
  PROPERTY = i_hasfocus
END PROPERTY
PROPERTY SET HasFocus(BYVAL v AS LONG)
  i_hasfocus = v
END PROPERTY

PROPERTY GET MaxLength AS LONG
  PROPERTY = i_maxlength
END PROPERTY
PROPERTY SET MaxLength(BYVAL v AS LONG)
  i_maxlength = v
END PROPERTY

PROPERTY GET Value AS STRING
  PROPERTY = i_value
END PROPERTY
PROPERTY SET Value(BYVAL v AS STRING)
  i_value = v
  IF i_controltype = %CTYPE_EDIT AND i_multiline > 0 THEN ME.EditSetTextLines
END PROPERTY

PROPERTY GET Caption AS STRING
  PROPERTY = i_caption
END PROPERTY
PROPERTY SET CAPTION(BYVAL v AS STRING)
  i_caption = v
END PROPERTY

PROPERTY GET SelectedItem AS LONG
  PROPERTY = i_selecteditem
END PROPERTY
PROPERTY SET SelectedItem(BYVAL v AS LONG)
  i_selecteditem = v
END PROPERTY

PROPERTY GET ItemCount AS LONG
  PROPERTY = i_itemcount
END PROPERTY

PROPERTY GET ItemValue(itemNr&) AS STRING
  LOCAL v$

  IF itemNr& >= 0 AND itemNr& <= UBOUND(i_items) THEN v$ = i_items(itemNr&)
  PROPERTY = v$
END PROPERTY

PROPERTY GET CurrentProgress AS LONG
  PROPERTY = i_curprogress
END PROPERTY
PROPERTY SET CurrentProgress(BYVAL v AS LONG)
  i_curprogress = MAX&(0, MIN&(v, i_maxprogress))
END PROPERTY

PROPERTY GET MaxProgress AS LONG
  PROPERTY = i_maxprogress
END PROPERTY
PROPERTY SET MaxProgress(BYVAL v AS LONG)
  i_maxprogress = v
END PROPERTY

PROPERTY GET ScrollPosition AS LONG
  PROPERTY = i_scrollposition
END PROPERTY
PROPERTY SET ScrollPosition(BYVAL v AS LONG)
  i_scrollposition = MAX&(0, MIN&(v, i_maxscroll))
END PROPERTY

PROPERTY GET MaxScroll AS LONG
  PROPERTY = i_maxscroll
END PROPERTY
PROPERTY SET MaxScroll(BYVAL v AS LONG)
  i_maxscroll = v
END PROPERTY

PROPERTY GET VisibleRows AS LONG
  PROPERTY = i_visiblerows
END PROPERTY
PROPERTY SET VisibleRows(BYVAL v AS LONG)
  i_visiblerows = v
END PROPERTY

PROPERTY SET HighlightColor(BYVAL v AS LONG)
  i_highlightcolor = v
END PROPERTY

PROPERTY GET ComboboxOpen AS LONG
  PROPERTY = i_comboboxopen
END PROPERTY
PROPERTY SET ComboboxOpen(BYVAL v AS LONG)
  i_comboboxopen = v
END PROPERTY

PROPERTY GET EditUppercase AS LONG
  PROPERTY = i_editflags
END PROPERTY
PROPERTY SET EditUppercase(BYVAL v AS LONG)
  i_editflags = v
END PROPERTY




'Moves and resizes the control
METHOD MoveControl(x&, y&, wd&, hg&)
  ME.XPos = x&
  ME.YPos = y&
  ME.Width = wd&
  ME.Height = hg&
END METHOD


'Renders the control (will be called automatically from the OnRender event of the CDIRECT2D class)
METHOD OnRender(RenderTarget AS ID2D1HwndRenderTarget, mx&, my&)
  SELECT CASE i_controltype
  CASE %CTYPE_EDIT: ME.OnRenderEdit(RenderTarget, mx&, my&)
  CASE %CTYPE_LISTBOX: ME.OnRenderListbox(RenderTarget, mx&, my&)
  CASE %CTYPE_BUTTON: ME.OnRenderButton(RenderTarget, mx&, my&)
  CASE %CTYPE_RADIOGROUP: ME.OnRenderRadiogroup(RenderTarget, mx&, my&)
  CASE %CTYPE_CHECKBOX: ME.OnRenderCheckbox(RenderTarget, mx&, my&)
  CASE %CTYPE_BITMAPBUTTON: ME.OnRenderBitmapButton(RenderTarget, mx&, my&)
  CASE %CTYPE_PROGRESSBAR: ME.OnRenderProgressbar(RenderTarget, mx&, my&)
  CASE %CTYPE_SCROLLBAR: ME.OnRenderScrollbar(RenderTarget, mx&, my&)
  END SELECT
END METHOD


'Processes key events (will be called automatically from the OnKeyPress event of the CDIRECT2D class)
METHOD OnKeyPress(k$)
  SELECT CASE i_controltype
  CASE %CTYPE_EDIT:
    ME.OnKeyPressEdit(k$)
  END SELECT
END METHOD


'Process mouse click events
METHOD OnClick(mx&, my&, btn&)
  SELECT CASE i_controltype
  CASE %CTYPE_EDIT: ME.OnClickEdit(mx&, my&, btn&)
  CASE %CTYPE_LISTBOX: ME.OnClickListbox(mx&, my&, btn&)
  CASE %CTYPE_BUTTON: ME.OnClickButton(mx&, my&, btn&)
  CASE %CTYPE_RADIOGROUP: ME.OnClickRadiogroup(mx&, my&, btn&)
  CASE %CTYPE_CHECKBOX: ME.OnClickCheckbox(mx&, my&, btn&)
  CASE %CTYPE_BITMAPBUTTON: ME.OnClickBitmapButton(mx&, my&, btn&)
  CASE %CTYPE_SCROLLBAR: ME.OnClickScrollbar(mx&, my&, btn&)
  END SELECT
END METHOD



'==================================================================================================
'Edit methods
'==================================================================================================


'Initialize the control
METHOD InitEdit(pd2d AS IDIRECT2D, cpt$, v$, x&, y&, wd&, hg&, maxlen&, fnt&, textcolor&, bordercolor&, backgroundcolor&, vis&)
  i_controltype = %CTYPE_EDIT
  ME.D2D = pd2d
  i_caption = cpt$
  i_value = v$
  ME.XPos = x&
  ME.YPos = y&
  ME.Width = wd&
  ME.Height = hg&
  i_maxlength = maxlen&
  i_cursorpos = 0
  i_font = fnt&
  i_textcolor = textcolor&
  i_bordercolor = bordercolor&
  i_backgroundcolor = backgroundcolor&
  ME.Visible = vis&
  ME.Enabled = 1

  ME.D2D.AddControl(ME)
END METHOD


'Initialize the control asc multi-line edit
METHOD InitEditMultiline(pd2d AS IDIRECT2D, cpt$, v$, x&, y&, wd&, hg&, maxlen&, fnt&, textcolor&, bordercolor&, backgroundcolor&, vis&)
  i_multiline = 1
  i_vscrollpos = 0
  ME.InitEdit(pd2d, cpt$, v$, x&, y&, wd&, hg&, maxlen&, fnt&, textcolor&, bordercolor&, backgroundcolor&, vis&)
  ME.EditSetTextLines
END METHOD


'Populates the text-lines array from the "i_value" property
METHOD EditSetTextLines
  LOCAL arraysize&, i&, wd&, hg&, t$

  i_itemcount = 0
  IF i_value = "" THEN EXIT METHOD

  'split text into lines
  t$ = i_value
  i& = 1
  arraysize& = 256
  REDIM i_items(arraysize&-1)
  WHILE i& <= LEN(t$)
    ME.D2D.GraphicTextSize(LEFT$(t$, i&+1), i_font, wd&, hg&)
    IF wd& > ME.Width-4 OR ASC(t$, i&) = 13 THEN
      i_items(i_itemcount) = LEFT$(t$, i&)
      i_itemcount = i_itemcount+1
      t$ = MID$(t$, i&+1)
      i& = 0
      IF i_itemcount = arraysize& THEN
        arraysize& = arraysize&+256
        REDIM PRESERVE i_items(arraysize&-1)
      END IF
    END IF
    i& = i&+1
  WEND
  i_items(i_itemcount) = t$
  i_itemcount = i_itemcount+1
  REDIM PRESERVE i_items(i_itemcount-1)

  'adjust scroll position
  IF i_vscrollpos >= i_itemcount THEN i_vscrollpos = i_itemcount-1
END METHOD


'Renders the edit control (will be called automatically from the OnRender event of the CDIRECT2D class)
METHOD OnRenderEdit(RenderTarget AS ID2D1HwndRenderTarget, mx&, my&)
  LOCAL i&, n&, wd&, hg&, lineheight!, maxlines&, t$
  LOCAL fnt AS IDWriteTextFormat
  LOCAL brush AS ID2D1SolidColorBrush

  'draw border and background
  ME.D2D.GraphicBox(ME.XPos, ME.YPos, ME.XPos+ME.Width, ME.YPos+ME.Height, i_bordercolor, i_backgroundcolor)

  'draw caption
  fnt = ME.D2D.Fonts(i_font)
  brush = ME.D2D.Brushes(i_textcolor)
  IF i_caption <> "" THEN
    ME.D2D.GraphicTextSize(i_caption, i_font, wd&, hg&)
    ME.D2D.GraphicPrint(i_caption, ME.XPos-wd&-5, ME.YPos+2, i_bordercolor, i_font)
  END IF

  ME.D2D.CreateClippingRegion(ME.XPos+1, ME.YPos+1, ME.XPos+ME.Width-2, ME.YPos+ME.Height-2)
  IF i_multiline = 0 THEN
    'draw text (single-line)
    IF i_value <> "" THEN
      ME.D2D.GraphicTextSize(i_value, i_font, wd&, hg&)
      ME.D2D.GraphicPrint(i_value, ME.XPos+2, ME.YPos+2, i_textcolor, i_font)
    END IF

    'draw cursor
    IF ME.HasFocus <> 0 AND (INT(TIMER*2) AND 1) = 0 THEN
      ME.D2D.GraphicTextSize(LEFT$(i_value, i_cursorpos), i_font, wd&, hg&)
      ME.D2D.GraphicLine(ME.XPos+wd&+2, ME.YPos+2, ME.XPos+wd&+2, ME.YPos+ME.Height-4, i_textcolor)
    END IF
  ELSE
    'draw text (multi-line)
    lineheight! = ME.D2D.FontSize(i_font)*1.6
    maxlines& = ME.Height/lineheight!
    FOR i& = i_vscrollpos TO i_itemcount-1
      ME.D2D.GraphicPrint(RTRIM$(i_items(i&), CHR$(13)), ME.XPos+2, ME.YPos+2+(i&-i_vscrollpos)*lineheight!, i_textcolor, i_font)
    NEXT i&

    'draw cursor
    IF ME.HasFocus <> 0 AND (INT(TIMER*2) AND 1) = 0 THEN
      wd& = 0
      i& = 0
      n& = 0
      WHILE i& < i_itemcount AND n& < i_cursorpos
        IF n&+LEN(RTRIM$(i_items(i&), CHR$(13))) >= i_cursorpos THEN
          ME.D2D.GraphicTextSize(LEFT$(i_items(i&), i_cursorpos-n&), i_font, wd&, hg&)
          EXIT LOOP
        END IF
        n& = n&+LEN(i_items(i&))
        i& = i&+1
      WEND
      ME.D2D.GraphicLine(ME.XPos+wd&+2, ME.YPos+2+(i&-i_vscrollpos)*lineheight!, ME.XPos+wd&+2, ME.YPos+(i&-i_vscrollpos)*lineheight!+lineheight!, i_textcolor)
      'scroll text to move cursor to visible area
      IF 2+(i&-i_vscrollpos)*lineheight! < 0 THEN i_vscrollpos = i_vscrollpos-1
      IF (i&-i_vscrollpos)*lineheight!+lineheight! >= ME.Height THEN i_vscrollpos = i_vscrollpos+1
    END IF
  END IF
  ME.D2D.ReleaseClippingRegion
END METHOD


'Processes key press events for the edit control (will be called automatically from the OnKeyPress event of the CDIRECT2D class)
METHOD OnKeyPressEdit(k$)
  LOCAL colnr&, rownr&, i&, a$

  SELECT CASE k$
  CASE CHR$(3):  'CTRL+C
    IF LEN(i_value) > 0 THEN
      a$ = i_value
      REPLACE CHR$(13) WITH CHR$(13,10) IN a$
      CLIPBOARD SET TEXT a$
    END IF
  CASE CHR$(22):  'CTRL+V
    CLIPBOARD GET TEXT TO a$
    REPLACE CHR$(13,10) WITH CHR$(13) IN a$
    REPLACE CHR$(10) WITH CHR$(13) IN a$
    IF a$ <> "" AND LEN(i_value)+LEN(a$) <= i_maxlength THEN
      i_value = LEFT$(i_value, i_cursorpos)+a$+MID$(i_value, i_cursorpos+1)
      i_cursorpos = i_cursorpos+LEN(a$)
    END IF
  CASE CHR$(8):  'Backspace
    IF LEN(i_value) > 0 AND i_cursorpos > 0 THEN
      i_value = LEFT$(i_value, i_cursorpos-1)+MID$(i_value, i_cursorpos+1)
      i_cursorpos = i_cursorpos-1
    END IF
  CASE CHR$(13):  'Enter
    IF i_multiline > 0 AND LEN(i_value) < i_maxlength THEN
      i_value = LEFT$(i_value, i_cursorpos)+k$+MID$(i_value, i_cursorpos+1)
      i_cursorpos = i_cursorpos+1
    END IF
  CASE CHR$(0, 83):  'Delete
    IF LEN(i_value) > 0 AND i_cursorpos < LEN(i_value) THEN
      i_value = LEFT$(i_value, i_cursorpos)+MID$(i_value, i_cursorpos+2)
    END IF
  CASE CHR$(0, 75):  'Left
    IF i_cursorpos > 0 THEN i_cursorpos = i_cursorpos-1
  CASE CHR$(0, 77):  'Right
    IF i_cursorpos < LEN(i_value) THEN i_cursorpos = i_cursorpos+1
  CASE CHR$(0, 72):  'Up
    IF i_multiline > 0 THEN
      ME.EditGetRowCol(i_cursorpos, colnr&, rownr&)
      IF rownr& > 0 THEN
        i_cursorpos = MIN&(colnr&, LEN(i_items(rownr&-1)))
        FOR i& = 0 TO rownr&-2
          i_cursorpos = i_cursorpos+LEN(i_items(i&))
        NEXT i&
      END IF
    END IF
  CASE CHR$(0, 80):  'Down
    IF i_multiline > 0 THEN
      ME.EditGetRowCol(i_cursorpos, colnr&, rownr&)
      IF rownr& < i_itemcount-1 THEN
        i_cursorpos = MIN&(colnr&, LEN(i_items(rownr&+1)))
        FOR i& = 0 TO rownr&
          i_cursorpos = i_cursorpos+LEN(i_items(i&))
        NEXT i&
      END IF
    END IF
  CASE CHR$(0, 71):  'Home
    IF i_multiline = 0 THEN
      i_cursorpos = 0
    ELSE
      ME.EditGetRowCol(i_cursorpos, colnr&, rownr&)
      i_cursorpos = i_cursorpos-colnr&
    END IF
  CASE CHR$(0, 79):  'End
    IF i_multiline = 0 THEN
      i_cursorpos = LEN(i_value)
    ELSE
      ME.EditGetRowCol(i_cursorpos, colnr&, rownr&)
      i_cursorpos = i_cursorpos-colnr&+LEN(RTRIM$(i_items(rownr&), CHR$(13)))
    END IF
  CASE ELSE
    IF LEN(k$) = 1 AND ASC(k$) > 31 AND LEN(i_value) < i_maxlength THEN
      i_value = LEFT$(i_value, i_cursorpos)+IIF$(ME.EditUppercase = 1, UCASE$(k$), k$)+MID$(i_value, i_cursorpos+1)
      i_cursorpos = i_cursorpos+1
    END IF
  END SELECT

  IF i_multiline > 0 THEN ME.EditSetTextLines
END METHOD


'Processes mouse click events
METHOD OnClickEdit(mx&, my&, btn&)
  LOCAL x&, i&, wd&, hg&, rownr&, lineheight!, t$

  IF btn& <> 2 THEN EXIT METHOD

  'set cursor position
  i_cursorpos = 0
  IF LEN(i_value) = 0 THEN EXIT METHOD
  'in multiline edit find correct row
  IF i_multiline > 0 THEN
    lineheight! = ME.D2D.FontSize(i_font)*1.6
    rownr& = INT((my&-ME.YPos)/lineheight!)
    IF rownr& >= i_itemcount THEN rownr& = i_itemcount-1
    IF rownr& < 0 THEN rownr& = 0
    t$ = i_items(rownr&)
    FOR i& = 0 TO rownr&-1
      i_cursorpos = i_cursorpos+LEN(i_items(i&))
    NEXT i&
  ELSE
    t$ = i_value
  END IF
  'find character position
  x& = ME.XPos
  FOR i& = LEN(t$) TO 1 STEP -1
    ME.D2D.GraphicTextSize(LEFT$(t$, i&), i_font, wd&, hg&)
    IF x&+wd& <= mx& THEN EXIT FOR
  NEXT i&
  i_cursorpos = i_cursorpos+i&
END METHOD


'Gets row and column for a position in a multi-line edit control
METHOD EditGetRowCol(BYVAL textpos&, BYREF colnr&, BYREF rownr&)
  LOCAL i&, n&

  'check if text is empty
  IF LEN(i_value) = 0 OR textpos& = 0 THEN
    colnr& = 0
    rownr& = 0
    EXIT METHOD
  END IF

  'find position in text
  i& = 0
  n& = 0
  WHILE i& < i_itemcount
    IF n&+LEN(RTRIM$(i_items(i&), CHR$(13))) >= textpos& THEN
      rownr& = i&
      colnr& = textpos&-n&
      EXIT METHOD
    END IF
    n& = n&+LEN(i_items(i&))
    i& = i&+1
  WEND

  'position is after the end of the text
  rownr& = i_itemcount-1
  colnr& = LEN(i_items(rownr&))
END METHOD



'==================================================================================================
'Listbox methods
'==================================================================================================

'Initialize the control
METHOD InitListbox(pd2d AS IDIRECT2D, x&, y&, wd&, hg&, items$, fnt&, textcolor&, bordercolor&, buttoncolor&, highlightcolor&, itemChangedFunction AS DWORD, vis&)
  i_controltype = %CTYPE_LISTBOX
  i_callback = itemChangedFunction
  ME.D2D = pd2d
  ME.XPos = x&
  ME.YPos = y&
  ME.Width = wd&
  ME.Height = hg&
  i_font = fnt&
  i_textcolor = textcolor&
  i_bordercolor = bordercolor&
  i_backgroundcolor = buttoncolor&
  i_highlightcolor = highlightcolor&
  i_comboboxopen = 0
  ME.Visible = vis&
  ME.Enabled = 1

  ME.SetItems(items$)
  ME.D2D.AddControl(ME)
END METHOD


'Set listbox items
METHOD SetItems(items$)
  LOCAL i&

  i_itemcount = PARSECOUNT(items$, ";")
  REDIM i_items(i_itemcount-1)
  FOR i& = 0 TO i_itemcount-1
    i_items(i&) = PARSE$(items$, ";", i&+1)
  NEXT i&

  IF i_selecteditem >= i_itemcount THEN i_selecteditem = 0
END METHOD


'Renders the listbox control (will be called automatically from the OnRender event of the CDIRECT2D class)
METHOD OnRenderListbox(RenderTarget AS ID2D1HwndRenderTarget, mx&, my&)
  LOCAL x&, y&, wd&, hg&, textwidth&, textheight&, cl&, t$

  x& = ME.XPos
  y& = ME.YPos
  wd& = ME.Width
  hg& = ME.Height

  'draw left triangle button
  cl& = i_backgroundcolor
  IF mx& >= x& AND mx& <= x&+hg&/2 AND my& >= y& AND my& <= y&+hg& THEN cl& = i_highlightcolor
  ME.D2D.DrawTriangle(x&+0.5, y&+hg&/2+0.5, x&+hg&/2+0.5, y&+0.5, x&+hg&/2+0.5, y&+hg&+0.5, i_bordercolor, cl&)

  'draw right triangle button
  cl& = i_backgroundcolor
  IF mx& >= x&+wd&-hg&/2 AND mx& <= x&+wd& AND my& >= y& AND my& <= y&+hg& THEN cl& = i_highlightcolor
  ME.D2D.DrawTriangle(x&+wd&+0.5, y&+hg&/2+0.5, x&+wd&-hg&/2+0.5, y&+0.5, x&+wd&-hg&/2+0.5, y&+hg&+0.5, i_bordercolor, cl&)

  'draw text frame
  ME.D2D.GraphicBox(x&+hg&/2+3, y&, x&+wd&-hg&/2-3, y&+hg&, i_bordercolor, -1)

  'draw text
  t$ = TRIM$(i_items(i_selecteditem))
  ME.D2D.GraphicTextSize(t$, i_font, textwidth&, textheight&)
  ME.D2D.GraphicPrint(t$, x&+(wd&-textwidth&)/2, y&+(hg-textheight&)/2, i_textcolor, i_font)
END METHOD


'Renders the dropped-down listbox control (will be called automatically from the OnRender event of the CDIRECT2D class)
METHOD OnRenderListboxOpen(RenderTarget AS ID2D1HwndRenderTarget, mx&, my&)
  LOCAL x&, y&, wd&, hg&, maxitems&, textwidth&, textheight&, i&, startpos&, cl&, t$

  x& = ME.XPos
  y& = ME.YPos
  wd& = ME.Width
  hg& = ME.Height
  maxitems& = MIN&(%LISTBOX_MAXOPENSIZE, i_itemcount)

  'draw items frame and background
  ME.D2D.GraphicBox(x&+hg&/2+3, y&+hg&, x&+wd&-hg&/2-3, y&+hg&+hg&*maxitems&, i_bordercolor, i_backgroundcolor)
  IF mx& > x&+hg&/2+3 AND mx& < x&+wd&-hg&/2-3 AND my& > y&+hg& AND my& < y&+hg&+hg&*maxitems& THEN
    i& = INT((my&-y&-hg&-1)/hg&)
    ME.D2D.GraphicBox(x&+hg&/2+4, y&+hg&+i&*hg&+1, x&+wd&-hg&/2-4, y&+hg&*2+i&*hg&-1, i_highlightcolor, i_highlightcolor)
  END IF

  'draw items
  startpos& = i_selecteditem
  IF startpos&+maxitems& > i_itemcount THEN startpos& = MAX&(0, i_itemcount-%LISTBOX_MAXOPENSIZE)
  FOR i& = 0 TO %LISTBOX_MAXOPENSIZE-1
    IF startpos&+i& >= i_itemcount THEN EXIT FOR
    t$ = TRIM$(i_items(startpos&+i&))
    ME.D2D.GraphicTextSize(t$, i_font, textwidth&, textheight&)
    ME.D2D.GraphicPrint(t$, x&+(wd&-textwidth&)/2, y&+(hg-textheight&)/2+hg&+hg&*i&, i_textcolor, i_font)
  NEXT i&
END METHOD


'Processes mouse click events
METHOD OnClickListbox(mx&, my&, btn&)
  LOCAL x&, y&, wd&, hg&, stp&, maxitems&, startpos&

  IF btn& <> 2 THEN EXIT METHOD
  x& = ME.XPos
  y& = ME.YPos
  wd& = ME.Width
  hg& = ME.Height

  'check click on open list
  IF my& > y&+hg& THEN
    maxitems& = MIN&(%LISTBOX_MAXOPENSIZE, i_itemcount)
    startpos& = i_selecteditem
    IF startpos&+maxitems& > i_itemcount THEN startpos& = MAX&(0, i_itemcount-%LISTBOX_MAXOPENSIZE)
    i_selecteditem = startpos&+INT((my&-y&-hg&-1)/hg&)
    i_comboboxopen = 0

    'trigger item-changed event
    IF i_callback <> 0 THEN CALL DWORD i_callback USING ItemChangedPrototype(i_id)
    EXIT METHOD
  END IF

  'check click on text
  IF mx& > x&+hg&/2 AND mx& < x&+wd&-hg&/2 THEN
    IF i_comboboxopen = 0 THEN
    ME.D2D.CloseAllListboxes
      i_comboboxopen = -1
    ELSE
      i_comboboxopen = 0
    END IF
    EXIT METHOD
  END IF

  'check click on left triangle button
  stp& = IIF&(i_comboboxopen = 0, 1, %LISTBOX_MAXOPENSIZE)
  IF mx& < x&+hg&/2 THEN
    i_selecteditem = i_selecteditem-stp&
    IF i_selecteditem < 0 THEN i_selecteditem = MAX&(0, i_itemcount-stp&)
  END IF

  'check click on right triangle button
  IF mx& > x&+wd-hg&/2 THEN
    i_selecteditem = i_selecteditem+stp&
    IF i_selecteditem >= i_itemcount THEN i_selecteditem = 0
  END IF

  'trigger item-changed event
  IF i_callback <> 0 THEN CALL DWORD i_callback USING ItemChangedPrototype(i_id)
END METHOD


'==================================================================================================
'Button methods
'==================================================================================================


'Initialize the control
METHOD InitButton(pd2d AS IDIRECT2D, cpt$, x&, y&, wd&, hg&, fnt&, textcolor&, bordercolor&, buttoncolor&, highlightcolor&, BYVAL buttonPressedFunction AS DWORD, vis&)
  LOCAL i&

  i_controltype = %CTYPE_BUTTON
  i_callback = buttonPressedFunction
  ME.D2D = pd2d
  ME.XPos = x&
  ME.YPos = y&
  ME.Width = wd&
  ME.Height = hg&
  i_font = fnt&
  i_textcolor = textcolor&
  i_bordercolor = bordercolor&
  i_backgroundcolor = buttoncolor&
  i_highlightcolor = highlightcolor&
  i_buttondown = 0
  ME.Visible = vis&
  ME.Enabled = 1
  i_caption = cpt$

  ME.D2D.AddControl(ME)
END METHOD


'Renders the button control (will be called automatically from the OnRender event of the CDIRECT2D class)
METHOD OnRenderButton(RenderTarget AS ID2D1HwndRenderTarget, mx&, my&)
  LOCAL x&, y&, wd&, hg&, textwidth&, textheight&, cl&, t$

  x& = ME.XPos
  y& = ME.YPos
  wd& = ME.Width
  hg& = ME.Height

  'draw border and background
  cl& = i_backgroundcolor
  IF mx& >= x& AND mx& <= x&+wd& AND my& >= y& AND my& <= y&+hg& AND ME.Enabled <> 0 THEN cl& = i_highlightcolor
  ME.D2D.GraphicBox(x&, y&, x&+wd&, y&+hg&, i_bordercolor, cl&)

  'draw caption
  ME.D2D.GraphicTextSize(i_caption, i_font, textwidth&, textheight&)
  ME.D2D.GraphicPrint(i_caption, x&+wd&/2-textwidth&/2, y&+hg&/2-textheight&/2, i_textcolor, i_font)
END METHOD


'Processes mouse click events
METHOD OnClickButton(mx&, my&, btn&)
  LOCAL x&, y&, wd&, hg&

  IF i_enabled = 0 THEN EXIT METHOD
  IF btn& = 1 THEN i_buttondown = 1
  IF btn& = 2 THEN i_buttondown = 0
  IF btn& <> 2 THEN EXIT METHOD
  IF i_callback = 0 THEN EXIT METHOD
  CALL DWORD i_callback USING ButtonPressedPrototype(i_id)
END METHOD


'==================================================================================================
'Bitmap-Button methods
'==================================================================================================


'Initialize the control
'bitmapindex = Y-position into the texture (the 3 bitmaps for each button are all in the same row: normal bitmap at X-position 0, down bitmap at iconwidth, disabled bitmap at iconwidth*2)
METHOD InitBitmapButton(pd2d AS IDIRECT2D, cpt$, x&, y&, wd&, hg&, fnt&, textcolor&, bordercolor&, shadowcolor&, buttoncolor&, highlightcolor&, bitmapindex&, iconwidth&, iconheight&, BYVAL buttonPressedFunction AS DWORD, vis&)
  LOCAL i&

  i_controltype = %CTYPE_BITMAPBUTTON
  i_callback = buttonPressedFunction
  ME.D2D = pd2d
  ME.XPos = x&
  ME.YPos = y&
  ME.Width = wd&
  ME.Height = hg&
  i_font = fnt&
  i_textcolor = textcolor&
  i_bordercolor = bordercolor&
  i_shadowcolor = shadowcolor&
  i_backgroundcolor = buttoncolor&
  i_highlightcolor = highlightcolor&
  i_bitmapindex = bitmapindex&  'Y-position in the bitmap-button texture
  i_iconwidth = IIF&(iconwidth& = 0, wd&-2, iconwidth&)
  i_iconheight = IIF&(iconheight& = 0, hg&-2, iconheight&)
  i_buttondown = 0
  ME.Visible = vis&
  ME.Enabled = 1
  i_caption = cpt$

  ME.D2D.AddControl(ME)
END METHOD


'Renders the button control (will be called automatically from the OnRender event of the CDIRECT2D class)
METHOD OnRenderBitmapButton(RenderTarget AS ID2D1HwndRenderTarget, mx&, my&)
  LOCAL x&, y&, wd&, hg&, srcx&, textwidth&, textheight&, cl&, t$

  x& = ME.XPos
  y& = ME.YPos
  wd& = ME.Width
  hg& = ME.Height

  'draw border and background
  cl& = i_backgroundcolor
  IF mx& >= x& AND mx& <= x&+wd& AND my& >= y& AND my& <= y&+hg& AND i_enabled = 1 THEN cl& = i_highlightcolor
  ME.D2D.GraphicBox(x&, y&, x&+wd&, y&+hg&, i_bordercolor, cl&)
  IF i_buttondown = 1 THEN
    ME.D2D.GraphicLine(x&+1, y&+1, x&+wd&-2, y&+1, i_shadowcolor)
    ME.D2D.GraphicLine(x&+1, y&+1, x&+1, y&+hg&-2, i_shadowcolor)
  END IF

  'draw caption
  ME.D2D.GraphicTextSize(i_caption, i_font, textwidth&, textheight&)
  ME.D2D.GraphicPrint(i_caption, x&+wd&/2-textwidth&/2+i_buttondown, y&+hg&/2-textheight&/2+i_buttondown, i_textcolor, i_font)

  'draw bitmap (there must be 3 bitmaps for the button, all in the same row in the texture)
  srcx& = 0
  IF i_buttondown = 1 THEN srcx& = i_iconwidth
  IF i_enabled = 0 THEN srcx& = i_iconwidth*2
  ME.D2D.GraphicStretch(ME.D2D.BitmapButtonTexture, srcx&, i_bitmapindex, srcx&+i_iconwidth, i_bitmapindex+i_iconheight, x&+2+i_buttondown, y&+2+i_buttondown, x&+wd&+i_buttondown, y&+hg&+i_buttondown)
END METHOD


'Processes mouse click events
METHOD OnClickBitmapButton(mx&, my&, btn&)
  LOCAL x&, y&, wd&, hg&

  IF i_enabled = 0 THEN EXIT METHOD
  IF btn& = 1 THEN i_buttondown = 1
  IF btn& = 2 THEN i_buttondown = 0
  IF btn& <> 2 THEN EXIT METHOD
  IF i_callback = 0 THEN EXIT METHOD
  CALL DWORD i_callback USING ButtonPressedPrototype(i_id)
END METHOD


'==================================================================================================
'Radiogroup methods
'==================================================================================================


'Initialize the control
METHOD InitRadiogroup(pd2d AS IDIRECT2D, x&, y&, wd&, hg&, items$, fnt&, textcolor&, bordercolor&, itemChangedFunction AS DWORD, vis&)
  i_controltype = %CTYPE_RADIOGROUP
  i_callback = itemChangedFunction
  ME.D2D = pd2d
  ME.XPos = x&
  ME.YPos = y&
  ME.Width = wd&
  ME.Height = hg&
  i_font = fnt&
  i_textcolor = textcolor&
  i_bordercolor = bordercolor&
  ME.Visible = vis&
  ME.Enabled = 1

  ME.SetItems(items$)
  ME.D2D.AddControl(ME)
END METHOD


'Renders the radiogroup control (will be called automatically from the OnRender event of the CDIRECT2D class)
METHOD OnRenderRadiogroup(RenderTarget AS ID2D1HwndRenderTarget, mx&, my&)
  LOCAL i&, x&, y&, hg&, diameter&, rowheight&

  IF i_itemcount = 0 THEN EXIT METHOD
  x& = ME.XPos
  hg& = ME.Height
  rowheight& = hg&/i_itemcount
  diameter& = ME.D2D.FontSize(i_font)*1.6

  'draw buttons
  y& = ME.YPos+(rowheight&-diameter&)/2
  FOR i& = 0 TO i_itemcount-1
    ME.D2D.GraphicEllipse(x&, y&, x&+diameter&, y&+diameter&, i_bordercolor, -1)
    IF i& = i_selecteditem THEN ME.D2D.GraphicEllipse(x&+2, y&+2, x&+diameter&-2, y&+diameter&-2, i_bordercolor, i_bordercolor)
    y& = y&+rowheight&
  NEXT i&

  'draw text
  y& = ME.YPos+(rowheight&-diameter&)/2
  FOR i& = 0 TO i_itemcount-1
    ME.D2D.GraphicPrint(i_items(i&), x&+diameter&+5, y&, i_textcolor, i_font)
    y& = y&+rowheight&
  NEXT i&
END METHOD


'Processes mouse click events
METHOD OnClickRadiogroup(mx&, my&, btn&)
  LOCAL rowheight&

  IF btn& <> 2 OR i_itemcount = 0 THEN EXIT METHOD

  rowheight& = ME.Height/i_itemcount
  i_selecteditem = MIN&(i_itemcount-1, MAX&(0, INT((my&-ME.YPos)/rowheight&)))

  IF i_callback <> 0 THEN CALL DWORD i_callback USING ItemChangedPrototype(i_id)
END METHOD


'==================================================================================================
'Checkbox methods
'==================================================================================================


'Initialize the control
METHOD InitCheckbox(pd2d AS IDIRECT2D, cpt$, x&, y&, wd&, hg&, fnt&, textcolor&, bordercolor&, BYVAL itemChangedFunction AS DWORD, vis&)
  LOCAL i&

  i_controltype = %CTYPE_CHECKBOX
  i_callback = itemChangedFunction
  ME.D2D = pd2d
  ME.XPos = x&
  ME.YPos = y&
  ME.Width = wd&
  ME.Height = hg&
  i_font = fnt&
  i_textcolor = textcolor&
  i_bordercolor = bordercolor&
  ME.Visible = vis&
  ME.Enabled = 1
  i_caption = cpt$

  ME.D2D.AddControl(ME)
END METHOD


'Renders the checkbox control (will be called automatically from the OnRender event of the CDIRECT2D class)
METHOD OnRenderCheckbox(RenderTarget AS ID2D1HwndRenderTarget, mx&, my&)
  LOCAL x&, y&, hg&, textwidth&, textheight&

  x& = ME.XPos
  y& = ME.YPos
  hg& = ME.Height

  'draw box
  ME.D2D.GraphicBox(x&+2, y&+2, x&+hg&-2, y&+hg&-2, i_bordercolor, -1)
  IF i_selecteditem <> 0 THEN ME.D2D.GraphicBox(x&+4, y&+4, x&+hg&-3, y&+hg&-3, i_bordercolor, i_bordercolor)

  'draw text
  ME.D2D.GraphicTextSize(i_caption, i_font, textwidth&, textheight&)
  ME.D2D.GraphicPrint(i_caption, x&+hg&+5, y&+(hg&-textheight&)/2, i_textcolor, i_font)
END METHOD


'Processes mouse click events
METHOD OnClickCheckbox(mx&, my&, btn&)
  LOCAL rowheight&

  IF btn& <> 2 THEN EXIT METHOD

  i_selecteditem = NOT i_selecteditem

  IF i_callback <> 0 THEN CALL DWORD i_callback USING ItemChangedPrototype(i_id)
END METHOD


'==================================================================================================
'Progressbar methods
'==================================================================================================


'Initialize the control
METHOD InitProgressbar(pd2d AS IDIRECT2D, maxprogress&, cpt$, x&, y&, wd&, hg&, fnt&, textcolor&, bordercolor&, backgroundcolor&, barcolor&, vis&)
  i_controltype = %CTYPE_PROGRESSBAR
  ME.D2D = pd2d
  ME.XPos = x&
  ME.YPos = y&
  ME.Width = wd&
  ME.Height = hg&
  i_maxprogress = MAX&(1, maxprogress&)
  i_curprogress = 0
  i_font = fnt&
  i_textcolor = textcolor&
  i_bordercolor = bordercolor&
  i_backgroundcolor = backgroundcolor&
  i_highlightcolor = barcolor&
  ME.Visible = vis&
  i_caption = cpt$

  ME.D2D.AddControl(ME)
END METHOD


'Renders the progressbar control (will be called automatically from the OnRender event of the CDIRECT2D class)
METHOD OnRenderProgressbar(RenderTarget AS ID2D1HwndRenderTarget, mx&, my&)
  LOCAL x&, y&, wd&, hg&, textwidth&, textheight&, barlen&, a$

  x& = ME.XPos
  y& = ME.YPos
  wd& = ME.Width
  hg& = ME.Height

  'draw border and background
  ME.D2D.GraphicBox(x&, y&, x&+wd&, y&+hg&, i_bordercolor, i_backgroundcolor)

  'draw bar
  barlen& = i_curprogress/i_maxprogress*(wd&-2)
  IF i_curprogress > 0 THEN
    ME.D2D.GraphicBox(x&+1, y&+1, x&+barlen&+1, y&+hg&-1, i_highlightcolor, i_highlightcolor)
  END IF

  'draw text
  a$ = i_caption
  REPLACE "%" WITH FORMAT$(INT(100*i_curprogress/i_maxprogress))+"%" IN a$
  IF a$ <> "" THEN
    ME.D2D.GraphicTextSize(a$, i_font, textwidth&, textheight&)
    ME.D2D.GraphicPrint(a$, x&+(wd&-textwidth&)/2, y&+(hg&-textheight&)/2, i_textcolor, i_font)
  END IF
END METHOD


'==================================================================================================
'Scrollbar methods
'==================================================================================================


'Initialize the control
METHOD InitScrollbar(pd2d AS IDIRECT2D, maxposition&, visiblerows&, x&, y&, wd&, hg&, bordercolor&, backgroundcolor&, barcolor&, vis&)
  i_controltype = %CTYPE_SCROLLBAR
  ME.D2D = pd2d
  ME.XPos = x&
  ME.YPos = y&
  ME.Width = wd&
  ME.Height = hg&
  i_maxscroll = MAX&(0, maxposition&)
  i_visiblerows = MAX&(1, visiblerows&)
  i_scrollposition = 0
  i_bordercolor = bordercolor&
  i_backgroundcolor = backgroundcolor&
  i_highlightcolor = barcolor&
  ME.Visible = vis&

  ME.D2D.AddControl(ME)
END METHOD


'Renders the scrollbar control (will be called automatically from the OnRender event of the CDIRECT2D class)
METHOD OnRenderScrollbar(RenderTarget AS ID2D1HwndRenderTarget, mx&, my&)
  LOCAL x&, y&, wd&, hg&, p&, q&, maxpos&, sz&, displayrows&, isvertical&, a$

  x& = ME.XPos
  y& = ME.YPos
  wd& = ME.Width
  hg& = ME.Height
  maxpos& = ME.MaxScroll
  displayrows& = ME.VisibleRows
  p& = ME.ScrollPosition
  sz& = MAX&(16, (hg&-32)*displayrows&/(displayrows&+maxpos&))
  isvertical& = (hg& > wd&)

  'draw border and background
  ME.D2D.GraphicBox(x&, y&, x&+wd&, y&+hg&, i_bordercolor, i_backgroundcolor)

  'draw buttons
  IF isvertical& THEN
    ME.D2D.DrawTriangle(x&+wd&/2+0.5, y&+0.5, x&+wd&+0.5, y&+15.5, x&+0.5, y&+15.5, i_highlightcolor, i_highlightcolor)
    ME.D2D.DrawTriangle(x&+wd&/2+0.5, y&+hg&+0.5, x&+wd&+0.5, y&+hg&-14.5, x&+0.5, y&+hg&-14.5, i_highlightcolor, i_highlightcolor)
  ELSE
    ME.D2D.DrawTriangle(x&+0.5, y&+hg&/2+0.5, x&+15.5, y&+0.5, x&+15.5, y&+hg&+0.5, i_highlightcolor, i_highlightcolor)
    ME.D2D.DrawTriangle(x&+wd&+0.5, y&+hg&/2+0.5, x&+wd&-14.5, y&+0.5, x&+wd&-14.5, y&+hg&+0.5, i_highlightcolor, i_highlightcolor)
  END IF

  'show current position
  IF isvertical& THEN
    q& = y&+16+p&*(hg&-32)/(displayrows&+maxpos&)
    ME.D2D.GraphicBox(x&+1, q&, x&+wd&, q&+sz&, i_highlightcolor, i_highlightcolor)
  ELSE
    q& = x&+16+p&*(wd&-32)/(displayrows&+maxpos&)
    ME.D2D.GraphicBox(q&, y&+1, q&+sz&, y&+hg&, i_highlightcolor, i_highlightcolor)
  END IF
END METHOD


'Processes mouse click events
METHOD OnClickScrollbar(mx&, my&, btn&)
  LOCAL x&, y&, wd&, hg&, p&, q&, maxpos&, sz&, displayrows&, isvertical&

  x& = ME.XPos
  y& = ME.YPos
  wd& = ME.Width
  hg& = ME.Height
  maxpos& = ME.MaxScroll
  displayrows& = ME.VisibleRows
  p& = ME.ScrollPosition
  sz& = MAX&(16, (hg&-32)*displayrows&/(displayrows&+maxpos&))
  isvertical& = (hg& > wd&)

  IF btn& = 2 THEN
    IF isvertical& THEN
      'check "Up" button
      IF my& < y&+16 AND p& > 0 THEN
        ME.ScrollPosition = ME.ScrollPosition-1
        EXIT METHOD
      END IF

      'check "Down" button
      IF my& > y&+hg&-16 AND p& < maxpos& THEN
        ME.ScrollPosition = ME.ScrollPosition+1
        EXIT METHOD
      END IF

      'check if sliding area was clicked
      q& = y&+16+p&*(hg&-32)/(displayrows&+maxpos&)
      IF my& < q& THEN
        ME.ScrollPosition = (my&-y&-16)/(hg&-32)*(displayrows&+maxpos&)
        EXIT METHOD
      END IF
      IF my& > q&+sz& THEN
        ME.ScrollPosition = ME.ScrollPosition+(my&-q&-sz&)/(hg&-32)*(displayrows&+maxpos&)
        EXIT METHOD
      END IF
    ELSE
      'check "Left" button
      IF mx& < x&+16 AND p& > 0 THEN
        ME.ScrollPosition = ME.ScrollPosition-1
        EXIT METHOD
      END IF

      'check "Right" button
      IF mx& > x&+wd&-16 AND p& < maxpos& THEN
        ME.ScrollPosition = ME.ScrollPosition+1
        EXIT METHOD
      END IF
    END IF
  END IF
END METHOD

END INTERFACE

END CLASS
