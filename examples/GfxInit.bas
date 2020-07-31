Attribute VB_Name = "GfxInit"
Option Explicit

'code timer
Public Declare Function GetTickCount Lib "kernel32" () As Long

Public Declare Function BitBlt Lib "gdi32" _
 (ByVal hDestDC As Long, _
 ByVal x As Long, _
 ByVal y As Long, _
 ByVal nWidth As Long, _
 ByVal nHeight As Long, _
 ByVal hSrcDC As Long, _
 ByVal xSrc As Long, _
 ByVal ySrc As Long, _
 ByVal dwRop As Long) As Long
 
'creating buffers / loading sprites
Private Declare Function CreateCompatibleBitmap Lib "gdi32" (ByVal hdc As Long, ByVal nWidth As Long, ByVal nHeight As Long) As Long
Private Declare Function CreateCompatibleDC Lib "gdi32" (ByVal hdc As Long) As Long
Private Declare Function GetDC Lib "user32" (ByVal hwnd As Long) As Long


'loading sprites
Private Declare Function SelectObject Lib "gdi32" (ByVal hdc As Long, ByVal hObject As Long) As Long

'cleanup
Private Declare Function DeleteObject Lib "gdi32" (ByVal hObject As Long) As Long
Private Declare Function DeleteDC Lib "gdi32" (ByVal hdc As Long) As Long

'our Buffer's DC

Public myBackBuffer As Long

Public myBufferBMP As Long

Public WIDTH As Integer
Public HEIGHT As Integer


' Initialisation - backbuffer bmp
Public Sub initialiseGraphics()

    WIDTH = 640
    HEIGHT = 480
    
    With frmView.pbView
        .Top = 0
        .Left = 0
        .WIDTH = GfxInit.WIDTH
        .HEIGHT = GfxInit.HEIGHT
    End With
    
    With frmView
        .HEIGHT = frmView.ScaleY(HEIGHT, vbPixels, vbTwips)
        .WIDTH = frmView.ScaleX(WIDTH, vbPixels, vbTwips)
    End With
    
    'cellPx = frmMain.ScaleY(LoadResPicture(101, vbResBitmap).Height, vbHimetric, vbPixels)
    'create a compatable DC for the back buffer..
    myBackBuffer = CreateCompatibleDC(GetDC(0))

    'create a compatible bitmap surface for the DC
    'that is the size of our form.
    'NOTE - the bitmap will act as the actual graphics surface inside the DC
    'because without a bitmap in the DC, the DC cannot hold graphical data..
    myBufferBMP = CreateCompatibleBitmap(GetDC(0), WIDTH, HEIGHT)
    
    'final step of making the back buffer...
    'load our created blank bitmap surface into our buffer
    '(this will be used as our canvas to draw-on off screen)
    SelectObject myBackBuffer, myBufferBMP
    
    'before we can blit to the buffer, we should fill it with black
    BitBlt myBackBuffer, 0, 0, WIDTH, HEIGHT, 0, 0, 0, vbBlackness
    
    
End Sub

Public Sub drawBufferToScreen()
    frmView.pbView.Cls
    
    BitBlt frmView.pbView.hdc, _
                            0, 0, _
                            WIDTH, HEIGHT, myBackBuffer, 0, 0, vbSrcCopy
    frmView.pbView.Refresh
    
End Sub

Public Sub unloadAll()
    'this clears up the memory we used to hold
    'the graphics and the buffers we made
    
    'Delete the bitmap surface that was in the backbuffer
    DeleteObject myBufferBMP
    
    'Delete the backbuffer HDC
    DeleteDC myBackBuffer
End Sub



