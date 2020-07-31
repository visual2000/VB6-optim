Attribute VB_Name = "GfxPrimitives"
Option Explicit

Public Declare Function GetPixel Lib "gdi32" (ByVal hdc As Long, ByVal x As Long, _
    ByVal y As Long) As Long
Public Declare Function SetPixel Lib "gdi32" (ByVal hdc As Long, ByVal x As Long, _
    ByVal y As Long, ByVal crColor As Long) As Long

Public Sub SetPx(x As Integer, y As Integer, rgbVec As TVec3)
    ' used to set a pixel on our backbuffer.
    Dim red As Integer
    Dim green As Integer
    Dim blue As Integer
    
    ' TODO decide if we want 2x Gamma correction by adding sqrt() around our rgbVec values?
    red = rgbVec.x * 255
    green = rgbVec.y * 255
    blue = rgbVec.z * 255
    
    Dim compositeColour As Long
    compositeColour = RGB(red, green, blue)
    
    Call SetPixel(GfxInit.myBackBuffer, x, GfxInit.HEIGHT - y, compositeColour)
End Sub
