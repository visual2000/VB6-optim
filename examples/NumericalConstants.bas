Attribute VB_Name = "NumericalConstants"
Option Explicit

' Stolen from https://stackoverflow.com/questions/885994/how-do-you-get-vb6-to-initialize-doubles-with-infinity-infinity-and-nan

Public Enum abIEEE754SpecialValues
    abInfinityPos
    abInfinityNeg
    abNaNQuiet
    abNaNSignalling
    abDoubleMax
    abDoubleMin
End Enum

Private Type TypedDouble
    value As Double
End Type

Private Type ByteDouble
    value(7) As Byte
End Type

Public Sub Example()
    MsgBox GetIEEE754SpecialValue(abDoubleMax)
End Sub

Public Function GetIEEE754SpecialValue(ByVal value As abIEEE754SpecialValues) As Double
    Dim dblRtnVal As Double
    Select Case value
    Case abIEEE754SpecialValues.abInfinityPos
        dblRtnVal = BuildDouble(byt6:=240, byt7:=127)
    Case abIEEE754SpecialValues.abInfinityNeg
        dblRtnVal = BuildDouble(byt6:=240, byt7:=255)
    Case abIEEE754SpecialValues.abNaNQuiet
        dblRtnVal = BuildDouble(byt6:=255, byt7:=255)
    Case abIEEE754SpecialValues.abNaNSignalling
        dblRtnVal = BuildDouble(byt6:=248, byt7:=255)
    Case abIEEE754SpecialValues.abDoubleMax
        dblRtnVal = BuildDouble(255, 255, 255, 255, 255, 255, 239, 127)
    Case abIEEE754SpecialValues.abDoubleMin
        dblRtnVal = BuildDouble(255, 255, 255, 255, 255, 255, 239, 255)
    End Select
    GetIEEE754SpecialValue = dblRtnVal
End Function

Public Function BuildDouble( _
    Optional byt0 As Byte = 0, _
    Optional byt1 As Byte = 0, _
    Optional byt2 As Byte = 0, _
    Optional byt3 As Byte = 0, _
    Optional byt4 As Byte = 0, _
    Optional byt5 As Byte = 0, _
    Optional byt6 As Byte = 0, _
    Optional byt7 As Byte = 0 _
    ) As Double
    Dim bdTmp As ByteDouble, tdRtnVal As TypedDouble
    bdTmp.value(0) = byt0
    bdTmp.value(1) = byt1
    bdTmp.value(2) = byt2
    bdTmp.value(3) = byt3
    bdTmp.value(4) = byt4
    bdTmp.value(5) = byt5
    bdTmp.value(6) = byt6
    bdTmp.value(7) = byt7
    LSet tdRtnVal = bdTmp
    BuildDouble = tdRtnVal.value
End Function

