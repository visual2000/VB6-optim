Attribute VB_Name = "ReturnValOrdering"
Option Explicit

Public Function TVec3_init(x As Double, y As Double, z As Double) As TVec3
    Dim v As TVec3
    v.x = x
    v.y = y
    v.z = z
    TVec3_init = v
    Exit Function
End Function

Public Function add(v1 As TVec3, v2 As TVec3) As TVec3
    add = TVec3_init(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z)
    Exit Function
End Function

Public Function try_it_out() As TVec3
    Dim blue As TVec3, white As TVec3, red As TVec3
    Dim green As TVec3

    white = TVec3_init(1, 1, 1)
    red = TVec3_init(1, 0, 0)
    green = TVec3_init(0, 1, 0)
    blue = TVec3_init(0.5, 0.3, 1#)

    try_it_out = add(add(green, blue), add(white, red))
    Exit Function
End Function
