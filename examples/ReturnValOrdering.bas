Attribute VB_Name = "ReturnValOrdering"
Option Explicit

Public Function add(v1 As TVec3, v2 As TVec3) As TVec3
    add = TVec3_init(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z)
    Exit Function
End Function

Public Function mul(v1 As TVec3, v2 As TVec3) As TVec3
    mul = TVec3_init(v1.x * v2.x, v1.y * v2.y, v1.z * v2.z)
    Exit Function
End Function

Public Function scalarMultiply(v As TVec3, t As Double) As TVec3
    scalarMultiply = TVec3_init(v.x * t, v.y * t, v.z * t)
    Exit Function
End Function

Public Function try_it_out() As TVec3
    Dim blue As TVec3, white As TVec3
    Dim t As Double
    t = 0.5

    white = TVec3_init(1, 1, 1)
    blue = TVec3_init(0.5, 0.3, 1#)

    try_it_out = add(scalarMultiply(blue, t), scalarMultiply(white, 1# - t))
    Exit Function
End Function
