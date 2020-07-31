Attribute VB_Name = "VecFuncts"
Option Explicit

Public Function scalarMultiply(v As TVec3, t As Double) As TVec3
  scalarMultiply = TVec3_init(v.x * t, v.y * t, v.z * t)
End Function

Public Function add(v1 As TVec3, v2 As TVec3) As TVec3
    add = TVec3_init(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z)
End Function

Public Function minus(v1 As TVec3, v2 As TVec3) As TVec3
    minus = TVec3_init(v1.x - v2.x, v1.y - v2.y, v1.z - v2.z)
End Function

Public Function dot(v1 As TVec3, v2 As TVec3) As Double
    dot = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z
End Function

Public Function length(v As TVec3) As Double
    length = Math.Sqr(v.x * v.x + v.y * v.y + v.z * v.z)
End Function

Public Function div(v1 As TVec3, v2 As TVec3) As TVec3
    div = TVec3_init(v1.x / v2.x, v1.y / v2.y, v1.z / v2.z)
End Function

Public Function mul(v1 As TVec3, v2 As TVec3) As TVec3
    mul = TVec3_init(v1.x * v2.x, v1.y * v2.y, v1.z * v2.z)
End Function

Public Function scalarDiv(v1 As TVec3, t As Double) As TVec3
    scalarDiv = TVec3_init(v1.x / t, v1.y / t, v1.z / t)
End Function

Public Function unit_vector(v As TVec3) As TVec3
    unit_vector = scalarDiv(v, length(v))
End Function

Public Function rand_vector() As TVec3
    rand_vector = TVec3_init(Rnd, Rnd, Rnd)
End Function

Public Function random_unit_in_sphere() As TVec3
    Dim p As TVec3
    Do
        p = minus(scalarMultiply(rand_vector, 2#), TVec3_init(1, 1, 1))
    Loop While dot(p, p) >= 1#
    random_unit_in_sphere = p
End Function
