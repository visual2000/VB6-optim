Attribute VB_Name = "Types"
Option Explicit

Public Type TVec3
    x As Double
    y As Double
    z As Double
End Type

Public Type TRay
    origin As TVec3
    direction As TVec3
End Type

Public Type TSphere
    center As TVec3
    radius As Double
End Type

Public Type THit
    t As Double
    p As TVec3
    normal As TVec3
End Type

Public Type TCamera
    lower_left_corner As TVec3
    horizontal As TVec3
    vertical As TVec3
    origin As TVec3
End Type

Public Function TCamera_init() As TCamera
    Dim c As TCamera
    c.lower_left_corner = TVec3_init(-2#, -1.5, -1#)
    c.horizontal = TVec3_init(4, 0, 0)
    c.vertical = TVec3_init(0, 3, 0)
    c.origin = TVec3_init(0, 0, 0)
    TCamera_init = c
End Function

Public Function TSphere_init(center As TVec3, radius As Double) As TSphere
    Dim s As TSphere
    s.center = center
    s.radius = radius
    TSphere_init = s
End Function

Public Function TVec3_init(x As Double, y As Double, z As Double) As TVec3
    Dim v As TVec3
    v.x = x
    v.y = y
    v.z = z
    TVec3_init = v
End Function

Public Function TRay_init(origin As TVec3, direction As TVec3) As TRay
    Dim r As TRay
    r.origin = origin
    r.direction = direction
    TRay_init = r
End Function
