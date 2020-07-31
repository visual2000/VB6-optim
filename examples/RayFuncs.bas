Attribute VB_Name = "RayFuncs"
Option Explicit

Public Function point_at_parameter(r As TRay, t As Double) As TVec3
    point_at_parameter = add(r.origin, scalarMultiply(r.direction, t))
End Function

Public Function color(r As TRay, world() As TSphere) As TVec3
    Dim rec As THit
    Dim t As Double
    Dim recurse As TVec3
    Dim unit_direction As TVec3
    
    Dim target As TVec3
    
    If hit_world(world, r, 0.001, GetIEEE754SpecialValue(abInfinityPos), rec) Then
        target = add(rec.p, add(rec.normal, random_unit_in_sphere))
        recurse = color(TRay_init(rec.p, minus(target, rec.p)), world)
        color = scalarMultiply(recurse, 0.5)
    Else
        '' This is the fall-back blue sky gradient.
        unit_direction = unit_vector(r.direction)
        
        t = 0.5 * (unit_direction.y + 1#)
        
        Dim white As TVec3
        white = TVec3_init(1, 1, 1)
        
        Dim blue As TVec3
        blue = TVec3_init(0.5, 0.7, 1#)
        
        color = add(scalarMultiply(blue, t), scalarMultiply(white, 1# - t))
    End If
End Function

