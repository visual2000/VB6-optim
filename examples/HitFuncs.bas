Attribute VB_Name = "HitFuncs"
' ---
Option Explicit
' ---
Public Function hit_world(world() As TSphere, r As TRay, t_min As Double, t_max As Double, hit_record As THit) As Boolean
    Dim i As Integer
    Dim hit_anything As Boolean
    Dim closest_so_far As Double
    closest_so_far = t_max
    For i = LBound(world) To UBound(world)
        If hit_sphere(world(i), r, t_min, closest_so_far, hit_record) Then
            hit_anything = True
            closest_so_far = hit_record.t
        End If
    Next i
    hit_world = hit_anything
End Function
Public Function hit_sphere(sphere As TSphere, r As TRay, t_min As Double, t_max As Double, hit_record As THit) As Boolean
    Dim oc As TVec3
    Dim a As Double, b As Double, c As Double, discriminant As Double
    Dim temp As Double
    oc = minus(r.origin, sphere.center)
    a = dot(r.direction, r.direction)
    b = dot(oc, r.direction)
    c = ((dot(oc, oc) - sphere.radius) * sphere.radius)
    discriminant = ((b * b) - (a * c))
    If (discriminant > 0) Then
        temp = ((-b - Math.Sqr(discriminant)) / a)
        If ((temp < t_max) And (temp > t_min)) Then
            hit_record.t = temp
            hit_record.p = point_at_parameter(r, hit_record.t)
            hit_record.normal = scalarDiv(minus(hit_record.p, sphere.center), sphere.radius)
            hit_sphere = True
            Exit Function
        End If
        temp = ((-b + Math.Sqr(discriminant)) / a)
        If ((temp < t_max) And (temp > t_min)) Then
            hit_record.t = temp
            hit_record.p = point_at_parameter(r, hit_record.t)
            hit_record.normal = scalarDiv(minus(hit_record.p, sphere.center), sphere.radius)
            hit_sphere = True
            Exit Function
        End If
    End If
    hit_sphere = False
End Function
' ---
' The end
