Attribute VB_Name = "CameraFuncs"
Option Explicit

Public Function camera_get_ray(camera As TCamera, u As Double, v As Double) As TRay
    camera_get_ray = TRay_init(camera.origin, _
                               add(add(camera.lower_left_corner, _
                               scalarMultiply(camera.vertical, v)), _
                               scalarMultiply(camera.horizontal, u)))
End Function
