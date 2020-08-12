Attribute VB_Name = "ReturnValOrdering"
' ---
Option Explicit
' ---
Private return_add As TVec3
Private return_mul As TVec3
Private return_scalarMultiply As TVec3
Private return_try_it_out As TVec3
Private arg_add_0(0 To 100) As TVec3
Private arg_add_1(0 To 100) As TVec3
Private arg_mul_0(0 To 100) As TVec3
Private arg_mul_1(0 To 100) As TVec3
Private arg_scalarMultiply_0(0 To 100) As TVec3
Private arg_scalarMultiply_1(0 To 100) As Double
Private dim_try_it_out_blue(0 To 100) As TVec3
Private dim_try_it_out_white(0 To 100) As TVec3
Private dim_try_it_out_t(0 To 100) As Double
Private rec_depth_add As Integer
Private rec_depth_mul As Integer
Private rec_depth_scalarMultiply As Integer
Private rec_depth_try_it_out As Integer
Public Function add() As TVec3
    rec_depth_add = (rec_depth_add + 1)
    If (rec_depth_add > 99) Then
        rec_depth_add = (rec_depth_add - 1)
        Exit Function
    End If
    return_add = TVec3_init((arg_add_0(rec_depth_add).x + arg_add_1(rec_depth_add).x), (arg_add_0(rec_depth_add).y + arg_add_1(rec_depth_add).y), (arg_add_0(rec_depth_add).z + arg_add_1(rec_depth_add).z))
    rec_depth_add = (rec_depth_add - 1)
    Exit Function
End Function
Public Function mul() As TVec3
    rec_depth_mul = (rec_depth_mul + 1)
    If (rec_depth_mul > 99) Then
        rec_depth_mul = (rec_depth_mul - 1)
        Exit Function
    End If
    return_mul = TVec3_init((arg_mul_0(rec_depth_mul).x * arg_mul_1(rec_depth_mul).x), (arg_mul_0(rec_depth_mul).y * arg_mul_1(rec_depth_mul).y), (arg_mul_0(rec_depth_mul).z * arg_mul_1(rec_depth_mul).z))
    rec_depth_mul = (rec_depth_mul - 1)
    Exit Function
End Function
Public Function scalarMultiply() As TVec3
    rec_depth_scalarMultiply = (rec_depth_scalarMultiply + 1)
    If (rec_depth_scalarMultiply > 99) Then
        rec_depth_scalarMultiply = (rec_depth_scalarMultiply - 1)
        Exit Function
    End If
    return_scalarMultiply = TVec3_init((arg_scalarMultiply_0(rec_depth_scalarMultiply).x * arg_scalarMultiply_1(rec_depth_scalarMultiply)), (arg_scalarMultiply_0(rec_depth_scalarMultiply).y * arg_scalarMultiply_1(rec_depth_scalarMultiply)), (arg_scalarMultiply_0(rec_depth_scalarMultiply).z * arg_scalarMultiply_1(rec_depth_scalarMultiply)))
    rec_depth_scalarMultiply = (rec_depth_scalarMultiply - 1)
    Exit Function
End Function
Public Function try_it_out() As TVec3
    rec_depth_try_it_out = (rec_depth_try_it_out + 1)
    If (rec_depth_try_it_out > 99) Then
        rec_depth_try_it_out = (rec_depth_try_it_out - 1)
        Exit Function
    End If
    dim_try_it_out_t(rec_depth_try_it_out) = 0.5
    dim_try_it_out_white(rec_depth_try_it_out) = TVec3_init(1, 1, 1)
    dim_try_it_out_blue(rec_depth_try_it_out) = TVec3_init(0.5, 0.3, 1.0)
    arg_scalarMultiply_0((rec_depth_scalarMultiply + 1)) = dim_try_it_out_blue(rec_depth_try_it_out)
    arg_scalarMultiply_1((rec_depth_scalarMultiply + 1)) = dim_try_it_out_t(rec_depth_try_it_out)
    Call scalarMultiply()
    arg_add_0((rec_depth_add + 1)) = return_scalarMultiply
    arg_scalarMultiply_0((rec_depth_scalarMultiply + 1)) = dim_try_it_out_white(rec_depth_try_it_out)
    arg_scalarMultiply_1((rec_depth_scalarMultiply + 1)) = (1.0 - dim_try_it_out_t(rec_depth_try_it_out))
    Call scalarMultiply()
    arg_add_1((rec_depth_add + 1)) = return_scalarMultiply
    Call add()
    return_try_it_out = return_add
    rec_depth_try_it_out = (rec_depth_try_it_out - 1)
    Exit Function
End Function
' ---
' The end
