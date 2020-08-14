Attribute VB_Name = "ReturnValOrdering"
' ---
Option Explicit
' ---
Private callsite_TVec3_init_4 As TVec3
Private callsite_TVec3_init_5 As TVec3
Private callsite_TVec3_init_6 As TVec3
Private callsite_TVec3_init_7 As TVec3
Private callsite_TVec3_init_8 As TVec3
Private callsite_add_9 As TVec3
Private callsite_add_10 As TVec3
Private callsite_add_12 As TVec3
Private return_TVec3_init As TVec3
Private return_add As TVec3
Private return_try_it_out As TVec3
Private arg_TVec3_init_0(0 To 100) As Double
Private arg_TVec3_init_1(0 To 100) As Double
Private arg_TVec3_init_2(0 To 100) As Double
Private dim_TVec3_init_v(0 To 100) As TVec3
Private arg_add_0(0 To 100) As TVec3
Private arg_add_1(0 To 100) As TVec3
Private dim_try_it_out_blue(0 To 100) As TVec3
Private dim_try_it_out_white(0 To 100) As TVec3
Private dim_try_it_out_red(0 To 100) As TVec3
Private dim_try_it_out_green(0 To 100) As TVec3
Private rec_depth_TVec3_init As Integer
Private rec_depth_add As Integer
Private rec_depth_try_it_out As Integer
Public Function TVec3_init() As TVec3
    rec_depth_TVec3_init = (rec_depth_TVec3_init + 1)
    If (rec_depth_TVec3_init > 99) Then
        rec_depth_TVec3_init = (rec_depth_TVec3_init - 1)
        Exit Function
    End If
    dim_TVec3_init_v(rec_depth_TVec3_init).x = arg_TVec3_init_0(rec_depth_TVec3_init)
    dim_TVec3_init_v(rec_depth_TVec3_init).y = arg_TVec3_init_1(rec_depth_TVec3_init)
    dim_TVec3_init_v(rec_depth_TVec3_init).z = arg_TVec3_init_2(rec_depth_TVec3_init)
    return_TVec3_init = dim_TVec3_init_v(rec_depth_TVec3_init)
    rec_depth_TVec3_init = (rec_depth_TVec3_init - 1)
    Exit Function
End Function
Public Function add() As TVec3
    rec_depth_add = (rec_depth_add + 1)
    If (rec_depth_add > 99) Then
        rec_depth_add = (rec_depth_add - 1)
        Exit Function
    End If
    arg_TVec3_init_0((rec_depth_TVec3_init + 1)) = (arg_add_0(rec_depth_add).x + arg_add_1(rec_depth_add).x)
    arg_TVec3_init_1((rec_depth_TVec3_init + 1)) = (arg_add_0(rec_depth_add).y + arg_add_1(rec_depth_add).y)
    arg_TVec3_init_2((rec_depth_TVec3_init + 1)) = (arg_add_0(rec_depth_add).z + arg_add_1(rec_depth_add).z)
    Call TVec3_init()
    callsite_TVec3_init_4 = return_TVec3_init
    return_add = callsite_TVec3_init_4
    rec_depth_add = (rec_depth_add - 1)
    Exit Function
End Function
Public Function try_it_out() As TVec3
    rec_depth_try_it_out = (rec_depth_try_it_out + 1)
    If (rec_depth_try_it_out > 99) Then
        rec_depth_try_it_out = (rec_depth_try_it_out - 1)
        Exit Function
    End If
    arg_TVec3_init_0((rec_depth_TVec3_init + 1)) = 1
    arg_TVec3_init_1((rec_depth_TVec3_init + 1)) = 1
    arg_TVec3_init_2((rec_depth_TVec3_init + 1)) = 1
    Call TVec3_init()
    callsite_TVec3_init_5 = return_TVec3_init
    dim_try_it_out_white(rec_depth_try_it_out) = callsite_TVec3_init_5
    arg_TVec3_init_0((rec_depth_TVec3_init + 1)) = 1
    arg_TVec3_init_1((rec_depth_TVec3_init + 1)) = 0
    arg_TVec3_init_2((rec_depth_TVec3_init + 1)) = 0
    Call TVec3_init()
    callsite_TVec3_init_6 = return_TVec3_init
    dim_try_it_out_red(rec_depth_try_it_out) = callsite_TVec3_init_6
    arg_TVec3_init_0((rec_depth_TVec3_init + 1)) = 0
    arg_TVec3_init_1((rec_depth_TVec3_init + 1)) = 1
    arg_TVec3_init_2((rec_depth_TVec3_init + 1)) = 0
    Call TVec3_init()
    callsite_TVec3_init_7 = return_TVec3_init
    green = callsite_TVec3_init_7
    arg_TVec3_init_0((rec_depth_TVec3_init + 1)) = 0.5
    arg_TVec3_init_1((rec_depth_TVec3_init + 1)) = 0.3
    arg_TVec3_init_2((rec_depth_TVec3_init + 1)) = 1.0
    Call TVec3_init()
    callsite_TVec3_init_8 = return_TVec3_init
    dim_try_it_out_blue(rec_depth_try_it_out) = callsite_TVec3_init_8
    arg_add_0((rec_depth_add + 1)) = green
    arg_add_1((rec_depth_add + 1)) = dim_try_it_out_blue(rec_depth_try_it_out)
    Call add()
    callsite_add_10 = return_add
    arg_add_0((rec_depth_add + 1)) = dim_try_it_out_white(rec_depth_try_it_out)
    arg_add_1((rec_depth_add + 1)) = dim_try_it_out_red(rec_depth_try_it_out)
    Call add()
    callsite_add_12 = return_add
    arg_add_0((rec_depth_add + 1)) = callsite_add_10
    arg_add_1((rec_depth_add + 1)) = callsite_add_12
    Call add()
    callsite_add_9 = return_add
    return_try_it_out = callsite_add_9
    rec_depth_try_it_out = (rec_depth_try_it_out - 1)
    Exit Function
End Function
' ---
' The end
