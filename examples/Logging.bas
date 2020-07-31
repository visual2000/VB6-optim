Attribute VB_Name = "Logging"
Option Explicit

Public Sub log(logLine As String)
    frmView.txtLog.Text = frmView.txtLog.Text + logLine + vbNewLine
    frmView.txtLog.SelStart = Len(frmView.txtLog.Text)
End Sub
