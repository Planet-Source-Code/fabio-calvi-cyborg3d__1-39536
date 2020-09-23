VERSION 5.00
Begin VB.Form frmSplash 
   Caption         =   "Init DX"
   ClientHeight    =   2820
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   3660
   LinkTopic       =   "Form1"
   ScaleHeight     =   2820
   ScaleWidth      =   3660
   StartUpPosition =   2  'CenterScreen
   Begin VB.ComboBox cmbDetail 
      Height          =   315
      Left            =   0
      Style           =   2  'Dropdown List
      TabIndex        =   5
      Top             =   1800
      Width           =   3615
   End
   Begin VB.ComboBox cmbRenderer 
      Height          =   315
      ItemData        =   "frmSplash.frx":0000
      Left            =   0
      List            =   "frmSplash.frx":0002
      Style           =   2  'Dropdown List
      TabIndex        =   2
      Top             =   360
      Width           =   3615
   End
   Begin VB.ComboBox cmbDisplay 
      Height          =   315
      Left            =   0
      Style           =   2  'Dropdown List
      TabIndex        =   1
      Top             =   1080
      Width           =   3615
   End
   Begin VB.CommandButton cmdProceed 
      Caption         =   "&Proceed"
      Height          =   375
      Left            =   1080
      TabIndex        =   0
      Top             =   2280
      Width           =   1335
   End
   Begin VB.Label Label2 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Please select level of detail:"
      Height          =   195
      Left            =   0
      TabIndex        =   6
      Top             =   1560
      Width           =   1965
   End
   Begin VB.Label Label1 
      BackStyle       =   0  'Transparent
      Caption         =   "Please select which renderer you would like to use:"
      Height          =   255
      Left            =   0
      TabIndex        =   4
      Top             =   120
      Width           =   3735
   End
   Begin VB.Label Label4 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Please sSelect the display mode:"
      Height          =   195
      Left            =   0
      TabIndex        =   3
      Top             =   840
      Width           =   2325
   End
End
Attribute VB_Name = "frmSplash"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim e_Dx As New DirectX7
Dim e_Dd As DirectDraw7
Dim e_D3d As Direct3D7
Dim e_Enum As Direct3DEnumDevices
Dim I As Integer, sRenderer As String
Dim e_DModes As DirectDrawEnumModes
Dim e_ddsd As DDSURFACEDESC2
Dim DisplayModesRef() As Byte


Private Sub cmdProceed_Click()
Dim sGuid As String, bDetail As Byte, Width As Integer, Height As Integer, BPP As Integer
Select Case cmbRenderer.Text
    Case "Hardware"
        sGuid = "IID_IDirect3DHALDevice"
    Case "Software"
        sGuid = "IID_IDirect3DRGBDevice"
    Case "Unknown Device"
        MsgBox "Please Choose another Device"
End Select

Select Case cmbDetail.Text
    Case "Low Detail (Faster)"
        bDetail = 0
    Case "High Detail (Slower)"
        bDetail = 1
End Select

e_DModes.GetItem DisplayModesRef(cmbDisplay.ListIndex), e_ddsd

frmMain.init_dx CInt(e_ddsd.lWidth), CInt(e_ddsd.lHeight), CInt(e_ddsd.ddpfPixelFormat.lRGBBitCount), sGuid, CInt(bDetail)  ', chkBuffer.Value
End Sub

Private Sub Form_Load()
'''''''''GET RENDERER INFO

Set e_Dd = e_Dx.DirectDrawCreate("")
Set e_D3d = e_Dd.GetDirect3D
Set e_Enum = e_D3d.GetDevicesEnum

For I = 1 To e_Enum.GetCount()
sRenderer = e_Enum.GetGuid(I)
Select Case sRenderer
    Case "IID_IDirect3DRGBDevice"
    cmbRenderer.AddItem "Software", 0
    cmbRenderer.Text = "Software"
    Case "IID_IDirect3DHALDevice"
    cmbRenderer.AddItem "Hardware", 0
    cmbRenderer.Text = "Hardware"
    Case Else
    cmbRenderer.AddItem "Unknown Device", 0
End Select
Next I

''''''''''''''DETAIL LEVELS
cmbDetail.AddItem "Low Detail (Faster)", 0
cmbDetail.AddItem "High Detail (Slower)", 1
cmbDetail.Text = "High Detail (Slower)"


''''''''''''''''RESOLUTIONS
    'Dim DisplayModesEnum As DirectDrawEnumModes
    'Dim ddsd2 As DDSURFACEDESC2
    'Dim dd As DirectDraw7
    'Set dd = m_dx.DirectDrawCreate(sGuid)
    e_Dd.SetCooperativeLevel Me.hWnd, DDSCL_NORMAL
    Dim Curr As Integer
    Set e_DModes = e_Dd.GetDisplayModesEnum(0, e_ddsd)
    For I = 1 To e_DModes.GetCount()
        e_DModes.GetItem I, e_ddsd
        If e_ddsd.lWidth > 600 And e_ddsd.lWidth <= 1024 Then
        If e_ddsd.lHeight > 400 And e_ddsd.lHeight <= 768 Then
        If e_ddsd.ddpfPixelFormat.lRGBBitCount >= 16 Then
            cmbDisplay.AddItem CStr(e_ddsd.lWidth) & "x" & CStr(e_ddsd.lHeight) & "x" & CStr(e_ddsd.ddpfPixelFormat.lRGBBitCount)
            cmbDisplay.Text = CStr(e_ddsd.lWidth) & "x" & CStr(e_ddsd.lHeight) & "x" & CStr(e_ddsd.ddpfPixelFormat.lRGBBitCount)
            ReDim Preserve DisplayModesRef(Curr)
            DisplayModesRef(Curr) = I
            Curr = Curr + 1
        End If
        End If
        End If
    Next
End Sub

