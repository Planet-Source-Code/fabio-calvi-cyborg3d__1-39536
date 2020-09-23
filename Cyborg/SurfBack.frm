VERSION 5.00
Begin VB.Form frmMain 
   BorderStyle     =   0  'None
   Caption         =   "Character Animation-By MartWare-FPS:"
   ClientHeight    =   6810
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   9945
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   454
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   663
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.Label Label1 
      Caption         =   "Label1"
      Height          =   15
      Left            =   360
      TabIndex        =   0
      Top             =   360
      Visible         =   0   'False
      Width           =   135
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'Hi all.
'Some people asked me if it's possible to use animated .x files with
'Direct3D Retain Mode;
'in some VB forums I had read that it's not possible, or it's possible only
'in Direct3D Immediate Mode, or that the only method in Retain Mode is to create
'a loop of several static x. files.
'Well, this is not correct: it's possible to use an .x file with animation
'data in Direct3DRM, as you can see in this example;
'I think the best way to do this is to create an animated 3DS file and
'then convert it in an animated .x file with Conv3ds.exe having the -A option.
'
'This application wants to be only an example not a real game (it was the beginning of
'a RobotsWar 3D game project, but I have suspended it at today), but I am sure you could
'find some interesting things in it.
'
'The robot bullet are moving with a traslation: best way is to get them the set.velocity
'
'The explosion was only a simple experiment of nice 3d effect using a surface, but the sequence
'decrease the speed of application; I have left it in this program because it's good for
'static objects.
'I have used this method to create trees in my City3D program because of several up and down views;
'however if you have the same height for view, the best way is using the billboard method.
'
'If you choose 1024x768 display mode, a weapon appear for the player
'(for other resolution you need to make some calculations).
'
'Everybody can modify the code or employ parts of it within own projects, I don't care (just
'a little credit, please) but NOBODY MUST USE ANY PART OF THIS PROGRAM IN COMMERCIAL
'PURPOSES.
'
'Every comments, suggestions, ideas and e-mails are always welcomed to:
'fabiocalvi@ yahoo.com
'
'Happy coding and have fun!
'Goodbye, Fabio.
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'Arrow Keys moving you around scene
'
'Pageup key: looking up
'Pagedown key: looking down
'Q key: rotate robot to left
'W key: rotate robot to right
'Space key: robot shoot
'Ctrl key: you are firing
'Z key:explosion

Option Explicit
Const pi = 3.1415927

' direct x objects
Dim dx As New DirectX7
Dim dd As DirectDraw4

Dim SurfPrimary As DirectDrawSurface4
Dim SurfBack As DirectDrawSurface4
Dim DDSDPrimary As DDSURFACEDESC2
Dim DDCapsBack As DDSCAPS2

Dim dev As Direct3DRMDevice3

Dim clip As DirectDrawClipper
Dim d3drm As Direct3DRM3

Dim scene As Direct3DRMFrame3
Dim cam As Direct3DRMFrame3
Dim pos As D3DVECTOR
Dim view As Direct3DRMViewport2
Dim mesh As Direct3DRMMeshBuilder3
Dim m_objectFrame(1000) As Direct3DRMFrame3
Dim m_meshBuilder(1000) As Direct3DRMMeshBuilder3
Dim m_object As Direct3DRMMeshBuilder3
Dim bullet(1000) As D3DVECTOR

' animation variables
Dim animframe As Direct3DRMFrame3 ' frame that holds the animation
Dim anim As Direct3DRMAnimationSet2 ' the actual animation
Dim animpos As D3DVECTOR
Dim curframe As Single ' the current frame that the animation is at

Dim active(1000) As Boolean
Dim Keyctrl As Boolean
Dim Keyright As Boolean
Dim Keyleft As Boolean
Dim Keydown As Boolean
Dim Keyup As Boolean
Dim KeyQ As Boolean
Dim KeyW As Boolean
Dim KeyZ As Boolean
Dim Keyspace As Boolean
Dim Keyescape As Boolean
Dim KeyPagedown As Boolean
Dim KeyPageup As Boolean

Dim I As Integer, j As Integer

Dim LastTime As Long
Dim NumFramesDone As Integer
Dim FrameText As String

Dim GameFont As IFont

Dim StartGameTime As Long, nowTime As Long
Dim MaxSpeed As Integer

Dim Grados As Integer
Dim grado2 As Integer
Dim valor As Integer
Dim corx As Single
Dim corz As Single

Dim NoEdgeW As DDCOLORKEY
Dim Weapon11 As DirectDrawSurface4
Dim Weapon12 As DirectDrawSurface4
Dim Weapon13 As DirectDrawSurface4

Dim Stretching As DDSURFACEDESC2
Dim Weapon1Width
Dim Weapon1Height
Dim Weapon1Attributes As RECT
Dim Weapon1X As Integer
Dim Weapon1Y As Integer
Dim MouseX
Dim MouseY
Dim WeaponSwitchable
Dim shootcount As Byte

Dim Sfondo As Direct3DRMTexture3
Dim explframe As Integer, explo As Boolean, nameexplo As String
Dim mexplo(4) As Direct3DRMFace2
Dim explox As Single, exploz As Single
Private Declare Function GetKeyState Lib "user32" (ByVal nVirtKey As Long) As Integer


Private Sub form_KeyDown(KeyCode As Integer, Shift As Integer)
   If KeyCode = vbKeyControl Then Keyctrl = True
   If KeyCode = vbKeyZ Then KeyZ = True
   If KeyCode = vbKeyRight Then Keyright = True
   If KeyCode = vbKeyLeft Then Keyleft = True
   If KeyCode = vbKeyDown Then Keydown = True
   If KeyCode = vbKeyEscape Then Keyescape = True
   If KeyCode = vbKeyUp Then Keyup = True
   If KeyCode = vbKeySpace Then
      Keyspace = True
      If j < 1001 Then j = j + 1
   End If
   If KeyCode = vbKeyQ Then KeyQ = True
   If KeyCode = vbKeyW Then KeyW = True
   If KeyCode = vbKeyPageUp Then KeyPageup = True
   If KeyCode = vbKeyPageDown Then KeyPagedown = True
End Sub
Private Sub form_Keyup(KeyCode As Integer, Shift As Integer)
   If KeyCode = vbKeyControl Then Keyctrl = False
   If KeyCode = vbKeyZ Then KeyZ = False
   If KeyCode = vbKeyRight Then Keyright = False
   If KeyCode = vbKeyLeft Then Keyleft = False
   If KeyCode = vbKeyDown Then Keydown = False
   If KeyCode = vbKeyEscape Then Keyescape = False
   If KeyCode = vbKeyUp Then Keyup = False
   If KeyCode = vbKeySpace Then Keyspace = False
   If KeyCode = vbKeyQ Then KeyQ = False
   If KeyCode = vbKeyW Then KeyW = False
   If KeyCode = vbKeyPageUp Then KeyPageup = False
   If KeyCode = vbKeyPageDown Then KeyPagedown = False
End Sub
' main sub
Public Sub init_dx(nWidth As Integer, nHeight As Integer, nDepth As Integer, nGUID As String, nDetail As Integer)
Dim t1 As Long, fogcolor As Single
Dim Starttick As Long, LastTick As Long
Dim collidev As Boolean, collideb As Boolean
Dim distance As Single, distanceb As Single

StartGameTime = dx.TickCount
MaxSpeed = 30
Unload frmSplash

   
   'RED GETS MULTIPLIED BY 2^16 OR 65536. GREEN GETS MULTIPLIED BY 2^8 OR 256.
   'BLUE GETS MULTIPLIED BY 2^0 OR 1. VERY EASY, ADD 'EM UP TO SPECIFY COLOR.
    fogcolor = 125 * 65536 + 125 * 256 + 125
    
    t1 = dx.TickCount()
    
    Set dd = dx.DirectDraw4Create("")
    dd.SetCooperativeLevel Me.hWnd, DDSCL_FULLSCREEN Or DDSCL_EXCLUSIVE
    
    ' this will be full-screen, so set the display mode
    dd.SetDisplayMode CLng(nWidth), CLng(nHeight), CLng(nDepth), 0, DDSDM_DEFAULT
    
    ' create the primary surface
    DDSDPrimary.lFlags = DDSD_CAPS Or DDSD_BACKBUFFERCOUNT
    DDSDPrimary.ddsCaps.lCaps = DDSCAPS_PRIMARYSURFACE Or DDSCAPS_3DDEVICE Or DDSCAPS_COMPLEX Or DDSCAPS_FLIP
    DDSDPrimary.lBackBufferCount = 1
    Set SurfPrimary = dd.CreateSurface(DDSDPrimary)
           
    ' get the back buffer
    DDCapsBack.lCaps = DDSCAPS_BACKBUFFER
    Set SurfBack = SurfPrimary.GetAttachedSurface(DDCapsBack)
    
    ' Create the Retained Mode object
    Set d3drm = dx.Direct3DRMCreate()
    
    SurfBack.SetForeColor RGB(0, 255, 0)
    
    Set dev = d3drm.CreateDeviceFromSurface(nGUID, dd, SurfBack, D3DRMDEVICE_DEFAULT)
    
    dev.SetBufferCount 2
    
    Select Case nDetail
       Case 0
          dev.SetQuality D3DRMLIGHT_ON Or D3DRMFILL_SOLID
       Case 1
          dev.SetQuality D3DRMLIGHT_ON Or D3DRMRENDER_GOURAUD
          'Linear texturing looks better
          dev.SetTextureQuality D3DRMTEXTURE_LINEAR
          dev.SetRenderMode D3DRMRENDERMODE_BLENDEDTRANSPARENCY
    End Select
    
    Set scene = d3drm.CreateFrame(Nothing)
    Set cam = d3drm.CreateFrame(scene)
    
'    Set Sfondo = d3drm.LoadTexture("sky.bmp")
'    scene.SetSceneBackgroundImage Sfondo
    
    dev.SetDither D_TRUE
    Set view = d3drm.CreateViewport(dev, cam, 0, 0, Me.ScaleWidth, Me.ScaleHeight)
    view.SetBack 5000!
   
    Set mesh = d3drm.CreateMeshBuilder()
    mesh.SetPerspective D_TRUE
    scene.AddVisual mesh
    
    For I = 1 To 1000
    active(I) = False
    Next
    
    ' create walls
    Call MakeWall(d3drm, mesh, -1000, -15, 1000, 1000, -15, 1000, 1000, -15, -1000, -1000, -15, -1000, "wall", 20, 20, 0, 0, 0) ' grass texture on the floor
    Call MakeWall(d3drm, mesh, 200, 15, 1000, -200, 15, 1000, -200, 15, -1000, 200, 15, -1000, "roof", 3, 3, 0, 0, 0)
    Call MakeWall(d3drm, mesh, 200, -15, 1000, -200, -15, 1000, -200, 15, 1000, 200, 15, 1000, "r10", 18, 3, 0, 0, 0)
    Call MakeWall(d3drm, mesh, 200, 15, -1000, -200, 15, -1000, -200, -15, -1000, 200, -15, -1000, "r10", 18, 3, 0, 0, 0)
    Call MakeWall(d3drm, mesh, 200, -15, 1000, 200, 15, 1000, 200, 15, -1000, 200, -15, -1000, "r10", 3, 60, 0, 0, 0)
    Call MakeWall(d3drm, mesh, -200, 15, 1000, -200, -15, 1000, -200, -15, -1000, -200, 15, -1000, "r10", 3, 60, 0, 0, 0)
    
    ' create animation frame
    Set animframe = d3drm.CreateFrame(scene)
    ' create animation set
    Set anim = d3drm.CreateAnimationSet()
    anim.LoadFromFile "cyborg.x", 0, D3DRMLOAD_FROMFILE, Nothing, Nothing, animframe ' load animation into frame and animation set
    animframe.AddScale D3DRMCOMBINE_BEFORE, 0.1, 0.1, 0.1
    animframe.SetPosition scene, 0, -15, 100

    For I = 1 To 1000
       Set m_objectFrame(I) = d3drm.CreateFrame(scene)
       Set m_meshBuilder(I) = d3drm.CreateMeshBuilder()
       m_meshBuilder(I).LoadFromFile "cuboid13.x", 0, D3DRMLOAD_FROMFILE, Nothing, Nothing
       m_objectFrame(I).AddScale D3DRMCOMBINE_REPLACE, 0.01, 0.01, 0.01
       m_objectFrame(I).AddVisual m_meshBuilder(I)
       m_objectFrame(I).SetPosition animframe, -50, 90, -20
    Next I
   
    ' add light to camera
    Dim light(4) As Direct3DRMLight
    Dim lightframe(4) As Direct3DRMFrame3
    
    For I = 1 To 4
    Set light(I) = d3drm.CreateLightRGB(D3DRMLIGHT_POINT, 1, 1, 1)
    light(I).SetRange 1000
    light(I).SetColorRGB 255, 255, 255
    light(I).SetUmbra 0.8
    light(I).SetPenumbra 1.1
    Set lightframe(I) = d3drm.CreateFrame(scene)
    lightframe(I).AddLight light(I)
    Next
    lightframe(1).SetPosition scene, 1000, 15, 1000
    lightframe(2).SetPosition scene, -1000, 15, 1000
    lightframe(3).SetPosition scene, -1000, 15, -1000
    lightframe(4).SetPosition scene, 1000, 15, -1000
   
    ' add a bit of ambient light to the scene
    scene.AddLight d3drm.CreateLightRGB(D3DRMLIGHT_AMBIENT, 0.5, 0.5, 0.5)
     
'    scene.SetSceneFogEnable 1 'true
'    scene.SetSceneFogMode D3DRMFOG_LINEAR 'EXPONENTIAL
'    scene.SetSceneFogColor fogcolor
'    scene.SetSceneBackground fogcolor
'    scene.SetSceneFogMethod D3DRMFOGMETHOD_TABLE
'    scene.SetSceneFogParams 1, 200, 1
'    scene.SetZbufferMode D3DRMZBUFFER_ENABLE
'    scene.SetSortMode D3DRMSORT_FRONTTOBACK
    
    DoEvents
    explo = False
    valor = 5
    
    Weapon1Width = Int(600 / 1) '1024
    Weapon1Height = Int(324 / 1) '768
    Stretching.lFlags = DDSD_CAPS
    Stretching.ddsCaps.lCaps = DDSCAPS_OFFSCREENPLAIN
    Stretching.lWidth = Weapon1Width
    Stretching.lHeight = Weapon1Height
    Set Weapon11 = dd.CreateSurfaceFromFile("weapon11.bmp", Stretching)
    Weapon11.SetColorKey DDCKEY_SRCBLT, NoEdgeW
    Set Weapon12 = dd.CreateSurfaceFromFile("weapon12.bmp", Stretching)
    Weapon12.SetColorKey DDCKEY_SRCBLT, NoEdgeW
    Set Weapon13 = dd.CreateSurfaceFromFile("weapon13.bmp", Stretching)
    Weapon13.SetColorKey DDCKEY_SRCBLT, NoEdgeW
    Weapon1Attributes.Left = 0
    Weapon1Attributes.Right = Weapon1Width
    Weapon1Attributes.Top = 0
    Weapon1Attributes.Bottom = Weapon1Height
    NoEdgeW.low = RGB(0, 0, 0)
    NoEdgeW.high = 0

    
    ' start main loop
    Do While DoEvents()
        
       DoEvents
       Starttick = dx.TickCount
       DoEvents
        
        nowTime = dx.TickCount
        Do Until nowTime - LastTick > MaxSpeed
            DoEvents
            nowTime = dx.TickCount
        Loop
        LastTick = nowTime
        FrameText = Int(1000 / (dx.TickCount - Starttick + 1))
        
       'Move forward
        If Keyup = True Then
           corx = corx + valor * Sin(Grados * pi / 180)
           corz = corz + valor * Cos(Grados * pi / 180)
           cam.SetPosition scene, corx, 0, corz
        End If
        
        'Move back
        If Keydown = True Then
           corx = corx - valor * Sin(Grados * pi / 180)
           corz = corz - valor * Cos(Grados * pi / 180)
           cam.SetPosition scene, corx, 0, corz
        End If
        
        'Rotate left
        If Keyleft = True Then
           Grados = Grados - valor
           cam.AddRotation D3DRMCOMBINE_BEFORE, 1, 0, 0, -(grado2 * pi / 180)
           cam.AddRotation D3DRMCOMBINE_BEFORE, 0, 1, 0, -(valor * pi / 180)
           cam.AddRotation D3DRMCOMBINE_BEFORE, 1, 0, 0, (grado2 * pi / 180)
        End If
        
        'Rotate right
        If Keyright = True Then
           Grados = Grados + valor
           cam.AddRotation D3DRMCOMBINE_BEFORE, 1, 0, 0, -(grado2 * pi / 180)
           cam.AddRotation D3DRMCOMBINE_BEFORE, 0, 1, 0, (valor * pi / 180)
           cam.AddRotation D3DRMCOMBINE_BEFORE, 1, 0, 0, (grado2 * pi / 180)
        End If
        
        'Look up
        If KeyPageup = True Then
            grado2 = grado2 - valor
            If grado2 > -90 Then
               cam.AddRotation D3DRMCOMBINE_BEFORE, 1, 0, 0, -(valor * pi / 180)
            Else
               grado2 = grado2 + valor
            End If
        End If
        
        'Look down
        If KeyPagedown = True Then
            grado2 = grado2 + valor
            If grado2 < 90 Then
               cam.AddRotation D3DRMCOMBINE_BEFORE, 1, 0, 0, (valor * pi / 180)
            Else
               grado2 = grado2 - valor
            End If
        End If
       
        curframe = curframe + 1
            
        ' check if current frame is too high or low
        If curframe > 40 Then curframe = 0
        If curframe < 0 Then curframe = 40
        
        'move the robot
        animframe.AddTranslation D3DRMCOMBINE_BEFORE, 0, 0, -2
        
        For I = 1 To 1000
            If active(I) = False Then
               m_objectFrame(I).SetPosition animframe, -50, 90, -20
            End If
        Next I
        
        'reset the animation frames
        If curframe > 40 Then anim.SetTime 40 Else anim.SetTime curframe
        
        ' rotate robot left
        If KeyQ = True Then
           animframe.AddRotation D3DRMCOMBINE_BEFORE, 0, 1, 0, 0.05
           For I = 1 To 1000
              If active(I) = False Then m_objectFrame(I).AddRotation D3DRMCOMBINE_BEFORE, 0, 1, 0, 0.05
           Next
        End If
        ' rotate robot right
        If KeyW = True Then
           animframe.AddRotation D3DRMCOMBINE_BEFORE, 0, 1, 0, -0.05
           For I = 1 To 1000
              If active(I) = False Then m_objectFrame(I).AddRotation D3DRMCOMBINE_BEFORE, 0, 1, 0, -0.05
           Next
        End If
        ' active robot shooting
        If Keyspace = True Then
           If j < 1001 Then active(j) = True
        End If
                
        For I = 1 To 1000
            m_objectFrame(I).GetPosition scene, bullet(I)
            If I <= j And active(I) = True Then
               distanceb = Sqr((bullet(I).X - corx) ^ 2 + (bullet(I).z - corz) ^ 2)
               If distanceb <= 10 Then
                  collideb = True
                  active(I) = False
               Else
                  m_objectFrame(I).AddTranslation D3DRMCOMBINE_BEFORE, 0, 0, -1500
               End If
            End If
        Next
        
        If collideb = True Then
           Dim k As Integer
           For k = 1 To 2
               If k = 1 Then
                  grado2 = grado2 + valor
                  cam.AddRotation D3DRMCOMBINE_BEFORE, 1, 0, 0, (valor * pi / 180)
                Else
'                  grado2 = grado2 - valor
'                  cam.AddRotation D3DRMCOMBINE_BEFORE, 1, 0, 0, -(valor * pi / 180)
                End If
                view.Clear D3DRMCLEAR_ALL
                view.Render scene
                dev.Update
           Next
           collideb = False
        End If
        
        cam.GetPosition scene, pos
        animframe.GetPosition scene, animpos
        
        ' don't overcross the limits of room
'        If pos.X < -175 Then pos.X = -175
'        If pos.X > 175 Then pos.X = 175
'        If pos.z < -975 Then pos.z = -975
'        If pos.z > 975 Then pos.z = 975
        If animpos.X < -185 Then animpos.X = -185
        If animpos.X > 185 Then animpos.X = 185
        If animpos.z < -985 Then animpos.z = -985
        If animpos.z > 985 Then animpos.z = 985
   
        cam.SetPosition scene, pos.X, pos.Y, pos.z
        animframe.SetPosition scene, animpos.X, animpos.Y, animpos.z
  
  
        If KeyZ = True Then
           explo = True
           explframe = -1
        End If
        If explo = True Then
           explframe = explframe + 1
           If explframe = 18 Then
              explo = False
              explframe = -1
           End If
           If explo = True And explframe > -1 And explframe < 18 Then
              nameexplo = "2200" & explframe
              explox = 50
              exploz = 50
              Call MakeExplo(d3drm, mesh, explox, 15, exploz - 20, explox, 15, exploz + 20, explox, -15, exploz + 20, explox, -15, exploz - 20, nameexplo, 1, 1, 1)
              Call MakeExplo(d3drm, mesh, explox - 20, 15, exploz, explox + 20, 15, exploz, explox + 20, -15, exploz, explox - 20, -15, exploz, nameexplo, 1, 1, 2)
              Call MakeExplo(d3drm, mesh, explox + 20, 15, exploz, explox - 20, 15, exploz, explox - 20, -15, exploz, explox + 20, -15, exploz, nameexplo, 1, 1, 3)
              Call MakeExplo(d3drm, mesh, explox, 15, exploz + 20, explox, 15, exploz - 20, explox, -15, exploz - 20, explox, -15, exploz + 20, nameexplo, 1, 1, 4)
           End If
       End If
       
        view.Clear D3DRMCLEAR_ALL
        view.Render scene
        dev.Update
              
        If explo = True And explframe > -1 And explframe < 18 Then
            For I = 1 To 4
                mesh.DeleteFace mexplo(I)
            Next
        End If

        ' check to exit
        If Keyescape = True Then Unload Me: End
        
        'Set GameFont = Label1.Font
        'SurfBack.SetFont GameFont
        'Call SurfBack.DrawText(10, 10, "D3DRM Full Screen, Esc to exit", False)
        'Call SurfBack.DrawText(10, 30, "Current frame rate: " & FrameText & " fps", False)
        
        If Keyctrl = True Then
           shootcount = shootcount + 1
           If shootcount > 3 Then
              Call SurfBack.BltFast(424, 443, Weapon12, Weapon1Attributes, DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY)
              shootcount = 0
            Else
               Call SurfBack.BltFast(424, 443, Weapon13, Weapon1Attributes, DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY)
            End If
        Else
            shootcount = 0
            Call SurfBack.BltFast(424, 443, Weapon11, Weapon1Attributes, DDBLTFAST_WAIT Or DDBLTFAST_SRCCOLORKEY)
        End If
        
        SurfPrimary.Flip Nothing, DDFLIP_WAIT

    
    Loop
    
End Sub

Private Sub MakeWall(d3drm As Direct3DRM3, mesh As Direct3DRMMeshBuilder3, X1 As Single, Y1 As Single, z1 As Single, x2 As Single, y2 As Single, z2 As Single, x3 As Single, y3 As Single, z3 As Single, x4 As Single, y4 As Single, z4 As Single, TexFile As String, TileX As Single, TileY As Single, r As Single, g As Single, b As Single)
    ' local variables
    Dim wall As Direct3DRMFace2
    Dim texwall As Direct3DRMTexture3
    ' create face
    Set wall = d3drm.CreateFace()
    ' add vertexs
    wall.AddVertex X1, Y1, z1
    wall.AddVertex x2, y2, z2
    wall.AddVertex x3, y3, z3
    wall.AddVertex x4, y4, z4
    ' get type of file
    If TexFile = "" Then
        ' set colors
        wall.SetColorRGB r, g, b
    Else
        ' create texture
        Set texwall = d3drm.LoadTexture(App.Path & "\" & TexFile & ".bmp")
        ' set u and v values
        wall.SetTextureCoordinates 0, 0, 0
        wall.SetTextureCoordinates 1, TileX, 0
        wall.SetTextureCoordinates 2, TileX, TileY
        wall.SetTextureCoordinates 3, 0, TileY
        ' set the texture
        wall.SetTexture texwall
    End If
    ' add face to mesh
    mesh.AddFace wall
End Sub
Private Sub MakeExplo(d3drm As Direct3DRM3, mesh As Direct3DRMMeshBuilder3, X1 As Single, Y1 As Single, z1 As Single, x2 As Single, y2 As Single, z2 As Single, x3 As Single, y3 As Single, z3 As Single, x4 As Single, y4 As Single, z4 As Single, TexFile As String, TileX As Single, TileY As Single, ind As Byte)
    ' local variables
    Dim t As Direct3DRMTexture3
    ' create face
    Set mexplo(ind) = d3drm.CreateFace()
    ' add vertexs
    mexplo(ind).AddVertex X1, Y1, z1
    mexplo(ind).AddVertex x2, y2, z2
    mexplo(ind).AddVertex x3, y3, z3
    mexplo(ind).AddVertex x4, y4, z4
    ' get type of file
    If TexFile = "" Then
        MsgBox ("Texture file missing")
        End
    Else
        ' create texture
        Set t = d3drm.LoadTexture(App.Path & "\" & TexFile & ".bmp")
        t.SetDecalTransparency D_TRUE
        t.SetDecalTransparentColor 0
        ' set u and v values
        mexplo(ind).SetTextureCoordinates 0, 0, 0
        mexplo(ind).SetTextureCoordinates 1, TileX, 0
        mexplo(ind).SetTextureCoordinates 2, TileX, TileY
        mexplo(ind).SetTextureCoordinates 3, 0, TileY
        ' set the texture
        mexplo(ind).SetTexture t
    End If
    ' add face to mesh
    mesh.AddFace mexplo(ind)
End Sub

Sub EndIT()
Call dd.RestoreDisplayMode
Call dd.SetCooperativeLevel(Me.hWnd, DDSCL_NORMAL)
End
End Sub
Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)

    MouseX = X
    MouseY = Y

End Sub
Private Sub Weapon1Edge(bytTileNumber As Byte, ByRef intTileX As Integer, ByRef intTileY As Integer, ByRef Weapon1Attributes As RECT)

    Weapon1Attributes.Left = 0
    Weapon1Attributes.Right = Weapon1Width
    Weapon1Attributes.Top = 0
    Weapon1Attributes.Bottom = Weapon1Height
    
    If intTileX < 0 Then
        Weapon1Attributes.Left = Weapon1Attributes.Left - intTileX
        intTileX = 0
    End If
    
    If intTileY < 0 Then
        Weapon1Attributes.Top = Weapon1Attributes.Top - intTileY
        intTileY = 0
    End If
    
    If intTileX + Weapon1Width > 1024 Then
        Weapon1Attributes.Right = Weapon1Attributes.Right + (1024 - (intTileX + Weapon1Width))
    End If
    
    If intTileY + Weapon1Height > 768 Then
        Weapon1Attributes.Bottom = Weapon1Attributes.Bottom + (768 - (intTileY + Weapon1Height))
    End If
    
End Sub
