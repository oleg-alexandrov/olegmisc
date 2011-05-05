; IMPORTANT INFO ABOUT GETTING STARTED: Lines that start with a
; semicolon, such as this one, are comments.  They are not executed.

; This script has a special filename and path because it is automatically
; launched when you run the program directly.  Also, any text file whose
; name ends in .ahk is associated with the program, which means that it
; can be launched simply by double-clicking it.  You can have as many .ahk
; files as you want, located in any folder.  You can also run more than
; one ahk file simultaneously and each will get its own tray icon.

; SAMPLE HOTKEYS: Below are two sample hotkeys.  The first is Win+Z and it
; launches a web site in the default browser.  The second is Control+Alt+N
; and it launches a new Notepad window (or activates an existing one).  To
; try out these hotkeys, run AutoHotkey again, which will load this file.

; Only one instance of this script
#SingleInstance force

; Reload this script with Win-a
#a:: Run "C:\Program Files\AutoHotkey\AutoHotkey.exe" "C:\Documents and Settings\olegalex\Desktop\AutoHotkey.ahk"

; Map Capslock to Control
Capslock::Control

; F1 to launch or switch to VncViewer
F1::
IfWinExist ahk_class rfb::win32::DesktopWindowClass
{
  WinActivate
}
Else
{
  Run "C:\Program Files\RealVNC\VNC4\vncviewer.exe"
  WinWait ahk_class rfb::win32::DesktopWindowClass
  WinActivate
}
Return

; F2 to launch or switch to Outlook
F2::
IfWinExist ahk_class rctrl_renwnd32
{
  WinActivate
}
Else
{
  Run "C:\Program Files\Microsoft Office\OFFICE11\OUTLOOK.EXE"
  WinWait ahk_class rctrl_renwnd32
  WinActivate
}
Return

; F3 to launch or switch to FireFox
F3::
IfWinExist ahk_class MozillaUIWindowClass
{
  WinActivate
}
Else
{
  Run "C:\Program Files\Mozilla Firefox\firefox.exe"
  WinWait ahk_class MozillaUIWindowClass
  WinActivate
}
Return

#IfWinActive ahk_class rctrl_renwnd32
{
  ; Outlook bindings
  !s::Send ^y{Home}s{Enter}  ; Go to the sent   folder with alt-s
  !i::Send ^y{Home}i{Enter}  ; Go to the inbox  folder with alt-i
  !d::Send ^y{Home}d{Enter}  ; Go to the del    folder with alt-d
  !w::Send ^y{Home}w{Enter}  ; Go to the work   folder with alt-w
  !v::Send ^+v{Home}w{Enter} ; Save to the work folder with alt-v
  Return
}


#IfWinActive ahk_class MozillaWindowClass
{
  ; Firefox bindings
  !h::Send {Browser_Home}   ; Go to the home page with alt-h
  !g::Send ^lgm{Enter}      ; Go to gmail with         alt-g
  !n::Send ^ln{Enter}       ; Strip NYT stuff with     alt-n
  Return
}


