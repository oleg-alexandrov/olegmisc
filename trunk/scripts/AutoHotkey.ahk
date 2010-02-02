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

#z::Run www.autohotkey.com

^!n::
IfWinExist Untitled - Notepad
	WinActivate
else
	Run Notepad
return


; Note: From now on whenever you run AutoHotkey directly, this script
; will be loaded.  So feel free to customize it to suit your needs.

; Please read the QUICK-START TUTORIAL near the top of the help file.
; It explains how to perform common automation tasks such as sending
; keystrokes and mouse clicks.  It also explains more about hotkeys.

; Only one instance of this script
#SingleInstance force

; Map Capslock to Tab
Capslock::Tab

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
;Return

