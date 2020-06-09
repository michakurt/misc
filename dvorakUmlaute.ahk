#Persistent
SetTimer, MoveMouse

MoveMouse:
If ( A_TimeIdle > 133000 ) {
  MouseMove, 1 , 1,, R
  MouseMove, -1,-1,, R
}
Return

; Keymappings f�r amerikanisches Dvorak-Layout mit deutschen Umlauten: AltGr-u = � AltGr-Shift-u = �, usw. AltGr-s = �, AltGr-e = EUR

#UseHook
vkE2sc056::Shift ;  Die kleine Taste links neben Shift auf Shift mappen.

Capslock::Ctrl

;vk14sc03A::Ctrl ; Die Tab-Taste
;\vkA2sc01D::Capslock ; Die linke Strg-Taste unten

RAlt & a::
IfWinActive, emacs
{
  if(GetKeyState("Shift"))
        Send ^q!D
    else
        Send ^q!d       
    return
} else {
  if(GetKeyState("Shift"))
        SendRaw �
    else
        SendRaw �        
    return
}

RAlt & u::
IfWinActive, emacs
{
  if(GetKeyState("Shift"))
        Send ^q!\
    else
        Send ^q!|
    return
} else {
  if(GetKeyState("Shift"))
        SendRaw �
    else
        SendRaw �        
    return
}

RAlt & o::
IfWinActive, emacs
{
  if(GetKeyState("Shift"))
        Send ^q!V
    else
        Send ^q!v
    return
} else {
  if(GetKeyState("Shift"))
        SendRaw �
    else
        SendRaw �       
    return
}


RAlt & s::
IfWinActive, emacs
{
   Send ^q!_
   return
} else {
   SendRaw �
   return
}

RAlt & e::
IfWinActive, emacs
{
   Send ^q20254{Enter}
   return
} else {
   SendRaw �
   return
}

;LWin wird vom DWM benutzt. Darum wollen wir f�r Windows als Windows-Key nur die rechte Windowstaste benutzen.
~LWin Up:: return

