#Persistent
SetTimer, MoveMouse

MoveMouse:
If ( A_TimeIdle > 133000 ) {
  MouseMove, 1 , 1,, R
  MouseMove, -1,-1,, R
}
Return

; Keymappings für amerikanisches Dvorak-Layout mit deutschen Umlauten: AltGr-u = ü AltGr-Shift-u = Ü, usw. AltGr-s = ß, AltGr-e = EUR

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
        SendRaw Ä
    else
        SendRaw ä        
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
        SendRaw Ü
    else
        SendRaw ü        
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
        SendRaw Ö
    else
        SendRaw ö       
    return
}


RAlt & s::
IfWinActive, emacs
{
   Send ^q!_
   return
} else {
   SendRaw ß
   return
}

RAlt & e::
IfWinActive, emacs
{
   Send ^q20254{Enter}
   return
} else {
   SendRaw €
   return
}

;LWin wird vom DWM benutzt. Darum wollen wir für Windows als Windows-Key nur die rechte Windowstaste benutzen.
~LWin Up:: return

