;;
;; An autohotkey script that provides gtk-emacs-key-theme like keybinding on Windows
;; forked from https://github.com/usi3/emacs.ahk
;;
#InstallKeybdHook
#UseHook

; The following line is a contribution of NTEmacs wiki http://www49.atwiki.jp/ntemacs/pages/20.html
SetKeyDelay 0

; Applications you want to disable emacs-like keybindings
; (Please comment out applications you don't use)
is_target()
{
    IfWinActive,ahk_class ConsoleWindowClass ; Cygwin
        Return 1
    IfWinActive,ahk_class MEADOW ; Meadow
        Return 1
    IfWinActive,ahk_class cygwin/x X rl-xterm-XTerm-0
        Return 1
    IfWinActive,ahk_class MozillaUIWindowClass ; keysnail on Firefox
        Return 1
    ; Avoid VMwareUnity with AutoHotkey
    IfWinActive,ahk_class VMwareUnityHostWndClass
        Return 1
    IfWinActive,ahk_class Vim ; GVIM
        Return 1
    IfWinActive,ahk_class SWT_Window0 ; Eclipse
        Return 1
    IfWinActive,ahk_class Xming X
        Return 1
    IfWinActive,ahk_class SunAwtFrame
        Return 1
    IfWinActive,ahk_class Emacs ; NTEmacs
        Return 1
    IfWinActive,ahk_class XEmacs ; XEmacs on Cygwin
        Return 1
    IfWinActive,ahk_exe WindowsTerminal.exe ; Windows Terminal
        Return 1
    IfWinActive,ahk_exe msrdc.exe ; WSLg
        Return 1
    IfWinActive,ahk_exe Code.exe ; Visual Studio Code
        Return 1
    IfWinActive,ahk_exe devenv.exe ; Visual Studio
        Return 1
    Return 0
}

; Act as Enter to Right Control
is_enter_pressed()
{
    If GetKeyState("Enter", "P") {
        Return 1
    } Else {
        Return 0
    }
}

;
; <ctrl>b
; move cursor backward
;
^b::
    If is_target() {
        Send %A_ThisHotkey%
    } Else {
        If is_enter_pressed() {
            Send {RControl Up}{Left}{RControl Down}
        } Else {
            Send {Left}
        }
    }
    Return

;
; <shift><ctrl>b
; move cursor backward selecting chars
;
+^b::
    If is_target() {
        Send %A_ThisHotkey%
    } Else {
        If is_enter_pressed() {
            Send {RControl Up}+{Left}{RControl Down}
        } Else {
            Send +{Left}
        }
    }
    Return

;
; <ctrl>f
; move cursor forward
;
^f::
    If is_target() {
        Send %A_ThisHotkey%
    } Else {
        If is_enter_pressed() {
            Send {RControl Up}{Right}{RControl Down}
        } Else {
            Send {Right}
        }
    }
    Return

;
; <shift><ctrl>f
; move cursor forward selecting chars
;
+^f::
    If is_target() {
        Send %A_ThisHotkey%
    } Else {
        If is_enter_pressed() {
            Send {RControl Up}+{Right}{RControl Down}
        } Else {
            Send +{Right}
        }
    }
    Return

;
; <ctrl>p
; move cursor up
;
^p::
    If is_target() {
        Send %A_ThisHotkey%
    } Else {
        If is_enter_pressed() {
            Send {RControl Up}{Up}{RControl Down}
        } Else {
            Send {Up}
        }
    }
    Return

;
; <shift><ctrl>p
; move cursor up selecting chars
;
+^p::
    If is_target() {
        Send %A_ThisHotkey%
    } Else {
        If is_enter_pressed() {
            Send {RControl Up}+{Up}{RControl Down}
        } Else {
            Send +{Up}
        }
    }
    Return

;
; <ctrl>n
; move cursor down
;
^n::
    If is_target() {
        Send %A_ThisHotkey%
    } Else {
        If is_enter_pressed() {
            Send {RControl Up}{Down}{RControl Down}
        } Else {
            Send {Down}
        }
    }
    Return

;
; <shift><ctrl>n
; move cursor down selecting chars
;
+^n::
    If is_target() {
        Send %A_ThisHotkey%
    } Else {
        If is_enter_pressed() {
            Send {RControl Up}+{Down}{RControl Down}
        } Else {
            Send +{Down}
        }
    }
    Return

;
; <ctrl>d
; delete following char
;
^d::
    If is_target() {
        Send %A_ThisHotkey%
    } Else {
        If is_enter_pressed() {
            Send {RControl Up}{Del}{RControl Down}
        } Else {
            Send {Del}
        }
    }
    Return

;
; <ctrl>h
; delete previous char(Backspace)
;
^h::
    If is_target() {
        Send %A_ThisHotkey%
    } Else {
        If is_enter_pressed() {
            Send {RControl Up}{Backspace}{RControl Down}
        } Else {
            Send {Backspace}
        }
    }
    Return

;
; <ctrl>a
; move cursor beginning of current line
;
^a::
    If is_target() {
        Send %A_ThisHotkey%
    } Else {
        If is_enter_pressed() {
            Send {RControl Up}{Home}{RControl Down}
        } Else {
            Send {Home}
        }
    }
    Return

;
; <shift><ctrl>a
; move cursor beginning of current line selecting chars
;
+^a::
    If is_target() {
        Send %A_ThisHotkey%
    } Else {
        If is_enter_pressed() {
            Send {RControl Up}+{Home}{RControl Down}
        } Else {
            Send +{Home}
        }
    }
    Return

;
; <ctrl>e
; move cursor end of current line
;
^e::
    If is_target() {
        Send %A_ThisHotkey%
    } Else {
        If is_enter_pressed() {
            Send {RControl Up}{End}{RControl Down}
        } Else {
            Send {End}
        }
    }
    Return

;
; <shift><ctrl>e
; move cursor end of current line selecting chars
;
+^e::
    If is_target() {
        Send %A_ThisHotkey%
    } Else {
        If is_enter_pressed() {
            Send {RControl Up}+{End}{RControl Down}
        } Else {
            Send +{End}
        }
    }
    Return

;
; <ctrl>y
; paste
;
^y::
    If is_target() {
        Send %A_ThisHotkey%
    } Else {
        If is_enter_pressed() {
            Send {RControl Up}^v{RControl Down}
        } Else {
            Send ^v
        }
    }
    Return

;
; <ctrl>w
; backward_kill_word
;
^w::
    If is_target() {
        Send %A_ThisHotkey%
    } Else {
        If is_enter_pressed() {
            Send {RControl Up}{Shift Down}^{Left}{Shift Up}{RControl Down}
            Sleep 10
            Send {RControl Up}{Del}{RControl Down}
        } Else {
            Send {Shift Down}^{Left}{Shift Up}
            Sleep 10
            Send {Del}
        }
    }
    Return

;
; <ctrl>k
; delete chars from cursor to end of line
;
^k::
    If is_target() {
          Send %A_ThisHotkey%
    } Else {
        If is_enter_pressed() {
            Send {RControl Up}{Shift Down}{End}{Shift Up}{RControl Down}
            Sleep 10
            Send {RControl Up}{Del}{RControl Down}
        } Else {
            Send {Shift Down}{End}{Shift Up}
            Sleep 10
            Send {Del}
        }
    }
    Return

;
; <ctrl>u
; delete chars from cursor to beginning of line
;
^u::
    If is_target() {
        Send %A_ThisHotkey%
    } Else {
        If is_enter_pressed() {
            Send {RControl Up}{Shift Down}{Home}{Shift Up}{RControl Down}
            Sleep 10
            Send {RControl Up}{Del}{RControl Down}
        } Else {
            Send {Shift Down}{Home}{Shift Up}
            Sleep 10
            Send {Del}
        }
    }
    Return

;
; <Alt>a
; select all
;
!a::
    If is_target() {
        Send %A_ThisHotkey%
    } Else {
        Send ^a
    }
    Return

;
; <Alt>w
; close window
;
!w::
    If is_target() {
        Send %A_ThisHotkey%
    } Else {
        Send ^w
    }
    Return

;
; OneShot Modifier
; Left control as Escape if post alone
;
*LControl::
    Send {LControl Down}
    Return
*LControl Up::
    Send {LControl Up}
    If (A_PriorKey == "LControl" And A_TimeSincePriorHotkey < 200) {
        Suspend On
        Send {Esc}
        Suspend Off
    }
    Return

;
; OneShot Modifier
; Enter as Right Control if post alone
;
*Enter::
    Send {Blind}{RControl Down}
    Return
*Enter Up::
    Send {Blind}{RControl Up}
    If (A_PriorKey == "Enter" And A_TimeSincePriorHotkey < 200) {
        Suspend On
        Send {Blind}{Enter}
        Suspend Off
    }
    Return

;
; Windows Terminal
; double tap Left Shift
;
~LShift::
    If (A_PriorHotkey == A_ThisHotkey And A_TimeSincePriorHotkey < 200) {
        Send ^+{F12}
    } Else {
        KeyWait LShift
        Return
    }
    Return

;
; Explorer
; double tap Right Shift
;
~RShift::
    If (A_PriorHotkey == A_ThisHotkey And A_TimeSincePriorHotkey < 200) {
        If (WinExist("ahk_class CabinetWClass")) {
            WinActivate
        } Else If ("C:\Windows\explorer.exe") {
            run "C:\Windows\explorer.exe"
        }
    } Else {
        KeyWait RShift
        Return
    }
    Return

;
; Mouse operation
;
F2::Send {Browser_Forward}
F8::Send {Browser_Back}
^WheelRight::Send #^{Right}
^WheelLeft::Send #^{Left}
!F24::Send !{Esc}
!+`::Send !+{Esc}