;;; agent-shell-alert.el --- Desktop notifications via OSC and macOS native -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Send desktop notifications from Emacs.  In GUI mode on macOS, uses
;; native UNUserNotificationCenter via a dynamic module
;; (agent-shell-alert-mac.dylib).  In terminal mode, sends OSC escape
;; sequences: OSC 9 (iTerm2, Ghostty, WezTerm), OSC 99 (kitty), or
;; OSC 777 (VTE/urxvt), with DCS passthrough for tmux/GNU screen.
;;
;; The JIT-compile-on-first-use pattern for the native dylib is
;; inspired by vterm's approach to vterm-module.so.  DCS wrapping
;; for terminal passthrough is inspired by clipetty's OSC 52 support.

;;; Code:

(declare-function agent-shell-alert-mac-notify "agent-shell-alert-mac")

(defvar agent-shell-alert-osc-protocol 'osc-9
  "OSC protocol for terminal notifications.

One of `osc-9' (iTerm2, Ghostty, WezTerm), `osc-99' (kitty),
or `osc-777' (VTE/urxvt).")

(defconst agent-shell-alert--screen-dcs-start "\eP"
  "DCS start escape for GNU screen.")

(defconst agent-shell-alert--tmux-dcs-start "\ePtmux;\e"
  "DCS start escape for tmux.")

(defconst agent-shell-alert--dcs-end "\e\\"
  "DCS end escape sequence.")

(defun agent-shell-alert--osc-payload (title body)
  "Build the raw OSC notification payload for TITLE and BODY.

Returns the escape sequence string (without DCS wrapping).

  (agent-shell-alert--osc-payload \"Done\" \"Task finished\")
  ;; => \"\\e]9;Done: Task finished\\a\"  (for osc-9)"
  (pcase agent-shell-alert-osc-protocol
    ('osc-9
     (format "\e]9;%s: %s\a" title body))
    ('osc-99
     (format "\e]99;i=1:d=0;%s: %s\e\\" title body))
    ('osc-777
     (format "\e]777;notify;%s;%s\e\\" title body))
    (_
     (format "\e]9;%s: %s\a" title body))))

(defun agent-shell-alert--dcs-wrap (payload)
  "Wrap PAYLOAD in DCS passthrough if inside tmux or GNU screen.

Returns PAYLOAD unchanged when running in a bare terminal.

  ;; Inside tmux:
  (agent-shell-alert--dcs-wrap \"\\e]9;hi\\a\")
  ;; => \"\\ePtmux;\\e\\e]9;hi\\a\\e\\\\\"

  ;; Bare terminal:
  (agent-shell-alert--dcs-wrap \"\\e]9;hi\\a\")
  ;; => \"\\e]9;hi\\a\""
  (cond
   ((getenv "TMUX" (selected-frame))
    (concat agent-shell-alert--tmux-dcs-start
            payload
            agent-shell-alert--dcs-end))
   ((when-let ((term (getenv "TERM" (selected-frame))))
      (string-match-p "^screen" term))
    (concat agent-shell-alert--screen-dcs-start
            payload
            agent-shell-alert--dcs-end))
   (t payload)))

(defun agent-shell-alert--mac-available-p ()
  "Return non-nil if the macOS native notification module is loaded."
  (and (eq system-type 'darwin)
       (display-graphic-p)
       (fboundp 'agent-shell-alert-mac-notify)))

(defun agent-shell-alert--source-directory ()
  "Return the directory containing agent-shell-alert source files."
  (file-name-directory
   (or load-file-name buffer-file-name default-directory)))

(defun agent-shell-alert--module-path ()
  "Return the expected path of the compiled native module."
  (expand-file-name
   (concat "agent-shell-alert-mac" module-file-suffix)
   (agent-shell-alert--source-directory)))

(defun agent-shell-alert--compile-mac-module ()
  "Compile the macOS native notification module.
Returns non-nil on success."
  (let* ((source (expand-file-name "agent-shell-alert-mac.m"
                                   (agent-shell-alert--source-directory)))
         (output (agent-shell-alert--module-path))
         (include-dir (expand-file-name
                       "include"
                       (file-name-directory
                        (directory-file-name invocation-directory)))))
    (when (file-exists-p source)
      (zerop
       (call-process "cc" nil nil nil
                     "-Wall" "-O2" "-fPIC"
                     "-shared" "-fobjc-arc"
                     (concat "-I" include-dir)
                     "-framework" "UserNotifications"
                     "-framework" "Foundation"
                     "-o" output source)))))

(defun agent-shell-alert--try-load-mac-module ()
  "Try to load the macOS native notification module, compiling if needed.
Returns non-nil on success."
  (when (and (eq system-type 'darwin)
             (display-graphic-p)
             module-file-suffix
             (not (fboundp 'agent-shell-alert-mac-notify)))
    (unless (file-exists-p (agent-shell-alert--module-path))
      (agent-shell-alert--compile-mac-module))
    (ignore-errors
      (module-load (agent-shell-alert--module-path)))
    (unless (fboundp 'agent-shell-alert-mac-notify)
      (message "agent-shell-alert: native module unavailable; \
install Xcode command line tools (`xcode-select --install') \
for macOS desktop notifications")))
  (fboundp 'agent-shell-alert-mac-notify))

(defun agent-shell-alert-notify (title body)
  "Send a desktop notification with TITLE and BODY.

In GUI Emacs on macOS, uses native notifications via
UNUserNotificationCenter.  In terminal Emacs, emits an OSC
escape sequence wrapped in DCS passthrough when inside tmux
or GNU screen.

  (agent-shell-alert-notify \"agent-shell\" \"Turn complete\")"
  (cond
   ((agent-shell-alert--mac-available-p)
    (agent-shell-alert-mac-notify title body))
   ((not (display-graphic-p))
    (send-string-to-terminal
     (agent-shell-alert--dcs-wrap
      (agent-shell-alert--osc-payload title body))))))

(agent-shell-alert--try-load-mac-module)

(provide 'agent-shell-alert)

;;; agent-shell-alert.el ends here
