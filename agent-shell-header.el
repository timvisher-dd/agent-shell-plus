;;; agent-shell-header.el --- Header rendering for agent-shell -*- lexical-binding: t; -*-

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
;; Header and mode line helpers for agent-shell.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)
(require 'dom)
(require 'image)
(require 'svg nil :noerror)
(require 'agent-shell-helpers)
(require 'agent-shell-usage)

(declare-function agent-shell--state "agent-shell")
(defvar agent-shell-header-style)
(defvar agent-shell-show-busy-indicator)
(defvar agent-shell-busy-indicator-frames)

(defvar-local agent-shell--header-cache nil
  "Cache for graphical headers (no need for regenerating regularly).

A buffer-local hash table mapping cache keys to header strings.")

(cl-defun agent-shell--make-header-model (state &key qualifier bindings)
  "Create a header model alist from STATE, QUALIFIER, and BINDINGS.
The model contains all inputs needed to render the graphical header."
  (let* ((model-name (or (map-elt (seq-find (lambda (model)
                                              (string= (map-elt model :model-id)
                                                       (map-nested-elt state '(:session :model-id))))
                                            (map-nested-elt state '(:session :models)))
                                  :name)
                         (map-nested-elt state '(:session :model-id))))
         (mode-id (map-nested-elt state '(:session :mode-id)))
         (mode-name (when mode-id
                      (or (agent-shell--resolve-session-mode-name
                           mode-id
                           (agent-shell--get-available-modes state))
                          mode-id))))
    `((:buffer-name . ,(map-nested-elt state '(:agent-config :buffer-name)))
      (:icon-name . ,(map-nested-elt state '(:agent-config :icon-name)))
      (:model-id . ,(map-nested-elt state '(:session :model-id)))
      (:model-name . ,model-name)
      (:mode-id . ,mode-id)
      (:mode-name . ,mode-name)
      (:directory . ,default-directory)
      (:frame-width . ,(frame-pixel-width))
      (:font-height . ,(frame-char-height))
      (:font-size . ,(when-let* (((display-graphic-p))
                                 (font (face-attribute 'default :font))
                                 ((fontp font)))
                       (font-get font :size)))
      (:background-mode . ,(frame-parameter nil 'background-mode))
      (:context-indicator . ,(agent-shell--context-usage-indicator))
      (:busy-indicator-frame . ,(agent-shell--busy-indicator-frame))
      (:qualifier . ,qualifier)
      (:bindings . ,bindings))))

(defun agent-shell--header-cache-key (model)
  "Generate a cache key from header MODEL.
Joins all values from the model alist."
  (mapconcat (lambda (pair) (format "%s" (cdr pair)))
             model "|"))

(cl-defun agent-shell--make-header (state &key qualifier bindings)
  "Return header text for current STATE.

STATE should contain :agent-config with :icon-name, :buffer-name, and
:session with :mode-id and :modes for displaying the current session mode.

QUALIFIER: Any text to prefix BINDINGS row with.

BINDINGS is a list of alists defining key bindings to display, each with:
  :key         - Key string (e.g., \"n\")
  :description - Description to display (e.g., \"next hunk\")"
  (unless state
    (error "STATE is required"))
  (let* ((header-model (agent-shell--make-header-model state :qualifier qualifier :bindings bindings))
         (text-header (format " %s%s%s @ %s%s%s"
                              (propertize (concat (map-elt header-model :buffer-name) " Agent")
                                          'font-lock-face 'font-lock-variable-name-face)
                              (if (map-elt header-model :model-name)
                                  (concat " ➤ " (propertize (map-elt header-model :model-name) 'font-lock-face 'font-lock-negation-char-face))
                                "")
                              (if (map-elt header-model :mode-name)
                                  (concat " ➤ " (propertize (map-elt header-model :mode-name) 'font-lock-face 'font-lock-type-face))
                                "")
                              (propertize (string-remove-suffix "/" (abbreviate-file-name (map-elt header-model :directory)))
                                          'font-lock-face 'font-lock-string-face)
                              (if (map-elt header-model :context-indicator)
                                  (concat " " (map-elt header-model :context-indicator))
                                "")
                              (if (map-elt header-model :busy-indicator-frame)
                                  (map-elt header-model :busy-indicator-frame)
                                ""))))
    (pcase agent-shell-header-style
      ((or 'none (pred null)) nil)
      ('text text-header)
      ('graphical
       (if (display-graphic-p)
           ;; +------+
           ;; | icon | Top text line
           ;; |      | Bottom text line
           ;; +------+
           ;; [Qualifier] Bindings row (optional, last row)
           (let* ((cache-key (agent-shell--header-cache-key header-model))
                  (cached (progn
                            (unless agent-shell--header-cache
                              (setq agent-shell--header-cache (make-hash-table :test #'equal)))
                            (map-elt agent-shell--header-cache cache-key))))
             (or cached
                 (let* ((char-height (map-elt header-model :font-height))
                        (font-size (map-elt header-model :font-size))
                        (has-bindings (or bindings qualifier))
                        (image-height (* 3 char-height))
                        (image-width image-height)
                        (text-height char-height)
                        (top-padding-height (/ font-size 2))
                        (bottom-padding-height (if has-bindings (+ text-height top-padding-height) top-padding-height))
                        (row-spacing (if has-bindings font-size 0))
                        (total-height (+ image-height row-spacing top-padding-height bottom-padding-height))
                        ;; icon position
                        (icon-x 6)
                        (icon-y top-padding-height)
                        ;; text position right of the icon area
                        (icon-text-x (+ icon-x image-width 10))
                        (icon-text-y (+ icon-y char-height (/ (- char-height font-size) 2)))
                        ;; Bindings positioned below the icon area
                        (bindings-x icon-x)
                        (bindings-y (+ image-height font-size row-spacing))
                        (svg (svg-create (map-elt header-model :frame-width) total-height))
                        (icon-filename
                         (if (map-elt header-model :icon-name)
                             (agent-shell--fetch-agent-icon (map-elt header-model :icon-name))
                           (agent-shell--make-agent-fallback-icon (map-elt header-model :buffer-name) 100)))
                        (image-type (or (agent-shell--image-type-to-mime icon-filename)
                                        "image/png")))
                   ;; Icon
                   (when (and icon-filename image-type)
                     (svg-embed svg icon-filename
                                image-type nil
                                :x icon-x :y icon-y :width image-width :height image-height))
                   ;; Top text line
                   (svg--append svg (let ((text-node (dom-node 'text
                                                               `((x . ,icon-text-x)
                                                                 (y . ,icon-text-y)
                                                                 (font-size . ,font-size)))))
                                      ;; Agent name
                                      (dom-append-child text-node
                                                        (dom-node 'tspan
                                                                  `((fill . ,(face-attribute 'font-lock-variable-name-face :foreground)))
                                                                  (concat (map-elt header-model :buffer-name) " Agent")))
                                      ;; Model name (optional)
                                      (when (map-elt header-model :model-name)
                                        ;; Add separator arrow
                                        (dom-append-child text-node
                                                          (dom-node 'tspan
                                                                    `((fill . ,(face-attribute 'default :foreground))
                                                                      (dx . "8"))
                                                                    "➤"))
                                        ;; Add model name
                                        (dom-append-child text-node
                                                          (dom-node 'tspan
                                                                    `((fill . ,(face-attribute 'font-lock-negation-char-face :foreground))
                                                                      (dx . "8"))
                                                                    (map-elt header-model :model-name))))
                                      ;; Session mode (optional)
                                      (when (map-elt header-model :mode-id)
                                        ;; Add separator arrow
                                        (dom-append-child text-node
                                                          (dom-node 'tspan
                                                                    `((fill . ,(face-attribute 'default :foreground))
                                                                      (dx . "8"))
                                                                    "➤"))
                                        ;; Add session mode text
                                        (dom-append-child text-node
                                                          (dom-node 'tspan
                                                                    `((fill . ,(or (face-attribute 'font-lock-type-face :foreground nil t)
                                                                                   "#6699cc"))
                                                                      (dx . "8"))
                                                                    (map-elt header-model :mode-name))))
                                      (when (map-elt header-model :context-indicator)
                                        (let* (;; Extract the face from the propertized string
                                               (face (get-text-property 0 'face (map-elt header-model :context-indicator)))
                                               ;; Get the foreground color from the face
                                               (color (if face
                                                          (face-attribute face :foreground nil t)
                                                        (face-attribute 'default :foreground))))
                                          (dom-append-child text-node
                                                            (dom-node 'tspan
                                                                      `((fill . ,color)
                                                                        (dx . "8"))
                                                                      (substring-no-properties (map-elt header-model :context-indicator))))))
                                      (when (map-elt header-model :busy-indicator-frame)
                                        (dom-append-child text-node
                                                          (dom-node 'tspan
                                                                    `((fill . ,(face-attribute 'default :foreground))
                                                                      (dx . "8"))
                                                                    (map-elt header-model :busy-indicator-frame))))
                                      text-node))
                   ;; Bottom text line
                   (svg-text svg (string-remove-suffix "/" (abbreviate-file-name (map-elt header-model :directory)))
                             :x icon-text-x :y (+ icon-text-y text-height (- char-height font-size))
                             :font-size font-size
                             :fill (face-attribute 'font-lock-string-face :foreground))
                   ;; Bindings row (last row if bindings or qualifier present)
                   (when (or bindings qualifier)
                     (svg--append svg (let ((text-node (dom-node 'text
                                                                 `((x . ,bindings-x)
                                                                   (y . ,bindings-y)
                                                                   (font-size . ,font-size))))
                                            (first t))
                                        ;; Add qualifier if present
                                        (when qualifier
                                          (dom-append-child text-node
                                                            (dom-node 'tspan
                                                                      `((fill . ,(face-attribute 'default :foreground)))
                                                                      qualifier))
                                          (setq first nil))
                                        (dolist (binding bindings)
                                          (when (map-elt binding :description)
                                            ;; Add key (XML-escape angle brackets)
                                            (dom-append-child text-node
                                                              (dom-node 'tspan
                                                                        `((fill . ,(face-attribute 'help-key-binding :foreground))
                                                                          ,@(unless first '((dx . "8"))))
                                                                        (replace-regexp-in-string
                                                                         "<" "&lt;"
                                                                         (replace-regexp-in-string
                                                                          ">" "&gt;"
                                                                          (map-elt binding :key)))))
                                            (setq first nil)
                                            ;; Add space and description
                                            (dom-append-child text-node
                                                              (dom-node 'tspan
                                                                        `((fill . ,(face-attribute 'default :foreground))
                                                                          (dx . "8"))
                                                                        (map-elt binding :description)))))
                                        text-node)))
                   (let ((result (format " %s" (with-temp-buffer
                                                 (svg-insert-image svg)
                                                 (buffer-string)))))
                     (map-put! agent-shell--header-cache cache-key result)
                     result))))
         text-header))
      (_ text-header))))

(defun agent-shell--image-type-to-mime (filename)
  "Convert image type from FILENAME to MIME type string.
Returns a MIME type like \"image/png\" or \"image/jpeg\"."
  (let* ((type (image-supported-file-p filename))
         (ext (downcase (or (file-name-extension filename) ""))))
    (cond
     (type
      (pcase type
        ('svg "image/svg+xml")
        (_ (format "image/%s" type))))
     ((member ext '("png" "gif" "webp"))
      (format "image/%s" ext))
     ((member ext '("jpg" "jpeg"))
      "image/jpeg")
     ((string= ext "svg")
      "image/svg+xml")
     (t nil))))

(defun agent-shell--update-header-and-mode-line ()
  "Update header and mode line based on `agent-shell-header-style'."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (cond
   ((eq agent-shell-header-style 'graphical)
    (setq header-line-format (agent-shell--make-header (agent-shell--state))))
   ((memq agent-shell-header-style '(text none nil))
    (setq header-line-format (agent-shell--make-header (agent-shell--state)))
    (force-mode-line-update))))

(defun agent-shell--fetch-agent-icon (icon-name)
  "Download icon with ICON-NAME from GitHub, only if it exists, and save as binary.

Names can be found at https://github.com/lobehub/lobe-icons/tree/master/packages/static-png

Icon names starting with https:// are downloaded directly from that location."
  (when icon-name
    (let* ((mode (if (eq (frame-parameter nil 'background-mode) 'dark) "dark" "light"))
           (is-url (string-prefix-p "https://" (downcase icon-name)))
           (url (if is-url
                    icon-name
                  (concat "https://raw.githubusercontent.com/lobehub/lobe-icons/refs/heads/master/packages/static-png/"
                          mode "/" icon-name)))
           (filename (if is-url
                         ;; For URLs, sanitize to create readable filename
                         ;; e.g., "https://opencode.ai/favicon.svg" -> "opencode.ai-favicon.svg"
                         (replace-regexp-in-string
                          "[/:]" "-"
                          (replace-regexp-in-string
                           "^https?://" ""
                           url))
                       ;; For lobe-icons names, use the original filename
                       (file-name-nondirectory url)))
           (cache-dir (file-name-concat (temporary-file-directory) "agent-shell" mode))
           (cache-path (expand-file-name filename cache-dir)))
      (unless (file-exists-p cache-path)
        (make-directory cache-dir t)
        (let ((buffer (url-retrieve-synchronously url t t 5.0)))
          (when buffer
            (with-current-buffer buffer
              (goto-char (point-min))
              (if (re-search-forward "^HTTP/[0-9.]+ 200" nil t)
                  (progn
                    (re-search-forward "\r?\n\r?\n")
                    (let ((coding-system-for-write 'no-conversion))
                      (write-region (point) (point-max) cache-path)))
                (message "Icon fetch failed: %s" url)))
            (kill-buffer buffer))))
      (when (file-exists-p cache-path)
        cache-path))))

(defun agent-shell--make-agent-fallback-icon (icon-name width)
  "Create SVG icon with first character of ICON-NAME and WIDTH.
Return file path of the generated SVG."
  (when (and icon-name (not (string-empty-p icon-name)))
    (let* ((icon-text (char-to-string (string-to-char icon-name)))
           (mode (if (eq (frame-parameter nil 'background-mode) 'dark) "dark" "light"))
           (filename (format "%s-%s.svg" icon-name width))
           (cache-dir (file-name-concat (temporary-file-directory) "agent-shell" mode))
           (cache-path (expand-file-name filename cache-dir))
           (font-size (* 0.7 width))
           (x (/ width 2))
           (y (/ width 2)))
      (unless (file-exists-p cache-path)
        (make-directory cache-dir t)
        (let ((svg (svg-create width width :stroke "white" :fill "black")))
          (svg-text svg icon-text
                    :x x :y y
                    :text-anchor "middle"
                    :dominant-baseline "central"
                    :font-weight "bold"
                    :font-size font-size
                    ;; :font-family "Monaco, Courier New, Courier, monospace"
                    :font-family (face-attribute 'default :family)
                    :fill (face-attribute 'default :foreground))
          (with-temp-buffer
            (let ((standard-output (current-buffer)))
              (svg-print svg))
            (write-region (point-min) (point-max) cache-path))))
      cache-path)))

(defun agent-shell--busy-indicator-frame ()
  "Return busy frame string or nil if not busy."
  (when-let* ((agent-shell-show-busy-indicator)
              ((eq 'busy (map-nested-elt (agent-shell--state) '(:heartbeat :status))))
              (frames (pcase agent-shell-busy-indicator-frames
                        ('wave '("▁" "▂" "▃" "▄" "▅" "▆" "▇" "█" "▇" "▆" "▅" "▄" "▃" "▂"))
                        ('dots-block '("⣷" "⣯" "⣟" "⡿" "⢿" "⣻" "⣽" "⣾"))
                        ('dots-round '("⢎⡰" "⢎⡡" "⢎⡑" "⢎⠱" "⠎⡱" "⢊⡱" "⢌⡱" "⢆⡱"))
                        ('wide '("░   " "░░  " "░░░ " "░░░░" "░░░ " "░░  " "░   " "    "))
                        ((pred listp) agent-shell-busy-indicator-frames)
                        (_ '("▁" "▂" "▃" "▄" "▅" "▆" "▇" "█" "▇" "▆" "▅" "▄" "▃" "▂")))))
    (concat " " (seq-elt frames (mod (map-nested-elt (agent-shell--state) '(:heartbeat :value))
                                     (length frames))))))

(defun agent-shell--mode-line-format ()
  "Return `agent-shell''s mode-line format.

Typically includes the container indicator, model, session mode and activity
or nil if unavailable.

For example: \" [C] [Sonnet] [Accept Edits] ░░░ \".
Shows \" [C]\" when running in a container."
  (when-let* (((derived-mode-p 'agent-shell-mode))
              ((memq agent-shell-header-style '(text none nil))))
    (concat (when agent-shell-container-command-runner
              (propertize " [C]"
                          'face 'font-lock-constant-face
                          'help-echo "Running in container"))
            (when-let ((model-name (or (map-elt (seq-find (lambda (model)
                                                            (string= (map-elt model :model-id)
                                                                     (map-nested-elt (agent-shell--state) '(:session :model-id))))
                                                          (map-nested-elt (agent-shell--state) '(:session :models)))
                                                :name)
                                       (map-nested-elt (agent-shell--state) '(:session :model-id)))))
              (propertize (format " [%s]" model-name)
                          'face 'font-lock-variable-name-face
                          'help-echo (format "Model: %s" model-name)))
            (when-let ((mode-name (agent-shell--resolve-session-mode-name
                                   (map-nested-elt (agent-shell--state) '(:session :mode-id))
                                   (agent-shell--get-available-modes (agent-shell--state)))))
              (propertize (format " [%s]" mode-name)
                          'face 'font-lock-type-face
                          'help-echo (format "Session Mode: %s" mode-name)))
            (when-let ((indicator (agent-shell--context-usage-indicator)))
              (concat " " indicator))
            (agent-shell--busy-indicator-frame))))

(defun agent-shell--setup-modeline ()
  "Set up the modeline to display session mode.
Uses :eval so the mode updates automatically when state changes."
  (setq-local mode-line-misc-info
              (append mode-line-misc-info
                      '((:eval (agent-shell--mode-line-format))))))

(provide 'agent-shell-header)

;;; agent-shell-header.el ends here
