;;; gemini-cli.el --- Emacs integration for the Gemini CLI

;; Copyright (C) 2024 Kyure-A
;;
;; Author: Kyure-A <github.com/Kyure-A>
;; Keywords: ai, tools, convenience
;; URL: https://github.com/Kyure-A/gemini-cli.el
;; Package-Requires: ((emacs "30.1") (transient "0.4.0") (eat "0.8"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides an Emacs integration for the Google Gemini CLI.
;; It allows you to interact with the Gemini AI from within Emacs,
;; providing a seamless workflow for coding, writing, and more.
;;
;; To get started, run `M-x gemini-cli-start`.
;;
;;; Code:

(require 'ansi-color)
(require 'transient)

;;;;; Customization Variables

(defgroup gemini-cli nil
  "Emacs integration for the Gemini CLI."
  :group 'applications)

(defcustom gemini-cli-start-command '("npx" "https://github.com/google-gemini/gemini-cli")
  "The base command and arguments to launch the Gemini CLI."
  :type '(repeat string)
  :group 'gemini-cli)

(defcustom gemini-cli-model nil
  "The Gemini model to use (e.g., \"gemini-1.5-pro\")."
  :type '(string :tag "Model Name")
  :group 'gemini-cli)

(defcustom gemini-cli-yolo-mode nil
  "If non-nil, run in YOLO mode, automatically accepting all actions."
  :type 'boolean
  :group 'gemini-cli)

(defcustom gemini-cli-keymap-prefix (kbd "C-c g")
  "Prefix key for `gemini-cli` commands."
  :type 'key-sequence
  :group 'gemini-cli)

(defcustom gemini-cli-start-key (kbd "s")
  "Key to bind `gemini-cli-start` under `gemini-cli-keymap-prefix`."
  :type 'key-sequence
  :group 'gemini-cli)

(defcustom gemini-cli-stop-key (kbd "q")
  "Key to bind `gemini-cli-stop` under `gemini-cli-keymap-prefix`."
  :type 'key-sequence
  :group 'gemini-cli)

(defcustom gemini-cli-restart-key (kbd "r")
  "Key to bind `gemini-cli-restart` under `gemini-cli-keymap-prefix`."
  :type 'key-sequence
  :group 'gemini-cli)

(defcustom gemini-cli-send-buffer-key (kbd "b")
  "Key to bind `gemini-cli-send-buffer-as-context` under `gemini-cli-keymap-prefix`."
  :type 'key-sequence
  :group 'gemini-cli)

(defcustom gemini-cli-send-region-key (kbd "R")
  "Key to bind `gemini-cli-send-region-as-context` under `gemini-cli-keymap-prefix`."
  :type 'key-sequence
  :group 'gemini-cli)

(defcustom gemini-cli-extract-code-key (kbd "e")
  "Key to bind `gemini-cli-extract-last-code-block` under `gemini-cli-keymap-prefix`."
  :type 'key-sequence
  :group 'gemini-cli)

(defcustom gemini-cli-prompt-templates
  '(("Summarize Buffer" "Summarize the following content:
@buffer")
    ("Explain Region" "Explain the following code:
@region")
    ("Fix Error" "Fix the following error in the code:
{{prompt}}
@buffer"))
  "A list of prompt templates. Each element is a list of (NAME TEMPLATE-STRING).
Placeholders:
- `@buffer`: Replaced by the current buffer's content.
- `@region`: Replaced by the current region's content.
- `{{prompt}}`: Prompts the user for additional input."
  :type '(repeat (list string string))
  :group 'gemini-cli)

;;;;; Core Process Management

(defvar-local gemini-cli--process nil
  "The Gemini CLI process object for the current buffer.")

(defvar-local gemini-cli--in-code-block nil
  "Non-nil if currently inside a code block.")

(defvar-local gemini-cli--code-block-lang nil
  "The language of the current code block.")

(defun gemini-cli--fontify-code-blocks (start end)
  "Fontify code blocks in the buffer between START and END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "^\s-*\(```\)\(.*\)?$" end t)
      (let* ((fence-start (match-beginning 1))
             (fence-end (match-end 1))
             (lang (if (match-string 2) (string-trim (match-string 2)) nil)))
        (if gemini-cli--in-code-block
            (progn
              (setq gemini-cli--in-code-block nil)
              (setq gemini-cli--code-block-lang nil)
              (put-text-property fence-start fence-end 'face 'font-lock-comment-face))
          (setq gemini-cli--in-code-block t)
          (setq gemini-cli--code-block-lang lang)
          (put-text-property fence-start fence-end 'face 'font-lock-comment-face))))
    (when gemini-cli--in-code-block
      (let ((mode (pcase gemini-cli--code-block-lang
                    ("elisp" 'emacs-lisp-mode)
                    ("python" 'python-mode)
                    ("js" 'js-mode)
                    ("json" 'json-mode)
                    ("sh" 'sh-mode)
                    ("bash" 'sh-mode)
                    ("diff" 'diff-mode)
                    (_ nil))))
        (when mode
          (font-lock-fontify-region start end nil mode))))))

(defun gemini-cli--make-urls-clickable (start end)
  "Make URLs clickable in the buffer between START and END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward url-regexp end t)
      (let ((url (match-string 0)))
        (add-text-properties (match-beginning 0) (match-end 0)
                             '(face font-lock-url-face
                               help-echo "mouse-2: follow link"
                               mouse-face highlight
                               local-map (keymap (mouse-2 . browse-url))
                               url t))))))

(defun gemini-cli--format-output (start end)
  "Format the output in the buffer between START and END."
  (gemini-cli--fontify-code-blocks start end)
  (gemini-cli--make-urls-clickable start end))

(defun gemini-cli--process-filter (proc string)
  "Process filter to handle output from the Gemini CLI."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (old-point (point)))
        (goto-char (process-mark proc))
        (insert string)
        (ansi-color-apply-on-region (process-mark proc) (point))
        (gemini-cli--format-output old-point (point))
        (set-marker (process-mark proc) (point))))))

(defun gemini-cli--process-sentinel (proc msg)
  "Process sentinel to handle process termination."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (message "Gemini CLI process terminated: %s" msg)
      (setq-local gemini-cli--process nil)
      (setq-local gemini-cli--in-code-block nil)
      (setq-local gemini-cli--code-block-lang nil))))

(defun gemini-cli-send-input (input)
  "Send INPUT to the Gemini CLI process."
  (if (and (local-variable-p 'gemini-cli--process) gemini-cli--process (process-live-p gemini-cli--process))
      (progn
        (process-send-string gemini-cli--process (concat input "
"))
        (with-current-buffer (process-buffer gemini-cli--process)
          (add-to-history 'gemini-cli--input-history input)))
    (error "Gemini CLI is not running. Run `gemini-cli-start` first")))

;;;;; UI Functions

(defvar-local gemini-cli--input-history nil
  "History of sent inputs.")

(defconst gemini-cli--slash-commands
  '("/help" "/tools" "/memory show" "/mcp")
  "A list of common Gemini CLI slash commands.")

(defun gemini-cli-read-slash-command ()
  "Read a slash command from the user with completion."
  (interactive)
  (let ((command (completing-read "Slash command: " gemini-cli--slash-commands nil t)))
    (gemini-cli-send-input command)))

(defun gemini-cli-read-prompt ()
  "Read a prompt from the user and send it."
  (interactive)
  (let ((prompt (read-string "Prompt: " nil 'gemini-cli--input-history)))
    (gemini-cli-send-input prompt)))

(defun gemini-cli-read-at-command ()
  "Read a file path to add to the prompt."
  (interactive)
  (let ((file (read-file-name "File to add: ")))
    (gemini-cli-send-input (concat "@" file))))

(defun gemini-cli--expand-template (template-string)
  "Expand TEMPLATE-STRING by replacing placeholders."
  (let ((expanded-string template-string))
    (when (string-match-p "@buffer" expanded-string)
      (setq expanded-string (replace-regexp-in-string "@buffer" (buffer-string) expanded-string)))
    (when (string-match-p "@region" expanded-string)
      (if (use-region-p)
          (setq expanded-string (replace-regexp-in-string "@region" (buffer-substring (region-beginning) (region-end)) expanded-string))
        (error "No region active for @region placeholder.")))
    (when (string-match-p "{{prompt}}" expanded-string)
      (let ((user-input (read-string "Enter additional input for template: ")))
        (setq expanded-string (replace-regexp-in-string "{{prompt}}" user-input expanded-string))))
    expanded-string))

(defun gemini-cli-send-template ()
  "Select a prompt template and send it to the Gemini CLI."
  (interactive)
  (let* ((template-names (mapcar #'car gemini-cli-prompt-templates))
         (selected-name (completing-read "Select template: " template-names nil t))
         (selected-template (cdr (assoc selected-name gemini-cli-prompt-templates)))
         (expanded-prompt (gemini-cli--expand-template selected-template)))
    (gemini-cli-send-input expanded-prompt)))

(defun gemini-cli--write-to-temp-file (content)
  "Write CONTENT to a temporary file and return its path."
  (let ((temp-file (make-temp-file "gemini-cli-" nil ".txt")))
    (with-temp-file temp-file
      (insert content))
    temp-file))

(defun gemini-cli--send-content-as-context (content type)
  "Send CONTENT to Gemini as context from a temporary file.

TYPE is a string indicating the source of the content (e.g., "buffer", "region")."
  (let ((temp-file (gemini-cli--write-to-temp-file content)))
    (gemini-cli-send-input (concat "@" temp-file))
    (message "Sent %s content as context from %s" type temp-file)))

(defun gemini-cli-send-buffer-as-context ()
  "Send the content of the current buffer as context to the Gemini CLI."
  (interactive)
  (gemini-cli--send-content-as-context (buffer-string) "buffer"))

(defun gemini-cli-send-region-as-context ()
  "Send the content of the active region as context to the Gemini CLI."
  (interactive)
  (if (use-region-p)
      (gemini-cli--send-content-as-context (buffer-substring (region-beginning) (region-end)) "region")
    (error "No region active. Mark a region first.")))

(defun gemini-cli-extract-last-code-block ()
  "Extract the last code block from the `*gemini-cli*` buffer."
  (interactive)
  (let ((cli-buffer (get-buffer "*gemini-cli*")))
    (unless cli-buffer
      (error "Gemini CLI buffer not found."))
    (with-current-buffer cli-buffer
      (let* ((end (point-max))
             (start (save-excursion
                      (re-search-backward "```" nil t)))
             (code (buffer-substring-no-properties start end)))
        (unless start
          (error "No code block found in the Gemini CLI buffer."))
        (if (called-interactively-p 'any)
            (let ((action (read-char-choice "Extract to (i)nsert or (c)opy? " '(?i ?c))))
              (pcase action
                (?i (with-current-buffer (window-buffer)
                      (insert code)))
                (?c (kill-new code)
                    (message "Code block copied to kill ring."))))
          (kill-new code))))))

;;;###autoload
(transient-define-prefix gemini-cli ()
  "Transient command for interacting with the Gemini CLI."
  ["Gemini CLI"
   ("p" "Prompt" gemini-cli-read-prompt)
   ("s" "Slash Command" gemini-cli-read-slash-command)
   ("t" "Template" gemini-cli-send-template)
   ("@" "Add File" gemini-cli-read-at-command)
   ("b" "Send Buffer as Context" gemini-cli-send-buffer-as-context)
   ("r" "Send Region as Context" gemini-cli-send-region-as-context)
   ("e" "Extract Last Code Block" gemini-cli-extract-last-code-block)]
  ["Process"
   ("R" "Restart" gemini-cli-restart)
   ("q" "Quit" gemini-cli-stop)])

;;;;; Major Mode and Entry Points

(define-derived-mode gemini-cli-mode comint-mode "Gemini-CLI"
  "Major mode for interacting with the Gemini CLI.
\{gemini-cli-mode-map}"
  :syntax-table nil
  :abbrev-table nil
  (setq-local comint-prompt-regexp "^> ")
  (setq-local comint-process-echoes t)
  (setq-local comint-get-old-input (lambda () "")))

;;;###autoload
(defun gemini-cli-start ()
  "Start the Gemini CLI process and open its interaction buffer."
  (interactive)
  (let* ((buf-name "*gemini-cli*")
         (buffer (get-buffer-create buf-name))
         (command (append gemini-cli-start-command
                          (when gemini-cli-model (list "--model" gemini-cli-model))
                          (when gemini-cli-yolo-mode (list "--yolo")))))
    (with-current-buffer buffer
      (gemini-cli-mode)
      (when (and (local-variable-p 'gemini-cli--process) gemini-cli--process (process-live-p gemini-cli--process))
        (error "Gemini CLI is already running in %s" buf-name))
      (setq-local gemini-cli--process (apply #'start-process "gemini-cli" buffer command))
      (set-process-filter gemini-cli--process #'gemini-cli--process-filter)
      (set-process-sentinel gemini-cli--process #'gemini-cli--process-sentinel)
      (set-buffer-process buffer gemini-cli--process))
    (display-buffer buffer)))

;;;###autoload
(defun gemini-cli-stop ()
  "Stop the Gemini CLI process."
  (interactive)
  (if-let ((process (get-buffer-process "*gemini-cli*")))
      (progn
        (quit-process process)
        (message "Gemini CLI stopped."))
    (message "Gemini CLI is not running.")))

(defun gemini-cli-restart ()
  "Restart the Gemini CLI process."
  (interactive)
  (gemini-cli-stop)
  (sleep-for 0.1)
  (gemini-cli-start))

;;;;; Keybindings

(define-key global-map gemini-cli-keymap-prefix 'gemini-cli)
(define-key (lookup-key global-map gemini-cli-keymap-prefix) gemini-cli-start-key 'gemini-cli-start)
(define-key (lookup-key global-map gemini-cli-keymap-prefix) gemini-cli-stop-key 'gemini-cli-stop)
(define-key (lookup-key global-map gemini-cli-keymap-prefix) gemini-cli-restart-key 'gemini-cli-restart)
(define-key (lookup-key global-map gemini-cli-keymap-prefix) gemini-cli-send-buffer-key 'gemini-cli-send-buffer-as-context)
(define-key (lookup-key global-map gemini-cli-keymap-prefix) gemini-cli-send-region-key 'gemini-cli-send-region-as-context)
(define-key (lookup-key global-map gemini-cli-keymap-prefix) gemini-cli-extract-code-key 'gemini-cli-extract-last-code-block)

(provide 'gemini-cli)


;;; gemini-cli.el ends here
