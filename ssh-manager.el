;;; ssh-manager.el --- A SSH manager remote servers  tools -*- lexical-binding: t -*-

;; Copyright © 2021, 7ym0n, all rights reserved.

;; Author: 7ym0n <bb.qnyd@gmail.com>
;; Keywords: ssh, tools
;; URL: https://github.com/7ym0n/dotfairy-ssh-manager
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (dash "2.19.0") (f "0.20.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A SSH session manager and files upload or download tools for Emacs.
;; It's like `xshell', `mobaxterm' or other tools same work.

;;; Code:
(require 'cl-generic)
(require 'cl-lib)
(require 'dash)
(require 'term)
(require 'f)
(require 'dired)
(require 'subr-x)
(require 'auth-source)
(require 'tramp)

(defcustom ssh-manager-sshpass-path (expand-file-name "sshpass" user-emacs-directory)
  "Set sshpass path."
  :group 'ssh-manager
  :type 'string)

(defcustom ssh-manager-sshpass-bin (concat ssh-manager-sshpass-path "/bin/sshpass")
  "Set sshpass bin."
  :group 'ssh-manager
  :type 'string)


(defcustom ssh-manager-ssh-config (expand-file-name "~/.ssh/config")
  "Set ssh config."
  :group 'ssh-manager
  :type 'string)

(defcustom ssh-manager-known-hosts (expand-file-name "~/.ssh/known_hosts")
  "Set ssh config."
  :group 'ssh-manager
  :type 'string)


(defcustom ssh-manager--sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc")
  "List of authentication sources.
Each entry is the authentication type with optional properties.
Entries are tried in the order in which they appear.
See Info node `(auth)Help for users' for details.

If an entry names a file with the \".gpg\" extension and you have
EPA/EPG set up, the file will be encrypted and decrypted
automatically.  See Info node `(epa)Encrypting/decrypting gpg files'
for details.

It's best to customize this with `\\[customize-variable]' because the choices
can get pretty complex."
  :group 'ssh-manager
  :type 'list)

(defcustom ssh-manager-store-dir
  (or (getenv "SSH_MANAGER_STORE_DIR") (expand-file-name ".ssh" user-emacs-directory))
  "Filename of the password-store folder."
  :group 'ssh-manager
  :type 'directory)

(defun ssh-manager--gpg-config ()
  "get gpg configs."
  (let ((store-dir (expand-file-name ssh-manager-store-dir))
        (default '()))
    (if (file-directory-p store-dir)
        (mapcar
         (lambda (file) (concat "gpg"
                           ":" (file-name-sans-extension (file-relative-name file store-dir))))
         (directory-files-recursively store-dir "\\.gpg\\'"))
      default)))

(defcustom ssh-manager-docker-use-names t
  "Whether use names instead of id."
  :type 'boolean
  :group 'ssh-manager)

(defun docker--running-containers ()
  "Collect docker running containers.

Return a list of containers of the form: \(ID NAME\)"
  (cl-loop for line in (cdr (ignore-errors (apply #'process-lines "docker"  (list "ps"))))
           for info = (split-string line "[[:space:]]+" t)
           collect (cons (car info) (last info))))

;;;###autoload
(defun ssh-manager-entries ()
  "Return a list of all password store entries."
  (interactive)
  (let ((hosts (ssh-manager--gpg-config)))
    (dolist (cand (tramp-parse-sconfig ssh-manager-ssh-config))
      (let ((user (if (car cand) (concat (car cand) "@")))
            (host (car (cdr cand))))
        (if host
            (push (concat "sshx" ":" user host) hosts))))
    ;; Known hosts
    (dolist (cand (tramp-parse-shosts ssh-manager-known-hosts))
      (let ((user (if (car cand) (concat (car cand) "@")))
            (host (car (cdr cand))))
        (if host
            (push (concat "sshx" ":" user host) hosts))))
    ;; Docker
    (dolist (cand (cl-loop for (id name) in (docker--running-containers)
                           collect (list "" (if ssh-manager-docker-use-names name id))))
      (let ((user (if (not (string-empty-p (car cand)))
                      (concat (car cand) "@")))
            (host (car (cdr cand))))
        (push (concat "docker:" user host) hosts)))
    (reverse hosts)))

(defun ssh-manager--lookup-secret (entry)
  "Return `entry' password."
  (if entry
      (funcall (plist-get entry :secret))
    nil))

;;;###autoload
(defun ssh-manager-sources ()
  "Set auth-sources."
  (interactive)
  (let ((entries (ssh-manager-entries)))
    (dolist (e entries)
      (let ((result (split-string e ":")))
        (if (string= (car result) "gpg")
            (push (format "%s/%s.gpg" ssh-manager-store-dir e) auth-sources))))))

(defun ssh-manager-get-entry (name)
  "Return ssh entry by `name'."
  (let ((sources (mapcar #'copy-sequence ssh-manager--sources)))
    (if (not (string-empty-p name))
        (push (format "%s/%s.gpg" ssh-manager-store-dir name) sources))
    (setq-local auth-sources sources)
    (let ((entry (auth-source-search :host name)))
      (if entry
          entry
        nil))))

(defcustom ssh-manager-totp-hooks '((:name "FreeOTP"
                                     :function (lambda (&rest args)
                                                 (with-temp-buffer
                                                   (or (apply
                                                        #'call-process "oathtool" nil t nil `("--totp" "-b" ,@args))
                                                       "")
                                                   (string-trim (buffer-string)))))
                                    (:name "Custom"
                                     :function (lambda (&rest args)
                                                 (setq args (read-string "Enter TOTP key: ")))))
  "Set totp verification code hook."
  :group 'ssh-manager
  :type 'list)

(defun ssh-manager--all-totp-name ()
  "Lookup all TOTP name."
  (let ((names '()))
    (dolist (hook ssh-manager-totp-hooks)
      (let ((name  (plist-get hook :name)))
        (push name names)))
    names))

(defun ssh-manager-lookup-totp-func (name &rest args)
  "Lookup TOTP hook function.
  Argument NAME TOTP kind name.
  Optional argument ARGS TOTP handler function args."
  (let ((fun nil))
    (dolist (hook ssh-manager-totp-hooks)
      (if (string= name (plist-get hook :name))
          (setq fun (plist-get hook :function))))
    (if (not (equal fun nil))
        (apply fun args))))

(defun ssh-manager--string-emptyp (str)
  "Check whether STRING is empty or nil."
  (or (string= "" str)
      (null str)))

;; ssh-manager-mode
(cl-defstruct ssh-manager-session-groups
  ;; contains the folders that are part of the current session
  servers
  (metadata (make-hash-table :test 'equal)))

(defvar ssh-manager--session-groups nil
  "Contain the `ssh-manager-session-groups' for the current Emacs instance.")

(defun ssh-manager-session-groups ()
  "Get all session by group."
  (or ssh-manager--session-groups (setq ssh-manager--session-groups (make-ssh-manager-session-groups))))

;;;###autoload
(defun ssh-manager-show-ssh-session-groups ()
  "Show SSH server groups."
  (interactive)
  (print (ssh-manager-session-groups-servers (ssh-manager-session-groups))))

;;;###autoload
(defun ssh-manager-add-this-ssh-session-to-groups ()
  "Add this SSH server session to groups."
  (interactive)
  (cl-pushnew (buffer-name) (ssh-manager-session-groups-servers (ssh-manager-session-groups)) :test 'equal))

;;;###autoload
(defun ssh-manager-remove-this-ssh-session-from-groups ()
  "Remove this SSH server session from groups."
  (interactive)
  (ssh-manager--remove-buffer-name-from-groups (buffer-name)))

(defun ssh-manager--remove-buffer-name-from-groups (buf-name)
  "Remove buffer name from groups.
  Argument BUF-NAME select buffer name."

  (setf (ssh-manager-session-groups-servers (ssh-manager-session-groups))
        (-remove-item buf-name (ssh-manager-session-groups-servers (ssh-manager-session-groups)))))

;;;###autoload
(defun ssh-manager-remove-ssh-session-from-groups (session)
  "Remove SSH server SESSION from groups."
  (interactive  (list (completing-read "Select server to connect: "
                                       (ssh-manager-session-groups-servers (ssh-manager-session-groups)))))
  (ssh-manager--remove-buffer-name-from-groups session))

(defun ssh-manager-send-cmd-to-session-groups (cmd)
  "Send CMD to session."
  (let ((current-buf (current-buffer)))
    (dolist (server (->> (ssh-manager-session-groups)
                         (ssh-manager-session-groups-servers)))
      (ssh-manager--send-cmd-to-buffer server cmd))
    (switch-to-buffer current-buf)))

(defvar ssh-manager-mode-map (let ((keymap (make-sparse-keymap)))
                               (define-key keymap (kbd "C-c C-c") 'ssh-manager-execute-buffer-cmd-to-ssh)
                               (define-key keymap (kbd "C-c C-e") 'ssh-manager-execute-region-cmd-to-ssh)
                               (define-key keymap (kbd "C-c C-.") 'ssh-manager-execute-current-line-cmd-to-ssh)
                               keymap)
  "Keymap for `ssh-manager-mode'.")

(define-derived-mode ssh-manager-mode prog-mode "SSH Manager Mode"
  (with-no-warnings
    (font-lock-fontify-buffer))
  (use-local-map ssh-manager-mode-map))

;;;###autoload
(defun ssh-manager-execute-current-line-cmd-to-ssh ()
  "Execute command to SSH server groups."
  (interactive)
  (let ((line (ssh-manager-read-current-line-cmd)))
    (ssh-manager-send-cmd-to-session-groups line)
    (reindent-then-newline-and-indent)))

;;;###autoload
(defun ssh-manager-read-current-line-cmd ()
  "Read current line command."
  (interactive)
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

;;;###autoload
(defun ssh-manager-execute-region-cmd-to-ssh ()
  "Execute region cmd to SSH."
  (interactive)
  (let ((begin (region-beginning))
        (end (region-end)))
    (ssh-manager-send-cmd-to-session-groups (buffer-substring begin end))))

;;;###autoload
(defun ssh-manager-execute-buffer-cmd-to-ssh ()
  "Execute buffer cmd to SSH."
  (interactive)
  (ssh-manager-send-cmd-to-session-groups (buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
(defun ssh-manager ()
  "Enable SSH manager mode."
  (interactive)
  (let ((buffer (generate-new-buffer "*SSH Manager*")))
    (set-buffer-major-mode buffer)
    (switch-to-buffer buffer)
    (funcall 'ssh-manager-mode)
    (setq buffer-offer-save t)))

;; SSH connect session
(cl-defstruct ssh-manager-session
  ;; contains the folders that are part of the current session
  servers
  folders
  (metadata (make-hash-table :test 'equal)))

(defcustom ssh-manager-session-file (expand-file-name (locate-user-emacs-file ".ssh-manager-session-v1"))
  "File where session information is stored."
  :group 'ssh-manager
  :type 'file)

(defvar ssh-manager--session nil
  "Contain the `ssh-manager-session' for the current Emacs instance.")
(defvar ssh-manager--show-message t
  "If non-nil, show debug message from `ssh-manager'.")

(defun ssh-manager--message  (format &rest args)
  "Wapper for message.
  Argument FORMAT format.
  Optional argument ARGS."
  (when ssh-manager--show-message
    (let ((inhibit-message (and (minibufferp)
                                (version< emacs-version "27.0"))))
      (apply #'message format args))))

(defun ssh-manager--info (format &rest args)
  "Display `ssh-manager' info message with FORMAT with ARGS."
  (ssh-manager--message "%s :: %s" (propertize "SSH" 'face 'success) (apply #'format format args)))

(defun ssh-manager--warn (format &rest args)
  "Display `ssh-manager' warn message with FORMAT with ARGS."
  (ssh-manager--message "%s :: %s" (propertize "SSH" 'face 'warning) (apply #'format format args)))

(defun ssh-manager--error (format &rest args)
  "Display `ssh-manager' error message with FORMAT with ARGS."
  (ssh-manager--message "%s :: %s" (propertize "SSH" 'face 'error) (apply #'format format args)))
(defun ssh-manager--read-from-file (file)
  "Read FILE content."
  (when (file-exists-p file)
    (cl-first (read-from-string (f-read-text file 'utf-8)))))

(defun ssh-manager--persist (file-name to-persist)
  "Persist TO-PERSIST in FILE-NAME.
  This function creates the parent directories if they don't exist
  yet."
  (let ((print-length nil)
        (print-level nil))
    ;; Create all parent directories:
    (apply #'f-mkdir (f-split (f-parent file-name)))
    (f-write-text (prin1-to-string to-persist) 'utf-8 file-name)))

(defun ssh-manager--persist-session (session)
  "Persist SESSION to `ssh-manager-session-file'."
  (ssh-manager--persist ssh-manager-session-file (make-ssh-manager-session
                                                  :servers (ssh-manager-session-servers session)
                                                  :folders (ssh-manager-session-folders session))))
(defun ssh-manager--load-default-session ()
  "Load default session."
  (setq ssh-manager--session (or (condition-case err
                                     (ssh-manager--read-from-file ssh-manager-session-file)
                                   (error (ssh-manager--error "Failed to parse the session %s, starting with clean one? "
                                                              (error-message-string err))
                                          nil))
                                 (make-ssh-manager-session))))

(defun ssh-manager-session ()
  "Get the session associated with the current buffer."
  (or ssh-manager--session (setq ssh-manager--session (ssh-manager--load-default-session))))

(defun ssh-manager--term-handle-close ()
  "Close current term buffer when `exit' from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (kill-buffer (process-buffer proc)))))))

;;; This code is referenced from multi-term.el
(defcustom ssh-manager-term-unbind-key-list
  '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>")
  "The key list that will need to be unbind."
  :type 'list
  :group 'ssh-manager)

(defcustom ssh-manager-term-bind-key-alist
  '(
    ("C-c C-c" . term-interrupt-subjob)
    ("<escape>" . ssh-manager-term-send-esc)
    ("C-p" . previous-line)
    ("C-n" . next-line)
    ("C-s" . isearch-forward)
    ("C-r" . isearch-backward)
    ("C-m" . ssh-manager-term-send-return)
    ("C-y" . term-paste)
    ("<backspace>" . term-send-backspace)
    ("M-p" . term-send-up)
    ("M-n" . term-send-down)
    ("M-M" . ssh-manager-term-send-forward-kill-word)
    ("M-N" . ssh-manager-term-send-backward-kill-word)
    ("<C-backspace>" . ssh-manager-term-send-backward-kill-word)
    ("C-c C-a" . ssh-manager-add-this-ssh-session-to-groups)
    ("C-c C-r" . ssh-manager-remove-this-ssh-session-from-groups)
    ("C-c M-a" . ssh-manager-show-ssh-session-groups)
    ("M-," . term-send-raw)
    ("M-." . comint-dynamic-complete))
  "The key alist that will need to be bind.
  If you do not like default setup, modify it, with (KEY . COMMAND) format."
  :type 'alist
  :group 'ssh-manager)

(defun ssh-manager-term-send-esc ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))

(defun ssh-manager-term-send-return ()
  "Use `term-send-raw-string' \\C\\-m instead `term-send-input'.
  Because `term-send-input' have bug that will duplicate input when you \\C\\-a and \\C\\-m in terminal."
  (interactive)
  (term-send-raw-string "\C-m"))

(defun ssh-manager-term-send-M-x ()
  "Type \\M\\-x in `term-mode'."
  (interactive)
  (term-send-raw-string "\ex"))

(defun ssh-manager-term-send-backward-kill-word ()
  "Backward kill word in term mode."
  (interactive)
  (term-send-raw-string "\C-w"))

(defun ssh-manager-term-send-forward-kill-word ()
  "Kill word in term mode."
  (interactive)
  (term-send-raw-string "\ed"))

(defun ssh-manager-keystroke-setup ()
  "Keystroke setup of `term-char-mode'.
  By default, the key bindings of `term-char-mode' conflict with user's keystroke.
  So this function unbinds some keys with `term-raw-map',
  and binds some keystroke with `term-raw-map'."
  (let (bind-key bind-command)
    ;; Unbind base key that conflict with user's keys-tokes.
    (cl-dolist (unbind-key ssh-manager-term-unbind-key-list)
      (cond
       ((stringp unbind-key) (setq unbind-key (read-kbd-macro unbind-key)))
       ((vectorp unbind-key) nil)
       (t (signal 'wrong-type-argument (list 'array unbind-key))))
      (define-key term-raw-map unbind-key nil))
    ;; Add some i use keys.
    ;; If you don't like my keystroke,
    ;; just modified `term-bind-key-alist'
    (cl-dolist (element ssh-manager-term-bind-key-alist)
      (setq bind-key (car element))
      (setq bind-command (cdr element))
      (cond
       ((stringp bind-key) (setq bind-key (read-kbd-macro bind-key)))
       ((vectorp bind-key) nil)
       (t (signal 'wrong-type-argument (list 'array bind-key))))
      (define-key term-raw-map bind-key bind-command))))
;;; end

(defun ssh-manager--init-term-mode (term-name)
  "Init term mode.
  Argument TERM-NAME set name."
  (remove-hook 'term-mode-hook 'ssh-manager-keystroke-setup)
  (add-hook 'term-mode-hook 'ssh-manager-keystroke-setup)
  (term-mode)
  (normal-erase-is-backspace-mode)
  (term-char-mode)
  (ssh-manager--term-handle-close)
  (add-hook 'kill-buffer-hook 'ssh-manager-term-kill-buffer-hook)
  (switch-to-buffer (format "*%s*" term-name))
  ;; use backspace delete
  (term-send-raw-string "stty erase '^?'\n"))

;;
;; sshpass for MacOS
;; curl -L https://raw.githubusercontent.com/kadwanev/bigboybrew/master/Library/Formula/sshpass.rb -o sshpass.rb
;; brew install sshpass.rb
;; sshpass with TOTP support https://github.com/dora38/sshpass.git
;; brew install oath-toolkit
;; yum install oathtool gnupg2
;; sshpass for Linux(debian, centos)
;; sudo apt-get install sshpass
;; sudo yum install sshpass
(defun ssh-manager-connect-ssh (server)
  "Connect to remote SERVER."
  (let* ((session-name (plist-get server :host))
         (kind (intern (plist-get server :kind)))
         (username (plist-get server :user))
         (password (if (string= kind "proxy")
                       (plist-get server :proxy-password)
                     (ssh-manager--lookup-secret server)))
         (port (plist-get server :port))
         (host (plist-get server :realhost))
         (totp-key (ssh-manager-lookup-totp-func (plist-get server :totp-kind)
                                                 (plist-get server :totp-key)))
         (totp-message (if (ssh-manager--string-emptyp (plist-get server :totp-message))
                           nil
                         (format "%s" (plist-get server :totp-message))))
         (proxy-kind (plist-get server :proxy-kind))
         (proxy-host (plist-get server :proxy-host))
         (proxy-port (plist-get server :proxy-port))
         (proxy-user (plist-get server :proxy-user))
         (index 1))
    (while (buffer-live-p (get-buffer (format "*%s<%s>*" session-name index)))
      (setq index (1+ index)))
    (let* ((argv '())
           (cmd ssh-manager-sshpass-bin)
           (term-name (format "%s<%s>" session-name index)))
      (cond ((string= proxy-kind "jumpserver")
             (setq argv (append argv `(,(format "%s@%s@%s@%s" proxy-user username host proxy-host) "-p" ,proxy-port))
                   cmd "ssh"))
            ((or (ssh-manager--string-emptyp proxy-kind)
                 (string= proxy-kind "other"))
             (if (not (ssh-manager--string-emptyp password))
                 (setq argv (append argv `("-p" ,password))))
             (if (not (ssh-manager--string-emptyp totp-key))
                 (setq argv (append argv `("-o" ,totp-key))))
             (if (not (ssh-manager--string-emptyp totp-message))
                 (setq argv (append argv `("-O" ,totp-message))))
             (setq argv (append argv `("ssh" "-o" "StrictHostKeychecking=no")))
             (if (not (ssh-manager--string-emptyp username))
                 (setq username (format "%s@" username)))
             (if (not (ssh-manager--string-emptyp host))
                 (setq argv (append argv `(,(format "%s%s" username host))))
               (ssh-manager--error "SSH hostname must be set. it cannot empty. "))
             (if (not (ssh-manager--string-emptyp port))
                 (setq argv (append argv `("-p" ,port))))
             (if (and (string= kind "proxy")
                      (not (ssh-manager--string-emptyp proxy-host))
                      (not (ssh-manager--string-emptyp proxy-user))
                      (not (ssh-manager--string-emptyp proxy-port)))
                 (setq argv (append argv `("-J" ,(format "%s@%s:%s" proxy-user proxy-host proxy-port)))))))
      ;; (ssh-manager--info (mapconcat 'identity `(,cmd ,@argv) " "))
      (set-buffer (apply 'make-term term-name
                         cmd
                         nil
                         argv))
      (ssh-manager--init-term-mode term-name))))

(defun ssh-manager-term-kill-buffer-hook ()
  "Function that hook `kill-buffer-hook'."
  (when (eq major-mode 'term-mode)
    ;; Quit the current subjob
    ;; when have alive process with current term buffer.
    (when (term-check-proc (current-buffer))
      ;; Quit sub-process.
      (term-quit-subjob))
    (ssh-manager--info "removed %s from server groups." (buffer-name (current-buffer)))
    (ssh-manager--remove-buffer-name-from-groups (buffer-name (current-buffer)))))

(defun ssh-manager--send-cmd-to-buffer (&optional buffer string)
  "Send STRING to a shell process associated with BUFFER.
  By default, BUFFER is \"*terminal*\" and STRING is empty."
  (let ((process (get-buffer-process (or buffer "*terminal*"))))
    (when (process-live-p process)
      (with-current-buffer (process-buffer process)
        (let ((input (or string "")))
          (cond ((derived-mode-p 'comint-mode)
                 (insert input)
                 (comint-send-input))
                ((derived-mode-p 'term-mode)
                 (term-send-string process input)
                 (term-send-input))))))))


(defun ssh-manager--call (cmd hostname &rest args)
  "Call ssh and docker."
  (let ((index 1))
    (while (buffer-live-p (get-buffer (format "*%s<%s>*" hostname index)))
      (setq index (1+ index)))
    (let ((term-name (format "%s<%s>" hostname index)))
      (set-buffer (apply 'make-term term-name
                         cmd
                         nil
                         args))
      (ssh-manager--init-term-mode term-name))))

;;;###autoload
(defun ssh-manager-switch-to-server (session)
  "Select SSH server to connect.
  Argument SESSION server session info."
  (interactive (list (completing-read "Select server to connect: "
                                      (ssh-manager-entries))))
  (let ((result (split-string session ":")))
    (cond ((string= (car result) "gpg")
           (ssh-manager-connect-ssh (car (ssh-manager-get-entry session))))
          ((string= (car result) "sshx")
           (ssh-manager--call "ssh" (cadr result) (cadr result)))
          ((string= (car result) "docker")
           (ssh-manager--call "docker" (cadr result) "exec" "-it" (cadr result) "sh")))))

;;;###autoload
(defun ssh-manager-install-tools ()
  "Install SSH manager tools."
  (interactive)
  (if (not (file-exists-p (expand-file-name ssh-manager-sshpass-bin)))
      (ssh-manager-exec-process "sh" "-c" (concat
                                           "rm -rf /tmp/sshpass &&"
                                           " git"
                                           " clone"
                                           " https://github.com/dora38/sshpass"
                                           " /tmp/sshpass"
                                           " &&"
                                           " cd /tmp/sshpass"
                                           " &&"
                                           " ./bootstrap"
                                           " &&"
                                           " ./configure --prefix=" (expand-file-name ssh-manager-sshpass-path)
                                           "&&"
                                           " make install; cd -")))
  (if (not (executable-find "oathtool"))
      (ssh-manager--info "your need install oathtool if used 2FA."))
  (ssh-manager--info "installed."))

;;;###autoload
(defun ssh-manager-exec-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.
Unlike `ssh-manager-call-process',
this pipes output to `standard-output' on the fly to
simulate 'exec' in the shell,
so batch scripts could run external programs
synchronously without sacrificing their output.

Warning: freezes indefinitely on any stdin prompt."
  ;; FIXME Is there any way to handle prompts?
  ;; (ssh-manager--info (mapconcat 'identity args " "))
  (with-temp-buffer
    (cons (let ((process
                 (make-process :name "ssh-manager"
                               :buffer (current-buffer)
                               :command (cons command (remq nil args))
                               :connection-type 'pipe))
                done-p)
            (set-process-filter
             process (lambda (_process output)
                       (princ output (current-buffer))
                       (princ output)))
            (set-process-sentinel
             process (lambda (process _event)
                       (when (memq (process-status process) '(exit stop))
                         (setq done-p t))))
            (while (not done-p)
              (sit-for 0.1))
            (process-exit-status process))
          (string-trim (buffer-string)))))

(defun ssh-manager--upload-or-download-files (server method cmd)
  "Use rsync command tool upload or download files.
Argument SERVER SSH session.
Argument METHOD select download or upload.
Argument CMD use rsync or scp."
  (let* ((argv '())
         (kind (plist-get server :kind))
         (password (if (string= kind 'proxy)
                       (plist-get server :proxy-password)
                     (ssh-manager--lookup-secret server)))
         (totp-kind (plist-get server :totp-kind))
         (totp-key (if (or (null totp-kind)
                           (string= totp-kind ""))
                       ""
                     (ssh-manager-lookup-totp-func totp-kind
                                                   (plist-get server :totp-key))))
         (totp-message (plist-get server :totp-message))
         (proxy-kind (plist-get server :proxy-kind))
         (proxy-host (plist-get server :proxy-host))
         (proxy-port (plist-get server :proxy-port))
         (proxy-user (plist-get server :proxy-user))
         (host (plist-get server :realhost))
         (port (plist-get server :port))
         (user (plist-get server :user)))
    (if (or (ssh-manager--string-emptyp proxy-kind)
            (string= proxy-kind "other"))
        (progn
          (if (not (ssh-manager--string-emptyp password))
              (setq argv (append argv `(,ssh-manager-sshpass-bin "-p" ,password))))
          (if (not (ssh-manager--string-emptyp totp-key))
              (setq argv (append argv `("-o" ,totp-key))))
          (if (not (ssh-manager--string-emptyp totp-message))
              (setq argv (append argv `("-O" ,(format "'%s'" totp-message)))))))
    (cond ((string= cmd "rsync")
           (if (string= kind 'proxy)
               (setq argv (append argv `("ssh" "-o" "'StrictHostKeychecking=no'" "-J" ,(format "%s@%s:%s" proxy-user proxy-host proxy-port))))
             (setq argv (append argv `("ssh" "-o" "'StrictHostKeychecking=no'"))))
           (if (not (ssh-manager--string-emptyp port))
               (setq argv (append argv `("-p" ,port))))
           (setq argv (list cmd "-r" "-P" (concat "-e \"" (mapconcat 'identity argv " ") "\""))))
          ((string= cmd "scp")
           (if (ssh-manager--string-emptyp host)
               (ssh-manager--error "SSH hostname must be set. HOST cannot empty. ")
             (setq argv (append argv `(,cmd "-r" "-o" "StrictHostKeychecking=no")))
             (if (string= kind 'proxy)
                 (setq argv (append argv `("-J" ,(format "%s@%s:%s" proxy-user proxy-host proxy-port)))))
             (if (not (ssh-manager--string-emptyp port))
                 (setq argv (append argv `("-P" ,port)))))))
    (if (and (not (ssh-manager--string-emptyp host))
             (not (ssh-manager--string-emptyp user)))
        (let* ((remote-dir-or-file (completing-read (format "Set remote file path (/home/%s): " user)
                                                    (ssh-manager-session-folders (ssh-manager-session))
                                                    nil nil))
               (target nil))
          (if (ssh-manager--string-emptyp remote-dir-or-file)
              (setq remote-dir-or-file (format "/home/%s" user)))
          (cl-pushnew remote-dir-or-file (ssh-manager-session-folders (ssh-manager-session)) :test 'equal)
          (ssh-manager--persist-session (ssh-manager-session))
          (cond ((string= method "upload")
                 (if (derived-mode-p 'dired-mode)
                     (setq argv (append argv `(,@(dired-get-marked-files) ,(format "%s@%s:%s" user host remote-dir-or-file))))
                   (if-let ((ask (y-or-n-p "Upload current buffer file? ")))
                       (setq target (buffer-file-name))
                     (setq target (read-file-name "Set upload for files: " )))
                   (setq argv (append argv `(,target ,(format "%s@%s:%s" user host remote-dir-or-file))))))
                ((string= method "download")
                 (if (derived-mode-p 'dired-mode)
                     (setq argv (append argv `(,(format "%s@%s:%s" user host remote-dir-or-file) ,(dired-current-directory))))
                   (setq target (read-file-name "Set download to: "))
                   (setq argv (append argv `(,(format "%s@%s:%s" user host remote-dir-or-file) ,target))))))))))
;;;###autoload
(defun ssh-manager-upload-or-download-files-to-remote-host (method)
  "SSH upload or download files.
Argument METHOD select download or upload."
  (interactive (list (completing-read "Select upload or download: "
                                      '(upload download))))
  (let ((entry-name (completing-read "Select connect to server: "
                                     (ssh-manager-entries))))

    (let* ((result (split-string entry-name ":"))
           (argv (if (not (string= (car result) "gpg"))
                     (if (string= method "upload")
                         (list (completing-read "Set local file path (~/): " nil nil nil)
                               (concat (cadr result) ":" (completing-read "Set remote file path (~/): " nil nil nil)))
                       (list (concat (cadr result) ":" (completing-read "Set remote file path (~/): " nil nil nil))
                             (completing-read "Set local file path (~/): " nil nil nil)))
                   nil)))
      (cond ((string= (car result) "gpg")
             (let ((entry (car (ssh-manager-get-entry entry-name))))
               (if (and entry
                        (not (string= (plist-get entry :proxy-kind) "jumpserver")))
                   (cond ((executable-find "rsync")
                          (if-let ((argv (ssh-manager--upload-or-download-files entry method "rsync")))
                              (ssh-manager-exec-process "sh" "-c" (mapconcat 'identity argv " "))))
                         ((executable-find "scp")
                          (if-let ((argv (ssh-manager--upload-or-download-files entry method "scp")))
                              (ssh-manager-exec-process "sh" "-c" (mapconcat 'identity argv " ")))))
                 (ssh-manager--error "jumpserver not support download and upload."))
               (if (derived-mode-p 'dired-mode)
                   (cond ((string= method "download")
                          (revert-buffer))
                         ((string= method "upload")
                          (dired-unmark-all-marks))))))
            ((string= (car result) "sshx")
             (cond ((executable-find "rsync")
                    (ssh-manager-exec-process "sh" "-c" (mapconcat 'identity
                                                                   (append '("rsync" "-avz" "-r" "--progress" "--delete") argv) " ")))
                   ((executable-find "scp")
                    (ssh-manager-exec-process "sh" "-c" (mapconcat 'identity (append '("scp" "-r") argv) " ")))))
            ((string= (car result) "docker")
             (ssh-manager-exec-process "sh" "-c" (mapconcat 'identity (append '("docker" "cp" "-q") argv) " "))))
      )))
(provide 'ssh-manager)
;;; ssh-manager.el ends here
