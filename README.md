[![build](https://github.com/7ym0n/dotfairy-ssh-manager/actions/workflows/build.yml/badge.svg)](https://github.com/7ym0n/dotfairy-ssh-manager/actions/workflows/build.yml)
[![License](http://img.shields.io/:license-GPL3-blue.svg)](LICENSE)
![Supports Emacs 27.1-28.x](https://img.shields.io/badge/Supports-Emacs_27.1_--_28.x-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)

# dotfairy-ssh-manager
Use SSH to manage remote servers like `xshell`, `mobaxterm` or other tools.

# Feature

- [x] session manager
- [x] save session to file
- [x] 2FA(TOTP)
- [x] proxy server
- [x] batch session execute command
- [x] upload and download

# TODOs
- [ ] Optimize upload and download, UI progress display
- [ ] support other solution

# Install
You need install `straight.el`, `ssh-manager` using this package install. first:
```elisp
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
```
Now use-package will use `straight.el`.
```elisp
(use-package ssh-manager
    :straight (el-patch :type git :host github :repo "7ym0n/dotfairy-ssh-manager"
                      :fork (:host github
                             :repo "7ym0n/ssh-manager"))
    :bind (("C-c m s s" . ssh-manager-switch-to-server)
        ("C-c m s c" . ssh-manager-create-ssh-remote)
        ("C-c m s d" . ssh-manager-remove-ssh-server)
        ("C-c m s e" . ssh-manager-edit-ssh-session-config)
        ("C-c m s u" . ssh-manager-upload-or-download-files-to-remote-host)
        ("C-c m s m" . ssh-manager)
        ("C-c M-s" . ssh-manager-show-ssh-session-groups))
    :config
    (with-eval-after-load 'dired
        (progn
            (define-key dired-mode-map (kbd "C-c C-<return>") 'ssh-manager-upload-or-download-files-to-remote-host))))
```
If you have many server management, it is recommended to use `ssh-manager-sessions` configuration. If you have TOTP, you may also need to set `ssh-manager-totp-hooks`.
```elisp
(setq ssh-manager-sessions '((:session-name "demo"
                              :kind "proxy"
                              :proxy-host "localhost"
                              :proxy-port "22"
                              :proxy-user "root"
                              :proxy-password ""
                              :remote-host "127.0.0.1"
                              :remote-port "22"
                              :remote-user "root"
                              :remote-password ""
                              :totp-kind "FreeOTP" ; default '(FreeOTP Custom)
                              :totp-key ""
                              :totp-message "verification code:")))
;; eg. default custom totp-hooks
(setq ssh-manager-totp-hooks '((:name "Custom"
    :function (lambda (totp-key)
        (setq totp-key (read-string "Enter TOTP key: "))))))
```
## Depends
- [sshpass](https://github.com/dora38/sshpass.git)
- oathtool
- rsync or scp
- ssh

Notes:

    choice scp upload or download files, it's don't show progress.

You can install sshpass through `ssh-manager-install-tools`.

MacOS:
```
brew install oath-tookit
# or
port install oath-tookit
```

Archlinux:
```
sudo pacman -Syy oath-tookit
```

debian:
```
sudo apt-get install oath-tookit
```

# Usage

1.  [ssh manager](#org3821008)
    1.  [create an session](#org1b67949)
    2.  [edit an session](#org2b497d2)
    3.  [remove an session](#org7bfe22d)
    4.  [batch an command line to remote server muilt-term](#org9746ee9)
    5.  [upload and download](#org0acc9e7)

Displays the buffer for batch execution of commands, use `M-x: ssh-manager-show-ssh-session-groups`.

SSH manager mode needs to be started for batch command execution. use `M-x: ssh-manager` or  open a file buffer, set `ssh-manager-mode`.

If you execute `ssh-manager-upload-or-download-files-to-remote-host` on the buffer, the current buffer file will be uploaded.
You need to upload a directory or multiple files. You need to enter `dired` and marked files or directory, after press the shortcut key `C-c C-<return>`.

<a id="org1b67949"></a>

## create an session
```
M-x: ssh-manager-create-ssh-remote
```

<a id="org2b497d2"></a>

## edit an session
```
M-x: ssh-manager-edit-ssh-session-config
```

<a id="org7bfe22d"></a>

## remove an session
```
M-x: ssh-manager-remove-ssh-server
```

<a id="org9746ee9"></a>

## batch an command line to remote server muilt-term

-   `C-c C-a` add a term buffer to groups.
-   `C-c C-r` remove a term buffer to groups.
-   `C-c C-.` send current command line to all term of groups
-   `C-c C-c` send current buffer to all term of groups.
-   `C-c C-e` send region to all term of groups.


<a id="org0acc9e7"></a>

## upload and download

-   upload a file to test server
-   upload muilt file to server
-   download a file to local /tmp
```
M-x: ssh-manager-upload-or-download-files-to-remote-host
```

<a id="orgdb7d181"></a>
