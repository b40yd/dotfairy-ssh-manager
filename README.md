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
You need install `quelpa.el`, `ssh-manager` using this package install. first:
```elisp
;; Use quelpa install packages
(use-package quelpa
  :init
  (setq quelpa-upgrade-p dotfairy-quelpa-upgrade
        quelpa-update-melpa-p nil
        quelpa-checkout-melpa-p nil
        quelpa-dir (expand-file-name "quelpa" user-emacs-directory)))

(use-package quelpa-use-package)
(eval-when-compile
  (require 'quelpa-use-package))
```
Now use-package will use `straight.el`.
```elisp
(use-package ssh-manager
  :ensure nil
  :quelpa (ssh-manager :fetcher github :repo "7ym0n/dotfairy-ssh-manager")
  :bind (("C-c m s s" . ssh-manager-switch-to-server)
        ("C-c m s u" . ssh-manager-upload-or-download-files-to-remote-host)
        ("C-c m s m" . ssh-manager)
        ("C-c M-s" . ssh-manager-show-ssh-session-groups))
  :init
  (setq ssh-manager-sshpass-path (expand-file-name "sshpass" user-emacs-directory)))

```
If you have many server management, it is recommended to use `auth-sources` configuration. If you have TOTP, you may also need to set `ssh-manager-totp-hooks`.
```elisp
host "demo" kind "proxy" proxy-host "localhost" proxy-port "22" proxy-user "root" proxy-password "" realhost "127.0.0.1" port "22" user "root" password "" totp-kind "FreeOTP" ; default '(FreeOTP Custom) totp-key "" totp-message "verification code:"
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
    1.  [batch an command line to remote server muilt-term](#org9746ee9)
    2.  [upload and download](#org0acc9e7)

Displays the buffer for batch execution of commands, use `M-x: ssh-manager-show-ssh-session-groups`.

SSH manager mode needs to be started for batch command execution. use `M-x: ssh-manager` or  open a file buffer, set `ssh-manager-mode`.

If you execute `ssh-manager-upload-or-download-files-to-remote-host` on the buffer, the current buffer file will be uploaded.
You need to upload a directory or multiple files. You need to enter `dired` and marked files or directory, after press the shortcut key `C-c C-<return>`.


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
