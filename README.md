[![build](https://github.com/7ym0n/dotfairy-ssh-manager/actions/workflows/build.yml/badge.svg)](https://github.com/7ym0n/dotfairy-ssh-manager/actions/workflows/build.yml)
[![License](http://img.shields.io/:license-GPL3-blue.svg)](LICENSE)
![Supports Emacs 27.1-28.x](https://img.shields.io/badge/Supports-Emacs_27.1_--_28.x-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)
[![MELPA](https://melpa.org/packages/ssh-manager-badge.svg)](https://melpa.org/#/ssh-manager)

# dotfairy-ssh-manager
Use SSH to manage remote servers like `xshell`, `mobaxterm` or other tools.

# Install
using package:
```elisp
(use-package ssh-manager
    :bind (("C-c m s s" . ssh-manager-switch-to-server)
    ("C-c m s c" . ssh-manager-create-ssh-remote)
    ("C-c m s d" . ssh-manager-remove-ssh-server)
    ("C-c m s e" . ssh-manager-edit-ssh-session-config)
    ("C-c m s u" . ssh-manager-upload-or-download-files-to-remote-host)
    ("C-c m s m" . ssh-manager)
    ("C-c M-s" . ssh-manager-show-ssh-session-groups))
```

# Usage
Create an session.
```elisp
M-x: ssh-manager-create-ssh-remote
```
Displays the buffer for batch execution of commands, use `M-x: ssh-manager-show-ssh-session-groups`.

SSH manager mode needs to be started for batch command execution. use `M-x: ssh-manager` or  open a file buffer, set `ssh-manager-mode`.

If you execute `ssh-manager-upload-or-download-files-to-remote-host` on the buffer, the current buffer file will be uploaded.
You need to upload a directory or multiple files. You need to enter `dired` and marked files or directory, after press the shortcut key `C-c C-<return>`.

# Screenshot
![](https://emacs-china.org/uploads/default/original/2X/7/766520af1e7970652b799dfe327d139887c776a8.png)
![](upload-and-download.gif)
