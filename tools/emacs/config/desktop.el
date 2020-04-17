;; -*- lexical-binding: t; -*-
;;
;; Configure desktop environment settings, including both
;; window-management (EXWM) as well as additional system-wide
;; commands.

(require 's)
(require 'f)
(require 'dash)
(require 'exwm)
(require 'exwm-config)
(require 'exwm-randr)
(require 'exwm-systemtray)

(defun pactl (cmd)
  (shell-command (concat "pactl " cmd))
  (message "Volume command: %s" cmd))

(defun volume-mute () (interactive) (pactl "set-sink-mute @DEFAULT_SINK@ toggle"))
(defun volume-up () (interactive) (pactl "set-sink-volume @DEFAULT_SINK@ +5%"))
(defun volume-down () (interactive) (pactl "set-sink-volume @DEFAULT_SINK@ -5%"))

(defun brightness-up ()
  (interactive)
  (shell-command "xbacklight -inc 5")
  (message "Brightness increased"))

(defun brightness-down ()
  (interactive)
  (shell-command "xbacklight -dec 5")
  (message "Brightness decreased"))

(defun lock-screen ()
  (interactive)
  ;; A sudoers configuration is in place that lets me execute this
  ;; particular command without having to enter a password.
  ;;
  ;; The reason for things being set up this way is that I want
  ;; xsecurelock.service to be started as a system-wide service that
  ;; is tied to suspend.target.
  (shell-command "/usr/bin/sudo /usr/bin/systemctl start xsecurelock.service"))

(defun set-xkb-layout (layout)
  "Set the current X keyboard layout."

  (shell-command (format "setxkbmap %s" layout))
  (message "Set X11 keyboard layout to '%s'" layout))

(defun create-window-name ()
  "Construct window names to be used for EXWM buffers by
  inspecting the window's X11 class and title.

  A lot of commonly used applications either create titles that
  are too long by default, or in the case of web
  applications (such as Cider) end up being constructed in
  awkward ways.

  To avoid this issue, some rewrite rules are applied for more
  human-accessible titles."

  (pcase (list (or exwm-class-name "unknown") (or exwm-title "unknown"))
    ;; In Cider windows, rename the class and keep the workspace/file
    ;; as the title.
    (`("Google-chrome" ,(and (pred (lambda (title) (s-ends-with? " - Cider" title))) title))
     (format "Cider<%s>" (s-chop-suffix " - Cider" title)))

    ;; Attempt to detect IRCCloud windows via their title, which is a
    ;; combination of the channel name and network.
    ;;
    ;; This is what would often be referred to as a "hack". The regexp
    ;; will not work if a network connection buffer is selected in
    ;; IRCCloud, but since the title contains no other indication that
    ;; we're dealing with an IRCCloud window
    (`("Google-chrome"
       ,(and (pred (lambda (title)
                     (s-matches? "^[\*\+]\s#[a-zA-Z0-9/\-]+\s\|\s[a-zA-Z\.]+$" title)))
             title))
     (format "IRCCloud<%s>" title))

    ;; The Virus Lounge should be, well ...
    (`("Google-chrome"
       ,(and (pred (lambda (title) (s-contains? "Meet - mng-biyw-xbb" title))) title))
     "The Virus Lounge")

    ;; For other Chrome windows, make the title shorter.
    (`("Google-chrome" ,title)
     (format "Chrome<%s>" (s-truncate 42 (s-chop-suffix " - Google Chrome" title))))

    ;; Gnome-terminal -> Term
    (`("Gnome-terminal" ,title)
     ;; fish-shell buffers contain some unnecessary whitespace and
     ;; such before the current working directory. This can be
     ;; stripped since most of my terminals are fish shells anyways.
     (format "Term<%s>" (s-trim-left (s-chop-prefix "fish" title))))

    ;; For any other application, a name is constructed from the
    ;; window's class and name.
    (`(,class ,title) (format "%s<%s>" class (s-truncate 12 title)))))

;; EXWM launch configuration
;;
;; This used to use use-package, but when something breaks use-package
;; it doesn't exactly make debugging any easier.

(let ((titlef (lambda ()
                (exwm-workspace-rename-buffer (create-window-name)))))
  (add-hook 'exwm-update-class-hook titlef)
  (add-hook 'exwm-update-title-hook titlef))

(fringe-mode 3)
(exwm-enable)

;; 's-N': Switch to certain workspace
(setq exwm-workspace-number 10)
(dotimes (i 10)
  (exwm-input-set-key (kbd (format "s-%d" i))
                      `(lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))

;; Launch applications / any command  with completion (dmenu style!)
(exwm-input-set-key (kbd "s-d") #'counsel-linux-app)
(exwm-input-set-key (kbd "s-x") #'ivy-run-external-command)
(exwm-input-set-key (kbd "s-p") #'ivy-password-store)

;; Add X11 terminal selector to a key
(exwm-input-set-key (kbd "C-x t") #'ts/switch-to-terminal)

;; Toggle between line-mode / char-mode
(exwm-input-set-key (kbd "C-c C-t C-t") #'exwm-input-toggle-keyboard)

;; Volume keys
(exwm-input-set-key (kbd "<XF86AudioMute>") #'volume-mute)
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'volume-up)
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'volume-down)

;; Brightness keys
(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") #'brightness-down)
(exwm-input-set-key (kbd "<XF86MonBrightnessUp>") #'brightness-up)
(exwm-input-set-key (kbd "<XF86Display>") #'lock-screen)

;; Keyboard layouts (these are bound separately in Cyrillic
;; because I don't use reverse-im)
;; (-map
;;  (lambda (pair)
;;    (exwm-input-set-key
;;     (kbd (format "s-%s" (cadr pair)))
;;     `(lambda () (interactive) (set-xkb-layout ,(car pair)))))
;;  '(("de" "k d")
;;    ("de" "л в")
;;    ("no" "k n")
;;    ("no" "л т")
;;    ("ru" "k r")
;;    ("ru" "л к")
;;    ("us" "k u")
;;    ("us" "л г")))

;; Line-editing shortcuts
(exwm-input-set-simulation-keys
 '(([?\C-d] . delete)
   ([?\C-w] . ?\C-c)))

;; Show time & battery status in the mode line
(display-time-mode)
(display-battery-mode)

;; enable display of X11 system tray within Emacs
(exwm-systemtray-enable)

;; Configure xrandr (multi-monitor setup).
;;
;; This makes some assumptions about how the machine (vauxhall) is
;; connected to my home setup during the COVID19 isolation period.

(defun set-randr-config (screens)
  (setq exwm-randr-workspace-monitor-plist
        (-flatten (-map (lambda (screen)
                          (-map (lambda (screen-id) (list screen-id (car screen))) (cdr screen)))
                        screens))))

(defun randr-layout-single ()
  "Laptop screen only!"
  (interactive)
  (set-randr-config '(("eDP1" (number-sequence 0 9))))
  (shell-command "xrandr --output eDP1 --auto --primary")
  (shell-command "xrandr --output HDMI1 --off")
  (shell-command "xrandr --output DP2 --off")
  (exwm-randr-refresh))

(defun randr-layout-all ()
  "Use all screens at home."
  (interactive)
  (set-randr-config
   '(("eDP1" 0)
     ("HDMI1" 1 2 3 4 5)
     ("DP2" 6 7 8 9)))

  (shell-command "xrandr --output HDMI1 --right-of eDP1 --auto --primary")
  (shell-command "xrandr --output DP2 --right-of HDMI1 --auto")
  (exwm-randr-refresh))

(defun randr-layout-wide-only ()
  "Use only the wide screen at home."
  (interactive)
  (set-randr-config
   '(("eDP1" 8 9 0)
     ("HDMI1" 1 2 4 5 6 7)))

  (shell-command "xrandr --output DP2 --off")
  (shell-command "xrandr --output HDMI1 --right-of eDP1 --auto --primary")
  (exwm-randr-refresh))

(exwm-randr-enable)

(exwm-input-set-key (kbd "s-m s") #'randr-layout-single)
(exwm-input-set-key (kbd "s-m a") #'randr-layout-all)
(exwm-input-set-key (kbd "s-m w") #'randr-layout-wide-only)

;; Let buffers move seamlessly between workspaces by making them
;; accessible in selectors on all frames.
(setq exwm-workspace-show-all-buffers t)
(setq exwm-layout-show-all-buffers t)

(provide 'desktop)
