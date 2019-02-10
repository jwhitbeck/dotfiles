;;; -*- lexical-binding: t; -*-

;;; Lazy-loaded mu4e customizations

(require 'mu4e)
(require 'mu4e-contrib)
(require 'smtpmail)
(require 'org-mu4e)
(require 'my-package)

(my-use-packages dash)

(defcustom my-mu4e-inboxes nil
  "List of paths relative to the `mu4e-maildir' that are inboxes."
  :type '(repeat string)
  :group 'my-mu4e)

(defun my-mu4e-inbox-query ()
  "Return a mu4e headers search for the inboxes defined in `my-mu4e-inboxes'."
  (string-join (mapcar (lambda (inbox)
                         (format "maildir:%s" inbox))
                       my-mu4e-inboxes)
               " OR "))

(defun my-email-addresses (smtp-accounts)
  "Return the list of email addresses from the smtp-accounts"
  (mapcar 'car smtp-accounts))

(defun my-set-smtp-accounts (sym smtp-accounts)
  (set-default sym smtp-accounts)
  (custom-set-variables
   ;; Exclude my email addresses from reply-all and use for indexing.
   `(mu4e-user-mail-address-list (list ,@(my-email-addresses smtp-accounts)))
   ;; Default sender is user-full-name <user-mail-address>
   `(user-full-name ,(cadar smtp-accounts))
   `(user-mail-address ,(caar smtp-accounts))))

(defcustom my-smtp-accounts nil
  "List of smtp accounts as (email username server port). The
passwords are stored in ~/.authinfo."
  :type '(repeat (string string string string integer))
  :group 'my-mu4e
  :set 'my-set-smtp-accounts)

(defun my-years-ago (n)
  (time-subtract (current-time) (days-to-time (* n 365))))

;;; General settings
(custom-set-variables
 '(mu4e-get-mail-command "offlineimap")
 '(mu4e-hide-index-messages t)          ; Don't show indexing messages
 '(smtpmail-stream-type 'ssl)
 '(starttls-use-gnutls t)
 '(mu4e-headers-date-format "%d/%m/%Y")
 '(mu4e-headers-time-format "%X")       ; TODO: change
 '(mu4e-compose-signature-auto-include nil) ; Don't include signature at end of message
 '(mu4e-compose-dont-reply-to-self t)
 ;'(mu4e-compose-complete-only-personal t)  ; This doesn't appear to work reliably
 '(mu4e-compose-complete-only-after (format-time-string "%Y-%m-%d"
                                                        (my-years-ago 2))
                                    t)
 '(message-send-mail-function 'smtpmail-send-it)
 '(mail-user-agent 'mu4e-user-agent)    ; mu4e as default emacs mail agent
 '(gnus-dired-mail-mode 'mu4e-user-agent)
 '(mu4e-headers-include-related nil ) ; Include related messages in search results
 '(mu4e-headers-skip-duplicates t)    ; Don't show duplicates in header view
 '(mu4e-view-show-addresses t)        ; Always show email addresses
 '(message-kill-buffer-on-exit t)     ; Kill message buffer after email is sent
 '(message-auto-save-directory nil)   ; Don't autosave message buffers
 '(mu4e-confirm-quit nil))            ; Don't ask for confirmation on quit

;;; Email rendering
(custom-set-variables
 '(mu4e-view-show-images t)             ; Show images inline
 '(mu4e-view-prefer-html t)
 '(mu4e-html2text-command 'mu4e-shr2text) ; Use EWW for html rendering
 '(shr-width 70)                          ; Wrap email body at 70 chars
 '(shr-color-visible-luminance-min 60)    ; Increase contrast of rendered html emails
 '(shr-color-visible-distance-min 5))

;;; Use format=flowed in composition
(custom-set-variables
 '(mu4e-compose-format-flowed t))

;;; Message actions

(defun my-private-view-in-browser (msg)
  "Wraps mu4e-action-view-in-browser to make sure we view
messages in a private offline view."
  (let (;; Always use firefox
        (browse-url-browser-function (lambda (url &rest args)
                                       (apply 'browse-url-firefox url args)))
        ;; Start in offline mode. See https://developer.mozilla.org/en-US/docs/Mozilla/Command_Line_Options
        (browse-url-firefox-arguments '("-offline" "-P" "Private no plugins")))
    (mu4e-action-view-in-browser msg)))

(setq mu4e-view-actions
      '(("offline browse" . my-private-view-in-browser)
        ("browse" . mu4e-action-view-in-browser)
        ("tag" . mu4e-action-retag-message)
        ("capture message" . mu4e-action-capture-message)))

;;; Header actions
(add-to-list 'mu4e-headers-actions
             '("tag" . mu4e-action-retag-message)
             t)

;;; Proselint integration
(my-enable-proselint-for-mode 'mu4e-compose-mode)

;;; Remove text-properties that interfere with company mode
(defun my-strip-message-mode-problematic-text-props ()
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp (concat "^" mail-header-separator) nil t)
      (remove-text-properties (match-beginning 0)
                              (match-end 0)
                              '(rear-nonsticky)))))

(add-hook 'message-mode-hook 'my-strip-message-mode-problematic-text-props)
(add-hook 'mu4e-compose-mode-hook 'my-strip-message-mode-problematic-text-props)

;;; Don't prompt for file encoding when queuing utf-8 encoded email
;;; See https://emacs.stackexchange.com/questions/7946/using-utf8-encoding-as-default-when-writing-emails
;;; for an explanation of the problem. More resources:
;;; - http://comments.gmane.org/gmane.emacs.gnus.general/30660
;;; - https://lists.gnu.org/archive/html/bug-gnu-emacs/2001-12/msg00220.html
(defun my-advise-write-raw (f &optional args)
  (let ((coding-system-for-write 'raw-text-unix))
    (apply f args)))
(advice-add 'smtpmail-send-it :around 'my-advise-write-raw)

;;; Notifications in the emacs mode-line

(defun my-count-maildir-message (maildir)
  "Returns the number of messages in a maildir."
  (+ (length (directory-files (concat maildir "/cur") nil "...+" ))
     (length (directory-files (concat maildir "/new") nil "...+" ))))

;;; For the ->> macro
(require 'dash)

(defun my-num-inbox-messages ()
  "Returns the number of messages in my inboxes."
  (->> my-mu4e-inboxes
       (mapcar (lambda (inbox)
                 (my-count-maildir-message (concat mu4e-maildir inbox))))
       (cl-reduce '+)))

(defvar my-inbox-messages "" "The message to display in the mode-line")

;;; Mark var as risky-local-variable. If not the mode-line won't use the faces.
(put 'my-inbox-messages 'risky-local-variable t)

(defun my-notify-num-inbox-messages ()
  "Updates the mode-line string for displaying the number of
messages in my inbox."
  (let ((n (my-num-inbox-messages)))
    (if (and n (> n 0))
        (setq my-inbox-messages (propertize (format "[✉ %d]" n)
                                            'face 'mode-line-buffer-id))
      (setq my-inbox-messages "")
      (force-mode-line-update t))))

(add-hook 'mu4e-index-updated-hook 'my-notify-num-inbox-messages)

;;; Needs to be appended to preserve the special empty string at the start of
;;; the global-mode-string list
(if (not global-mode-string)
    (custom-set-variables '(global-mode-string '("" my-inbox-messages)))
  (add-to-list 'global-mode-string 'my-inbox-messages t))

;;; Update inbox notifications every time we execute marks
(defun my-notify-num-inbox-messages-delayed (&rest _)
  (run-at-time 1 nil 'my-notify-num-inbox-messages))

(advice-add 'mu4e-mark-execute-all :after 'my-notify-num-inbox-messages-delayed)

;;; Update inbox notifications
(my-notify-num-inbox-messages)

;;; Choose right SMTP credentials for message
(defun my-set-smtp (account)
  "Sets SMTP related variables for the provided smtp account info."
  (custom-set-variables
   `(user-mail-address ,(nth 0 account))
   `(user-full-name ,(nth 1 account))
   `(smtpmail-smtp-user ,(nth 2 account))
   `(smtpmail-smtp-server ,(nth 3 account))
   `(smtpmail-smtp-service ,(nth 4 account)))
  (message "Use SMTP settings for %s" (nth 0 account)))

(defun my-set-stmp-for-email (email)
  "Sets SMTP related variables for the SMTP account identified by email."
  (let* ((address (cadr (mail-extract-address-components email)))
         (account (or (assoc address my-smtp-accounts)
                      (first my-smtp-accounts))))
    (my-set-smtp account)))

(defun my-set-smtp-for-email-advice (rcpt buf &optional ask)
  (with-current-buffer buf
    (my-set-stmp-for-email (message-field-value "from"))))

(advice-add 'smtpmail-via-smtp :before 'my-set-smtp-for-email-advice)

;;; Respond with proper email address
(defun my-respond-with-right-email-address ()
  "Infer the account with which to send the reply. In not
possible, the From header will be set to the default."
  (let ((msg mu4e-compose-parent-message)
        (done nil))
    (when msg
      (let ((fields '(:to :cc :bcc :from)))
        (while (and fields (not done))
          (let ((field (pop fields))
                (accounts my-smtp-accounts))
            (while (and accounts (not done))
              (let ((account (pop accounts)))
                (when (mu4e-message-contact-field-matches msg
                                                          field
                                                          (car account))
                  (my-set-smtp account)
                  (setq done t))))))))
    (when (not done)
      (my-set-smtp (first my-smtp-accounts)))))

(add-hook 'mu4e-compose-pre-hook 'my-respond-with-right-email-address)

;;; Detect internet connection to make good choices about online/offline operations
(defun my-have-internet-p ()
  (zerop (call-process "ip" nil nil nil "route" "get" "8.8.8.8")))

(defun my-mu4e-online-p ()
  (and (not smtpmail-queue-mail)
       mu4e-update-interval))

(defun my-mu4e-offline-p ()
  (and smtpmail-queue-mail
       (not mu4e-update-interval)))

(defun my-mu4e-set-online-vars ()
  (custom-set-variables
   ;; Disable mail queuing
   '(smtpmail-queue-mail nil)
   ;; IMAP sync every 5 minutes
   '(mu4e-update-interval 300)))

(defun my-mu4e-set-offline-vars ()
  (custom-set-variables
   ;; Enable mail queuing
   '(smtpmail-queue-mail t)
   ;; Disable IMAP sync
   '(mu4e-update-interval nil)))

(defun my-mu4e-infer-default-vars ()
  (if (my-have-internet-p)
      (my-mu4e-set-online-vars)
    (my-mu4e-set-offline-vars)))

;;; Initialize default vars
(my-mu4e-infer-default-vars)

;;; org-mu4e workaround for org-9.0 (org-add-link-type is deprecated)
(org-link-set-parameters "mu4e"
                         :follow 'org-mu4e-open
                         :store 'org-mu4e-store-link)

(defun my-ensure-mu4e-is-running (&optional force-offline)
  (when (not (mu4e-running-p))
    (if (or force-offline (not (my-have-internet-p)))
        (my-mu4e-set-offline-vars)
      (my-mu4e-set-online-vars))
    (mu4e t)))

(defun my-start-mu4e (&optional force-offline)
  "Starts mu4e with appropriate settings depending on whether or
not we have an internet connection. If called with a prefix arg,
always use offline settings."
  (interactive "P")
  (my-ensure-mu4e-is-running force-offline)
  (mu4e))

(defun my-go-to-inbox (&optional force-offline)
  "Open mu4e inbox, taking care of starting mu4e if necessary. If
called with a prefix and mu4e is not yet running, always use
offline settings."
  (interactive)
  (my-ensure-mu4e-is-running force-offline)
  (mu4e-headers-search (my-mu4e-inbox-query)))

(defun my-compose-email ()
  "Start composing a mail message to send. If mu4e is not yet
running, start it in the background."
  (interactive)
  (my-ensure-mu4e-is-running)
  (call-interactively 'compose-mail))

;;; Fix to the `mu4e~compose-setup-fcc-maybe` function, so that when proper
;;; maildirs are created when a directory is created when sending an email.
;;; https://github.com/djcb/mu/pull/1190
(defun my-mu4e~compose-setup-fcc-maybe ()
  "Maybe setup Fcc, based on `mu4e-sent-messages-behavior'.
If needed, set the Fcc header, and register the handler function."
  (let* ((sent-behavior
          ;; Note; we cannot simply use functionp here, since at least
          ;; delete is a function, too...
          (if (member mu4e-sent-messages-behavior '(delete trash sent))
              mu4e-sent-messages-behavior
            (if (functionp mu4e-sent-messages-behavior)
                (funcall mu4e-sent-messages-behavior)
              mu4e-sent-messages-behavior)))
         (mdir
          (case sent-behavior
            (delete nil)
            (trash (mu4e-get-trash-folder mu4e-compose-parent-message))
            (sent (mu4e-get-sent-folder mu4e-compose-parent-message))
            (otherwise
             (mu4e-error "unsupported value '%S' `mu4e-sent-messages-behavior'."
                         mu4e-sent-messages-behavior))))
         (fccfile (and mdir
                       (concat mu4e-maildir mdir "/cur/"
                               (mu4e~draft-message-filename-construct "S")))))
    ;; if there's an fcc header, add it to the file
    (when fccfile
      (message-add-header (concat "Fcc: " fccfile "\n"))
      ;; sadly, we cannot define as 'buffer-local'...  this will screw up gnus
      ;; etc. if you run it after mu4e so, (hack hack) we reset it to the old
      ;; handler after we've done our thing.
      (setq message-fcc-handler-function
            (lexical-let ((maildir mdir)
                          (old-handler message-fcc-handler-function))
              (lambda (file)
                ;; reset the fcc handler
                (setq message-fcc-handler-function old-handler)
                (let ((mdir-path (concat mu4e-maildir maildir)))
                  ;; Create the full maildir structure for the sent folder if it
                  ;; doesn't exist. `mu4e~proc-mkdir` runs asynchronously but no
                  ;; matter whether it runs before or after `write-file`, the
                  ;; sent maildir ends up in the correct state.
                  (unless (file-exists-p mdir-path)
                    (mu4e~proc-mkdir mdir-path)))
                (write-file file) ;; writing maildirs files is easy
                (mu4e~proc-add file (or maildir "/")))))))) ;; update the database

(advice-add 'mu4e~compose-setup-fcc-maybe
            :override 'my-mu4e~compose-setup-fcc-maybe)

(provide 'my-mu4e)
