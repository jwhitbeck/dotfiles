;;; -*- lexical-binding: t; -*-

;;; Wikipedia quick access

(defun my-browse-wikimedia (site word prompt-language?)
  (let ((lang (if prompt-language?
                  (ido-completing-read "Language:" '("en" "fr"))
                "en")))
    (browse-url (format "https://%s.%s.org/wiki/Special:Search?search=%s"
                        lang
                        site
                        word))))

(defun my-loopkup-wikipedia (word &optional prompt-language?)
  (interactive
   (list (word-at-point)
         current-prefix-arg))
  (my-browse-wikimedia "wikipedia" word prompt-language?))

(defun my-loopkup-wiktionary (word &optional prompt-language?)
  (interactive
   (list (word-at-point)
         current-prefix-arg))
  (my-browse-wikimedia "wiktionary" word prompt-language?))

(provide 'my-wiki)
