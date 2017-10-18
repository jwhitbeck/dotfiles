;;; -*- lexical-binding: t; -*-

;;; Wikipedia quick access

(defun my-browse-wikipedia (lang word &optional wiktionary?)
  (interactive
   (list (ido-completing-read "Language:" '("en" "fr"))
         (word-at-point)
         current-prefix-arg))
  (browse-url (format "https://%s.%s.org/wiki/Special:Search?search=%s"
                      lang
                      (if wiktionary? "wiktionary" "wikipedia")
                      word)))

(provide 'my-wiki)
