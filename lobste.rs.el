;;; lobste.rs.el --- view the front page of lobste.rs in an emacs buffer
;; Copyright (C) 2013 Kyle Isom

;; Author: Kyle Isom
;; URL: http://github.com/kisom/lobsters.el
;; Version: 1.0
;; Created: 2013-04-01
;; License: ISC

;; This file is NOT part of GNU Emacs.

;; This package provides the 'lobste.rs' command, which will
;; display the current lobste.rs front page in a new emacs buffer.
;; The buffer can be closed with the '

;;; Installation:

;; Copy into your emacs source directory, and add
;; (autoload 'lobsters "lobsters" "View the lobste.rs home page." t nil)
;; to your emacs init file.

;;; Usage:

;; M-x lobsters displays the front page.
;; M-x lobste.rs-newest displays the newest posts.
;; q closes the buffer.

;;; License:

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


(require 'cl)
(require 'json)
(require 'url)

(defvar lobsters-feed "https://lobste.rs/rss.json")
(defvar lobsters-feed-newest "https://lobste.rs/newest.json")
(defvar lobsters-bufname "lobste.rs")

(defun lobsters-updated-time ()
  (insert (concat "\n\n- Updated "
                  (format-time-string "%Y-%m-%d %H:%M:%S"
                                      (current-time))
                  "\n\n")))

(defun lobsters-buffer-to -string (b)
  (unless (bufferp b)
    nil)
  (let ((cb (current-buffer)))
    (set-buffer b)
    (let ((s (buffer-string)))
      (set-buffer cb)
      s)))

(defun lobsters-fetch-url (url)
  (let* ((ub (browse-url-emacs url))
         (content (lobsters-buffer-to-string ub)))
    (kill-buffer ub)
    (message content)
    content))

(defun lobsters-fetch (url)
  (let* ((lobsters-buffer (generate-new-buffer "lobsters")))
    (setq lobsters-buffer (url-retrieve-synchronously url))
    (with-current-buffer lobsters-buffer
      (if (string-match "200 OK$" (buffer-string))
          (progn
            (re-search-forward "^[ \t]*$" nil 'move)
            (let ((lobsters-json (json-read-from-string
                                  (buffer-substring-no-properties (point) (point-max)))))
              (kill-buffer lobsters-buffer)
              lobsters-json))
        (error "couldn't connect to server.")))))

(defun lobsters-get-key (key p)
  (cdr (assoc key p)))

(defun lobsters-make-browse-url (url)
  (lexical-let ((url url))
    (lambda (foo)
      (interactive "p")
      (browse-url url))))

(defun lobsters-post-link (p)
  (if (numberp (lobsters-get-key 'score p))
      (insert (concat "(" (prin1-to-string (lobsters-get-key 'score p)) ") ")))
  (lexical-let ((title (lobsters-get-key 'title p))
                (url (lobsters-get-key 'url p))
                (kmap (make-sparse-keymap)))
    (define-key kmap (kbd "<RET>") (lobsters-make-browse-url url))
    (define-key kmap (kbd "<down-mouse-1>") (lobsters-make-browse-url url))
    (insert (propertize title
                        'keymap kmap
                        'face '(:foreground blue :weight bold)
                        'help-echo (concat "Visit " url ".")
                        'mouse-face 'highlight)))
  (if (null (lobsters-get-key 'short p))
      (insert "\n")
    (lexical-let ((slug (concat "https://lobste.rs/s/" (lobsters-get-key 'short p)))
                  (kmap (make-sparse-keymap)))
      (define-key kmap (kbd "<RET>") (lobsters-make-browse-url slug))
      (define-key kmap (kbd "<down-mouse-1>") (lobsters-make-browse-url slug))
      (insert " (")
      (insert (propertize (concat (prin1-to-string (lobsters-get-key 'comment_count p))
                                  " comments")
                          'keymap kmap
                          'face '(:foreground blue :weight bold)
                          'mouse-face 'highlight))
      (insert ")\n"))))

(defun lobsters-write-title (title title-url)
  (lobsters-post-link `((title . ,title) (url . ,title-url)))
  (dotimes (i (length title)) (insert "-"))
  (insert "\n\n"))

(defun lobsters-show-feed (title title-url feed)
  (switch-to-buffer (get-buffer-create lobsters-bufname))
  (local-set-key (kbd "q") (lambda ()
                             (interactive)
                             (progn
                               (local-unset-key (kbd "q"))
                               (kill-buffer (buffer-name)))))
  (setq buffer-read-only nil)
  (erase-buffer)
  (lobsters-write-title title title-url)
  (mapc #'lobsters-post-link (lobsters-fetch feed))
  (lobsters-updated-time)
  (setq buffer-read-only t)
  (move-to-window-line 2))

(defun lobste.rs ()
  (interactive)
  (lobsters-show-feed "lobste.rs front page"
                      "https://lobste.rs/"
                      lobsters-feed))

(defun lobste.rs-newest ()
  (interactive)
  (lobsters-show-feed "newest lobste.rs posts"
                      "https://lobste.rs/newest/"
                      lobsters-feed-newest))

(provide 'lobste.rs)

;;; lobsters.el ends here
