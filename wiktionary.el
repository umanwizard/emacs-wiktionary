;;; wiktionary.el --- Look up words in Wiktionary  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Brennan Vincent

;; Author: Brennan Vincent <brennan@umanwizard.com>
;; Version: 0
;; Package-Requires: ((request "0.3.3") (dash "2.19.1") (emacs "25.1"))
;; Keywords: comm
;; URL: https://github.com/umanwizard/emacs-wiktionary

                                        
;;; Commentary:
;;;
;;; Look up words in English Wiktionary.  See README.md for details.

(require 'request)
(require 'dash)
(require 'url-util)

;;; Code:

(defgroup wiktionary nil
  "Look up words in Wiktionary."
  :group 'comm
  :prefix "wiktionary-")

(defcustom wiktionary-language-order nil
  "The order of languages to appear in Wiktionary results."
  :type '(repeat string)
  :group 'wiktionary)

(defcustom wiktionary-show-unordered-languages t
  "Whether to show languages that don't appear in `wiktionary-language-order'."
  :type 'boolean
  :group 'wiktionary)

(defun wiktionary-navigate ()
  "Navigate to the link at point."
  (interactive)
  (let ((title (plist-get (text-properties-at (point)) 'wiktionary-link-target)))
    (when (and title
               ;; TODO - t means self-link. Figure out how to deal with this.
               (not (eq title t)))
      (wiktionary-search-word title))))

(defvar wiktionary-result-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (keymap-set map "RET" #'wiktionary-navigate)
    (keymap-set map "l" #'wiktionary-back)
    (keymap-set map "r" #'wiktionary-forward)
    (keymap-set map "s" #'wiktionary-search-word)
    map))

(defun wiktionary--lang-order-val (lang)
  "Get the position of LANG in `wiktionary-language-order', if any."
  (cl-position lang wiktionary-language-order :test #'string-equal))

(define-derived-mode wiktionary-result-mode special-mode "Wiktionary Results"
  :interactive nil
  (buffer-disable-undo)
  (setq buffer-read-only t))

(defun wiktionary--insert-several-body-elts (elts props)
  "Render ELTS with text properties PROPS in the current buffer."
  (dolist (body-element elts)
    (wiktionary--insert-body-elt body-element props)))

(defun wiktionary--insert-body-elt (elt props)
  "Render ELT with text properties PROPS in the current buffer."
  (pcase elt
    ((and (pred stringp) x)
     (let ((insert-point (point)))
       (insert (apply #'propertize x props))
       (save-excursion
         (goto-char insert-point)
         (while (let ((before (char-before)))
                  (or (= before ?\s)
                      (= before ?\t)))
           (backward-char 1))
         (while (re-search-forward "[ \t]+" nil t)
           (replace-match " " nil nil)))))
    (`(a ,a-props . ,inners)
     (cond
      ((string-equal (cdr (assq 'class a-props)) "mw-selflink-fragment")
       (wiktionary--insert-several-body-elts inners (append '(face link wiktionary-link-target t) props)))
      ((assq 'title a-props)
       (wiktionary--insert-several-body-elts inners (append `(face link wiktionary-link-target ,(cdr (assq 'title a-props))) props)))
      (t (wiktionary--insert-several-body-elts inners props))))
    ;; todo - these mask earlier definitions
    (`(b ,_ . ,inners) (wiktionary--insert-several-body-elts inners (append `(face bold) props)))
    (`(i ,_ . ,inners) (wiktionary--insert-several-body-elts inners (append `(face italic) props)))
    (`(u ,_ . ,inners) (wiktionary--insert-several-body-elts inners (append `(face underline) props)))
    (`(comment . ,_))
    (`(,_ ,_ . ,inners) (wiktionary--insert-several-body-elts inners props))))

(defun wiktionary--insert-html (src)
  "Render snippet with html source SRC in the current buffer."

  (let* ((html
          (with-temp-buffer
            (insert src)
            (libxml-parse-html-region)))
         (body (pcase html
                 (`(html ,_ (body ,_ . ,body)) body)
                 (`(html ,_ (head . ,_) (body ,_ . ,body)) body))))
    (when body
      (wiktionary--insert-several-body-elts body nil))))

(defun wiktionary--insert-data (data)
  "Render word-data pair DATA in the current buffer."
  (let ((word (car data))
        (data (cdr data))
        langs)
    (insert (propertize word 'face 'info-title-2) ?\n ?\n)
    (dolist (x data)
      (let* ((chunks (cdr x)))
        (cl-loop
         for et being the elements of chunks
         do (let ((lang (or (cdr (assq 'language et)) "<no language>"))
                  (pos (or (cdr (assq 'partOfSpeech et)) "<no part of speech>"))
                  (gender (assq 'gender et))
                  (defns (let ((q (assq 'definitions et)))
                           (if q (cdr q)
                             []))))
              (cl-loop
               for dfn being the elements of defns
               do (push
                   `(,pos . ,(if gender
                                 (cons gender dfn)
                               dfn))
                   (alist-get lang langs nil nil #'string-equal)))))))
      ;; TODO -- slow? reads wiktionary-language-order
      ;; lots of times
      (let* ((langs
              (if wiktionary-show-unordered-languages
                  langs
                (--filter
                 (member (car it) wiktionary-language-order)
                 langs)))
             (langs
              (sort langs
                    :lessp (lambda (l1 l2)
                             (let* ((l1 (car l1))
                                    (l2 (car l2))
                                    (pos1 (wiktionary--lang-order-val l1))
                                    (pos2 (wiktionary--lang-order-val l2)))
                               (cond
                                ((and pos1 pos2) (< pos1 pos2))
                                (pos1 t)
                                (pos2 nil)
                                (t (string< l1 l2))))))))
        (dolist (lang langs)
          (let ((dfns (cdr lang))
                (lang (car lang)))
            (insert (propertize lang 'face 'info-title-4) ?\n)
            (cl-loop
             for dfn being the elements of (nreverse (--filter (< 0 (length (cdr (assq 'definition (cdr it))))) dfns)) using (index i)
             do (let* ((pos (car dfn))
                       (dfn-alist (cdr dfn))
                       (dfn-content (cdr (assq 'definition dfn-alist)))
                       (dfn-gender (cdr (assq 'gender dfn-alist)))
                       (examples (cdr (assq 'examples dfn-alist))))
                  (insert (propertize (format "%d. %s%s" (1+ i) pos (if dfn-gender (format " (%s)" dfn-gender) "")) 'face '(italic))
                          ": ")
                  (wiktionary--insert-html dfn-content)
                  (insert ?\n)
                  (cl-loop
                   for ex being the elements of examples
                   do
                   (insert "  • " )
                   (wiktionary--insert-html ex)
                   (insert ?\n))
                  (insert ?\n)))
            (insert ?\n))))))

(defvar-local wiktionary--history nil)
(defvar-local wiktionary--future nil)
(defvar-local wiktionary--current-data nil)

(defun wiktionary--load-data (data)
  "Render word-data pair DATA in the current buffer, updating history."
  (let* ((inhibit-read-only t))
    (erase-buffer)
    (let ((cur wiktionary--current-data))
      (when cur
        (push cur wiktionary--history)))
    (setq-local wiktionary--future nil)
    (setq-local wiktionary--current-data data)
    (wiktionary--insert-data data)
    (goto-char (point-min))))

(defun wiktionary-back ()
  "Navigate back in the history of the current buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (if wiktionary--history
        (progn
          (when wiktionary--current-data
            (push wiktionary--current-data wiktionary--future))
          (setq-local wiktionary--current-data (pop wiktionary--history))
          (erase-buffer)
          (wiktionary--insert-data wiktionary--current-data)
          (goto-char (point-min)))
      (error "No previous data"))))

(defun wiktionary-forward ()
  "Navigate forward in the history of the current buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (if wiktionary--future
        (progn
          (when wiktionary--current-data
            (push wiktionary--current-data wiktionary--history))
          (setq-local wiktionary--current-data (pop wiktionary--future))
          (erase-buffer)
          (wiktionary--insert-data wiktionary--current-data)
          (goto-char (point-min)))
      (error "No future data"))))

;;;###autoload
(defun wiktionary-search-word (word)
  "Search for WORD in English Wiktionary and display it in a pop-up buffer."
  (interactive "M")
  (let* ((url (format "https://en.wiktionary.org/api/rest_v1/page/definition/%s?redirect=true" (url-encode-url word)))
         (r (request url :sync t))
         (_ (let ((code (request-response-status-code r)))
              (unless code
                (let ((err (cdr (request-response-error-thrown r))))
                  (if err
                      (error (format "Wiktionary query request error: %s" (string-trim err)))
                    (error "Wiktionary query request failed with unknown error"))))
              (unless (= 200 code)
                (error (format "Wiktionary query for word %s failed with code %d" word code)))))
         (data (request-response-data r))
         (data (json-read-from-string data)))
    (pop-to-buffer "*Wiktionary*")
    (unless (eq major-mode #'wiktionary-result-mode)
      (wiktionary-result-mode))
    (wiktionary--load-data `(,word . ,data))))

(provide 'wiktionary)

;;; wiktionary.el ends here
