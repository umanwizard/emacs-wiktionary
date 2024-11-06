;; -*- lexical-binding: t; mode: emacs-lisp;-*-
;;; wiktionary.el --- Look up words in Wiktionary

;; Copyright (C) 2024 Brennan Vincent

;; Author: Brennan Vincent <brennan@umanwizard.com>
;; Version: 0
;; Package-Requires: ((request "0.3.3") (dash "2.19.1") (emacs "25.1"))
;; Keywords: comm
;; URL: https://github.com/umanwizard/emacs-wiktionary

                                        
;;; Commentary:
;;;
;;; Look up words in English Wiktionary. See README.md for details.

(require 'request)
(require 'thingatpt)

(defgroup wiktionary nil
  "Look up words in Wiktionary"
  :group 'comm
  :prefix "wiktionary-")

(defcustom wiktionary-language-order nil
  "The order of languages to appear in Wiktionary results"
  :type '(repeat string)
  :group 'wiktionary)

(defcustom wiktionary-show-unordered-languages t
  "Whether to show languages that don't appear in wiktionary-language-order"
  :type 'boolean
  :group 'wiktionary)

(defun wiktionary-navigate ()
  (interactive)
  (wiktionary-search-word (word-at-point)))

(defvar wiktionary-result-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (keymap-set map "RET" #'wiktionary-navigate)
    (keymap-set map "l" #'wiktionary-back)
    (keymap-set map "r" #'wiktionary-forward)
    map))

(defun wiktionary--lang-order-val (lang)
  (cl-position lang wiktionary-language-order :test #'string-equal))

(define-derived-mode wiktionary-result-mode special-mode "Wiktionary Results"
  :interactive nil
  (buffer-disable-undo)
  (setq buffer-read-only t))

(defun wiktionary--insert-several-body-elts (elts props)
  (dolist (body-element elts)
    (wiktionary--insert-body-elt body-element props)))

(defun wiktionary--insert-body-elt (elt props)
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
    (`(,_ ,_ . ,inners) (wiktionary--insert-several-body-elts inners props))))

(defun wiktionary--insert-dfn (dfn)
  (pcase-let ((`(html _ (body _ . ,body))
               (with-temp-buffer
                 (insert dfn)
                 (libxml-parse-html-region))))            
    (wiktionary--insert-several-body-elts body nil)))

(defun wiktionary--insert-data (data)
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
                  (defns (let ((q (assq 'definitions et)))
                           (if q (cdr q)
                             []))))
              (cl-loop
               for dfn being the elements of defns
               do (push
                   `(,pos . ,dfn)
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
                       (dfn-content (cdr (assq 'definition dfn-alist))))
                  (insert (propertize (format "%d. %s:" (1+ i) pos) 'face '(italic ;; underline
                                                                                   ))
                          " ")
                  (wiktionary--insert-dfn dfn-content)
                  (insert ?\n)))
            (insert ?\n))))))

(defvar-local wiktionary--history nil)
(defvar-local wiktionary--future nil)
(defvar-local wiktionary--current-data nil)

(defun wiktionary--load-data (data)
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
      (error "no previous data"))))

(defun wiktionary-forward ()
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
      (error "no future data"))))

;;;###autoload
(defun wiktionary-search-word (word)
  (interactive "M")
  (let* ((url (format "https://en.wiktionary.org/api/rest_v1/page/definition/%s?redirect=true" word))
         (r (request url :sync t))
         (_ (let ((code (request-response-status-code r)))
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
