;;; mark-select.el --- Mark and select interface

;; Copyright (C) 2010  Yuta Namiki

;; Author: Yuta Namiki
;; Keywords: lisp
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;; mark-select-multi
;;
;; (let ((hash (make-hash-table :test 'equal)))
;;   (setf (gethash "lang" hash) '("Select program languages"
;;                                 (("Lisp" . t) "Perl" "Ruby")))
;;   (setf (gethash "file" hash) '("Select files"
;;                                 (("file1" . t)
;;                                  ("file2" . t)
;;                                  ("file3" . t))))
;;   (mark-select-multi hash 'do-func "mode-name"))

;;; Code:

(eval-when-compile (require 'cl))

(defvar mark-select-mode-name "mark-select")

(defvar mark-select-list nil
  "*List of lines to select")

(defvar mark-select-help-message "x: execute  q: quit  m: mark  u: unmark  U: up  D: down")

(defvar mark-select-marker-char ?*)

(defvar mark-select-no-marker-char ?\ )

(defvar mark-select-re-mark "^\\[\\([^\n]\\)\\]")

(defvar mark-select-mode-hook nil
  "Run at the very end of `mark-select-mode'.")

(defvar mark-select-buffer-name (concat "*" mark-select-mode-name "*"))

(defvar mark-select-mode-map
  (let ((map (make-keymap)))
    (define-key map "x"       'mark-select-do-marked)
    (define-key map "q"       'mark-select-quit)
    (define-key map [mouse-1] 'mark-select-toggle-mark)
    (define-key map "m"       'mark-select-mark)
    (define-key map "u"       'mark-select-unmark)
    (define-key map "U"       'mark-select-up)
    (define-key map "D"       'mark-select-down)
    map)
  "Local keymap for `mark-select-mode' buffers")

(defgroup mark-select-faces nil
  "Faces used by Mark-Select."
  :group 'mark-select
  :group 'faces)

(defface mark-select-mark
  '((t (:inherit font-lock-constant-face)))
  "Face used for mark-select marks."
  :group 'mark-select-faces
  :version "22.1")

(defvar mark-select-mark-face 'mark-select-mark
  "Face name used for mark-select marks.")

(defface mark-select-marked
  '((t (:inherit font-lock-warning-face)))
  "Face used for marked files."
  :group 'mark-select-faces
  :version "22.1")

(defvar mark-select-marked-face 'mark-select-marked
  "Face name used for marked files.")

(defvar mark-select-font-lock-keywords
  (list
   ;;
   ;; Mark-Select marks.
   (list mark-select-re-mark '(0 mark-select-mark-face))
   ;; Marked files.
   (list (concat "^\\[[" (char-to-string mark-select-marker-char) "]\\]")
         '(".+" (mark-select-move-to-element) nil (0 mark-select-marked-face))))
  "Additional expressions to highlight in Mark-select mode.")

(defvar mark-select-do-marked-func
  (lambda (hash)
    (let ((keys (mark-select-hash-keys hash)))
      (dolist (key keys)
        (message (format "%s %s" key (gethash key hash)))))))

(defun mark-select-hash-keys (hash)
  (loop for k being the hash-keys in hash collect k))

(defun mark-select-transpose-lines (n)
  "Transpose a current line with a line n."
  (interactive "P")
  (let ((transposable nil)
        beg1 end1 beg2 end2)
    (save-excursion
      (beginning-of-line)
      (setq beg1 (point))
      (when (looking-at mark-select-re-mark)
        (end-of-line)
        (setq end1 (point))
        (forward-line n)
        (beginning-of-line)
        (setq beg2 (point))
        (when (looking-at mark-select-re-mark)
          (setq transposable t)
          (end-of-line)
          (setq end2 (point)))))
    (if transposable
        (let ((inhibit-read-only t))
          (transpose-regions beg1 end1 beg2 end2))
        (message "Cannot replace this line."))))

(defun mark-select-up ()
  (interactive)
  (mark-select-transpose-lines -1))

(defun mark-select-down ()
  (interactive)
  (mark-select-transpose-lines 1))

(defun mark-select-mark-sub (&optional marker)
  "Mark the current element."
  (interactive "P")
  (beginning-of-line)
  (if (looking-at mark-select-re-mark)
      (let ((inhibit-read-only t))
        (mark-select-move-to-marker)
        (unless marker ;; toggle marker
          (setq marker
                (if (equal (char-after) mark-select-marker-char)
                    mark-select-no-marker-char mark-select-marker-char)))
        (delete-char 1)
        (insert marker))
    (message "Cannot mark at this line."))
  (forward-line 1)
  (mark-select-move-to-element))

(defun mark-select-toggle-mark (&optional arg)
  "Toggle mark of the current element."
  (interactive "P")
  (mark-select-mark-sub))

(defun mark-select-mark (&optional arg)
  "Mark the current element."
  (interactive "P")
  (mark-select-mark-sub mark-select-marker-char))

(defun mark-select-unmark (&optional arg)
  "Mark the current element."
  (interactive "P")
  (mark-select-mark-sub mark-select-no-marker-char))

(defun mark-select-quit (&optional arg)
  "Quit mark select."
  (interactive "P")
  (kill-buffer mark-select-buffer-name))

(defun mark-select-do-marked (&optional arg)
  "Mark the current element."
  (interactive "P")
  (let ((hash (make-hash-table :test 'equal))
        (do-func mark-select-do-marked-func)
        (marked? nil)
        beg end id)
    (goto-char (point-min))
    (while (re-search-forward mark-select-re-mark nil t)
      (mark-select-move-to-marker)
      (setq marked? (equal (char-after) mark-select-marker-char))
      (mark-select-move-to-element)
      (setq beg (point))
      (end-of-line)
      (setq end (point))
      (setq id (get-text-property beg 'id))
      (setf (gethash id hash)
            (append (gethash id hash)
                    (list (cons (buffer-substring-no-properties beg end) marked?)))))
    (mark-select-quit)
    (funcall do-func hash)))

(defun mark-select-move-to-marker ()
  (beginning-of-line)
  (forward-char 1))

(defun mark-select-move-to-element (&optional raise-error eol)
  "Move to the beginning of the element on the current line.
Return the position of the beginning of the element, or nil if none found."
  ;; This is the UNIX version.
  (or eol (setq eol (line-end-position)))
  (beginning-of-line)
  (let ((change (next-single-property-change (point) 'mark-select-element nil eol)))
    (cond
     ((and change (< change eol))
      (goto-char change))
     (raise-error
      (error "No element on this line")))))

(defun mark-select-move-to-end-of-element (&optional no-error)
  (if (get-text-property (point) 'mark-select-element)
      (goto-char (next-single-property-change (point) 'mark-select-element))
    (move-end-of-line 1)))

(defun mark-select-insert-element (element &optional mark? id)
  (let ((beg (point))
        end)
    (insert
     "["
     (char-to-string (if mark?
                         mark-select-marker-char
                       mark-select-no-marker-char))
     "] ")
    (setq beg (point))
    (insert element)
    (setq end (point))
    (add-text-properties
     beg end
     (list
      'mouse-face 'highlight
      'mark-select-element t
      'id id
      'help-echo "mouse-1: toggle mark"))
    (insert "\n")))

(defun mark-select-insert-elements (element-list &optional header id)
  (let ((beg (point)))
    (dolist (elem element-list)
      (cond
       ((consp elem)
        (mark-select-insert-element (car elem) (cdr elem) id))
       ((atom elem)
        (mark-select-insert-element elem nil id))))
    ;; Insert header at the beginning.
    (save-excursion
      (goto-char beg)
      (if header
          (insert header "\n")
        (insert "Select elements below.\n")))))

(defun mark-select-buffer (do-func new-mode-name body-func &rest args)
  (let ((inhibit-read-only t)
        ;; Don't make undo entries for readin.
        (buffer-undo-list t))
    (switch-to-buffer (get-buffer-create mark-select-buffer-name))
    (erase-buffer)
    (mark-select-mode do-func new-mode-name)
    (when mark-select-help-message
      (insert mark-select-help-message "\n\n"))
    (apply body-func args)
    (goto-char (point-min))
    (when (re-search-forward mark-select-re-mark nil t)
      (mark-select-move-to-element))
    ))

;;;###autoload
(defun mark-select-multi (elements-hash &optional do-func new-mode-name)
  (interactive)
  (mark-select-buffer
   do-func new-mode-name
   (lambda (elements-hash)
     (let (elements)
       (dolist (key (mark-select-hash-keys elements-hash))
         (setq elements (gethash key elements-hash))
         (mark-select-insert-elements
          (cadr elements)
          (car elements)
          key)
         (insert "\n"))))
   elements-hash))

;;;###autoload
(defun mark-select (element-list &optional header do-func new-mode-name)
  (interactive)
  (mark-select-buffer
   do-func new-mode-name
   (lambda (element-list &optional header)
     (mark-select-insert-elements element-list header))
   element-list header))

;;;###autoload
(defun mark-select-mode (&optional do-func new-mode-name)
  (interactive)
  (kill-all-local-variables)
  (use-local-map mark-select-mode-map)
  (setq major-mode 'mark-select-mode
        mode-name (capitalize (if new-mode-name new-mode-name mark-select-mode-name))
        ;; case-fold-search nil
        buffer-read-only t)
  (set (make-local-variable 'font-lock-defaults)
       '(mark-select-font-lock-keywords t nil nil beginning-of-line))
  (set (make-local-variable 'mark-select-do-marked-func)
       (if do-func do-func mark-select-do-marked-func))
  (set (make-local-variable 'mark-select-buffer-name)
       (concat "*" (downcase mode-name) "*"))
  (rename-buffer mark-select-buffer-name)
  (turn-on-font-lock)
  (run-mode-hooks 'mark-select-mode-hook))

(provide 'mark-select)