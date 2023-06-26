;;; code-groups.el --- Minor mode for grouping code hunks and auto-code

;; code-groups support for Emacs
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Description: Combine related code hunks into groups that are:
;; language-neutral, foldable, and has special 'auto-code' group that can be
;; auto-generated from data models and templates.
;;
;; Author: Victor Rybynok
;; Copyright (C) 2021, Victor Rybynok, all rights reserved.

;;; Code:

(require 'hideshow)

(defgroup code-groups nil
  "Code Groups mode."
  :prefix "cgs-"
  ;; :group '???
  )

(defvar-local cgs-forward-list-original #'forward-list
  "Original forward-list function used by the major mode before loading
code-groups minor mode - i.e. the function usually bound to C-M-n")

(defvar-local cgs-backward-list-original #'backward-list
  "Original backward-list function used by the major mode before loading
code-groups minor mode - i.e. the function usually bound to C-M-p")

(defvar-local cgs-hs-toggle-hiding-original #'hs-toggle-hiding
  "Original hs-toggle-hiding function used by the major mode before loading
code-groups minor mode - i.e. the function usually bound to C-S-j")

(defvar cgs--doxygen-group-open-token "///@{")
(defvar cgs--doxygen-group-close-token "///@}")

(defvar cgs--auto-code-group-open-token "/a/{")
(defvar cgs--auto-code-group-close-token "/a/}")

(defvar cgs--block-code-group-open-token "/b/{")
(defvar cgs--block-code-group-close-token "/b/}")

(defvar cgs--custom-code-group-open-token "/c/{")
(defvar cgs--custom-code-group-close-token "/c/}")

(defvar cgs--auto-code-group-param-token "/a/$")

(defun cgs--group-head-regexp (open-token)
  (concat "^.*" open-token ".*$"))

(defun cgs--group-tail-regexp (close-token)
  (concat "^.*" close-token ".*$"))

(defun cgs--looking-at-group-head (open-token)
  (let ((line (thing-at-point 'line t)))
    (when (and line
               (string-match-p (concat "^.*" open-token ".*$") line))
      open-token)))

(defun cgs--looking-at-group-tail (close-token)
  (let ((line (thing-at-point 'line t)))
    (when (and line
               (string-match-p (concat "^.*" close-token ".*$") line))
      close-token)))

(defun cgs--group-head-or-tail-length (token line)
  (length
   (replace-regexp-in-string
    (concat "^.*\\(" token ".*\\)[\r\n]?$")
    "\\1"
    line)))

(defun cgs--group-reverse-token (token)
  (cond
   ((string= cgs--doxygen-group-open-token token)
    cgs--doxygen-group-close-token)
   ((string= cgs--doxygen-group-close-token token)
    cgs--doxygen-group-open-token)
   ((string= cgs--auto-code-group-open-token token)
    cgs--auto-code-group-close-token)
   ((string= cgs--auto-code-group-close-token token)
    cgs--auto-code-group-open-token)
   ((string= cgs--custom-code-group-open-token token)
    cgs--custom-code-group-close-token)
   ((string= cgs--custom-code-group-close-token token)
    cgs--custom-code-group-open-token)
   ((string= cgs--block-code-group-open-token token)
    cgs--block-code-group-close-token)
   ((string= cgs--block-code-group-close-token token)
    cgs--block-code-group-open-token)))

(defun cgs-looking-at-auto-code-group-head-or-tail ()
  (cond ((cgs--looking-at-group-head
          cgs--auto-code-group-open-token)
         cgs--auto-code-group-open-token)
        ((cgs--looking-at-group-head
          cgs--auto-code-group-close-token)
         cgs--auto-code-group-close-token)))

(defun cgs--looking-at-any-group-head ()
  (cond ((cgs--looking-at-group-head
          cgs--doxygen-group-open-token)
         cgs--doxygen-group-open-token)
        ((cgs--looking-at-group-head
          cgs--auto-code-group-open-token)
         cgs--auto-code-group-open-token)
        ((cgs--looking-at-group-head
          cgs--custom-code-group-open-token)
         cgs--custom-code-group-open-token)
        ((cgs--looking-at-group-head
          cgs--block-code-group-open-token)
         cgs--block-code-group-open-token)))

(defun cgs--looking-at-any-group-tail ()
  (cond ((cgs--looking-at-group-tail
          cgs--doxygen-group-close-token)
         cgs--doxygen-group-close-token)
        ((cgs--looking-at-group-tail
          cgs--auto-code-group-close-token)
         cgs--auto-code-group-close-token)
        ((cgs--looking-at-group-tail
          cgs--custom-code-group-close-token)
         cgs--custom-code-group-close-token)
        ((cgs--looking-at-group-tail
          cgs--block-code-group-close-token)
         cgs--block-code-group-close-token)))

(defun cgs--search-backward-group-balanced-head ()
  (let ((open-token)
        (close-token)
        (mark-pos (point)))
    (setq close-token (cgs--looking-at-any-group-tail))
    (when close-token
      (setq open-token (cgs--group-reverse-token close-token))
      (move-beginning-of-line nil)
      (if (cgs--looking-at-group-head open-token)
          (search-forward open-token)
        (let ((pos nil)
              (found nil)
              (skip-tail 0))
          (push-mark mark-pos t)
          (while (and (not found)
                      (setq pos (re-search-backward
                                 (concat (cgs--group-head-regexp open-token)
                                         "\\|"
                                         (cgs--group-tail-regexp close-token)))))
            (if (cgs--looking-at-group-tail close-token)
                (cl-incf skip-tail)
              (if (<= skip-tail 0)
                  (setq found t)
                (cl-decf skip-tail))))
          (when (cgs--looking-at-group-head open-token)
            (move-end-of-line nil)
            (backward-char (cgs--group-head-or-tail-length
                            open-token (thing-at-point 'line t))))
          (point))))))

(defun cgs--search-forward-group-balanced-tail ()
  (let ((open-token)
        (close-token)
        (mark-pos (point)))
    (setq open-token (cgs--looking-at-any-group-head))
    (when open-token
      (setq close-token (cgs--group-reverse-token open-token))
      (move-end-of-line nil)
      (if (cgs--looking-at-group-tail close-token)
          (search-backward close-token)
        (let ((pos nil)
              (found nil)
              (skip-tail 0))
          (push-mark mark-pos t)
          (while (and (not found)
                      (setq pos (re-search-forward
                                 (concat (cgs--group-head-regexp open-token)
                                         "\\|"
                                         (cgs--group-tail-regexp close-token)))))
            (if (cgs--looking-at-group-head open-token)
                (cl-incf skip-tail)
              (if (<= skip-tail 0)
                  (setq found t)
                (cl-decf skip-tail))))
          pos)))))

(defun cgs-hs-hide-group ()
  (interactive)
  (let (open-token close-token)
    (when (cgs--looking-at-any-group-tail)
      (cgs--search-backward-group-balanced-head))
    (setq open-token (cgs--looking-at-any-group-head))
    (when open-token
      (setq close-token (cgs--group-reverse-token open-token))
      (move-beginning-of-line nil)
      (let* ((beg (search-forward open-token))
             (end (- (cgs--search-forward-group-balanced-tail)
                     (cgs--group-head-or-tail-length
                      close-token (thing-at-point 'line t)))))
        (hs-make-overlay beg end 'comment beg end)
        (goto-char beg)))))

(defun cgs-hs-toggle-hiding ()
  (interactive)
  (let (open-token close-token)
    (setq open-token (cgs--looking-at-any-group-head))
    (if open-token
        (setq close-token (cgs--group-reverse-token open-token))
      (progn
        (setq close-token (cgs--looking-at-any-group-tail))
        (when close-token
          (setq open-token (cgs--group-reverse-token close-token)))))
    (if open-token
        (let ((hidden nil)
              (at-tail (cgs--looking-at-group-tail close-token)))
          (save-excursion
            (move-beginning-of-line nil)
            (if (cgs--looking-at-group-head open-token)
                (progn
                  (end-of-visual-line)
                  (if (cgs--looking-at-group-tail close-token)
                      (setq hidden t)))))
          (if hidden
              (progn
                (move-beginning-of-line nil)
                (search-forward open-token)
                ;; (if (not at-tail)
                ;;     (let ((hs-minor-mode t))
                ;;       (hs-show-block)))
                (let ((hs-minor-mode t))
                  (hs-show-block)))
            (cgs-hs-hide-group)))
      (if cgs-hs-toggle-hiding-original
          (funcall cgs-hs-toggle-hiding-original)
        (hs-toggle-hiding))
      ;; (hs-toggle-hiding)
      )))

(defun cgs--delete-code-group ()
  (interactive)
  (when (cgs--looking-at-any-group-tail)
    (cgs--search-backward-group-balanced-head))
  (when (cgs--looking-at-any-group-head)
    (let ((start) (end))
      (move-beginning-of-line 2)
      (setq start (point))
      (forward-line -1)
      (cgs--search-forward-group-balanced-tail)
      (move-beginning-of-line nil)
      (setq end (point))
      (goto-char start)
      (delete-region start end))))

(defun cgs--get-auto-code-main-path (file-name)
  (let* ((buffer-dir (file-name-directory file-name))
         (src-tree-root (locate-dominating-file buffer-dir "auto-code/main")))
    (when src-tree-root
      (expand-file-name (concat src-tree-root "auto-code/main")))))

(defun cgs--generate-auto-code
    (generator-name
     model-name
     template-name
     auto-codded-file-path
     indentation-str)
  (let ((auto-code-main-path
         (cgs--get-auto-code-main-path auto-codded-file-path))
        auto-code-command lines last-line)
    (unless auto-code-main-path (error "auto-code/main path not found." ))
    (setq auto-code-command (concat (file-name-as-directory
                                     auto-code-main-path)
                                    generator-name))
    (unless (file-exists-p auto-code-command)
      (error (concat
              "auto-code/main/" generator-name " generator does not exist.")))
    (setq auto-code-command (string-join (list auto-code-command
                                               model-name
                                               template-name
                                               auto-codded-file-path)
                                         " "))
    (setq lines (split-string
                 (shell-command-to-string auto-code-command) "\n"))
    (setq last-line (car (last lines)))
    (setq lines (nbutlast lines))
    (seq-each
     (lambda (line)
       (if (string-empty-p line)
           (insert "\n")
         (insert (concat indentation-str line "\n"))))
     lines)
    (unless (string-empty-p last-line)
      (insert (concat indentation-str last-line "\n")))))

;;;###autoload
(defun cgs-generate-auto-code-group ()
  (interactive)
  (let* ((current-line (thing-at-point 'line t))
         (open-token (regexp-quote cgs--auto-code-group-open-token))
         (close-token (regexp-quote cgs--auto-code-group-close-token))
         (param-token (regexp-quote cgs--auto-code-group-param-token))
         (regex-begin
          "^\\([[:blank:]]*\\)[^[:blank:]\r\n]+[[:blank:]]*")
         (regex-params-end
          (concat
           "[[:blank:]]*\\([^[:blank:]\r\n]+\\)[[:blank:]]+"
           "\\([^[:blank:]\r\n]+\\)[[:blank:]]+\\([^[:blank:]\r\n]+\\)"))
         (regex-end "$")
         (close-regex
          (concat regex-begin close-token))
         (open-regex
          (concat regex-begin open-token))
         (open-single-line-regex
          (concat regex-begin open-token regex-params-end))
         (open-multi-line-regex
          (concat regex-begin open-token regex-end))
         (param-single-line-regex
          (concat regex-begin param-token regex-params-end))
         (auto-codded-file-path (buffer-file-name))
         generator data template open-indent-str close-indent-str indent-str)
    (unless auto-codded-file-path
      (error "auto-code only works with buffers representing real files." ))
    (save-match-data
      (cond ((string-match close-regex current-line)
             (setq close-indent-str (match-string 1 current-line))
             (cgs--search-backward-group-balanced-head)
             (setq current-line (thing-at-point 'line t)))
            ((string-match open-regex current-line)
             (save-excursion
               (cgs--search-forward-group-balanced-tail)
               (let ((line (thing-at-point 'line t)))
                 (string-match close-regex line)
                 (setq close-indent-str (match-string 1 line))))))
      (cond ((string-match open-single-line-regex current-line)
             (setq open-indent-str (match-string 1 current-line))
             (unless (string= open-indent-str close-indent-str)
               (error "auto-code open and close indentations do not match." ))
             (setq generator (match-string 2 current-line))
             (setq data (match-string 3 current-line))
             (setq template (match-string 4 current-line))
             (setq indent-str (match-string 1 current-line))
             (cgs--delete-code-group)
             (cgs--generate-auto-code generator
                                      data
                                      template
                                      auto-codded-file-path
                                      indent-str))
            ((string-match open-multi-line-regex current-line)
             (setq open-indent-str (match-string 1 current-line))
             (unless (string= open-indent-str close-indent-str)
               (error "auto-code open and close indentations do not match." ))
             (save-excursion
               (move-beginning-of-line 0)
               (setq current-line (thing-at-point 'line t)))
             (if (not (string-match param-single-line-regex current-line))
                 (error "Invalid auto-code open multi-line block." )
               (setq generator (match-string 2 current-line))
               (setq data (match-string 3 current-line))
               (setq template (match-string 4 current-line))
               ;; The following condition should be removed
               ;; once all templates are moved to automatic indentation
               (unless (string= open-indent-str (match-string 1 current-line))
                 (error (concat "auto-code open multi line block "
                                "is not uniformly indented.")))
               (setq indent-str (match-string 1 current-line))
               (cgs--delete-code-group)
               (cgs--generate-auto-code generator
                                        data
                                        template
                                        auto-codded-file-path
                                        indent-str)))))))

;;;###autoload
(defun cgs-forward-list (arg)
  (interactive "^p")
  (if (cgs--looking-at-any-group-head)
      (cgs--search-forward-group-balanced-tail)
    (if cgs-forward-list-original
        (funcall cgs-forward-list-original arg)
      (forward-list arg))))

;;;###autoload
(defun cgs-backward-list (arg)
  (interactive "^p")
  (if (cgs--looking-at-any-group-tail)
      (cgs--search-backward-group-balanced-head)
    (if cgs-backward-list-original
        (funcall cgs-backward-list-original arg)
      (backward-list arg))))

(defvar code-groups-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-S-j") #'cgs-hs-toggle-hiding)
    (define-key map (kbd "C-M-n") #'cgs-forward-list)
    (define-key map (kbd "C-M-p") #'cgs-backward-list)
    (define-key map (kbd "C-'") #'cgs-generate-auto-code-group)
    map))

;;;###autoload
(define-minor-mode code-groups-mode
  "Minor mode for grouping code hunks and auto-code."
  :lighter " code-groups"
  :keymap code-groups-mode-keymap)

(provide 'code-groups)
;;; code-groups.el ends here
