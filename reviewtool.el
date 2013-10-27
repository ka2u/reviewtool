;;; reviewtool ---

;; Copyright (C) 2013 by Shibuya Kazuhiro

;; Author: Shibuya Kazuhiro <stevenlabs@gmail.com>
;; URL:
;; Version: 0.01

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

;;; Code:

;;;##autoload

(defun reviewtool:execute-command (cmd)
  (call-process-shell-command cmd nil t))

(defun reviewtool:in-git-repository-p ()
  (with-temp-buffer
    (let ((cmd "git rev-parse --is-inside-work-tree"))
      (when (zerop (reviewtool:execute-command cmd))
        (goto-char (point-min))
        (string= "true" (buffer-substring-no-properties
                         (point) (line-end-position)))))))

(defun reviewtool:root-directory ()
  (with-temp-buffer
    (let* ((cmd "git rev-parse --show-toplevel")
           (ret (reviewtool:execute-command cmd)))
      (when (zerop ret)
        (goto-char (point-min))
        (let ((root (buffer-substring-no-properties (point) (line-end-position))))
          (unless (string= root "")
            (file-name-as-directory root)))))))

(defun reviewtool:git-graph ()
  (with-temp-buffer
    (let* ((cmd "git log --graph --date=short --decorate=short --pretty=format:'%h %cd %cn %d %s' ")
           (ret (reviewtool:execute-command cmd)))
      (when (zerop ret)
        (buffer-string)))))

(defun reviewtool:git-diff (rev)
  (with-temp-buffer
    (let* ((cmd (concat "git diff " rev " HEAD"))
           (ret (reviewtool:execute-command cmd)))
      (when (zerop ret)
        (buffer-string)))))

(defun reviewtool:open-git-graph (path)
  (when (file-directory-p path)
    (setq path (concat path "/"))
    (setq default-directory path)
    (cond ((reviewtool:in-git-repository-p)
           (with-output-to-temp-buffer "*graph*" (princ (reviewtool:git-graph)))
           (switch-to-buffer "*graph*")
           (let ((map (make-sparse-keymap)))
             (define-key map "n" 'next-line)
             (define-key map "p" 'previous-line)
             (define-key map "q" 'burry-buffer)
             (define-key map "\r" 'reviewtool:open-git-diff)
             (use-local-map map)))
          (t (message "%s" (concat "this is not git repo : " path))))))

(defun reviewtool:open-git-diff ()
  (interactive)
  (let ((rev (thing-at-point 'word)))
    (with-output-to-temp-buffer "*git-diff*" (princ (reviewtool:git-diff rev)))
    (switch-to-buffer "*git-diff*")
    (let ((map (make-sparse-keymap)))
      (define-key map "n" 'next-line)
      (define-key map "p" 'previous-line)
      (define-key map "q" 'burry-buffer)
      (define-key map "\r" 'reviewtool:open-file)
      (use-local-map map))))

(defun reviewtool:open-file ()
  (interactive)
  (let* ((name (thing-at-point 'filename))
         (file (progn (debug (string-match "[ab]/\\(.*\\)" name))
                      (match-string 1 name))))
    (if (file-exists-p file)
        (find-file file)
      (mesage "%s" (concat "can't find file: " name)))))

(defun reviewtool (path)
  "review tool"
  (interactive "DRepo Path:")
  (reviewtool:open-git-graph path)
  (use-local-map reviewtool:graph-map)
  (setq major-mode 'reviewtool)
  (setq mode-name "reviewtool")
  (run-hooks 'reviewtol-hook))

(defun debug (mess)
  (y-or-n-p (message "%s" mess)))

(provide 'reviewtool)

;;; reviewtool ends here

