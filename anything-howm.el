;;; anything-howm.el --- Anything completion for howm

;; Copyright (C) 2009-2011 kitokitoki

;; Author: kitokitoki <mori.dev.asdf@gmail.com>
;; Keywords: anything, howm
;; Prefix: anything-howm-

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

;;; Installation:

;; install requires libraries:
;; `migemo'                    http://0xcc.net/migemo/
;; `anything.el'               http://www.emacswiki.org/emacs/anything.el
;; `anything-config.el'        http://www.emacswiki.org/emacs/anything-config.el
;; `anything-match-plugin.el'  http://www.emacswiki.org/emacs/anything-match-plugin.el
;; `anything-migemo.el'        http://www.emacswiki.org/emacs/anything-migemo.el
;; `howm'                      http://howm.sourceforge.jp/index-j.html

;; `anything-howm.el'          http://github.com/kitokitoki/anything-howm (this file)

;;; Setting Sample

;; (require 'anything-howm)
;; 
;; (setq anything-howm-recent-menu-number-limit 600)
;; (setq anything-howm-data-directory "/path/to/howm-directory")
;; (global-set-key (kbd "C-2") 'anything-howm-menu-command)
;; (global-set-key (kbd "C-3") 'anything-cached-howm-menu)
;;
;; (defun anything-buffers ()
;;   (interactive)
;;   (anything-other-buffer
;;    '(anything-c-source-buffers+-howm-title
;;      anything-c-source-recentf
;;      ...
;;      )
;;    "*Buffer+File*"))
;; (global-set-key (kbd "M-h") 'anything-buffers)
;;
;; or
;;
;; (setq anything-sources
;;       (list
;;         anything-c-source-buffers+-howm-title ;これを追加
;;         ;; anything-c-source-buffers はコメントアウト
;;         anything-c-source-recentf など
;;         ...
;;         ))

;;; Bug Report:
;;
;; If you have problem, send a bug report via M-x anything-howm-send-bug-report.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of anything-howm.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "anything-howm.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x anything-howm-send-bug-report and M-x insert-buffer *Backtrace*
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)

;; Change Log
;; 1.0.7: ファイル名ではなくタイトルを一覧表示する
;;        anything-c-source-buffers+-howm-title を追加
;; 1.0.6: 専用の anything-resume を作成
;; 1.0.5: メニューリストに検索などの項目を追加。メニューソースでの (migemo)を廃止
;; 1.0.4: アクション"Open Marked howm file", "Delete file(s)" を作成
;; 1.0.3: メニュー用のソースを新規作成
;; 1.0.2: ファイル削除、新ウィンドウで開く、新フレームで開くアクションを追加
;;        リファクタリング
;; 1.0.1: 新しいメモをつくる機能を追加, migemo 対応
;; 1.0.0: 新規作成

;;; Commentary:

;;; Code:

(require 'cl)
(require 'anything)
(require 'anything-match-plugin)
(require 'anything-migemo)
(require 'howm)
(require 'howm-menu)

(defvar anything-howm-recent-menu-number-limit 10)
(defvar anything-howm-persistent-action-buffer "*howm-tmp*")
(defvar anything-howm-menu-buffer "*anything-howm-menu*")
(defvar anything-howm-default-title "")
(defvar anything-howm-data-directory "/path/to/howm-data-directory")

;;; Version

(defconst anything-howm-version "1.0.7"
  "The version number of the file anything-howm.el.")

(defun anything-howm-version (&optional here)
  "Show the anything-howm version in the echo area.
With prefix arg HERE, insert it at point."
  (interactive "P")
  (let ((version (format "anything-howm version %s" anything-howm-version)))
    (message version)
    (if here
      (insert version))))

(defvar anything-c-howm-recent
  '((name . "最近のメモ")
    (init . (lambda ()
              (with-current-buffer (anything-candidate-buffer 'global)
                (insert (mapconcat 'identity
                                   (anything-howm-get-recent-title-list
                                    (howm-recent-menu anything-howm-recent-menu-number-limit))
                                   "\n")))))
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action .
      (("Open howm file(s)" . anything-howm-find-files)
       ("Open howm file in other window" .
          (lambda (candidate)
            (find-file-other-window
             (anything-howm-select-file-by-title candidate))))
       ("Open howm file in other frame" .
          (lambda (candidate)
            (find-file-other-frame
             (anything-howm-select-file-by-title candidate))))
       ("Create new memo" .
          (lambda (template)
            (anything-howm-create-new-memo "")))
       ("Create new memo on region" .
          (lambda (template)
            (anything-howm-create-new-memo (anything-howm-set-selected-text))))
       ("Delete file(s)" . anything-howm-delete-marked-files)))
    (persistent-action . anything-howm-persistent-action)
    (cleanup .
      (lambda ()
        (anything-aif (get-buffer anything-howm-persistent-action-buffer)
          (kill-buffer it))))
    (migemo)
    ))

(defun anything-howm-persistent-action (candidate)
  (let ((buffer (get-buffer-create anything-howm-persistent-action-buffer)))
      (with-current-buffer buffer
        (erase-buffer)
        (insert-file-contents (anything-howm-select-file-by-title candidate))
        (goto-char (point-min)))
      (pop-to-buffer buffer)
      (howm-mode t)))

(defun anything-howm-select-file-by-title (title)
  (loop for recent-menu-x in (howm-recent-menu anything-howm-recent-menu-number-limit)
        for list-item-file  = (first recent-menu-x)
        for list-item-name  = (second recent-menu-x)
        if (string-equal title list-item-name)
          return list-item-file))

(defun anything-howm-find-files (candidate)
  (anything-aif (anything-marked-candidates)
      (dolist (i it)
        (find-file (anything-howm-select-file-by-title i)))
    (find-file (anything-howm-select-file-by-title candidate))))

(defun anything-howm-get-recent-title-list (recent-menu-list)
  (loop for recent-menu-x in recent-menu-list
        for list-item-name  = (second recent-menu-x)
        collect list-item-name))

(defun anything-howm-create-new-memo (text)
  (let (memo-text str
        (cbuf (current-buffer)))
    (setq str text)
    (howm-create-file-with-title anything-howm-default-title nil nil nil nil)
    (save-excursion
      (goto-char (point-max))
      (insert str))
    (goto-char (point-min))
    (end-of-line)))

(defun anything-howm-delete-marked-files (candidate)
  (anything-aif (anything-marked-candidates)
      (if (y-or-n-p (format "Delete *%s Files " (length it)))
          (progn
            (dolist (i it)
              (set-text-properties 0 (length i) nil i)
              (delete-file
                (anything-howm-select-file-by-title i)))
            (message "%s Files deleted" (length it)))
          (message "(No deletions performed)"))
    (set-text-properties 0 (length candidate) nil candidate)
    (if (y-or-n-p
         (format "Really delete file `%s' " (anything-howm-select-file-by-title candidate)))
        (progn
          (delete-file
            (anything-howm-select-file-by-title candidate))
          (message "1 file deleted"))
        (message "(No deletions performed)"))))

(defun anything-howm-set-selected-text ()
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    ""))

(defvar anything-howm-menu-list
      '(("c [メモを作成]" . "(anything-howm-create-new-memo \"\")")
        ("cr[リージョンからメモを作成]" . "(anything-howm-create-new-memo (anything-howm-set-selected-text))")
        ("s [固定]" . "(howm-list-grep-fixed)")
        ("g [正規]" . "(howm-list-grep)")
        ("m [roma]" . "(howm-list-migemo)")
        ("y [予定]" . "(howm-list-todo)")
        ("t [Todo]" . "(howm-list-schedule)")))

(defvar anything-c-source-howm-menu
  '((name . "メニュー")
    (candidates . anything-howm-menu-list)    
    (type . sexp)))

(defun anything-cached-howm-menu ()
  (interactive)
  (let ((anything-display-function 'anything-howm-display-buffer))    
    (if (get-buffer anything-howm-menu-buffer)
        (anything-resume anything-howm-menu-buffer)
      (anything-howm-menu-command))))

(defun anything-howm-menu-command ()
  (interactive)
  (let ((anything-display-function 'anything-howm-display-buffer))
    (anything-other-buffer
     '(anything-c-source-howm-menu
       anything-c-howm-recent)
     anything-howm-menu-buffer)))

(defun anything-howm-resume ()
  (interactive)
  (when (get-buffer anything-howm-menu-buffer)
    (anything-resume anything-howm-menu-buffer)))

(defun anything-howm-display-buffer (buf)
  "左右分割で表示する"
  (delete-other-windows)
  (split-window (selected-window) nil t)
  (pop-to-buffer buf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; howm のファイルは日付形式のため，複数開いていると見分けにくい。
;; anything-c-source-buffers+-howm-title では、一覧時にタイトルを表示する

(defvar anything-c-source-buffers+-howm-title
  '((name . "Buffers")
    (candidates . anything-c-buffer-list)
    (real-to-display . anything-howm-title-real-to-display)
    (type . buffer)
    (candidate-transformer
         anything-c-skip-current-buffer
         anything-c-highlight-buffers
         anything-c-skip-boring-buffers)
    (persistent-action . anything-c-buffers+-persistent-action)
    (persistent-help . "Show this buffer / C-u \\[anything-execute-persistent-action]: Kill this buffer")))
;;(anything anything-c-source-buffers+-howm-title)

(defun anything-howm-title-real-to-display (file-name)
  (if (equal "howm" (file-name-extension file-name))
      (anything-howm-title-get-title file-name)
    file-name))

(defun anything-howm-title-get-title (buffer)
  (with-current-buffer buffer
    (let ((point (point-min)))
      (save-excursion
        (goto-char (point-min))
        (end-of-line)
        (buffer-substring point (point))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; e.x, (global-set-key (kbd "C-c e") (anything-howm-fixed-term-command "emacs"))
(defun anything-howm-fixed-term-command (initial)
  (lexical-let ((initial initial))
    (lambda () (interactive) (anything 'anything-c-source-howm-recent initial))))

;; experimental code
;(anything-howm-get-filename (list howm-directory))
(defun anything-howm-get-filename (file-list)
    (loop for x in file-list
          with path-list = nil
          when (file-directory-p x)
            for path-list =
              (append
                (anything-howm-get-filename
                 (remove-if
                  (lambda(y) (string-match "\\.$\\|\\.svn" y))
                  (directory-files x t)))
                path-list)
          else
            collect x into path-list            
          end
          finally return path-list))

(defvar anything-c-source-howm-contents-grep
  `((name . "anything-howm-contents-grep")
    (grep-candidates . ,(anything-howm-get-filename (list howm-directory)))
    (header-name . (lambda (x) (concat x ": " anything-pattern)))
    (candidate-number-limit . 99999)))
;; (anything 'anything-c-source-howm-contents-grep)

;;;; Bug report
(defvar anything-howm-maintainer-mail-address
  (concat "mor" "i.dev.asdf@gm" "ail.com"))
(defvar anything-howm-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of anything-howm.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"anything-howm.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun anything-howm-send-bug-report ()
  (interactive)
  (reporter-submit-bug-report
   anything-howm-maintainer-mail-address
   "anything-howm.el"
   (apropos-internal "^eldoc-" 'boundp)
   nil nil
   anything-howm-bug-report-salutation))

(provide 'anything-howm)
