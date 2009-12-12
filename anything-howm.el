;;; anything-howm.el

;; Copyright (C) 2009,2010  kitokitoki

;; Author: kitokitoki <morihenotegami@gmail.com>
;; Keywords: anything, howm
;; Prefix: anything-howm

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
;; `anything.el' http://www.emacswiki.org/emacs/anything.el
;; `anything-match-plugin.el'  http://www.emacswiki.org/emacs/anything-match-plugin.el
;; `anything-migemo.el' http://www.emacswiki.org/emacs/anything-migemo.el
;; `howm'  http://howm.sourceforge.jp/index-j.html

;; `anything-howm.el' http://github.com/kitokitoki/anything-howm (this file)

;;; Setting Sample

;; (require 'anything-howm)
;; (setq anything-howm-recent-menu-number-limit 70)
;; (global-set-key (kbd "M-h") 'anything-howm-search)
;; (setq anything-sources
;;       (list anything-c-source-buffers
;;               ...
;;             ))

;; Change Log

;; 1.0.2: ファイル削除、新ウィンドウで開く、新フレームで開くアクションを追加
;;        リファクタリング
;; 1.0.1: 新しいメモをつくる機能を追加, migemo 対応
;; 1.0.0: 新規作成

;;; Commentary:

;; TODO

;; howm-list-all に基づくソースの提供
;; 全文検索機能
;; (defun anything-c-moccur-occur-by-moccur-only-function ()を元に
;; (defun anything-howm-search-only-title ()をつくる
;;; Code:

(require 'cl)
(require 'anything)
(require 'anything-match-plugin)
(require 'anything-migemo)
(require 'howm)
(require 'howm-menu)

(defvar anything-howm-recent-menu-number-limit 10)
(defvar anything-howm-persistent-action-buffer "*howm-tmp*")
(defvar anything-howm-selected-text "")
(defvar anything-howm-default-title "")

(defvar anything-c-howm-recent
  '((init .
      (lambda ()
        (setq anything-howm-selected-text
          (if mark-active
              (buffer-substring-no-properties (region-beginning) (region-end))
            ""))))
    (name . "最近のメモ")
;;(howm-list-all) で全部とれる
    (candidates .
      (lambda ()
        (anything-howm-get-recent-title-list
         (howm-recent-menu anything-howm-recent-menu-number-limit))))
    (action .
      (("Open howm file" .
          (lambda (candidate)
            (find-file
             (anything-howm-select-file-by-title candidate))))
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
            (anything-howm-create-new-memo anything-howm-selected-text)))
       ("Delete File" .
          (lambda (candidate)
            (if (y-or-n-p (format "Really delete file %s? "
                  (anything-howm-select-file-by-title candidate)))
              (delete-file
                (anything-howm-select-file-by-title candidate)))))))
    (persistent-action .
      (lambda (candidate)
        (anything-howm-persistent-action
         (anything-howm-select-file-by-title candidate))))
    (cleanup .
      (lambda ()
        (if (get-buffer anything-howm-persistent-action-buffer)
          (kill-buffer anything-howm-persistent-action-buffer))))
    (migemo)))

(defun anything-howm-persistent-action (c)
  (let ((b (get-buffer-create anything-howm-persistent-action-buffer)))
      (with-current-buffer b
        (erase-buffer)
        (insert-file-contents c)
        (goto-char (point-min)))
      (pop-to-buffer b)
      (howm-mode t)))

(defun anything-howm-select-file-by-title (title)
  (loop for recent-menu-x in (howm-recent-menu anything-howm-recent-menu-number-limit)
        for list-item-file  = (first recent-menu-x)
        for list-item-name  = (second recent-menu-x)
        if (string-equal title list-item-name)
          return list-item-file))

(defun anything-howm-get-recent-title-list (recent-menu-list)
  (loop for recent-menu-x in recent-menu-list
        for list-item-name  = (second recent-menu-x)
        collect list-item-name))

(defun anything-howm-create-new-memo (text)
  (let (memo-text str
        (cbuf (current-buffer)))
    (setq str text)
    (howm-create-file-with-title anything-howm-default-title nil nil nil "")
    (save-excursion
      (goto-char (point-max))
      (insert str))
    (goto-char (point-min))
    (end-of-line)))

(defun howm-create-file-with-title (title &optional
                                    which-template not-use-file here content)
  (let ((b (current-buffer)))
    (when (not here)
      (howm-create-file))
    (cond ((howm-buffer-empty-p) nil)
          ((and here howm-create-here-just) (beginning-of-line))
          (t (howm-create-newline)))
    (let ((p (point))
          (insert-f (lambda (switch)
                      (howm-insert-template (if switch title "")
                                            b which-template (not switch))))
          (use-file (not not-use-file)))
      ;; second candidate which appears when undo is called
      (let ((end (funcall insert-f not-use-file)))
        (save-excursion
          (goto-char end)
          (insert (or content "")))
        (undo-boundary)
        (delete-region p end))
      (funcall insert-f use-file))
    (howm-create-finish)))

(defun anything-howm-search ()
  (interactive)
  (anything
   (list anything-c-howm-recent) nil nil nil nil
   "*howm-title-search*"))

(provide 'anything-howm)