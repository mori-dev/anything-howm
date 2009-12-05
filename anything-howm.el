;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(require 'cl)
(require 'anything)
(require 'anything-match-plugin)
(require 'howm)
(require 'howm-menu)

(defvar anything-howm-recent-menu-number-limit 70)
(defvar anything-howm-persistent-action-buffer "*howm-tmp*")

(defun anything-howm-select-file-by-title (title recent-menu-list)
  (loop for recent-menu-x in recent-menu-list
        for list-item-file  = (first recent-menu-x)
        for list-item-name  = (second recent-menu-x)
        if (string-equal title list-item-name)
          return list-item-file))

(defun anything-howm-get-recent-title-list (recent-menu-list)
  (loop for recent-menu-x in recent-menu-list
        for list-item-name  = (second recent-menu-x)
        collect list-item-name))

(setq anything-c-howm-recent
  '((name . "最近のメモ")
    (candidates .
      (lambda ()
        (anything-howm-get-recent-title-list
         (howm-recent-menu anything-howm-recent-menu-number-limit))))
    (action . (("Open howm file" .
      (lambda (candidate)
        (find-file
         (anything-howm-select-file-by-title candidate
                                             (howm-recent-menu anything-howm-recent-menu-number-limit)))))))
    (persistent-action .
      (lambda (candidate)
        (anything-howm-persistent-action (anything-howm-select-file-by-title candidate
                                              (howm-recent-menu anything-howm-recent-menu-number-limit)))))
    (cleanup .
      (lambda ()
        (if (get-buffer anything-howm-persistent-action-buffer)
          (kill-buffer anything-howm-persistent-action-buffer))))
    ))

(defun anything-howm-persistent-action (c)
  (let ((b (get-buffer-create anything-howm-persistent-action-buffer)))
      (with-current-buffer b
        (erase-buffer)
        (insert-file-contents c)
        (goto-char (point-min)))
      (pop-to-buffer b)
      (howm-mode t)))


;; ToDo
;; スクラップ機能
(defun anything-mmemo-howm-scrap (&optional arg)
  (interactive "P")
  (let (category
        memo-text str
        (cbuf (current-buffer))
        (via (cond
              ((string= 'w3m-mode major-mode)
               w3m-current-url
               ))))
    (cond
     (mark-active
      (setq str (buffer-substring (region-beginning) (region-end)))
      (setq category (read-from-minibuffer "input title : "
                                           "スクラップ"))
      (howm-create-file-with-title category nil nil nil "")
      (goto-char (point-max))
      (save-excursion
        (insert str)
        (if via
            (insert (concat "\nvia " via)))
        ))
     (t
      (message "メモを取る範囲をリージョンで選択してください")))))

(provide 'anything-howm)