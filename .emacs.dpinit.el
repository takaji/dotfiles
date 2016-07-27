(require 'cask "~/.cask/cask.el")

(cask-initialize)

;;;; -*- mode: lisp-interaction; syntax: elisp; coding: utf-8-unix -*-
;; ~/elisp をライブラリパスに追加
(setq load-path
      (append
       (list
	(expand-file-name "~/elisp/")
	)
       load-path))

;; Set load-path
(setq load-path (cons "~/.emacs.d/lisp" load-path))

;; 日本語設定 (UTF-8)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(set-face-attribute 'default nil
		    :family "Consolas"
		    :height 120)

;(set-fontset-font t 'japanese-jisx0208 (font-spec :family "Meiryo"))

(load-theme 'misterioso t)

;; カーソル行に下線を表示
(setq hl-line-face 'underline)
(global-hl-line-mode)

;; 現在行をハイライト
;(global-hl-line-mode t)

;; フォントロックモード (強調表示等) を有効にする
(global-font-lock-mode t)

;; 一時マークモードの自動有効化
(setq-default transient-mark-mode t)

;; C-x C-u が何もしないように変更する (undo の typo 時誤動作防止)
(global-unset-key "\C-x\C-u")

;; 括弧の対応をハイライト.
(show-paren-mode 1) 

;; バッファ末尾に余計な改行コードを防ぐための設定
(setq next-line-add-newlines nil) 

;; C-x l で goto-line を実行
(define-key ctl-x-map "l" 'goto-line) 

;; C-mにnewline-andindentを割り当てる。初期値はnewline
(global-set-key (kbd "C-m") 'newline-and-indent)

;; 時間を表示
(display-time) 

;; 列数表示
(column-number-mode 1) 

;; メニューバーを消す
(menu-bar-mode -1)

;; C-h でカーソルの左にある文字を消す
(define-key global-map "\C-h" 'delete-backward-char)

;; C-h に割り当てられている関数 help-command を C-x C-h に割り当てる
(define-key global-map "\C-x\C-h" 'help-command)

;; C-o に動的略語展開機能を割り当てる
(define-key global-map "\C-o" 'dabbrev-expand)
(setq dabbrev-case-fold-search nil) ; 大文字小文字を区別

;; Window移動をC-x 矢印キーで
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)
		     
;; 日本語・英語混じり文での区切判定
;; http://www.alles.or.jp/~torutk/oojava/meadow/Meadow210Install.html
(defadvice dabbrev-expand
  (around modify-regexp-for-japanese activate compile)
  "Modify `dabbrev-abbrev-char-regexp' dynamically for Japanese words."
  (if (bobp)
      ad-do-it
    (let ((dabbrev-abbrev-char-regexp
           (let ((c (char-category-set (char-before))))
             (cond 
              ((aref c ?a) "[-_A-Za-z0-9]") ; ASCII
              ((aref c ?j) ; Japanese
               (cond
                ((aref c ?K) "\\cK") ; katakana
                ((aref c ?A) "\\cA") ; 2byte alphanumeric
                ((aref c ?H) "\\cH") ; hiragana
                ((aref c ?C) "\\cC") ; kanji
                (t "\\cj")))
              ((aref c ?k) "\\ck") ; hankaku-kana
              ((aref c ?r) "\\cr") ; Japanese roman ?
              (t dabbrev-abbrev-char-regexp)))))
      ad-do-it)))

;; BS で選択範囲を消す
(delete-selection-mode 1)

;; The local variables list in .emacs と言われるのを抑止
(add-to-list 'ignored-local-variables 'syntax) 

;; リセットされた場合に UTF-8 に戻す
;; http://0xcc.net/blog/archives/000041.html
(set-default-coding-systems 'utf-8)

;;; *.~ とかのバックアップファイルを作らない
;(setq make-backup-files nil)
;;; .#* とかのバックアップファイルを作らない
;(setq auto-save-default nil)

;;
;; backup の保存先
;;
(setq backup-directory-alist
      (cons (cons ".*" (expand-file-name "~/.emacs.d/backup"))
	    backup-directory-alist))


(setq auto-save-file-name-transforms
      `((".*", (expand-file-name "~/.emacs.d/backup/") t)))

;; cperl-mode の設定。インデントをいい感じに、他。
(autoload 'perl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
(setq cperl-auto-newline t)
(setq cperl-indent-parens-as-block t)
(setq cperl-close-paren-offset -4)
(setq cperl-indent-level 4)
(setq cperl-label-offset -4)
(setq cperl-continued-statement-offset 4)
(setq cperl-highlight-variables-indiscriminately t)
(add-hook 'cperl-mode-hook
          (lambda ()
            (set-face-italic-p 'cperl-hash-face nil)))
(add-hook 'cperl-mode-hook
	  '(lambda () 
	     (define-key cperl-mode-map "\C-cc" 'cperl-check-syntax)
	     (setq indent-tabs-mode nil)))

;;; ===== nXML =====
(add-hook 'nxml-mode-hook
	  (lambda ()
	    (setq nxml-slash-auto-complete-flag t)
	    (setq nxml-child-indent 2)
	    (setq indent-tabs-mode nil)
	    (setq tab-width 2)
	    (define-key nxml-mode-map "\r" 'newline-and-indent)
	    )

	  )

;;; === clojure-mode ====
(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  :diminish subword-mode)

;;; === cider-mode ===
(use-package cider
  :init
  (add-hook 'cider-mode-hook #'clj-refactor-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t
;        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))

;;; === clj-refactor-mode ===
(use-package clj-refactor
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c C-j"))

;;; === company-mode ===
(use-package company
  :config
  (global-company-mode)
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-selection-wrap-around t)

;  (bind-keys :map company-mode-map
;             ("C-i" . company-complete))
  (bind-keys :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("C-s" . company-search-words-regexp))
  (bind-keys :map company-search-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)))

;;; === smartparens-mode ===
(use-package smartparens
  :diminish smartparens-mode
  :init
  (smartparens-global-mode))

;;; === elscreen-mode ===
(use-package elscreen
  :config
  (setq elscreen-prefix-key (kbd "C-z"))
  (elscreen-start)
  ;;; タブの先頭に[X]を表示しない
  (setq elscreen-tab-display-kill-screen nil)
  ;;; header-lineの先頭に[<->]を表示しない
  (setq elscreen-tab-display-control nil))

;;; === real-auto-save ===
(use-package real-auto-save
  :config
  (setq real-auto-save-interval 1)
  (add-hook 'prog-mode-hook 'real-auto-save-mode)
  (add-hook 'find-file-hook 'real-auto-save-mode))

;;(eval-after-load 'clojure-mode
;;  '(progn
;;    (define-key clojure-mode-map (kbd "C-c C-h") #'clojure-cheatsheet)))

(global-set-key (kbd "C-x C-b") 'anything-for-files)
(global-set-key (kbd "M-y") 'anything-show-kill-ring)
(global-set-key (kbd "M-o") 'occur-by-moccur)

(setq moccur-split-word t)
;(require 'wgrep nil t)


;;; cua-modeの設定
(cua-mode t)
(setq cua-enable-cua-keys nil)  ; CUAキーバインドを無効にする

;;; simpleclipの設定
(simpleclip-mode t)
(global-set-key (kbd "C-c w") 'simpleclip-copy) ; C-c wでクリップボードにコピー
(global-set-key (kbd "C-c y") 'simpleclip-paste) ; C-c yでクリップボードからペースト

