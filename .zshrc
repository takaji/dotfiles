###############
# zsh Setting
###############

###############
# ヒストリ関連
###############
# 履歴の保存先
HISTFILE=$HOME/.zsh-history
## メモリに展開する履歴の数
# HISTSIZE=10000
## 保存する履歴の数
SAVEHIST=10000

## コマンドラインの先頭がスペースで始まる場合ヒストリに追加しない
setopt hist_ignore_space
## history (fc -l) コマンドをヒストリリストから取り除く。
setopt hist_no_store
## 直前と同じコマンドをヒストリに追加しない
setopt hist_ignore_dups
## ヒストリを呼び出してから実行する間に一旦編集
setopt hist_verify
## zsh の開始, 終了時刻をヒストリファイルに書き込む
setopt extended_history
## 余分な空白は詰めて記録
setopt hist_reduce_blanks
## 古いコマンドと同じものは無視
setopt hist_save_no_dups
## ヒストリに追加されるコマンド行が古いものと同じなら古いものを削除
setopt hist_ignore_all_dups
## 補完時にヒストリを自動的に展開
setopt hist_expand

## Screenでのコマンド共有用
## シェルを横断して.zshhistoryに記録
setopt inc_append_history
## ヒストリを共有
setopt share_history

###################
# ディレクトリ変更
###################
## ディレクトリ名だけで cd
setopt auto_cd
## cd 時に自動で push
setopt auto_pushd
## 同じディレクトリを pushd しない
setopt pushd_ignore_dups

############
# 補間関連
############
## 補完機能の有効
autoload -U compinit
compinit -u

## TAB で順に補完候補を切り替える
setopt auto_menu
## サスペンド中のプロセスと同じコマンド名を実行した場合はリジューム
setopt auto_resume
## 補完候補を一覧表示
setopt auto_list
## カッコの対応などを自動的に補完
setopt auto_param_keys
## ディレクトリ名の補完で末尾の / を自動的に付加し、次の補完に備える
setopt auto_param_slash
## 補完候補のカーソル選択を有効に
zstyle ':completion:*:default' menu select=1
## 補完候補の色づけ
eval `dircolors`
export ZLS_COLORS=$LS_COLORS
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
## 補完候補一覧でファイルの種別をマーク表示
setopt list_types
## 補完候補を詰めて表示
setopt list_packed

#########
# グロブ
#########
## 拡張グロブを使用
## ファイル名で #, ~, ^ の 3 文字を正規表現として扱う
setopt extended_glob
## =command を command のパス名に展開する
setopt equals
## --prefix=/usr などの = 以降も補完
setopt magic_equal_subst
## ファイル名の展開で辞書順ではなく数値的にソート
setopt numeric_glob_sort

##########
# 入出力
##########
## 出力時8ビットを通す
setopt print_eight_bit
## スペルチェック。間違うと訂正してくれる
setopt correct
## 出力の文字列末尾に改行コードが無い場合でも表示
unsetopt promptcr
## {a-c} を a b c に展開する機能を使えるようにする
setopt brace_ccl
## Ctrl+S/Ctrl+Q によるフロー制御を使わないようにする
setopt NO_flow_control
## コマンドラインでも # 以降をコメントと見なす
setopt interactive_comments
## ファイル名の展開でディレクトリにマッチした場合末尾に / を付加する
setopt mark_dirs
## 最後のスラッシュを自動的に削除しない
setopt noautoremoveslash

###############
# キーバインド
###############
## Emacsライクキーバインド設定
bindkey -e

##################
# プロンプト関連
##################
# 色有効
autoload -U colors
colors

## 色を使う
setopt prompt_subst

# 色を定義
local GREEN=$'%{\e[1;32m%}'
local BLUE=$'%{\e[1;34m%}'
local DEFAULT=$'%{\e[1;m%}'

# 通常のプロンプト
#PROMPT=$BLUE'[%n@%m] %(!.#.$) '$WHITE
PROMPT=$BLUE'[%n]%# '$WHITE
# 右側のプロンプト。ここでカレントディレクトリを出す。
RPROMPT=$GREEN'[%~]'$WHITE
setopt transient_rprompt

##############
# ジョブ制御
##############
## 内部コマンド jobs の出力をデフォルトで jobs -l にする
setopt long_list_jobs


#################
# その他・未分類
#################
## コアダンプサイズを制限
# limit coredumpsize 102400


## ビープを鳴らさない
setopt nobeep

#########
# Alias
#########
alias ls='ls -F --show-control-chars --color=aut'
alias ll='ls -ltr'
alias la='ls -a'
alias lal='ls -al'

alias c='cygstart'
alias r='ruby'
alias l='less'
alias m='emacsclient -nw'

#alias cd=cdls
alias cdp='cd ../'
alias cdpp='cd ../../'

# short function
alias enable_bashrc="source ~/.bashrc"
alias enable_emacs="source ~/.bashrc"
alias reboot_emacs="emacsclient -e \'\(kill-emacs\)\';emacs --daemon"

# screen実行中にemacsで保存が効かない場合の対策
stty ixany
stty ixoff -ixon

#######################
# グローバルエイリアス
#######################
alias -g L='| less'
alias -g H='| head'
alias -g T='| tail'
alias -g G='| grep'
alias -g GI='| grep -i'

############
# Function
############
# cd して ls する
function cdls() {
    # cdがaliasでループするので\をつける
    \cd $1;
    ls;
}

###############
# Environment
###############
export LANG=ja_JP.UTF-8
export LD_LIBRARY_PATH=/usr/local/lib
export LIBRARY_PATH=/usr/local/lib
export CPATH=/usr/local/include

# screen実行中にemacsで閉じた場合の対策
stty ixany
stty ixoff -ixon

#initial exec
#screen -xR
#export LANG=ja_JP.utf8


# less vのときにemacs起動
# export EDITOR=emacs
# EmacsClient起動
export EDITOR='emacsclient -nw'


#THIS MUST BE AT THE END OF THE FILE FOR GVM TO WORK!!!
[[ -s "/home/takahashi/.gvm/bin/gvm-init.sh" ]] && source "/home/takahashi/.gvm/bin/gvm-init.sh"

# エイリアス
alias pg_ctl='pg_ctl -l /var/log/postgresql.log'
alias less='less -N'
alias cs='cygstart'

# グローバルエイリアス
alias -g A='| ag'
alias -g G='| grep'
alias -g X='| xargs'
alias -g C='| cat'
alias -g L='| less'
alias -g H='| head'
alias -g T='| tail'
alias -g W='| wc'
