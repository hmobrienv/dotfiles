set PATH $HOME/.emacs.d/bin/ $PATH
set PATH $HOME/bin $PATH
set PATH $HOME/go/bin $PATH
set PATH "$HOME/.cargo/bin" $PATH;
set -Ux PYENV_ROOT $HOME/.pyenv
set -Ux fish_user_paths $PYENV_ROOT/bin $fish_user_paths

set -g fish_user_paths "/usr/local/opt/util-linux/bin" $fish_user_paths

status --is-interactive; and source (pyenv init -|psub)
eval (direnv hook fish)

# Easier navigation: .., ..., ...., ....., ~ and -
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."

# Shortcuts
alias dl="cd ~/Downloads"
alias dt="cd ~/Desktop"
alias mkdir="mkdir -p"
alias reload!="source ~/.bash_profile"
alias ipme="ipconfig getifaddr en0"
alias show80="sudo lsof -iTCP -sTCP:LISTEN -Pnl | grep :80"

# Lolz
alias tmuch="tmux"

# Git
alias g="git"
alias gb="git branch"
alias gc="git commit"
alias gp="git push"
alias gpl="git pull"
alias gcm="git commit -m"
alias gcmr="git commit -m 'Shut up, rubocop'"
alias gcam="git commit -am"
alias gs="git status"
alias gco="git checkout"
alias gcom="git checkout master"
alias gba="git branch -a"
alias gca="git commit -a"
alias gadd="git add"
alias gaa="git add -A"
alias gpo="git push origin -u"
alias grhh="git reset --hard HEAD"

# Checksums (for mac)
alias sha256="openssl dgst -sha256"

# Prettify JSON from clipboard
alias jsonpp='pbpaste | json_pp | pbcopy'

# Docker
alias d='docker'
alias dc='docker-compose'
alias dm='docker-machine'
alias dsa='docker stop (docker ps -a -q)' # Docker stop all containers

# Go
alias gcover='go test -coverprofile=coverage.out && go tool cover -html=coverage.out'
alias gtest='go test (go list ./... | grep -v /vendor/)'
alias glint='golint (go list ./... | grep -v /vendor/)'

# List all files colorized in long format
alias l="ls -lF -G"
alias ll="ls -lF -G"

# List all files colorized in long format, including dot files
alias la="ls -laF -G"

# List only directories
alias lsd="ls -lF -G | grep --color=never '^d'"

# Always use color output for `ls`
alias ls="command ls -G"

# View HTTP traffic
alias sniff="sudo ngrep -d 'en1' -t '^(GET|POST) ' 'tcp and port 80'"
alias httpdump="sudo tcpdump -i en1 -n -s 0 -w - | grep -a -o -E \"Host\: .*|GET \/.*\""

# Recursively delete `.DS_Store` files
alias cleanup="find . -type f -name '*.DS_Store' -ls -delete"
