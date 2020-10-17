set PATH $HOME/.emacs.d/bin/ $PATH
set PATH $HOME/bin $PATH
set PATH $HOME/go/bin $PATH
set -Ux PYENV_ROOT $HOME/.pyenv
set -Ux fish_user_paths $PYENV_ROOT/bin $fish_user_paths

function vterm_printf;
    if [ -n "$TMUX" ]
        # tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end

function vterm_prompt_end;
    vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)
end
functions -c fish_prompt vterm_old_fish_prompt

function fish_prompt --description 'Write out the prompt; do not replace this. Instead, put this at end of your file.'
    # Remove the trailing newline from the original prompt. This is done
    # using the string builtin from fish, but to make sure any escape codes
    # are correctly interpreted, use %b for printf.
    printf "%b" (string join "\n" (vterm_old_fish_prompt))
    vterm_prompt_end
end


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
