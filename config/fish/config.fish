
### Environment Set Up ###
bash -l $HOME/env/etc/indeed_profile
set -x PATH $HOME/bin $HOME/env/bin $PATH
set -g -x PYTHONSTARTUP=$HOME/.pythonrc
set -g -x EDITOR=vim
set -g -x TERM=xterm-256color

### Prompt ###

function fish_prompt
    env FISH_VERSION=$FISH_VERSION PROMPTLINE_LAST_EXIT_CODE=$status bash ~/.prompt.sh
end

#function fish_right_prompt
#    env FISH_VERSION=$FISH_VERSION PROMPTLINE_LAST_EXIT_CODE=$status bash ~/.prompt.sh right
#end:q


### Other ###
function fish_greeting
end
