#!/bin/bash

_fb()
{
  local curw
  curw=${COMP_WORDS[COMP_CWORD]}
  COMPREPLY=($(compgen -W "$(fb commands)" -- $curw))
  return 0
}

# the dirnames backup is just to help with debugging for now
# there is probably something better to do (somehow error nicely),
# but this works for me for now
complete -F _fb -o dirnames fb
