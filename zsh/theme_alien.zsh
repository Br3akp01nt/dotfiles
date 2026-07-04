# Alien theme configuration for non-Warp terminals
export ALIEN_SECTIONS_LEFT=(
  exit
  battery
  user
  path
  vcs_branch:async
  vcs_status:async
  vcs_dirty:async
  newline
  ssh
  venv
  prompt
)

export ALIEN_SECTIONS_RIGHT=(
  time
)

export ALIEN_THEME="red"
ZSH_THEME="alien"
