function dired 
  emacsclient -e "(dired \"$PWD\")"
end 

function cde
  emacsclient -e "(my:return-current-working-directory-to-shell)" | sed 's/^"\(.*\)"$/\1/' | read EMACS_CWD
  echo "chdir to $EMACS_CWD"
  cd "$EMACS_CWD"        
end

function fzf_z
  set -l query (commandline)

  if test -n $query
    set fzf_flags --query "$query"
  end

  z -l | awk '{ print $2 }' | fzf $fzf_flags | read recent
  if [ $recent ]
      cd $recent
      commandline -r ''
      commandline -f repaint
  end
end
