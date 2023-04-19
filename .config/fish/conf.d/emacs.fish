function dired 
  emacsclient -e "(dired \"$PWD\")"
end 

function cde
  emacsclient -e "(return-current-working-directory-to-shell)" | sed 's/^"\(.*\)"$/\1/' | read EMACS_CWD
  echo "chdir to $EMACS_CWD"
  cd "$EMACS_CWD"        
end
