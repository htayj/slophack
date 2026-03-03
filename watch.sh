#!/bin/sh
# Watch .lisp files for changes and signal the running game to reload.
# Requires inotifywait (inotify-tools).
DIR="$(dirname "$(readlink -f "$0")")"
echo "Watching $DIR for .lisp changes..."
inotifywait -m -e close_write --include '\.lisp$' "$DIR" |
while read -r _ _ file; do
    echo "Changed: $file — signaling reload"
    touch "$DIR/.reload"
done
