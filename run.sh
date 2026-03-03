#!/bin/sh
exec sbcl --eval '(require :asdf)' \
     --eval '(push #p"'$(dirname "$(readlink -f "$0")")'/" asdf:*central-registry*)' \
     --eval '(asdf:load-system :flaghack)' \
     --eval '(flaghack:run)'
