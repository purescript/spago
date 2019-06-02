#!/usr/bin/env nix-shell
#!nix-shell -i bash -p nix stack2nix

# prelude
script_dir=$(dirname "$(readlink -f "$BASH_SOURCE")")

# script
stack2nix $script_dir --verbose > $(basename $script_dir).nix
