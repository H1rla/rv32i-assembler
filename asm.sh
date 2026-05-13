#!/bin/bash
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
sbcl --script "$SCRIPT_DIR/code/main.lisp" \
     "$SCRIPT_DIR/input.s" \
     "$SCRIPT_DIR/output.bin"
