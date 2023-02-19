#!/bin/bash
dune exec adaptDune -- -i "$1"
python3 python/adaptfun.py -e "$1"