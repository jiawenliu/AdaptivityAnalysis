#!/bin/bash
dune exec costAnalysisDune -- -i "$1"
python3 python/runner.py -e "$1"