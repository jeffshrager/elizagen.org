#!/bin/bash
set -e  # exit on any error
# echo "=== 1. BOOTSTRAP INITIAL VOCAB ==="
# python3 elizagen_bootstrap.py
# echo "=== 2. ROUND-ROBIN FEATURE EXPANSION ==="
# python3 elizagen_round_robin.py
# echo "=== 3. FINAL CLASSIFICATION ==="
# python3 elizagen_classify.py 
# echo "=== ALL PHASES COMPLETED SUCCESSFULLY ==="
#python3 cvs2nexus.py results/program_features.csv results/elizas.nexus
#cd results
/Users/jeffshrager/Desktop/AIHistory/bin/paup4a168_osx elizas.nexus
