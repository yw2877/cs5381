#!/bin/bash

# setup_env.sh - Quick environment setup script
# This script loads the .bashrc configuration

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Load .bashrc from dsai directory
if [ -f "$SCRIPT_DIR/../.bashrc" ]; then
    source "$SCRIPT_DIR/../.bashrc"
    echo "✅ Environment loaded successfully!"
    echo ""
    echo "Available commands:"
    echo "  - ollama: $(command -v ollama || echo '/c/Users/16015/AppData/Local/Programs/Ollama/ollama.exe')"
    echo "  - jq: $(command -v jq || echo '/c/Users/16015/AppData/Local/Programs/jq/jq.exe')"
    echo "  - python: $(command -v python || echo 'not found')"
    echo ""
else
    echo "❌ Error: .bashrc not found at $SCRIPT_DIR/../.bashrc"
fi




