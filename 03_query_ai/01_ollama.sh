#!/bin/bash

# 01_ollama.sh - Ollama Startup Script
# Serves Ollama on a specific port, pulls a small model, runs it, and provides stop controls
# 🛑🌐🤖📡🚀
# Load your local paths and variables
# .bashrc is in the parent directory (dsai/)
if [ -f "../.bashrc" ]; then
    source ../.bashrc
fi

# Configuration
PORT=11434  # Default Ollama port (change as needed)
# Set environment variable for port
export OLLAMA_HOST="0.0.0.0:$PORT"
MODEL="smollm2:1.7b"  # Small, reputable model (3.3GB)
# Alternative smaller model if smollm2 is not available:
# MODEL="gemma3:latest"  # Smaller alternative model

SERVER_PID=""
MODEL_PID=""

# Set Ollama path - use direct path to ensure it works
OLLAMA_CMD="/c/Users/16015/AppData/Local/Programs/Ollama/ollama.exe"
# Try to use command from PATH if available, otherwise use direct path
if command -v ollama &> /dev/null 2>&1; then
    OLLAMA_CMD="ollama"
elif [ -f "$OLLAMA_CMD" ]; then
    # Path is correct, use it
    :
else
    echo "Error: ollama.exe not found. Please check installation."
    exit 1
fi

# Start server in background, and assign the process ID to the SERVER_PID variable
$OLLAMA_CMD serve > /dev/null 2>&1 & SERVER_PID=$!
# View the process ID of ollama
echo $SERVER_PID

# Pull model of interest
# ollama pull $MODEL

# run model of interest interactively -- usually I don't want this
# ollama run $MODEL & MODEL_PID=$!
# echo $MODEL_PID

# Need to kill the server and model if they are running? These might help.
# kill $SERVER_PID 2>/dev/null
# pkill -f "ollama serve" 2>/dev/null
# pkill -f "ollama run" 2>/dev/null


# Optional: Test query the model (commented out - use 02_ollama.py or 02_ollama.R to test instead)
# echo "Testing the model..."
# test=$(curl -s -X POST http://localhost:$PORT/api/generate \
#     -H "Content-Type: application/json" \
#     -d '{
#         "model": "'$MODEL'",
#         "prompt": "Hello! Please respond with just: Model is working.",
#         "stream": false
#     }' 2>/dev/null)



# Optional: Test code (commented out - use 02_ollama.py or 02_ollama.R to test instead)
# install jq for json parsing
# https://chocolatey.org/install to install chocolatey
# choco install jq

# or for other systems... sudo apt-get install jq

# Set jq path
# JQ_CMD="/c/Users/16015/AppData/Local/Programs/jq/jq.exe"
# if command -v jq &> /dev/null 2>&1; then
#     JQ_CMD="jq"
# elif [ ! -f "$JQ_CMD" ]; then
#     JQ_CMD=""
# fi

# Use jq to extract the response text (if available), otherwise use Python or show raw JSON
# if [ -n "$JQ_CMD" ] && command -v "$JQ_CMD" &> /dev/null 2>&1; then
#     echo "$test" | $JQ_CMD '.'
#     echo ""
#     echo "$test" | $JQ_CMD '.model, .response'
# else
#     echo "Note: jq not found. Showing raw JSON output:"
#     echo "$test"
#     echo ""
#     echo "To install jq on Windows:"
#     echo "  1. Install Chocolatey: https://chocolatey.org/install"
#     echo "  2. Run: choco install jq"
#     echo ""
#     echo "Or use Python to parse JSON (if Python is available):"
#     if command -v python &> /dev/null; then
#         echo "$test" | python -m json.tool 2>/dev/null || echo "$test"
#     fi
# fi
