#!/bin/bash

# Local .bashrc for this repository
# This file contains project-specific bash configurations

# Add LM Studio to PATH for this project (here's mine)
export PATH="$PATH:/c/Users/tmf77/.lmstudio/bin"
alias lms='/c/Users/tmf77/.lmstudio/bin/lms.exe'

# Add Ollama to PATH for this project
# Update this path to match your Ollama installation location
export PATH="$PATH:/c/Users/16015/AppData/Local/Programs/Ollama"
alias ollama='/c/Users/16015/AppData/Local/Programs/Ollama/ollama.exe'

# Add R to your Path for this project (here's mine)
export PATH="$PATH:/c/Program Files/R/R-4.4.1/bin"
alias Rscript='/c/Program Files/R/R-4.4.1/bin/Rscript.exe'
# Add R libraries to your path for this project (here's mine)
export R_LIBS_USER="/c/Users/tmf77/AppData/Local/R/win-library/4.2"

# Add Python to your Path for this project (here's mine)
# Update this path to match your Python installation location
export PATH="$PATH:/c/Users/16015/AppData/Local/Programs/Python/Python311"
export PATH="$PATH:/c/Users/16015/AppData/Local/Programs/Python/Python311/Scripts"
alias python='/c/Users/16015/AppData/Local/Programs/Python/Python311/python.exe'

# Add uvicorn to your Path for this project - if using Python for APIs (here's mine)
# Update this path to match your Python Scripts location
export PATH="$PATH:/c/Users/16015/AppData/Roaming/Python/Python311/Scripts"

# Add jq to PATH for this project
export PATH="$PATH:/c/Users/16015/AppData/Local/Programs/jq"
alias jq='/c/Users/16015/AppData/Local/Programs/jq/jq.exe'

echo "✅ Local .bashrc loaded."