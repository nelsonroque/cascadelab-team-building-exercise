#!/bin/bash

# Prompt for the project name
read -p "Enter the project name: " PROJECT_NAME

# Create a new directory for the project
mkdir "$PROJECT_NAME" || { echo "Failed to create directory"; exit 1; }
cd "$PROJECT_NAME" || { echo "Failed to enter directory"; exit 1; }

# Create a virtual environment
python3 -m venv venv || { echo "Failed to create virtual environment"; exit 1; }

# Activate the virtual environment
source venv/bin/activate || { echo "Failed to activate virtual environment"; exit 1; }

# Upgrade pip and install Typer
pip install --upgrade pip || { echo "Failed to upgrade pip"; exit 1; }
pip install -r requirements.txt || { echo "Failed to install Typer"; exit 1; }