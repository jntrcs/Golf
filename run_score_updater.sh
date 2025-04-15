#!/bin/bash

# Define the directory where logs should be stored
LOG_DIR="/Users/jacksoncurtis/Documents/Masters/score_updater_logs"
mkdir -p "$LOG_DIR" # Create the log directory if it doesn't exist

# Define log files
LOG_FILE="$LOG_DIR/job_output.log"
ERROR_FILE="$LOG_DIR/job_error.log"

echo "----------------------------------------" >> "$LOG_FILE"
echo "Job started at: $(date)" >> "$LOG_FILE"
echo "----------------------------------------" >> "$LOG_FILE"

# --- Execute Python Script ---
echo "Running Python script..." >> "$LOG_FILE"
# IMPORTANT: Replace with the ACTUAL full path to your python executable and script
/Users/jacksoncurtis/Documents/Masters/.venv/bin/python /Users/jacksoncurtis/Documents/Masters/scraper.py >> "$LOG_FILE" 2>> "$ERROR_FILE"

# Check if Python script succeeded (exit code 0 means success)
if [ $? -eq 0 ]; then
  echo "Python script completed successfully." >> "$LOG_FILE"

  # --- Execute R Script ---
  echo "Running R script..." >> "$LOG_FILE"
  # IMPORTANT: Replace with the ACTUAL full path to Rscript and your R script
  /usr/local/bin/Rscript /Users/jacksoncurtis/Documents/Masters/update_live_googlesheet.R >> "$LOG_FILE" 2>> "$ERROR_FILE"

  if [ $? -eq 0 ]; then
    echo "R script completed successfully." >> "$LOG_FILE"
  else
    echo "R script failed. Check $ERROR_FILE for details." >> "$LOG_FILE"
  fi

else
  echo "Python script failed. Check $ERROR_FILE for details. Skipping R script." >> "$LOG_FILE"
fi

echo "Job finished at: $(date)" >> "$LOG_FILE"
echo "" >> "$LOG_FILE"

exit 0
