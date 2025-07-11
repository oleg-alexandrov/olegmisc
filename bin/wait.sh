#!/bin/bash

# This script waits for a specific job (identified by its name) to disappear
# from the user's qstat queue, and then executes a given command.

# Check if the correct number of arguments is provided
if [ "$#" -lt 2 ]; then
    echo "Usage: $0 <job_name> <command_to_run>"
    exit 1
fi

JOB_NAME="$1"
COMMAND_TO_RUN="$2"
USERNAME=$(whoami)
SLEEP_INTERVAL=60 # seconds

echo "Monitoring qstat for job: '$JOB_NAME' submitted by user: '$USERNAME'"
echo "Will execute command: '$COMMAND_TO_RUN' once the job is no longer found."
echo "Checking every ${SLEEP_INTERVAL} seconds."

# Loop until the job is no longer found in the qstat output
while true; do
    qstat -u "${USERNAME}" | grep -q "${JOB_NAME}"

    # Check the exit status of the grep command
    # 0 means grep found a match (job is still running)
    # 1 means grep found no match (job is no longer running)
    if [ $? -eq 0 ]; then
        echo "$(date): Job '$JOB_NAME' is still running or in queue. Waiting."
        sleep "${SLEEP_INTERVAL}"
    else
        echo "$(date): Job '$JOB_NAME' no longer found in queue. Proceeding with command execution."
        break # Exit the loop
    fi
done

# Execute the command
echo "Executing: $COMMAND_TO_RUN"
eval "${COMMAND_TO_RUN}" # Using eval to correctly handle commands with spaces and arguments

echo "Script finished."
