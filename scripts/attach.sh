#!/bin/bash

RUNNING_TERM_ID=$(docker ps -q --filter name=dev-term)
STOPPED_TERM_ID=$(docker ps -aq --filter name=dev-term)

exec_container () {
    echo "Attaching to $1..."
    docker exec -it $1 /bin/zsh
}

start_container () {
    echo "Starting dev-term..."
    docker run -dt --name dev-term jmataya/dev-term /bin/zsh
}

stop_container () {
    echo "Starting $1..."
    docker start $1
}

if [ -n "$RUNNING_TERM_ID" ]; then
    exec_container $RUNNING_TERM_ID
elif [ -n "$STOPPED_TERM_ID" ]; then
    stop_container $STOPPED_TERM_ID
    exec_container $STOPPED_TERM_ID
else
    start_container
    exec_container dev-term
fi
