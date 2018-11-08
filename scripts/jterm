#!/bin/bash

FORCE_FLAG=false

exec_container () {
    echo "Attaching to $1..."
    docker exec -it $1 /bin/zsh
}

start_container () {
    echo "Starting dev-term..."
    docker run -dt --name dev-term jmataya/dev-term /bin/zsh
}

restart_container () {
    echo "Restarting $1..."
    docker start $1
}

stop_container () {
    echo "Stopping $1..."
    docker stop $1
}

kill_container () {
    echo "Killing $1..."
    docker kill $1
}

remove_container () {
    echo "Removing $1..."
    docker rm $1
}

run_up () {
    FLAG_HELP=false

    for a in ${BASH_ARGV[*]}; do
        if [ "$a" = "--help" ]; then
            FLAG_HELP=true
        fi
    done

    if [ "$FLAG_HELP" = true ]; then
        run_up_help
    else
        RUNNING_TERM_ID=$(docker ps -q --filter name=dev-term)
        STOPPED_TERM_ID=$(docker ps -aq --filter name=dev-term)

        if [ -n "$RUNNING_TERM_ID" ]; then
            exec_container $RUNNING_TERM_ID
        elif [ -n "$STOPPED_TERM_ID" ]; then
            restart_container $STOPPED_TERM_ID
            exec_container $STOPPED_TERM_ID
        else
            start_container
            exec_container dev-term
        fi
    fi
}

run_up_help () {
    echo ""
    echo "Usage:  jterm up"
    echo ""
    echo "Attaches to a terminal image - will start the image, if necessary."
}

run_down () {
    FLAG_FORCE=false
    FLAG_HELP=false

    for a in ${BASH_ARGV[*]}; do
        if [ "$a" = "--force" ]; then
            FLAG_FORCE=true
        elif [ "$a" = "-f" ]; then
            FLAG_FORCE=true
        elif [ "$a" = "--help" ]; then
            FLAG_HELP=true
        fi
    done
    
    if [ "$FLAG_HELP" = true ]; then
        run_down_help
    else
        RUNNING_TERM_ID=$(docker ps -q --filter name=dev-term)

        if [ -n "$RUNNING_TERM_ID" ]; then
            if [ "$FLAG_FORCE" = true ]; then
                kill_container $RUNNING_TERM_ID
            else
                stop_container $RUNNING_TERM_ID
            fi
        fi

        STOPPED_TERM_ID=$(docker ps -aq --filter name=dev-term)
        if [ -n "$STOPPED_TERM_ID" ]; then
            remove_container $STOPPED_TERM_ID
        else
            echo "Container dev-term not found..."
        fi
    fi
}

run_down_help () {
    echo ""
    echo "Usage:  jterm down [OPTIONS]"
    echo ""
    echo "Stops and discards any existing images."
    echo ""
    echo "Options:"
    echo "  -f, --force     Kills, rather than gracefully stops, the existing container(s)."
}

run_help() {
    echo ""
    echo "Usage:  jterm COMMAND [OPTIONS]"
    echo ""
    echo "A command-line utility to help running the Docker-based terminal and utils."
    echo ""
    echo "Commands:"
    echo "  up         Starts and connects to the Docker-base terminal and any companion utilities."
    echo "  down       Shuts down and removes containerize applications."
    echo "  help       Prints usage information."
    echo ""
    echo "Run 'jterm COMMAND --help' for more information on a command."
}

if [ "$1" = "up" ]; then
    run_up
elif [ "$1" = "down" ]; then
    run_down
elif [ "$1" = "help" ]; then
    run_help
else
    run_help
fi
