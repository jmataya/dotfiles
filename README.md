# Dotfiles

While it feels sacrilegious after so many years in Vim and tmux, my primary
development workflow now goes through Emacs. There are two incarnations of this
setup:

1. Coding locally in MacOS on while at work;
2. A remote Ubuntu-based Docker environment that I connect with via SSH and Mosh
   for all of my personal development.

This configuration is entirely home grown, with inspiration pulled from dotfiles
across the Internet and from my co-workers.

## What's Included?

A standard install of this repo will setup the following systems:

* **Emacs, Vim, tmux, and zsh**: while Emacs and zsh are the stars of the show,
  my legacy configurations for Vim and zsh are still here.
* Support for the base languages that I _primarily_ use:
  * GoLang;
  * HTML/JS/CSS;
  * Node.js;
  * PHP;
  * Ruby;
  * Scala.
* Utilities such a Docker and optional support for relational databases like
  PostgreSQL, MySQL, and Redis.
  
Obviously, all of these can be added on!

## More Documentation

I'm a big fan of
[literate programming](https://en.wikipedia.org/wiki/Literate_programming) for
things like documentation and have chosen to do that with the Emacs
configuration. Check that out [here](/emacs/configuration-v2.org).

## Getting Started

### The Easy Way (Docker)

The easiest way to get started is to use prebuilt Docker container that is
intended for Linux use. It will set up the environment as an exact replica of
my personal setup.

One note: Docker for Mac (and Windows, I presume) has some real performance
bottlenecks when binding to the local filesystem. Therefore, I recommend only
using this Docker image to get a feel for the installation if you're on MacOS or
Windows.

#### Steps

1. Install [Docker](https://docker.com) for your platform.
1. Install git.
1. Clone this repository:

    $ git clone https://github.com/jmataya/dotfiles

1. Navigate to the `docker/` folder and run Docker Compose:

    $ cd dotfiles/docker
    $ docker compose up --build -d
    
1. Connect to the main image:

    $ docker exec -it jmataya_dev /bin/bash
    
You're done! Enjoy your new environment.

### Getting Started (MacOS)

_Coming soon..._

### Getting Started (Remote Linux)

_Coming soon..._

# Dot Files

I do almost all of my work in vim, tmux, and zsh. This is my configuration for those three basic tools.

# License

MIT
