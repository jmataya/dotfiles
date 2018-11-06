FROM ubuntu:18.04

ARG user=fox
ENV TERM xterm-256color

# Get a bunch of basic software installed.
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    git \
    emacs \
    vim \
    tmux \
    zsh \
    sudo

# Setup the default user
RUN useradd -ms /bin/zsh ${user} && \
    echo "${user} ALL=(root) NOPASSWD:ALL" > /etc/sudoers.d/${user} && \
    chmod 0440 /etc/sudoers.d/${user}

USER ${user}
WORKDIR /home/${user}

# Configure zsh
RUN curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh | zsh || true
