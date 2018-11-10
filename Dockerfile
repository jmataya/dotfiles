FROM ubuntu:18.04

ARG user=fox
ARG phpVersion=7.1
ENV TERM xterm-256color

# Get a bunch of basic software installed.
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    emacs \
    git \
    man \
    nodejs \
    software-properties-common \
    sudo \
    tmux \
    vim \
    zsh

# Set the image's timezone
RUN ln -fs /usr/share/zoneinfo/America/New_York /etc/localtime

# Install oh-my-zsh
RUN git clone https://github.com/robbyrussell/oh-my-zsh.git /etc/oh-my-zsh
RUN mkdir -p /etc/skel/.oh-my-zsh/cache

# Create a template for the zsh configuration
RUN echo "export ZSH=/etc/oh-my-zsh" > /etc/skel/.zshrc && \
    echo "export ZSH_CACHE_DIR=~/.oh-my-zsh/cache" >> /etc/skel/.zshrc && \
    echo 'export ZSH_THEME="cdimascio-lambda"' >> /etc/skel/.zshrc && \
    echo 'export plugins=(git)' >> /etc/skel/.zshrc && \
    echo 'source $ZSH/oh-my-zsh.sh' >> /etc/skel/.zshrc

# Setup the command prompt theme
RUN git clone https://github.com/cdimascio/lambda-zsh-theme && \
    cp lambda-zsh-theme/cdimascio-lambda.zsh-theme /etc/oh-my-zsh/themes && \
    rm -rf lambda-zsh-theme  

# Setup the default user
RUN useradd -ms /bin/zsh ${user} && \
    echo "${user} ALL=(root) NOPASSWD:ALL" > /etc/sudoers.d/${user} && \
    chmod 0440 /etc/sudoers.d/${user}

# Emacs configuration
COPY emacs /home/${user}/.emacs.d
RUN chown -R ${user} /home/${user}/.emacs.d
RUN su - ${user} -c "emacs --script ~/.emacs.d/init.el"

# Vim configuration
ADD vim/vimrc /home/${user}/.vimrc
RUN su - ${user} -c \
    "git clone https://github.com/VundleVim/Vundle.vim.git /home/${user}/.vim/bundle/Vundle.vim && \
    vim +PluginInstall +qall"

# Tmux configuration
ADD tmux/tmux.conf /home/${user}/.tmux.conf

# Install the latest stable Node version with nvm
RUN su - ${user} -c \
    "curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.11/install.sh | zsh && \
    source ~/.nvm/nvm.sh && \
    nvm install stable"

# Install PHP 7.1
RUN add-apt-repository -y ppa:ondrej/php
RUN apt-get update && apt-get install -y php${phpVersion}

USER ${user}
WORKDIR /home/${user}
