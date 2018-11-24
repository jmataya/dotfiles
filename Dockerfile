FROM ubuntu:18.04

ARG user=fox
ARG userName=fox
ARG userEmail=me@fox
ARG goVersion=1.11
ARG nodeVersion=11.1
ARG phpVersion=7.1
ENV TERM xterm-256color

# Get a bunch of basic software installed.
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
    build-essential \
    curl \
    emacs \
    git \
    jq \
    leiningen \
    locales \
    man \
    ncurses-term \
    nodejs \
    openjdk-11-jdk \
    software-properties-common \
    sudo \
    tree \
    tmux \
    vim \
    zsh

# Set the locale
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    dpkg-reconfigure --frontend=noninteractive locales && \
    update-locale LANG=en_US.UTF-8

ENV LANG en_US.UTF-8 

# Set the image's timezone
RUN ln -fs /usr/share/zoneinfo/America/New_York /etc/localtime

# Install oh-my-zsh
RUN git clone https://github.com/robbyrussell/oh-my-zsh.git /etc/oh-my-zsh
RUN mkdir -p /etc/skel/.oh-my-zsh/cache

# Create a template for the zsh configuration
RUN echo 'export ZSH=/etc/oh-my-zsh' > /etc/skel/.zshrc && \
    echo 'export ZSH_CACHE_DIR=~/.oh-my-zsh/cache' >> /etc/skel/.zshrc && \
    echo 'export ZSH_THEME="cdimascio-lambda"' >> /etc/skel/.zshrc && \
    echo 'export plugins=(git)' >> /etc/skel/.zshrc && \
    echo 'source $ZSH/oh-my-zsh.sh' >> /etc/skel/.zshrc

# Setup the command prompt theme
RUN git clone https://github.com/cdimascio/lambda-zsh-theme && \
    cp lambda-zsh-theme/cdimascio-lambda.zsh-theme /etc/oh-my-zsh/themes && \
    rm -rf lambda-zsh-theme  

# Install Go 1.11
WORKDIR /tmp

RUN curl https://dl.google.com/go/go${goVersion}.linux-amd64.tar.gz > go.tar.gz && \
    tar -xvf go.tar.gz && \
    mv go /usr/local

RUN mkdir /usr/local/gobin && chmod 777 /usr/local/gobin

RUN echo 'export GOROOT=/usr/local/go' >> /etc/skel/.zshrc && \
    echo 'export GOBIN=/usr/local/gobin' >> /etc/skel/.zshrc && \
    echo 'export PATH=$PATH:$GOROOT/bin:$GOBIN' >> /etc/skel/.zshrc

# Install PHP
RUN add-apt-repository -y ppa:ondrej/php
RUN apt-get update && apt-get install -y php${phpVersion}

# Install NVM
RUN git clone https://github.com/creationix/nvm /etc/nvm && \
    chmod -R 777 /etc/nvm && \
    echo 'export NVM_DIR=/etc/nvm' >> /etc/skel/.zshrc && \
    echo '[ -s "$NVM_DIR/nvm.sh" ] && source $NVM_DIR/nvm.sh' >> /etc/skel/.zshrc

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
ADD vim/setup.sh /home/${user}
RUN su - ${user} -c "/home/${user}/setup.sh /home/${user}/.vimrc"
RUN rm /home/${user}/setup.sh

# Tmux configuration
ADD tmux/tmux.conf /home/${user}/.tmux.conf

# Install the latest stable Node version with nvm
RUN su - ${user} -c "source /etc/nvm/nvm.sh && nvm install ${nodeVersion}"

# Setup the GOPATH
RUN echo 'export GOPATH=$HOME/code/go' >> /home/${user}/.zshrc && \
    echo 'export PATH=$PATH:$GOPATH/bin' >> /home/${user}/.zshrc

# Install goimports
RUN su - ${user} -c "source ~/.zshrc && \
    go get golang.org/x/tools/cmd/goimports && \
    go install golang.org/x/tools/cmd/goimports"

# Configure git
RUN su - ${user} -c "git config --global user.name \"${userName}\""
RUN su - ${user} -c "git config --global user.email \"${userEmail}\""

USER ${user}
WORKDIR /home/${user}
