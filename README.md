# Dot Files

I do almost all of my work in vim, tmux, and zsh. This is my configuration for those three basic tools.

## Setup of vim and zsh

_Note: these instructions will only work for Mac OSX._

Since I spend so much time in vim and zsh, and I pathologically care too much about unimportant details like color scheme, I ride on the bleeding edge of iTerm, vim, and tmux to get proper 24-bit color. Here's what I do:

**Get the nightly iTerm2**

You can find that [here](http://iterm2.com/downloads.html). While it is bleeding edge, I've found very few bugs with it. But, be warned: breakages are possible!

**Use ZyX's vim fork**

```bash
$ hg clone https://bitbucket.org/ZyX_I/vim
$ cd vim
$ hg bookmark 24-bit-xterm
$ ./configure --with-features=huge --enable-rubyinterp --enable-termtruecolor
$ make && sudo make install
```

**Use a version of tmux that supports 24-bit color**

If you already have tmux installed through homebrew:

```bash
$ brew uninstall tmux
```

Now, install the new tmux:

```bash
brew tap choppsv1/term24
brew install choppsv1/term24/tmux
```

**Get an awesome color scheme.**

My current favorite it Dracula. Get it [here](https://github.com/zenorocha/dracula-theme).
