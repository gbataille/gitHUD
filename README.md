[![Build Status](https://travis-ci.org/gbataille/gitHUD.svg?branch=master)](https://travis-ci.org/gbataille/gitHUD)
[![Release](https://img.shields.io/github/release/gbataille/gitHUD.svg)](https://github.com/gbataille/gitHUD/releases)
[![Hackage](https://img.shields.io/hackage/v/gitHUD.svg)](https://hackage.haskell.org/package/gitHUD)
[![Hackage](https://img.shields.io/hackage-deps/v/gitHUD.svg)](https://hackage.haskell.org/package/gitHUD)

As you might have guessed from its name, gitHUD is a heads up display for the
command line that will show git information

![Example]
See [Prompt explained](docs/PROMPT_EXPLAINED.md) for a detailled element by element description of what
you see

_**Note:** this example is taken from the iTerm2 OSX terminal, with custom
colors from the Solarized Dark theme_

* [Why gitHUD?](#why_githud?)
* [Install](#install)
* [Setup](#setup)
* [ZSH](#zsh)
* [Configuration](#configuration)
* [Understanding the gitHUD prompt](#understanding-the-githud-prompt)
* [Benefits](#benefits)
* [Benchmarks](#benchmarks)
* [Thanks](#thanks)

Why gitHUD?
-----------

I was really psyched a few months ago (mid-2015) by
[git-radar](https://github.com/michaeldfallen/git-radar). Git-radar does the exact
same thing as gitHUD, but is implemented in shell. While I had a great time
using it for a while, I realized that on my particular setup, git-radar was
introducing a visible delay (>200ms, too long for me) in the displaying of my
prompt.

At that time, I was looking for an exercise to implement in Haskell, so that's
how I created gitHUD

Install
-------

Whatever the way you install gitHUD, don't forget to complete the [Setup](#setup)

### Mac OSX with brew

* link my tap

```
brew tap gbataille/homebrew-gba
```

* install githud

```
brew install githud
```

### With Cabal

gitHUD is available on hackage. Therefore just get it as usual

```
cabal install gitHUD
```

### With stack

You can install the `stack` build tool from [haskellstack.com](http://haskellstack.com/) and then install the `gitHUD` executable by running:

```bash
$ stack install --install-ghc gitHUD
```

### From sources

* Get the source (here)
* Compile them (haskell, stack configured). So from the project folder:
```
stack install
```


Setup
-----

If you simply call the githud executable, you'll get a short status of your
repository. It's meant to be called each time you display your prompt.
Therefore you want to put it in your PS1 env variable.

#### Bash

For example, in my `.bashrc` file, with the executable at
`/usr/local/bin/gitHUD`, I have a prompt definition that looks like that:

```
export PS1="\[\033[0;37m\][\A]\[\033[0m\] \[\033[0;36m\]\u\[\033[0m\]
\W\[\033[0;32m\]\$(/usr/local/bin/gitHUD)\[\033[0m\]\$ "
```

_(it has a lot more things into it, including the current directory, the hour,
and a prompt '$' terminating character)_

#### ZSH

ZSH has some fancy way of managing prompt when you do things like
autocompletion and the like. For that it needs to know the size of the prompt.
Special characters used to express the color of the prompt need to be
surrounded by special markup for them not to be counted.

GitHUD knows how to handle this. All you have to do is to run the program with
a "zsh" parameter and those special characters will be used in the output:

```
gitHUD zsh
```

_**Note**: Those special characters %{ %} are only interpreted and hidden when
zsh renders a prompt. If you simply call githud with this parameter 'zsh' from
the command line, you'll see them in the output!_

Putting it together in my `.zshrc`, I have the following PROMPT variable with
the executable at `/usr/local/bin/gitHUD`


```
export PROMPT=%{$fg_bold[white]%}%T%{$reset_color%}%{$fg[cyan]%} %n%{$reset_color%}
%{$fg_bold[green]%}$(shorter_path)%{$reset_color%} $(/usr/local/bin/gitHUD zsh) $ '
```

_(it has a lot more things into it, including the current directory, the
current user, the hour, and a prompt '$' terminating character)_

#### Fish

Add this code to your config.fish file.

```
function fish_prompt
  set_color white
  echo -n [(date "+%H:%M")]
  set_color cyan
  echo -n (whoami):
  set_color yellow
  echo -n (prompt_pwd)
  set_color $fish_color_cwd
  echo -n (/usr/local/bin/gitHUD)
  set_color normal
  echo -n "> "
end
```

Configuration
-------------

The prompt format is nicely configurable. The defaults give you the look and
feel from the screenshot above, with a terminal configured with the Solarized
Dark theme colors.

To change those colors, or the markers used in the prompt:
* Copy the `.githudrc` file from this repository into your home directory.
  Then, from your home directory
```
wget https://raw.githubusercontent.com/gbataille/gitHUD/master/.githudrc
```
* Edit the file by uncommenting some fields and changing their values
  (instructions are enclosed in the file)

You can control which section of the output are shown (if you want to mask
some) with the configuration keys starting with "show\_part\_"

Understanding the gitHUD prompt
-------------------------------

See [Prompt explained](docs/PROMPT_EXPLAINED.md)

Benefits
--------

- gitHUD is fast (on my system, about twice as fast as git-radar, with exec
  times below 100ms)
- gitHUD is easily maintainable through proper test coverage

The only downside compared to git-radar is that you need to compile it on your
platform, as opposed to being just shell.

On Mac, it's now easy since I packaged it as a brew bottle. For Linux, I'm
waiting for contributions to put it in RPM or DEB packages :)

Benchmarks
----------

So of course, I wanted to check that whatever I was doing was useful. So I did
a couple of benchmarks with the Haskell Criterion library. It's based on my
system and does not guarantee any performances but it gives you an idea of the
improvements. Here goes:
* git-radar - full shell implementation
* gitHUD-syncIO - with normal IOs done one at a time
* gitHUD-asyncIO - with IOs programmed asynchronously for better performance.

![Bench]

[Here](./bench/bench.html) you can find the details

For information: I ran that on a Macbook Pro 13", 2014, fully boosted, running
with iTerm 2, tmux, oh-my-zsh, inside a git repo with quite some information
to parse

Thanks
------

Well, my thanks to [git-radar](https://github.com/michaeldfallen/git-radar) for the great idea, and to
[guibou](https://github.com/guibou) for the code
reviews


[Example]: ./images/prompt.png
[Bench]: ./bench/bench.png
