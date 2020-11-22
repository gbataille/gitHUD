[![Build Status](https://travis-ci.org/gbataille/gitHUD.svg?branch=master)](https://travis-ci.org/gbataille/gitHUD)
[![Release](https://img.shields.io/github/release/gbataille/gitHUD.svg)](https://github.com/gbataille/gitHUD/releases)
[![Hackage](https://img.shields.io/hackage/v/githud.svg)](https://hackage.haskell.org/package/githud)
[![Hackage](https://img.shields.io/hackage-deps/v/githud.svg)](https://hackage.haskell.org/package/githud)

As you might have guessed from its name, githud is a heads up display for the
command line that will show git information. The focus is on information and performance.

If you are as crazy as I am about your prompt, you might want to check my somewhat related project
[envstatus](https://github.com/gbataille/envstatus)

![Example]
See [Prompt explained](docs/PROMPT_EXPLAINED.md) for a detailled element by element description of what
you see

_**Note:** this example is taken from the iTerm2 OSX terminal, with custom
colors from the Solarized Dark theme_

* [Why githud?](#why_githud?)
* [Install](#install)
* [Setup](#setup)
* [Configuration](#configuration)
* [Understanding the githud prompt](#understanding-the-githud-prompt)
* [Benefits](#benefits)
* [Benchmarks](#benchmarks)
* [Thanks](#thanks)

Why githud?
-----------

I was really psyched a few months ago (mid-2015) by
[git-radar](https://github.com/michaeldfallen/git-radar). Git-radar does the exact
same thing as githud, but is implemented in shell. While I had a great time
using it for a while, I realized that on my particular setup, git-radar was
introducing a visible delay (>200ms, too long for me) in the displaying of my
prompt.

At that time, I was looking for an exercise to implement in Haskell, so that's
how I created githud

Install
-------

Whichever way you install githud, don't forget to complete the [Setup](#setup)

### Mac OSX with brew

_(Maintained on each release)_

* link my tap

```
brew tap gbataille/homebrew-gba
```

* install githud

```
brew install githud
```

#### Binary packages on linux

_Looking for contributor to provide a recipe (in github actions form?)_

### With cabal and Nix

_(Used in the development process, therefore it is maintained and up-to-date)_

A Nix config is maintained in compatibility with the cabal file. So to be sure to use a compatible
ghc version, and corresponding libraries, just

```sh
nix-shell
cabal v2-install
```

### With Stack

_(Not maintained. Dev happens using Nix + Cabal. Don't hesitate to contribute)_

Stack is a haskell package manager. 1 command install can be found
[here](https://docs.haskellstack.org/en/stable/README/)

githud is available on hackage, but some dependencies have to be explicited.
You need to add the following to the extra-deps in your stack.yml file

``` ./.stack/global-project/stack.yaml
extra-deps:
- daemons-0.3.0
- network-2.8.0.1
```

then you can run

```
stack install githud
```

### With Cabal

_(Not maintained. Dev happens using Nix + Cabal. Don't hesitate to contribute)_

githud is available on hackage. Therefore just get it as usual

```
cabal v2-install exe:githud
```

You can then update your path to include your installation directory (typically `~/.cabal/bin`) or
copy the installed executable to a common location like `/usr/local/bin`

Setup
-----

If you simply call the githud executable, you'll get a short status of your
repository. It's meant to be called each time you display your prompt.
Therefore you want to put it in your PS1 env variable.

Shells have some fancy way of managing prompt when you do things like
autocompletion and the like. For that it needs to know the size of the prompt.
Special characters used to express the color of the prompt need to be
surrounded by special markup for them not to be counted.

GitHUD knows how to handle this. All you have to do is to run the program with
a parameter depending on your shell of choice and those special characters will be used in the
output

#### Bash

```
githud bash
```

For example, in my `.bashrc` file, with the executable at
`/usr/local/bin/githud`, I have a prompt definition that looks like that:

```
export PS1="\[\033[0;37m\][\A]\[\033[0m\] \[\033[0;36m\]\u\[\033[0m\]
\W\[\033[0;32m\]\$(/usr/local/bin/githud bash)\[\033[0m\]\$ "
```

_(it has a lot more things into it, including the current directory, the hour,
and a prompt '$' terminating character)_

#### ZSH

```
githud zsh
```

_**Note**: Those special characters `%{` `%}` are only interpreted and hidden when
zsh renders a prompt. If you simply call githud with this parameter 'zsh' from
the command line, you'll see them in the output!_

Putting it together in my `.zshrc`, I have the following PROMPT variable with
the executable at `/usr/local/bin/githud`


```
setopt PROMPT_SUBST
export PROMPT='%F{white}%T%F{cyan} %n%{$reset_color%} $(/usr/local/bin/githud zsh) $'
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
  echo -n (/usr/local/bin/githud)
  set_color normal
  echo -n "> "
end
```

#### TMUX

Proposed by @Thermatix

```
githud tmux
```

Putting it together in my `.tmux.conf`, I have the following `status-right` variable with
```
set -g status-right '#{pane_current_command} #(~/.zsh/bin/githud_status "#{pane_current_path}")'
```
which necessitates a small script `~/.zsh/bin/githud_status`
```
#!/usr/local/bin/zsh -f
cd $1 && /usr/local/bin/githud zsh
```

and the executable at `/usr/local/bin/githud`

#### NONE

Proposed by @Thermatix

You can get a raw text output (no special formatting) by calling

```
githud none
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

#### The fetcher daemon

`githud` includes a companion daemon called `githudd`. This daemon will start the first time
`githud` is invoked and will run forever.

This daemon will simply execute a `git fetch` periodically in the last git repository in which
`githud` was executed. In the standard installation where you use `githud` in your prompt, this
means that the daemon executes `git fetch` in the last git repository visited.

The `.githudrc` configuration file can contain the following configuration for the daemon (default
values given here)
```ini
# Whether githud will launch the background daemon
run_fetcher_daemon=True
# How long does the daemon sleep between cycles
githudd_sleep_seconds=30
# Path where the githudd pid file will be stored. Needs to exist and be accessible by the current
# user
githudd_pid_file_path=/usr/local/var/run/githudd.pid
# Path where the githudd lock file will be stored. Needs to exist and be accessible by the current
# user
githudd_lock_file_path=/$TMPDIR/githudd.lock
# Path where the githudd socket file will be stored. Needs to exist and be accessible by the current
# user
githudd_socket_file_path=/usr/local/var/run/githudd.socket
# Path where the githudd stdout/stderr capture logfile will be store.
# Githudd logs can be verbose. They are here for debugging only. It is not advised that you
# activate them
# Use the value /dev/null to disable the logs
githudd_log_file_path=/dev/null
```

To stop the daemon, you can simply do
```bash
pkill githudd
```

Note that due to instability, the daemon is currently disabled by default

The health of the `githudd` daemon is indicated by a red hearth (broken when unhealthy) at the start
of the prompt (only when the daemon is activated)


Understanding the githud prompt
-------------------------------

See [Prompt explained](docs/PROMPT_EXPLAINED.md)

Benefits
--------

- githud is fast (on my system, about twice as fast as git-radar, with exec
  times below 100ms)
- githud is easily maintainable through proper test coverage

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
* githud-syncIO - with normal IOs done one at a time
* githud-asyncIO - with IOs programmed asynchronously for better performance.

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
