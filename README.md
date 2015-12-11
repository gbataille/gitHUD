As you might have guessed from its name, gitHUD is a heads up display for the
command line that will show git information

![Example]

# Why gitHUD?

I was really psyched a few months ago by
[git-radar](https://github.com/michaeldfallen/git-radar). Git-radar does the exact
same thing as gitHUD, but is implemented in shell. While I had a great time
using it for a while, I realized that on my particular setup, git-radar was
introducing a visible delay (200ms, too long for me) in the displaying of my
prompt.

At that time, I was looking for an exercise to implement in Haskell, so that's
how I created gitHUD

# Install

* Get the source
* Compile them with stack (haskell)
* Call the gitHUD executable in your prompt definition ($PS1 ou $PROMPT)

My PROMPT variable

```
%{$fg_bold[white]%}%T%{$reset_color%}%{$fg[cyan]%} %n%{$reset_color%} %{$fg_bold[green]%}$(shorter_path)%{$reset_color%} $(~/.local/bin/gitHUD zsh)%{$(virtualenv_info)%}%(?,,%{${fg_bold[blue]}%}[%?]%{$reset_color%} )$ '
```

# ZSH

ZSH has some fancy way of managing prompt when you do things like
autocompletion and the like. For that it needs to know the size of the prompt.
Special characters used to express the color of the prompt need to be
surrounded by special markup for them not to be counted.

To make it work with ZSH, add a "zsh" parameter:

```
gitHUD zsh
```

# Benefits

- gitHUD is fast (on my system, about twice as fast as git-radar, with exec
  times below 100ms). And I have a couple more ideas to make it better
- gitHUD is easily maintainable through proper test coverage

The only downside compared to git-radar is that you need to compile it on your
platform, or I need to provide a packaged version (that's in my todo list)

# Dependencies

Until I make it configurable, I have hardcoded a "repository" character at the
start of the output that can be found in the [patched
fonts](https://github.com/powerline/fonts) for
[Powerline](https://github.com/powerline/powerline)

# Benchmarks

So of course, I wanted to check that whatever I was doing was useful. So I did
a couple of benchmark with the Haskell criterion library. It's based on my
system and does not guarantee any performances but it gives you an idea of the
improvements. It goes:
* git-radar - full shell implementation
* gitHUD-syncIO - with normal IOs done one at a time
* gitHUD-asyncIO - with IOs programmed asynchronously for better performance.

![Bench]

[Here](./bench/bench.html) you can find the details

For information: I ran that on a Macbook Pro 13", 2014, fully boosted, running
with iTerm 2, tmux, oh-my-zsh, inside a git repo with quite some information
to parse

# Thanks

Well, my thanks to [git-radar](https://github.com/michaeldfallen/git-radar) for the great idea, and to
[guibou](https://github.com/guibou) for the code
reviews


[Example]: ./images/prompt.png
[Bench]: ./bench/bench.png
