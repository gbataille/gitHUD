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

# Thanks

Well, my thanks to [git-radar](https://github.com/michaeldfallen/git-radar) for the great idea, and to
[guibou](https://github.com/guibou) for the code
reviews


[Example]: https://raw.githubusercontent.com/gbataille/gitHUD/master/images/prompt.png
