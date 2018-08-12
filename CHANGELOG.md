From v2.0.2
-----------
* Feat: Add support for TMUX with `githud tmux`. You can now use tmux inside the status bar or the
  pane title of TMUX for example

From v2.0.1
-----------
* Fix: Properly reset "normal" styling after displaying "Bold" characters (#13)

From v1.3.7
-----------
* BREAKING: the executable is renamed from `gitHUD` to `githud`, as suggested in
  [#14](https://github.com/gbataille/gitHUD/issues/14) by [@voidus](https://github.com/voidus)

From v1.3.6
-----------
* When in detached, but on a commit that has a tag, display the tag name rather than the commit SHA

From v1.3.5
-----------
* BASH shell: properly escape invisible control characters so that the prompt length is computed
  properly

From v1.3.4
-----------
* in 1.3.1 I introduced `merge_branch_ignore_branches` to be able to deal with
  "false" branches like the famous gh-pages. It happens that if this branch
was properly created as an orphan branch (`git branch --orphan gh-pages`),
then I can detect that it does not merge back into master and not show this
part of the prompt automatically without people having to manually maintain
exception. Thanks to [Markus](https://github.com/mgee) for pointing that out

From v1.3.3
-----------
* Minor breaking change: Removed the trailing space. If you need it, just put
  it in your prompt definition
* Do not override background color. Just act on foreground text color

From v1.3.2
-----------
* Merge branch count indicator will not include merge-commit anymore. This is
  actually redundant in all case. In fact, in a develop/master typical flow,
when develop merges into master and develop and master are actually equal, if
you count the merge-commit, you'll have the impression that there is a delta
between the 2 branches

From v1.3.1
-----------
* Introduce `merge_branch_ignore_branches` config list that allows to not
  display the merge-branch part of the prompt for certain branch names

From v1.3.0
-----------
* Allow for a special "NoColor" configuration color to reset to the terminal
  foreground color
* NoColor is the default if the config file cannot be read
* Protect the prompt from possible color set before gitHUD is invoked
* Branch name has no color by default
* The default commit pull-push symbol is changed to a (hopefully) clearer '⥯'

From v1.2.0
-----------
* Remove dependency on a patched font in the default config
* Allow for partial prompt through configuration

From v1.1.0
-----------
* Configurable "no remote tracking branch" color
* Fix brew formula in gbataille/homebrew-gba which failed on some configs

From v1.0.0
-----------
* Configurable prompt parts (text and colors)
* Tech: refactoring to a writer and wide test coverage
