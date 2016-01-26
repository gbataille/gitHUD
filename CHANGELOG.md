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
