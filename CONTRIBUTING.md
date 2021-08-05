# Contributing to Spago


## Do I belong here?

Everybody is welcome! People of all experience levels can join and begin contributing and
should feel comfortable and safe making mistakes.

People of all backgrounds belong here as long as they treat others with dignity
and respect and do not harass or belittle others.


## What is the correct way to ask a question?

It's ok to ask questions by [opening an issue][spago-issues]!
I ([@f-f][f-f]) am also on the [PureScript Discord][discord], so you can also ask
questions in the `#spago` channel.


## I'd like to help, how do I pick something to work on?

Any [open issue][spago-issues] that is not assigned to anyone is good to work on!

If it's your first time contributing then it's probably best to pick the ones marked
with "good first issue".

The easiest way you can help is by contributing documentation (look for issues with
the label "document me").

Do not worry if it's just about copypasting some instructions from an issue to the README,
everything is welcome!

If you wish to contribute documentation, [this is a suggested read](https://www.divio.com/blog/documentation/)

## Developing `spago`

If you'd like to develop spago locally, the recommended tool to use is [stack][stack].

We use `make` to coordinate the build, here's a compilation of useful targets:

```bash
# To compile the project from source:
$ make

# File-watching build:
$ make dev

# Running tests:
$ make test

# Installing system-wide the current build:
$ make install
```

If you edit any title in the readme, run `doctoc` to update the Table of Contents:

```bash
doctoc --notitle README.md
```

If you want to contribute new or edit existing flowcharts, download [yEd](https://www.yworks.com/products/yed), use that program to edit the `.graphml` files, and then export them as an SVG or PNG file via the <kbd>CTRL+E</kbd> shortcut. When a dialog box appears, just press 'ok' and use the default export settings.

The following are recommendations to make using `yEd` easier/faster:
- In the 'Preferences' dialog
    - Under the 'General' tab, change the "Mouse Wheel Behavior" to "[Wheel] Up/Down, [Shift+Wheel] Left/Right, [Ctrl+Wheel] In/Out"
    - Under the 'Editor' tab, check the "Create Node on background click" box
    - Under the 'Editor' tab, check the "Edit Label on Create Node" box
    - Under the 'Editor' tab, check the "Dynamically Adjust Node Size to Label Size" box
- Click on an edge between two nodes...
    - ... and press <kbd>F2</kbd> to add text to that edge
    - ... and press <kbd>F6</kbd> to edit its properties. Under the 'Label' tab, change the "Placement"'s "Model" dropdown to "Free" to get full control over where the edge's text can appear.

## Running tests

The CI runs the tests on new pull requests, so it's not possible to merge a change without them passing.

So you might want to run them locally. This is a way to do it:

```bash
# Build from source and install system-wide
$ stack install

# Install bower since end-to-end tests require it
$ npm install -g bower

# This runs the tests which make use of the `spago` executable
$ stack test

# A single test can be running by providing a pattern to the 'match' flag
$ stack test --test-arguments='--match "/Spago/spago run/Spago should use exec-args"'
```


## Merging changes

All changes must happen through a Pull Request.

Everyone with the "commit bit" can merge changes.


## How do I get the "commit bit"?

Just ask!  I ([@f-f][f-f]) hand out the commit bit freely to anybody who
displays sustained interest in the project, sometimes even if they don't ask.
However, you shouldn't assume that you are undeserving if I haven't given you
the commit bit.  I might just be busy or forgetful, so I appreciate a helpful
reminder.  You can ask directly by e-mail (see my account profile) or by opening
an issue, whichever you prefer.

I hand out the commit bit freely because mistakes are easy to fix or roll back.
Learn by doing and get your hands dirty!


[f-f]: https://github.com/f-f
[stack]: http://haskellstack.org/
[discord]: https://purescript.org/chat
[spago-issues]: https://github.com/purescript/spago/issues
