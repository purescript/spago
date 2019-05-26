# Contributing to Spago


## Do I belong here?

Everybody is welcome! People of all experience levels can join and begin contributing and
should feel comfortable and safe making mistakes.

People of all backgrounds belong here as long as they treat others with dignity
and respect and do not harass or belittle others.


## What is the correct way to ask a question?

It's ok to ask questions by [opening an issue][spago-issues]!  
I ([@f-f][f-f]) am also on the [Functional Programming Slack][fp-slack], so you can also ask
questions in the `#purescript` and `#purescript-beginners` channels.


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

If you'd like to develop spago locally, the recommended tool to use is [stack][stack]

To compile the project from source you can do

```bash
$ stack build --fast
```

To install the version you're developing system-wide, do

```bash
$ stack install
```

If you edit any title in the readme, run `doctoc` to update the Table of Contents:

```bash
doctoc --notitle README.md
```

## Running tests

The CI runs the tests on new pull requests, so it's not possible to merge a change without them passing.

So you might want to run them locally. This is a way to do it:

```bash
# Build from source and install system-wide
$ stack install

# This runs the tests which make use of the `spago` executable
$ stack test
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
[fp-slack]: https://fpchat-invite.herokuapp.com/
[spago-issues]: https://github.com/spacchetti/spago/issues
