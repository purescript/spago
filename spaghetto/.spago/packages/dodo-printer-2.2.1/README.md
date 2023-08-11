# purescript-dodo-printer

An adequate printer.

This library implements the equivalent functions of many other pretty-printers in
the Wadler/Leijen style but with all the names gratuitously changed.

It also provides nice ANSI integration.

## Getting Started

The core type of `Dodo` is `Doc`, which represents a document. It has one
type parameter which represents an annotation. Annotations let you mark parts
of your document tree with things such as styles and colors (e.g. `Dodo.Ansi`
for ANSI graphics). `Doc`s which don't use annotations can use a type
variable or `Void`. The most primitive ways to construct and manipulate
`Doc`s are via `text` and `Monoid`.

```purescript
import Prelude
import Dodo (Doc, text)

hello :: forall a. Doc a
hello = text "Hello, " <> text "World!"
```

Using `(<>)` (or `append`, or `fold`, or any other `Monoid` function) will put
text next to each other on a line. In order to render this, we need to choose
a particular printer interface and provide some print options.

The most basic printer is `plainText`, which only renders plain text,
ignoring all annotations. Print options let you configure things like
indentation and page width. There are several presets for print options, so
we will just pick one for now (`twoSpaces`).

```purescript
import Prelude
import Effect (Effect)
import Effect.Console as Console
import Dodo (Doc, text, plainText, twoSpaces, print)

hello :: forall a. Doc a
hello = text "Hello, " <> text "World!"

main :: Effect Unit
main = Console.log $ print plainText twoSpaces hello
```
```
Hello, World!
```

### Indentation

Dodo treats indentation as a separate configurable thing. Indentation is only
ever a line prefix, and printed on demand after lines breaks and before text.
There are two primitive functions for manipulating indentation:

* `indent` which increases the indentation level by one indent unit
  (configurable with print options).
* `align` which increases the indentation level by a certain number of spaces
  (not configurable).

Let's try indenting the second word in our example:

```purescript
hello :: forall a. Doc a
hello = text "Hello, " <> indent (text "World!")
```

Printing this, however, will yield an identical result. That's because we
already printed text, and indentation only affects the line prefix. We can
insert a `break`:

```purescript
hello :: forall a. Doc a
hello = text "Hello, " <> indent (break <> text "World!")
```
```
Hello,
  World!
```

We can also align to a certain number of spaces:

```purescript
hello :: forall a. Doc a
hello = text "Hello, " <> align 10 (break <> text "World!")
```
```
Hello,
          World!
```

Or even align to the current column:

```purescript
hello :: forall a. Doc a
hello = text "Hello, " <> alignCurrentColumn (break <> text "World!")
```
```
Hello,
       World!
```

### Flexible Layouts

We've learned how to append text, insert line breaks, and indent, but often
times we want our layout to be a little more flexible. If we have plenty of
space left on the line, we may want to print our document on a single line,
and only insert breaks as needed. There are two primitives for building
flexible layouts:

* `flexGroup` which specifies a flexible region of the document and may
  need to respond to page width constraints.
* `flexAlt` which specifies an alternative document to try and render while
  in a flex group, falling back to a default if it doesn't fit.

When Dodo enters a flex group, it will use the first argument to any flex
alternatives, rendering until content either extends past the configured
page width or until there is a line break. If it encounters either of these
conditions, it will back up and use the second arguments to any flex
alternatives.

One of the most basic flexible documents is `spaceBreak`, which will either
insert a space or a line break. `spaceBreak` is defined as:

```purescript
spaceBreak :: forall a. Doc a
spaceBreak = flexAlt (text " ") break
```

If we substitute `break` for `spaceBreak` in our example:

```purescript
hello :: forall a. Doc a
hello = text "Hello," <> indent (spaceBreak <> text "World!")
```

Nothing will change! Without a surrounding flex group, flex alternatives
always render the second argument.

```purescript
hello :: forall a. Doc a
hello = flexGroup (text "Hello," <> indent (spaceBreak <> text "World!"))
```
```
Hello, World!
```

Since there was available width on the line, it rendered with a space. If we
configure our page width to something really small though:

```purescript
main :: Effect Unit
main = Console.log $ print plainText (twoSpaces { pageWidth = 10 }) hello
```
```
Hello,
  World!
```

Dodo inserts the break and indent. "Hello, World!" on a single line would
exceed the maximum page width, so it uses the flex default (`break`) instead.

### Two-Dimensional Layouts

Dodo also supports two-dimensional layouts through the `Dodo.Box` interface.
Boxes can be joined and aligned both vertically and horizontally to create
complex layouts such as tables or grids.

## Examples

* Colorful, flexible JSON printer ([code](test/snapshots/DodoExampleJson.purs), [output](test/snapshots/DodoExampleJson.output))
* Text paragraphs ([code](test/snapshots/DodoTextParagraph.purs), [output](test/snapshots/DodoTextParagraph.output))
* 2D layout ([code](test/snapshots/DodoBox.purs), [output](test/snapshots/DodoBox.output))
