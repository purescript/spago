Simple bidirectional parser/printers for your routing data types.

# Why?

Strongly-typed languages let you define your routes as a data type, ensuring invalid routes fail to compile. But the browser represents locations as strings, so you have to write functions to decode strings into your routing data type and functions to write a route to a string.

Unfortunately, writing separate functions to parse and print your routing data type is error-prone and boilerplate-heavy. It’s easy to update a parser and forget to update the accompanying printer, even though almost all routing definitions should round-trip (parsing then printing returns the original string value).

`routing-duplex` takes an approach that solves both problems. This library lets you define a codec, or a means to both decode and encode a particular data type, for your routes. Write this codec once and it will handle parsing and printing the same representation for you.

## A Brief Example

Let’s build a codec for a simple app with two routes: the homepage and user profiles (identified by usernames).

1. Write a data type to represent our two routes, deriving `Generic`.
2. Build a codec using generics and combinators from `Routing.Duplex`

```purescript
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', path, root, segment, string)
import Routing.Duplex.Generic as G

data Route = Home | Profile String

derive instance genericRoute :: Generic Route _

route :: RouteDuplex' Route
route = root $ G.sum
  { "Home": G.noArgs
  , "Profile": path "profile" (string segment)
  }
```

With our codec in place we can parse browser locations (strings) into our routing data type:

```shell
parse route "/"
> Right Home

parse route "/profile/jake-delhomme"
> Right (Profile "jake-delhomme")
```

And we can also serialize our routing data type into browser locations:

```shell
print route $ Profile "jake-delhomme"
> "/profile/jake-delhomme"
```

# How to use this library

`routing-duplex` works by letting you define a codec which represents how to encode and decode your routing data type. You can define your routing data however you see fit, and then provide it to the library’s codec type, `RouteDuplex`:

```purescript
data RouteDuplex i o = RouteDuplex (i -> RoutePrinter) (RouteParser o)
```

Intuitively you can think of this as a data type that describes how you can parse a browser location into a route or print a route as a browser location. This type is used for low-level combinators and records and allows you to print / parse a different input and output. However, a proper codec for a routing data type would be bidirectional: it should parse and print the _same_ type, your `Route`. For that reason, you will generally use the `RouteDuplex’` type instead:

```purescript
type RouteDuplex' a = RouteDuplex a a
```

This library exports a number of helper functions and combinators for constructing this codec with minimal boilerplate, mostly concentrated in two modules:

- `Routing.Duplex` exports the `RouteDuplex` type, the `print` and `parse` functions, and combinators that represent constants (`“/post“`), segments (`/:id`), parameters (`?foo=`), prefixes and suffixes, optional values, and more.
- `Routing.Duplex.Generic` exports helpers for deriving code via your data type’s `Generic` instance, most notably the `sum` function for describing a route as a sum type and the `product` and `noArgs` functions for working with product types.
- `Routing.Duplex.Generic.Syntax` exports some symbols that can be used to write terse codecs similar to those found in string-based routers.

# Examples

We’ll explore several practical examples of this library in practice while building a real-world routing data type and codec.

## Example: Writing a codec for a sum type

Let’s begin developing a more complex set of routes. We’ll design routes for a small blogging site made up of users and their posts, as well as feed showing new posts from across the site. We can represent these routes in a small data type:

```purescript
data Route
  = Root
  | Profile String
  | Post String Int
  | Feed

derive instance genericRoute :: Generic Route _
```

Next, we can define our codec. First, we’ll use the `root` combinator to match on an opening slash:

```purescript
-- Will match '/', and then anything further specified by the codec
-- it is passed as an argument.
root :: forall a b. RouteDuplex a b -> RouteDuplex a b
```

Next, we need to represent the ability to parse static and dynamic segments from a path. Paths are always separated by slashes (like `user/5/favorites`), where each string between slashes is considered a segment (like `”user"`, `”5”`, and `”favorites"`. Sometimes we need to match a segment exactly, like matching the segment `”user"`, but at other times we want to capture the value of a segment that could have many different values. Here, we have a dynamic segment representing a user ID that we’d like to capture.

We can use the `path`, `segment`, and `param` helper functions to capture these static and dynamic segments.

```purescript
-- Allows you to match a static segment. For example, to match the
-- /feed path, use `path "feed" ...` where `...` represents further
-- segments.
path :: forall a b. String -> RouteDuplex a b -> RouteDuplex a b

-- Allows you to capture a variable segment. For example, the path
-- /user/5 has one static segment and one variable segment. This always
-- reads and writes a string, but when used with other combinators, it
-- can transform that string into a type of your choice. For example:
-- `path "user" (int segment)` parses to `User Int`.
segment :: RouteDuplex' String

-- Allows you to capture a query parameter. For example, the path
-- /user?foo=bar has one query parameter and could be represented
-- with `param "foo"`.
param :: String -> RouteDuplex' String
```

While the path you work with is always a `String`, you can transform that input into your routing data type by using parser combinators from the library. We’ve just seen an example of one of them, `int`, but there are several more, including:

```purescript
string :: RouteDuplex' String -> RouteDuplex' String
boolean :: RouteDuplex' String -> RouteDuplex' Boolean
int :: RouteDuplex' String -> RouteDuplex' Int
optional :: forall a b. RouteDuplex a b -> RouteDuplex (Maybe a) (Maybe b)
```

You can easily implement your own combinators using `as`, the function used to construct each of the built-in combinators. We’ll see an example of that later on!

At this point, we have the tools we need to:

- Handle static segments of a path, like `"/user"`
- Handle variable segments of a path, like `/:username` or `/:postid`
- Handle query parameters, like `?foo=bar`
- Transform string segments into other types, like using the `int` combinator to turn a post ID into a String

However, we still need two more tools to construct our codec. First, we need to be able to specify codecs for every case in our routing sum type: this can be done with the `sum` function from the `Routing.Duplex.Generic` module. Second, we need to be able to specify that a type should match zero or more of these dynamic segments. For routes that have no arguments, we’ll use `noArgs`; for routes with one argument, we’ll just provide a codec; and for routes with multiple arguments, we’ll combine codecs with `product`.

Let’s see all of this in action!

First, we’ll use the `root` combinator to match a leading slash. Then, we’ll use the `sum` function to specify that we’re matching a sum type (our `Route` data type):

```purescript
route = root $ sum
  { ... }
```

`sum` exposes a nice record syntax so that we can specify codecs for each constructor of the type; if you forget to handle a constructor, you’ll get a compiler error. We need to write a few codecs:

- The `Root` constructor takes no arguments and should match when the path is empty. We can represent that with a simple `noArgs`.
- The `Profile` constructor takes one argument, a string `Username`, and should match a path that begins with `”user“`. We can represent that using the `path` and `segment` functions, along with the `string` combinator.
- The `Post` constructor takes two arguments: a string `Username` and an integer `PostId`. We’ll need to use the `product` function to put two dynamic segments together.
- The `Feed` constructor should only match a string constant in the path, `”feed”`. We can represent that with the `path` function and `noArgs`.

```purescript
route = root $ sum
  { "Root": noArgs
  , "Profile": path "user" (string segment)
  , "Post":
      product
        (path "user" (string segment))
        (path "post" (int segment))
  , "Feed": path "feed" noArgs
  }
```

It can be a little awkward using `product` for complex routes, so there’s also an operator version, `(/)`, which is more convenient to use infix. We can also omit the `string` combinator because all segments are strings by default. With this in mind, let’s revise our `”Post”` case:

```purescript
  {
  , "Post": path "user" segment / path "post" (int segment)
  }
```

In fact, when we’re matching string constants, we can omit the call to `path` and just provide the string directly:

```purescript
  {
  , "Post": "user" / segment / "post" / int segment
  }
```

## Example: Working with optional or required query params

Users need to be able to search their feeds. This information will come via query parameters, which will be optional. We haven’t dealt with optional segments or query params so far, but they’re easy to add.

First, let’s adjust our route type so that it can accommodate query parameters. Query parameters have a key:value pairing, so it’s typical to represent them with a record type.

```purescript
data Route
  = Root
  | Profile String
  | Post String Int
  | Feed { search :: Maybe String }
```

We’ll have to update our codec so that `Feed` takes a record as an argument. We can do this manually with the `record` function and its `:=` operator, which lets you assign a key in the record to a particular codec. Record keys are type-level strings, so we’ll need to use `Proxy` to create them.

Intuitively, we can read the below codec as “Match `”feed”` and then, if it exists, a query parameter with the key “search”, storing its value at the key “search” in the output record.” This time, we'll use the `optional` combinator to represent an optional value:

```purescript
route = root $ sum
  { ...
  , "Feed": path "feed" (record # _search := optional (param "search"))
  }
  where
    _search = Proxy :: Proxy "search"
```

This explicit record creation can be done any time you have a record in your route type. However, using a record for query parameters is common enough that this library exports a helper function, `params`, which lets you just provide a record of codecs where the record keys are treated as the query param keys, too. There's also an operator version of `params`, `(?)`. We could rewrite our above codec using this helper function:

```purescript
  { ...
  , "Feed": path "feed" $ params { search: optional <<< string }
  -- alternately
  , "Feed": "feed" / params { search: optional <<< string }
  -- alternately
  , "Feed": "feed" ? { search: optional <<< string }
  }
```

At this point, our codec is looking much cleaner:

```purescript
route = root $ sum
  { "Root": noArgs
  , "Profile": "user" / segment
  , "Post": "user" / segment / "post" / int segment
  , "Feed": "feed" ? { search: optional <<< string }
  }
```

## Example: Defining a new codec for custom data

Unfortunately, our route data type is not as type-safe as we’d like it to be. We aren’t really parsing just string and ints — we’re dealing with `Username`s and `PostId`s. In addition, we’ve had a last-minute request to allow users to choose how to sort posts in their feed. We’ll need a custom data type for that, too.

Our codec can easily handle our custom data types. We just have to make our own combinator that describes how to transform to and from a string. In fact, the primitive combinators we saw before (`int`, `boolean`, `string`, `optional`, etc.) are all built using a helper function, `as`, which we can leverage as well.

```purescript
-- Note: the actual function is more polymorphic, but here I've specialized
-- some types to how you will almost always use them in practice.
as
  :: forall a
   . (a -> String)
  -> (String -> Either String a)
  -> RouteDuplex' String
  -> RouteDuplex a
```

The `as` function allows you to produce your own combinator. In short, you are responsible for providing a function from your custom type to a `String`, and one from a string segment to either an error or your custom type. That leaves one argument free, which will be some existing codec that you are augmenting to produce your custom type. For example, consider the partially-applied `as` for our type vs. the `int` combinator:

```purescript
as' :: forall a. RouteDuplex' String -> RouteDuplex' a
int ::           RouteDuplex' String -> RouteDuplex' Int
```

Let’s use this to revise our routing data type. First, let’s define a way to represent sorting, and functions that convert to and from strings for the data type.

```purescript
data Sort = Asc | Desc

derive instance genericSort :: Generic Sort _

sortToString :: Sort -> String
sortToString = case _ of
  Asc -> "asc"
  Desc -> "desc"

sortFromString :: String -> Either String Sort
sortFromString = case _ of
  "asc" -> Right Asc
  "desc" -> Right Desc
  val -> Left $ "Not a sort: " <> val
```

With these functions in place, it’s trivial to write a new combinator for our sorting data type:

```purescript
sort :: RouteDuplex' String -> RouteDuplex' Sort
sort = as sortToString sortFromString
```

Let’s put this to use! We’ll add `Sort` as a new query parameter, and we’ll use our new combinator to update our route codec, too.

```purescript
data Route
  = Root
  | Profile String
  | Post String Int
  | Feed { search :: Maybe String, sorting :: Maybe Sort }

derive instance genericRoute :: Generic Route _

route :: RouteDuplex' Route
route = root $ sum
  { ...
  , "Feed": "feed" ? { search: optional, sorting: optional <<< sort }
  }
```

Next, lets make our `Route` data type better by providing newtypes to uniquely identify a string as a `Username` or an int as a `PostId`.

```purescript
newtype Username = Username String
derive instance newtypeUsername :: Newtype Username _

newtype PostId = PostId Int
derive instance newtypePostId :: Newtype PostId _

data Route
  = Root
  | Profile Username
  | Post Username PostId
  | Feed { search :: Maybe String, sorting :: Maybe Sort }
```

We could write new combinators as we did for the `Sort` data type, but we don’t really have brand-new data types here. We have newtypes around a `String` and `Int`, which already have combinators available, so we ought to re-use them. This re-use is trivial if we use the `_Newtype` iso from `purescript-profunctor-lenses` along with the existing combinators to create our two new codecs:

```purescript
uname :: RouteDuplex' Username
uname = _Newtype segment

-- re-use the `int` combinator
postId :: RouteDuplex' PostId
postId = _Newtype (int segment)
```

Finally, we can replace our earlier segments with these new codecs.

```purescript
route :: RouteDuplex' Route
route = root $ sum
  { "Root": noArgs
  , "Profile": "user" / uname
  , "Post": "user" / uname / "post" / postId
  , "Feed": "feed" ? { search: optional, sorting: optional <<< sort }
  }
```

## Example: Composing codecs to represent CRUD operations

We’ve seen the `RouteDuplex’ a` type all over the place, whether to represent a small codec for integers or strings or a larger one for our complex sum type. We can create codecs of any size and compose them into larger structures. Let’s walk through an example by extending our routing data type to accommodate create, read, and update operations for posts in our system.

First, we’ll define a data type to represent creating, reading, and updating a resource dependent on some kind of identifier, `a`:

```purescript
data CRU a
  = Create
  | Read a
  | Update a

derive instance genericCRU :: Generic (CRU a) _
```

Next, we’ll again use the `sum` function to write a codec for this sum type. We don’t know how to handle `a`, so we’ll accept a codec to handle it as an argument. We’d like to handle three cases:

- `/` should represent creation
- `/:id` should represent reading
- `/edit/:id` should represent updating

Exactly the same way we wrote a codec for our `Route` type we can write one for our new `CRU` type:

```purescript
cru :: forall a. RouteDuplex' a -> RouteDuplex' (CRU a)
cru inner = sum
  { "Create": noArgs
  , "Read": inner
  , "Update": "edit" / inner
  }
```

Even better, we can use our new data type as part of our `Route` type to describe a resource that follows this URL structure:

```purescript
data Route
  = ...
  | Post Username (CRU PostId)
```

And re-use our codec to produce the larger `Route` codec:

```purescript
route = root $ sum
  { ...
  , "Post": "user" / uname / "post" / cru postId
  }
```

## Example: Running our codec with `purescript-routing`

We've developed a capable parser and printer for our route data type. To be useful, though, we'll want to use our parser along with a library that handles hash-based or pushState routing for us. The most common choice is the `purescript-routing` library. If you aren't familiar with how the library works, [consider skimming the official guide](https://github.com/slamdata/purescript-routing/blob/v8.0.0/GUIDE.md).

We'll use the library to handle hashes and pushState, but rather than use their parser combinators, we'll provide our own, custom parser -- our codec.

First, we'll choose the `matchesWith` function that fits our use case:

- [`Routing.Hash.matchesWith`](https://pursuit.purescript.org/packages/purescript-routing/8.0.0/docs/Routing.Hash#v:matchesWith)
- [`Routing.PushState.matchesWith`](https://pursuit.purescript.org/packages/purescript-routing/8.0.0/docs/Routing.PushState#v:matchesWith)

From here, we'll assume hash-based routing. Next, we'll take a look at the type signature of `matchesWith`. In a nutshell, this function expects a custom parser and a function that will accept a (possible) previous route and the route that just matched and perform some effects with them. It returns an effect that can be used to remove the event listener this will create.

```purescript
matchesWith :: forall f a. Foldable f => (String -> f a) -> (Maybe a -> a -> Effect Unit) -> Effect (Effect Unit)
```

Our custom parser will be the `parse` function from `Routing.Duplex` given our codec:

```purescript
parse :: forall i o. RouteDuplex i o -> String -> Either Parser.RouteError o
```

Filling in the types with our `Route` data type, we get:

```purescript
matchesWith :: (String -> Either RouteError Route) -> (Maybe Route -> Route -> Effect Unit) -> Effect (Effect Unit)
```

To perform your routing effects, provide your custom callback function:

```purescript
canceller <- matchesWith (parse route) \old new -> do
  ... your routing effects, called every time the route changes ...
```
