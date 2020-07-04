# How To Represent Command-Line Options as Dhall Type

This How To describes basic ways how command line options can be represented as
typed values in Dhall. This allows us to construct command lines for execution
in more type-safe way.


## Representing Command Line Arguments

First we have to talk about more generic problem, how to represent all command
line arguments (including options) in Dhall. Here we have to realise that there
are two representations that we are talking about:

*   Representing them as arbitrary typed values.
*   `List Text` – List of command-line arguments as they will be passed to the
    command itself.

In other words we want to use typed representation that can be rendered as
`List Text` value, which can be then passed to an executable.  For that to work
we always need to supply some version of `toArguments` function:

```Dhall
let Verbosity = < Quiet | Normal | Verbose >

let Verbosity/toArguments =
      λ(_ : Verbosity) →
        merge
          { Quiet = [ "--quiet" ]
          , Normal = [] : List Text
          , Verbose = [ "--verbose" ]
          }
          _

let Options = { verbosity : Optional Verbosity }

let default = { verbosity = None Verbosity }

let optionalOptions =
      https://raw.githubusercontent.com/trskop/command-wrapper/master/command-wrapper/dhall/Exec/utils/optionalOptions

let toArguments =
      λ(options : Options) →
          optionalOptions Verbosity Verbosity/toArguments opts.verbosity
        # ([] : List Text)

in  { Type = Options, default, toArguments }
```

Where the `toArguments` function has following type:

```Dhall
toArguments : Options → List Text
```

There are some very useful helper functions available in the Exec Dhall
library. They are documented here: [Command Wrapper Exec Dhall Library: Other
Utilities](../command-wrapper/dhall/Exec#other-utilities).


## Flags

Options that are not required and take no arguments are sometimes referred to
as flags.  In a way they represent a boolean value, however there may be some
nuances involved.

Let's say that the command line tool we are trying to describe takes `--stuff`
option:

```
COMMAND [--stuff] ...
```

We can represent that in Dhall as:

```Dhall
let Options =
      { -- ...
      , stuff : Bool
        -- ...
      }

let default =
      { -- ...
      , stuff = False
        -- ...
      }

in { Type = Options, default }
```

In the above example the `Bool` value has following meaning:

*   `False` – The option **is not present** on the command line.
*   `True` – The option **is present** on the command line.

Sometimes flags like these have duals.  For `--stuff` the dual can be
`--no-stuff`:

```
COMMAND [--stuff|--no-stuff] ...
```

The way they usually behave is:

*   `COMMAND --stuff --no-stuff` is the same as `COMMAND --no-stuff`
*   `COMMAND --no-stuff --stuff` is the same as `COMMAND --stuff`
*   The default will be applied when neither flag is listed.

If we can guarantee that only one of these will be in the generated output then
the obvious encoding is:

```Dhall
let Options =
      { -- ...
      , stuff : Optional Bool
        -- ...
      }

let default =
      { -- ...
      , stuff = None Bool
        -- ...
      }

in { Type = Options, default }
```

In the above example the `Optional Bool` value has following meaning:

*   `None Bool` – The option **is not present** on the command line.
*   `Some False` – The option **`--no-stuff` is present** on the command line.
*   `Some True` – The option **`--stuff` is present** on the command line.

Some applications may require one of these to be always supplied:

```
COMMAND {--stuff|--no-stuff} ...
```

In such cases we just need to drop the `Optional` in our model, and make it a
required field by not supplying default value:

```Dhall
let Options =
      { -- ...
      , stuff : Bool
        -- ...
      }

let default =
      { -- ...
      }

in { Type = Options, default }
```

In the above example the `Optional Bool` value has following meaning:

*   `False` – The option **`--no-stuff` is present** on the command line.
*   `True` – The option **`--stuff` is present** on the command line.


# Options

In context of this document we'll define as options as command line options
that take an argument.

Very common options for applications that do intensive computations is passing
how many threads they should use:

```
COMMAND [--jobs=N]
```

This can be modeled as:

```Dhall
let Options =
      { -- ...
      , jobs : Optional Natural
        -- ...
      }

let default =
      { -- ...
      , jobs = None Natural
        -- ...
      }

in { Type = Options, default }
```

However, the above model has two special values `None Natural` and `Some 0`
that may or may not mean the same thing. Really depends on the application we
are targeting.  If these two cases are actually the same value (no `--jobs=N`
option supplied) then it may make sense to either not render it in these cases
or to switch to `jobs : Natural` representation and use `0` as a special value.
Both have advantages and disadvantages. In case of the former we get following:

*   `None Natural` – The option **is not present** on the command line.
*   `Some 0` – The option **is not present** on the command line.
*   `Some n` – The option **`--jobs=${Natural/show n}` is present** on the command
    line.


Some applications even make the number of threads optional which makes them use
some kind of heuristic to figure out a value:

```
COMMAND [--jobs[=N]]
```

In this case the values `None Natural` and `Some 0` become distinct:

*   `None Natural` – The option **is not present** on the command line.
*   `Some 0` – The option **`--jobs` is present** on the command line.
*   `Some n` – The option **`--jobs=${Natural/show n}` is present** on the command
    line.

Some options takes an enum as an argument:

```
COMMAND [--colour={always|never|auto}]
```

We can easily model it using union types (also known as sum types):

```Dhall
let Colour = < Always | Never | Auto >

let Options =
      { -- ...
      , colour : Optional Colour
        -- ...
      }

let default =
      { -- ...
      , colour = None Colour
        -- ...
      }

in { Type = Options, default }
```

How rendering should work:

*   `None Colour` – The option **is not present** on the command line.
*   `Some colour` – The option **`--colour=${merge { Always = "always", Never =
    "never", Auto = "auto" } colour}` is present** on the command line.

Sometimes there are defaults for the values, i.e. the argument can be omitted:

```
COMMAND [--colour[={always|never|auto}]]
```

There are two basic ways how we can modify the above model to support the
above.  First one is nested `Optional`:

```Dhall
let Colour = < Always | Never | Auto >

let Options =
      { -- ...
      , colour : Optional (Optional Colour)
        -- ...
      }

let default =
      { -- ...
      , colour = None (Optional Colour)
        -- ...
      }

in { Type = Options, default }
```

*   `None (Optional Colour)` – The option **is not present** on the command
    line.
*   `Some (None Colour)` – The option **`--colour` is present** on the command
    line. Notice the absence of value.
*   `Some (Some colour)` – The option **`--colour=${merge { Always = "always",
    Never = "never", Auto = "auto" } colour}` is present** on the command line.

The second approach is having a special value to represent the "default":

```Dhall
let Colour = < Default | Always | Never | Auto >

let Options =
      { -- ...
      , colour : Optional Colour
        -- ...
      }

let default =
      { -- ...
      , colour = None Colour
        -- ...
      }

in { Type = Options, default }
```

*   `None Colour` – The option **is not present** on the command
    line.
*   `Some Colour.Default` – The option **`--colour` is present** on the command
    line. Notice the absence of value.
*   `Some (Some colour)` – The option **`--colour=${merge { Always = "always",
    Never = "never", Auto = "auto" } colour}` is present** on the command line.
