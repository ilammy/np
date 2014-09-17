A little note on file and macro-function naming principles here.


#### **partition-_something_.sld**

These files contain functions that partition, group, and regroup expressions
into various classes based on their _structural_ features.

They also can do some preprocessing of the forms: as squashing extension
groups like `(+ decl1 decl2 ...)` for example.


#### **structure-_something_.sld**

These files contain functions that match and check the structure of clauses.
They only check the structure of forms--count and positions of their elements,
but not types of the elements or their semantics.

Functions there can be divided into several classes:

  - `$can-be:<something>?` functions are **necessity** predicates. I.e.,
    getting `#t` as a result is necessary—but not sufficient!—for the
    thing in question to be `<something>`; and getting `#f` means that
    the thing is definitely not `<something>`.

  - `$is-a:<something>?` functions are **sufficiency** predicates. I.e.,
    getting `#t` means that the thing is an absolutely structurally
    valid `<something>`, while `#f` still means that the thing cannot
    be `<something>`.

  - `$must-be:<something>` functions are essentially the same as `$is-a`
    ones, but they are **assetions**. I.e., they fail with a helpful
    syntax error if the thing is not `<something>`. They are used for
    reporting errors, and they are _meant_ to fail, not return values.

  - Other self-explanatory functions.


##### A note on `$is-a` and `$must-be` implementation

They are generally implemented in terms of `$verify` functions, which
return `#t` is the verification is successful, and non-`#t` exception value
in the other case. The exception values is a list of human-readable message
that describes the error and a stack of subexpressions which have caused it.
The exception is either turned into `#f` by `$is-a` or printed out as
a `syntax-error` by `$must-be` functions.

Such exception-like behavior is implemented via `%verify` functions that
exploit the built-in call/cc ability of CK functions. Hence the `%` prefix:
it mirrors the fact that these functions have _two_ possible continuations.
