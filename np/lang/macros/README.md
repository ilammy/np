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

  - `$expected-a:<something>` functions are error reporting shortcuts
    used by _partitioning_ files. They always fail with an error message.
    Techincally, `$must-be` can be used instead, but these functions
    are explicitly called on expressions that failed `$is-a` checks.

  - `$squash-<something>` functions squash `(+ ...)` groups into flat
    lists of elements.

  - `$get-`/`$set-` structural accessors that extract and inject parts
    of terms.


##### A note on `$is-a` and `$must-be` implementation

They are implemented in terms of _verifier_ functions. `verify-utils` has helper
macros for defining such verifiers as well as `$is-a` and `$must-be` checkers.

There are certain convetions for verification macrofunctions:

  - names start with `%`, it mirrors the fact that these functions have _two_
    continuations, not one as regular CK functions

  - argument patterns must have the following form:
    `(_ s '(k t) 'term '<matching-expression>)`

  - `s` is a regular CK continuation used as a _success_ branch,
    pass `#t` value to it to signal a success

  - `k` is a reified escape continuation used as a _failure_ branch,
    pass `(<string-message> <stack>)` value to it to signal a failure

  - `t` is currently accumulated stack of subexpressions that are currently
    verified; the stack together with the string message can be displayed to
    the user as a hint about what is wrong with the expression being verified

  - use the last argument to match the term in question as you like

  - `term` is exactly the same term as the following argument. It is here
    in case it is necessary to match with literal identifiers _and_ pass
    the term to some other function. You should pass the `term` argument,
    because you cannot reconstruct a term with literal identifiers is macro
    templates.

  - call verifiers like this:

      `(%verify '(k <updated-t>) '<term-to-verify>)`

    update the stack as necessary, do not duplicate the term
