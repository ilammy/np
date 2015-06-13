define-language: macros processing
==================================

[**Files**](#files)<br/>
— [codegen.sld](#codegensld)<br/>
— [normalization.sld](#normalizationsld)<br/>
— [partitioning.sld](#partitioningsld)<br/>
— [structure-_something_.sld](#structure-somethingsld)<br/>
[**Implementation notes**](#implementation-notes)<br/>
— [General](#general)<br/>
— [`$is-a` and `$must-be` implementation](#is-a-and-must-be-implementation)<br/>
[**Partitioning**](#partitioning)<br/>
— [_Language definition_](#language-definition)<br/>
— [_Toplevel clauses_](#toplevel-clauses)<br/>
— — [Language properties](#language-properties)<br/>
— — — [Extension](#extension)<br/>
— — — [Predicate](#predicate)<br/>
— — — [Parser](#parser)<br/>
— — — [Unparser](#unparser)<br/>
— — [Terminal definitions](#terminal-definitions)<br/>
— — [Nonterminal definitions](#nonterminal-definitions)<br/>
— [_Terminal definitions_](#terminal-definitions-1)<br/>
— — [Standalone](#standalone)<br/>
— — [Extension](#extension-1)<br/>
— — — [Addition](#addition)<br/>
— — — [Removal](#removal)<br/>
— — — [Modification](#modification)<br/>
— [_Nonterminal definitions_](#nonterminal-definitions-1)<br/>
— — [Standalone](#standalone-1)<br/>
— — [Extension](#extension-2)<br/>
— — — [Addition](#addition-1)<br/>
— — — [Removal](#removal-1)<br/>
— — — [Modification](#modification-1)<br/>
— [_Meta-variables_](#meta-variables)<br/>
— — [Standalone](#standalone-2)<br/>
— — [Extension](#extension-3)<br/>
— [_Nonterminal productions_](#nonterminal-productions)<br/>
— — [Standalone](#standalone-3)<br/>
— — [Extension](#extension-4)<br/>
— [_Common terms_](#common-terms)<br/>
[**Normalization**](#Normalization)<br/>
— [_Terminal definitions_](#terminal-definitions-2)<br/>
— — [Standalone](#standalone-4)<br/>
— — [Extension](#extension-5)<br/>
— — — [Addition](#addition-2)<br/>
— — — [Removal](#removal-2)<br/>
— — — [Modification](#modification-2)<br/>
— [_Nonterminal definitions_](#nonterminal-definitions-2)<br/>
— — [Standalone](#standalone-5)<br/>
— — [Extension](#extension-6)<br/>
— — — [Addition](#addition-3)<br/>
— — — [Removal](#removal-3)<br/>
— — — [Modification](#modification-3)<br/>
— [_Other clauses_](#other-clauses)<br/>


Files
-----

### codegen.sld

Functions that generate Scheme code snippets from syntatic values operated by
other files. They are called near the end of processing, to generate the actual
output of the define-language form when everything has been already verified
and normalized.


### normalization.sld

Functions that convert partitioned expressions into their canonical form by
filling in default values, flattening nested lists. That is, they do additional
postprocessing after the partitioning stage.


### partitioning.sld

These files contain functions that partition, group, and regroup expressions
into various classes based on their _syntactical_ features.

They also can do some preprocessing of the forms: as squashing extension
groups like `(+ decl1 decl2 ...)` for example.


### structure-_something_.sld

Functions that match and check the structure of clauses. They check the
structure of forms—count and positions of their elements—and occasionally
do some types of the elements, but not their semantics.

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
    ones, but they are **assertions**. I.e., they fail with a helpful
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


Implementation notes
--------------------

### General

By _functions_ we mean macrofunctions for the CK machine defined in `(sr ck)`
library. These are 'improved' macros which compose better due to the fact that
they expand their arguments before themselves.

CK entry point is named `$`. By convention, macrofunction names are also
prefixed with `$`. Their arguments are quoted. Seek more documentation on them
elsewhere.


### `$is-a` and `$must-be` implementation

They are implemented in terms of _verifier_ functions. `verify-utils` has helper
macros for defining such verifiers as well as `$is-a` and `$must-be` checkers.

There are certain conventions for verification macrofunctions:

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


Partitioning
------------

_**Partitioning**_ is reponsible for partitioning clauses into classes by their
_syntactical_ differences. Effectively, only structure of subclauses is taken
into account while actually partitioning clauses (i.e., the patterns of lists,
vectors, and atoms matter). But this stage also verifies lexical correctness of
the input (e.g., whether symbols and stuff are used where they should be used).

A number of oddities can be detected and reported while analysing the syntax of
the clauses. They are gathered here together with respective error messages to
keep everything consistent.


### Language definition ########################################################

Referenced as: _language-definition_

`(define-language name toplevel-clauses ...)` — the only acceptable form

_**Syntax violations**_:

 1. Definitions must have exactly this form.

    — Invalid syntax of the language definition

 2. **name** must be an identifier.

    — Name of the language must be an identifier: <br/>
      <_invalid-language-name_>

 3. Each **toplevel-clause** must be a _toplevel-clause_.


### Toplevel clauses ###########################################################

Referenced as: _toplevel-clause_

`toplevel-clause` — the only acceptable form

_**Syntax violations**_:

 1. **toplevel-clause** must be an _extension-clause_, or a _predicate-clause_,
    or a _parser-clause_, or an _unparser-clause_, or a _terminals-clause_,
    or a _nonterminal-clause_.

    — Invalid syntax of the toplevel clause: <br/>
      <_language_> <_invalid-toplevel-clause_>

 2. _extension-clause_, _predicate-clause_, _parser-clause_, and
    _unparser-clause_ can be used at most once in language definition.

    — Only one 'extends' clause can be specified: <br/>
      <_language_> <_all-extension-clauses_>

    — Only one 'predicate' clause can be specified: <br/>
      <_language_> <_all-predicate-clauses_>

    — Only one 'parser' clause can be specified: <br/>
      <_language_> <_all-parser-clauses_>

    — Only one 'unparser' clause can be specified: <br/>
      <_language_> <_all-unparser-clauses_>


#### Language properties #############################################

##### Extension ############################################

Referenced as: _extension-clause_

`(extends name)` — the only acceptable form

_**Syntax violations**_:

 1. Clauses must have exactly this form.

    — Invalid syntax of the extension clause: <br/>
      <_language_> <_invalid-extension-clause_>

 1. **name** must be an identifier.

    — Name of the language to be extended must be an identifier: <br/>
      <_language_> <_extension-clause_> <_invalid-name_>

 2. **name** must be singular.

    — Name of the language to be extended cannot be empty: <br/>
      <_language_> <_invalid-extension-clause_>

    — Only one language can be extended: <br/>
      <_language_> <_invalid-extension-clause_>


##### Predicate ############################################

Referenced as: _extension-clause_

`(predicate name)` — the only acceptable form

_**Syntax violations**_:

 1. Clauses must have exactly this form.

    — Invalid syntax of the predicate clause: <br/>
      <_language_> <_invalid-predicate-clause_>

 2. **name** must be an identifier.

    — Name of the language predicate must be an identifier: <br/>
      <_language_> <_predicate-clause_> <_invalid-name_>

 3. **name** must be singular.

    — Name of the language predicate cannot be empty: <br/>
      <_language_> <_invalid-predicate-clause_>

    — Only one language predicate name can be specified: <br/>
      <_language_> <_invalid-predicate-clause_>


##### Parser ###############################################

Referenced as: _parser-clause_

`(parser name)` — the only acceptable form

_**Syntax violations**_:

 1. Clauses must have exactly this form.

    — Invalid syntax of the parser clause: <br/>
      <_language_> <_invalid-parser-clause_>

 2. **name** must be an identifier.

    — Name of the language parser must be an identifier: <br/>
      <_language_> <_parser-clause_> <_invalid-name_>

 3. **name** must be singular.

    — Name of the language parser cannot be empty: <br/>
      <_language_> <_invalid-parser-clause_>

    — Only one language parser name can be specified: <br/>
      <_language_> <_invalid-parser-clause_>


##### Unparser #############################################

Referenced as: _unparser-clause_

`(unparser name)` — the only acceptable form

_**Syntax violations**_:

 1. Clauses must have exactly this form.

    — Invalid syntax of the unparser clause: <br/>
      <_language_> <_invalid-unparser-clause_>

 2. **name** must be an identifier.

    — Name of the language unparser must be an identifier: <br/>
      <_language_> <_unparser-clause_> <_invalid-name_>

 3. **name** must be singular.

    — Name of the language unparser cannot be empty: <br/>
      <_language_> <_invalid-unparser-clause_>

    — Only one language unparser name can be specified: <br/>
      <_language_> <_invalid-unparser-clause_>


#### Terminal definitions ############################################

Referenced as: _terminals-clause_

`(terminals terminal-definitions ...)` — the only acceptable form

_**Syntax violations**_:

 1. Clauses must have exactly this form.

    — Invalid syntax of the terminals clause: <br/>
      <_language_> <_invalid-terminals-clause_>

 2. Each **terminal-definition** must be a _standalone-terminal_ if
    _extension-clause_ is not present in the language definition;
    otherwise it must be either _terminal-addition_, _terminal-removal_,
    or _terminal-modification_.


#### Nonterminal definitions #########################################

Referenced as: _nonterminal-clause_

`nonterminal-definition` — the only acceptable form

_**Syntax violations**_:

 1. **nonterminal-definition** must be a _standalone-nonterminal_ if
    _extension-clause_ is not present in the language definition;
    otherwise it must be either _nonterminal-addition_, _nonterminal-removal_,
    or _nonterminal-modification_.


### Terminal definitions #######################################################

#### Standalone ######################################################

Referenced as: _standalone-terminal_

`(name predicate (meta-vars ...))` — the only acceptable form

_**Syntax violations**_:

 1. Clauses must have exactly this form.

    — Invalid syntax of the terminal: <br/>
      <_language_> <_invalid-standalone-terminal_>

 2. **name** must be an identifier.

    — Name of the terminal must be an identifier: <br/>
      <_language_> <_standalone-terminal_> <_invalid-name_>

 3. **meta-vars** must a proper non-empty list.

    — At least one meta-variable should be specified for a terminal: <br/>
      <_language_> <_terminal-name_>

 4. Each **meta-var** must be a _meta-variable-name_.


#### Extension #######################################################

##### Addition #############################################

Referenced as: _terminal-addition_

`(+ added-terminals ...)` — the only acceptable form

_**Syntax violations**_:

 1. Clauses must have exactly this form.

    — Invalid syntax of the terminal extension: <br/>
      <_language_> <_invalid-terminal-addition_>

 2. **added-terminals** must be a proper non-empty list.

    — At least one terminal should be specified for addition: <br/>
      <_language_> <_invalid-terminal-addition_>

 3. Each **added-terminal** must be a _standalone-terminal_.


##### Removal ##############################################

Referenced as: _terminal-removal_

`(- removed-terminals ...)` — the only acceptable form

_**Syntax violations**_:

 1. Clauses must have exactly this form.

    — Invalid syntax of the terminal extension: <br/>
      <_language_> <_invalid-terminal-removal_>

 2. **removed-terminals** must be a proper non-empty list.

    — At least one terminal should be specified for removal: <br/>
      <_language_> <_invalid-terminal-removal_>

 3. Each **removed-terminal** must be either a _standalone-terminal_,
    or an identifier representing the terminal name.

    — Invalid syntax of the terminal: <br/>
      <_language_> <_not-standalone-terminal-or-identifier_>


##### Modification #########################################

Referenced as: _terminal-modification_

`(! modified-terminals ...)` — the only acceptable form

_**Syntax violations**_:

 1. Clauses must have exactly this form.

    — Invalid syntax of the terminal extension: <br/>
      <_language_> <_invalid-terminal-modification_>

 2. **modified-terminals** must be a proper non-empty list.

    — At least one terminal should be specified for modification: <br/>
      <_language_> <_invalid-terminal-modification_>

 3. Each **modified-terminal** must be a _modified-terminal_.


Referenced as: _modified-terminal_

`(name (meta-var-modifications ...))` — the only acceptable form

_**Syntax violations**_:

 1. Clauses must have exactly this form.

    — Invalid syntax of the terminal modification: <br/>
      <_language_> <_invalid-modified-terminal_>

 2. **name** must be an identifier.

    — Name of the terminal must be an identifier: <br/>
      <_language_> <_modified-terminal_> <_name_>

 3. **meta-var-modifications** must be a proper non-empty list.

    — Terminal modification should modify meta-variables: <br/>
      <_language_> <_terminal-name_>

 4. Each **meta-var-modification** must be a _meta-variable-modification_.


### Nonterminal definitions ####################################################

#### Standalone ######################################################

Referenced as: _standalone-nonterminal_

`(name                (meta-vars ...) productions ...)` — normal form

`(name predicate-name (meta-vars ...) productions ...)` — form with predicate binding

_**Syntax violations**_:

 1. Clauses must have exactly one of these forms.

    — Invalid syntax of the nonterminal: <br/>
      <_language_> <_invalid-standalone-nonterminal_>

 2. **name** must be an identifier.

    — Name of the nonterminal must be an identifier: <br/>
      <_language_> <_standalone-nonterminal-without-productions_> <_invalid-name_>

 3. **predicate-name** must be an identifier.

    — Name of the nonterminal predicate must be an identifier: <br/>
      <_language_> <_standalone-nonterminal-without-productions_> <_invalid-predicate-name_>

 4. Each **meta-var** must be _meta-variable-name_.

 5. **productions** must be a proper non-empty list.

    — At least one production should be specified for a nonterminal: <br/>
      <_language_> <_nonterminal-name_>

 6. Each **production** must be a _production-definition_.


#### Extension #######################################################

##### Addition #############################################

Referenced as: _nonterminal-addition_

`(+ added-nonterminals ...)` — the only acceptable form

_**Syntax violations**_:

 1. Clauses must have exactly this form.

    — Invalid syntax of the nonterminal extension: <br/>
      <_language_> <_invalid-nonterminal-addition_>

 2. **added-nonterminals** must be a proper non-empty list.

    — At least one nonterminal should be specified for addition: <br/>
      <_language_> <_invalid-nonterminal-addition_>

 3. Each **added-nonterminal** must be a _standalone-nonterminal_.


##### Removal ##############################################

Referenced as: _nonterminal-removal_

`(- removed-nonterminals ...)` — the only acceptable form

_**Syntax violations**_:

 1. Clauses must have exactly this form.

    — Invalid syntax of the nonterminal extension: <br/>
      <_language_> <_invalid-nonterminal-removal_>

 2. **removed-nonterminals** must be a proper non-empty list.

    — At least one nonterminal should be specified for removal: <br/>
      <_language_> <_invalid-nonterminal-removal_>

 3. Each **removed-nonterminal** must be either a _standalone-nonterminal_
    (with an exception that absent production list is allowed), or an identifier
    representing the nonterminal name.

    — Invalid syntax of the nonterminal: <br/>
      <_language_> <_not-standalone-nonterminal-or-identifier_>


##### Modification #########################################

Referenced as: _nonterminal-modification_

`(! modified-nonterminals ...)` — the only acceptable form

_**Syntax violations**_:

 1. Clauses must have exactly this form.

    — Invalid syntax of the nonterminal extension: <br/>
      <_language_> <_invalid-nonterminal-modification_>

 2. **modified-nonterminals** must be a proper non-empty list.

    — At least one nonterminal should be specified for modification: <br/>
      <_language_> <_invalid-nonterminal-modification_>

 3. Each **modified-nonterminal** must be a _modified-nonterminal_.


Referenced as: _modified-nonterminal_

`(name (meta-var-modifications ...) production-modifications ...)` — normal form

`(name predicate-name (meta-var-modifications ...) production-modifications ...)` — normal form with predicate rebind

`(name predicate-name)` — pure predicate rebind form

_**Syntax violations**_:

 1. Clauses must have exactly one of these forms.

    — Invalid syntax of the nonterminal modification: <br/>
      <_language_> <_invalid-modified-nonterminal_>

 2. **name** must be an identifier.

    — Name of the nonterminal must be an identifier: <br/>
      <_language_> <_modified-nonterminal_> <_name_>

 3. **predicate-name** must be an identifier.

    — Name of the nonterminal predicate must be an identifier: <br/>
      <_language_> <_modified-nonterminal_> <_invalid-predicate-name_>

 4. Each **meta-var-modification** must be a _meta-variable-modification_.

 5. Each **production-modification** must be a _production-modification_.

 6. **meta-var-modifications** and **production-modifications** cannot be
    simultaneously empty.

    — Nonterminal modification should modify either meta-variables or productions: <br/>
      <_language_> <_nonterminal-name_>


### Meta-variables #############################################################

#### Standalone ######################################################

Referenced as: _meta-variable-name_

`meta-var-name` — the only acceptable form

_**Syntax violations**_:

 1. **meta-var-name** must be an identifier.

    — Name of the meta-variable must be an identifier: <br/>
      <_language_> <_terminal-or-nonterminal-name_> <_invalid-meta-var-name_>


#### Extension #######################################################

Referenced as: _meta-variable-modification_

`(+ meta-var-names ...)` — addition form

`(- meta-var-names ...)` — removal form

_**Syntax violations**_:

 1. Clauses must have exactly one of these forms.

    — Invalid syntax of the meta-variable modification: <br/>
      <_language_> <_terminal-or-nonterminal-name_> <_invalid-meta-variable-modification_>

 2. **meta-var-names** must be a proper non-empty list.

    — At least one meta-variable should be specified for addition: <br/>
      <_language_> <_terminal-or-nonterminal-name_> <_invalid-addition-form_>

    — At least one meta-variable should be specified for removal: <br/>
      <_language_> <_terminal-or-nonterminal-name_> <_invalid-removal-form_>

 3. Each **meta-var-name** must be a _meta-variable-name_.


### Nonterminal productions ####################################################

#### Standalone ######################################################

Referenced as: _production-definition_

`production` — the only acceptable form

_**Syntax violations**_:

 1. **production** must not be a vector and must not contain them.

    — Invalid syntax of the production: vector patterns are not allowed: <br/>
      <_language_> <_nonterminal-name_> <_invalid-production_>

    — Invalid syntax of the production: vector patterns are not allowed: <br/>
      <_language_> <_nonterminal-name_> <_invalid-production_> <_nested-vector_>


#### Extension #######################################################

Referenced as: _production-modification_

`(+ productions ...)` — addition form

`(- productions ...)` — removal form

_**Syntax violations**_:

 1. Clauses must have exactly one of these forms.

    — Invalid syntax of the production modification: <br/>
      <_language_> <_nonterminal-name_> <_invalid-production-modification_>

 2. **productions** must be a proper non-empty list.

    — At least one production should be specified for addition: <br/>
      <_language_> <_nonterminal-name_> <_invalid-addition-form_>

    — At least one production should be specified for removal: <br/>
      <_language_> <_nonterminal-name_> <_invalid-removal-form_>

 3. Each **production** must be a _production_.


### Common terms ###############################################################

<dl>
    <dt>Atom</dt>
    <dd>Not a <i>list</i>, a <i>pair</i>, or a <i>vector</i>. Generally,
        atoms are matched as <tt>atom</tt> in the source code.</dd>

    <dt>Empty list</dt>
    <dd>Matches as <tt>()</tt> in the source code.</dd>

    <dt>Proper list</dt>
    <dd>Matches as <tt>(x ...)</tt> in the source code.</dd>

    <dt>Dotted list</dt>
    <dd>Matches as <tt>(x y ... . d)</tt> in the source code.</dd>

    <dt>List</dt>
    <dd>An <i>empty list</i>, a <i>proper list</i>, or a <i>dotted list</i>.
        Or a <i>pair</i>, technically.</dd>

    <dt>Pair</dt>
    <dd>Matches as <tt>(a . d)</tt> in the source code. Can be thought of as
        a 'special case' of list, or vice versa.</dd>

    <dt>Vector</dt>
    <dd>Matches as <tt>#(x ...)</tt> in the source code.</dd>

    <dt>Identifier</dt>
    <dd>An identifier as specified by the syntax of Scheme. Any form is
        acceptable (`regular`, `|explicit|`, `with-\x55;nicode`, etc.).
        Generally referred to as _symbol_ in the source code.</dd>
</dl>


Normalization
-------------

_**Normalization**_ is responsible for converting partitioned clauses into
their normal, canonical form. This includes filling in default values, removing
unnecessary elements, some element reordering, etc. Normalization operates on
valid input and do not check for errors; this should have been already done
during the partitioning step.

Here the rules of normalization are described in human-readable form.


### Terminal definitions #######################################################

#### Standalone ######################################################

`(name predicate (meta-vars ...))` — full form (normalized form)

_**Normalization**_:

None.


#### Extension #######################################################

##### Addition #############################################

`(name predicate (meta-vars ...))` — full form (normalized form)

_**Normalization**_:

None.


##### Removal ##############################################

`name` — identifier form (normalized form)

`(name predicate (meta-vars ...))` — full form

_**Normalization**_:

 1. Full form is converted into normalized form by leaving only the `name`.


##### Modification #########################################

`(name ((meta-var-additions ...) (meta-var-removals ...)))` — regular form

`(name (meta-var-additions ...) (meta-var-removals ...))` — normalized form

_**Normalization**_:

 1. Regular form is converted into normalized form by splicing the list of
    meta-variable modifications into the form body.


### Nonterminal definitions ####################################################

#### Standalone ######################################################

`(name                 (meta-vars ...) productions ...)` — regular form

`(name predicate-name  (meta-vars ...) productions ...)` — form with predicate binding

`(name predicate-name? (meta-vars ...) (productions ...))` — normalized form

_**Normalization**_:

 1. Both forms have the list of productions unspliced into the form body.

 2. For regular form, the default value of `predicate-name?` is `#f`.

 3. For binding form, `predicate-name?` is transferred as-is.


#### Extension #######################################################

##### Addition #############################################

`(name                 (meta-vars ...) productions ...)` — regular form

`(name predicate-name  (meta-vars ...) productions ...)` — form with predicate binding

`(name predicate-name? (meta-vars ...) (productions ...))` — normalized form

_**Normalization**_:

 1. Both forms have the list of productions unspliced into the form body.

 2. For regular form, the default value of `predicate-name?` is `#f`.

 3. For binding form, `predicate-name?` is transferred as-is.


##### Removal ##############################################

`name` — identifier form (normalized form)

`(name                (meta-vars ...) productions ...)` — regular form

`(name predicate-name (meta-vars ...) productions ...)` — form with predicate binding

_**Normalization**_:

 1. Both forms have only the `name` left.


##### Modification #########################################

`(name                 ((meta-var-additions ...) (meta-var-removals ...)) ((production-additions ...) (production-removals ...)))` — regular form

`(name predicate-name  ((meta-var-additions ...) (meta-var-removals ...)) ((production-additions ...) (production-removals ...)))` — for with predicate (re)binding

`(name predicate-name? (meta-var-additions ...) (meta-var-removals ...) (production-additions ...) (production-removals ...))` — normalized form

_**Normalization**_:

 1. Input form is converted into normalized form by splicing the lists of
    meta-variable and production modifications into the form body.

 2. For regular form, the default value of `predicate-name?` is `#f`.

 3. For binding form, `predicate-name?` is transferred as-is.


### Other clauses ##############################################################

Other clauses are not affected by normalization. Toplevel language property
clauses are already normalized, and other ones are only simple lists that do
not require any additional processing.
