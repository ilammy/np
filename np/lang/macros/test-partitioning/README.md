_**Partitioning**_ is reponsible for partitioning clauses into classes by their
_syntactical_ differences. Effectively, only structure of subclauses is taken
into account while actually partitioning clauses (i.e., the patterns of lists,
vectors, and atoms matter). But this stage also verifies lexical correctness of
the input (e.g., whether symbols and stuff are used where they should be used).

A number of oddities can be detected and reported while analysing the syntax of
the clauses. They are gathered here together with respective error messages to
keep everything consistent.


Language definition
===================

Referenced as: _language-definition_

`(define-language name toplevel-clauses ...)` — the only acceptable form

##### _Syntax violations_: #######################

 1. Definitions must have exactly this form.

    — Invalid syntax of the language definition

 2. **name** must be an identifier.

    — Name of the language must be an identifier: <br/>
      <_invalid-language-name_>

 3. Each **toplevel-clause** must be a _toplevel-clause_.



Toplevel clauses
================

Referenced as: _toplevel-clause_

`toplevel-clause` — the only acceptable form

##### _Syntax violations_: #######################

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


Language properties
-------------------

### Extension

Referenced as: _extension-clause_

`(extends name)` — the only acceptable form

##### _Syntax violations_: #######################

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


### Predicate

Referenced as: _extension-clause_

`(predicate name)` — the only acceptable form

##### _Syntax violations_: #######################

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


### Parser

Referenced as: _parser-clause_

`(parser name)` — the only acceptable form

##### _Syntax violations_: #######################

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


### Unparser

Referenced as: _unparser-clause_

`(unparser name)` — the only acceptable form

##### _Syntax violations_: #######################

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


Terminal definitions
--------------------

Referenced as: _terminals-clause_

`(terminals terminal-definitions ...)` — the only acceptable form

##### _Syntax violations_: #######################

 1. Clauses must have exactly this form.

    — Invalid syntax of the terminals clause: <br/>
      <_language_> <_invalid-terminals-clause_>

 2. Each **terminal-definition** must be a _standalone-terminal_ if
    _extension-clause_ is not present in the language definition;
    otherwise it must be either _terminal-addition_, _terminal-removal_,
    or _terminal-modification_.


Nonterminal definitions
-----------------------

Referenced as: _nonterminal-clause_

`nonterminal-definition` — the only acceptable form

##### _Syntax violations_: #######################

 1. **nonterminal-definition** must be a _standalone-nonterminal_ if
    _extension-clause_ is not present in the language definition;
    otherwise it must be either _nonterminal-addition_, _nonterminal-removal_,
    or _nonterminal-modification_.



Terminal definitions
====================

Standalone
----------

Referenced as: _standalone-terminal_

`(name predicate (meta-vars ...))` — full form

`(predicate-name (meta-vars ...))` — short form

##### _Syntax violations_: #######################

 1. Clauses must have exactly one of these forms.

    — Invalid syntax of the terminal: <br/>
      <_language_> <_invalid-standalone-terminal_>

 2. **name** must be an identifier.

    — Name of the terminal must be an identifier: <br/>
      <_language_> <_standalone-terminal_> <_invalid-name_>

 3. **predicate-name** must be an identifier.

    — Terminal predicate must be a variable in short form: <br/>
      <_language_> <_standalone-terminal_> <_invalid-predicate-name_>

 4. **meta-vars** must a proper non-empty list.

    — At least one meta-variable should be specified for a terminal: <br/>
      <_language_> <_terminal-name_>

 5. Each **meta-var** must be a _meta-variable-name_.


Extension
---------

### Addition

Referenced as: _terminal-addition_

`(+ added-terminals ...)` — the only acceptable form

##### _Syntax violations_:

 1. Clauses must have exactly this form.

    — Invalid syntax of the terminal extension: <br/>
      <_language_> <_invalid-terminal-addition_>

 2. **added-terminals** must be a proper non-empty list.

    — At least one terminal should be specified for addition: <br/>
      <_language_> <_invalid-terminal-addition_>

 3. Each **added-terminal** must be a _standalone-terminal_.


### Removal

Referenced as: _terminal-removal_

`(- removed-terminals ...)` — the only acceptable form

##### _Syntax violations_:

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


### Modification

Referenced as: _terminal-modification_

`(! modified-terminals ...)` — the only acceptable form

##### _Syntax violations_: #######################

 1. Clauses must have exactly this form.

    — Invalid syntax of the terminal extension: <br/>
      <_language_> <_invalid-terminal-modification_>

 2. **modified-terminals** must be a proper non-empty list.

    — At least one terminal should be specified for modification: <br/>
      <_language_> <_invalid-terminal-modification_>

 3. Each **modified-terminal** must be a _modified-terminal_.


Referenced as: _modified-terminal_

`(name (meta-var-modifications ...))` — the only acceptable form

##### _Syntax violations_: #######################

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



Nonterminal definitions
=======================

Standalone
----------

Referenced as: _standalone-nonterminal_

`(name                (meta-vars ...) productions ...)` — normal form

`(name predicate-name (meta-vars ...) productions ...)` — form with predicate binding

##### _Syntax violations_: #######################

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


Extension
---------

### Addition

Referenced as: _nonterminal-addition_

`(+ added-nonterminals ...)` — the only acceptable form

##### _Syntax violations_: #######################

 1. Clauses must have exactly this form.

    — Invalid syntax of the nonterminal extension: <br/>
      <_language_> <_invalid-nonterminal-addition_>

 2. **added-nonterminals** must be a proper non-empty list.

    — At least one nonterminal should be specified for addition: <br/>
      <_language_> <_invalid-nonterminal-addition_>

 3. Each **added-nonterminal** must be a _standalone-nonterminal_.


### Removal

Referenced as: _nonterminal-removal_

`(- removed-nonterminals ...)` — the only acceptable form

##### _Syntax violations_: #######################

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


### Modification

Referenced as: _nonterminal-modification_

`(! modified-nonterminals ...)` — the only acceptable form

##### _Syntax violations_: #######################

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

##### _Syntax violations_: #######################

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



Meta-variables
==============

Standalone
----------

Referenced as: _meta-variable-name_

`meta-var-name` — the only acceptable form

##### _Syntax violations_: #######################

 1. **meta-var-name** must be an identifier.

    — Name of the meta-variable must be an identifier: <br/>
      <_language_> <_terminal-or-nonterminal-name_> <_invalid-meta-var-name_>


Extension
---------

Referenced as: _meta-variable-modification_

`(+ meta-var-names ...)` — addition form

`(- meta-var-names ...)` — removal form

##### _Syntax violations_: #######################

 1. Clauses must have exactly one of these forms.

    — Invalid syntax of the meta-variable modification: <br/>
      <_language_> <_terminal-or-nonterminal-name_> <_invalid-meta-variable-modification_>

 2. **meta-var-names** must be a proper non-empty list.

    — At least one meta-variable should be specified for addition: <br/>
      <_language_> <_terminal-or-nonterminal-name_> <_invalid-addition-form_>

    — At least one meta-variable should be specified for removal: <br/>
      <_language_> <_terminal-or-nonterminal-name_> <_invalid-removal-form_>

 3. Each **meta-var-name** must be a _meta-variable-name_.



Nonterminal productions
=======================

Standalone
----------

Referenced as: _production-definition_

`production` — the only acceptable form

##### _Syntax violations_: #######################

 1. **production** must not be a vector and must not contain them.

    — Invalid syntax of the production: vector patterns are not allowed: <br/>
      <_language_> <_nonterminal-name_> <_invalid-production_>

    — Invalid syntax of the production: vector patterns are not allowed: <br/>
      <_language_> <_nonterminal-name_> <_invalid-production_> <_nested-vector_>


Extension
---------

Referenced as: _production-modification_

`(+ productions ...)` — addition form

`(- productions ...)` — removal form

##### _Syntax violations_: #######################

 1. Clauses must have exactly one of these forms.

    — Invalid syntax of the production modification: <br/>
      <_language_> <_nonterminal-name_> <_invalid-production-modification_>

 2. **productions** must be a proper non-empty list.

    — At least one production should be specified for addition: <br/>
      <_language_> <_nonterminal-name_> <_invalid-addition-form_>

    — At least one production should be specified for removal: <br/>
      <_language_> <_nonterminal-name_> <_invalid-removal-form_>

 3. Each **production** must be a _production_.



Common terms
============

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
