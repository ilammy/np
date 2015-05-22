_**Normalization**_ is responsible for converting partitioned clauses into
their normal, canonical form. This includes filling in default values, removing
unnecessary elements, some element reordering, etc. Normalization operates on
valid input and do not check for errors; this should have been already done
during the partitioning step.

Here the rules of normalization are described in human-readable form.


Terminal definitions
====================

Standalone
----------

`(name predicate (meta-vars ...))` — full form (normalized form)

`(predicate-name (meta-vars ...))` — short form

##### _Normalization_: #######################

 1. Short form is converted into normalized form by assuming the
    `predicate-name` to be both the name of the terminal and a reference
    to its predicate.


Extension
---------

### Addition

`(name predicate (meta-vars ...))` — full form (normalized form)

`(predicate-name (meta-vars ...))` — short form

##### _Normalization_: #######################

 1. Short form is converted into normalized form by assuming the
    `predicate-name` to be both the name of the terminal and a reference
    to its predicate.


### Removal

`name` — identifier form (normalized form)

`(name predicate (meta-vars ...))` — full form

`(predicate-name (meta-vars ...))` — short form

##### _Normalization_: #######################

 1. Full form is converted into normalized form by leaving only the `name`.

 2. Short form is converted into normalized form by leaving only the
    `predicate-name` which is assumed to be the name of the terminal.


### Modification

`(name ((meta-var-additions ...) (meta-var-removals ...)))` — regular form

`(name (meta-var-additions ...) (meta-var-removals ...))` — normalized form

##### _Normalization_: #######################

 1. Regular form is converted into normalized form by splicing the list of
    meta-variable modifications into the form body.



Nonterminal definitions
=======================

Standalone
----------

`(name                 (meta-vars ...) productions ...)` — regular form

`(name predicate-name  (meta-vars ...) productions ...)` — form with predicate binding

`(name predicate-name? (meta-vars ...) (productions ...))` — normalized form

##### _Normalization_: #######################

 1. Both forms have the list of productions unspliced into the form body.

 2. For regular form, the default value of `predicate-name?` is `#f`.

 3. For binding form, `predicate-name?` is transferred as-is.


Extension
---------

### Addition

`(name                 (meta-vars ...) productions ...)` — regular form

`(name predicate-name  (meta-vars ...) productions ...)` — form with predicate binding

`(name predicate-name? (meta-vars ...) (productions ...))` — normalized form

##### _Normalization_: #######################

 1. Both forms have the list of productions unspliced into the form body.

 2. For regular form, the default value of `predicate-name?` is `#f`.

 3. For binding form, `predicate-name?` is transferred as-is.


### Removal

`name` — identifier form (normalized form)

`(name                (meta-vars ...) productions ...)` — regular form

`(name predicate-name (meta-vars ...) productions ...)` — form with predicate binding

##### _Normalization_: #######################

 1. Both forms have only the `name` left.


### Modification

`(name                 ((meta-var-additions ...) (meta-var-removals ...)) ((production-additions ...) (production-removals ...)))` — regular form

`(name predicate-name  ((meta-var-additions ...) (meta-var-removals ...)) ((production-additions ...) (production-removals ...)))` — for with predicate (re)binding

`(name predicate-name? (meta-var-additions ...) (meta-var-removals ...) (production-additions ...) (production-removals ...))` — normalized form

##### _Normalization_: #######################

 1. Input form is converted into normalized form by splicing the lists of
    meta-variable and production modifications into the form body.

 2. For regular form, the default value of `predicate-name?` is `#f`.

 3. For binding form, `predicate-name?` is transferred as-is.


Other clauses
=============

Other clauses are not affected by normalization. Toplevel language property
clauses are already normalized, and other ones are only simple lists that do
not require any additional processing.
