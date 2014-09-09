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

Functions there are generally called **$can-be:_something_?** or
**$must-be:_something_**.

_Can-be_-functions are _necessity_ predicates. I.e., getting `#t` as a result
is necessary for the thing in question to be _something_, and getting `#f`
means that the thing is definitely not _something_.

_Must-be_-functions are _sufficiency_ assertions. I.e., passing such assertion
is sufficient for the thing in question to be considered _something_ (however,
only _structurally_). These functions return their original argument intact
if the assertion holds, and fail with syntax error if it is not true.
