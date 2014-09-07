Neptunium – A Nanopass Framework
================================

This is an R7RS implementation of the [Nanopass framework](//github.com/akeep/nanopass-framework)
designed by Andrew W. Keep, Dipanwita Sarkar, R. Kent Dybvig, and Oscar Waddell.

**Nanopass** is an embedded domain-specific language for writing compilers. It allows to describe
languages in a declarative way with S-expression syntax as well as to describe passes that locate
patterns in programs and transform them into another programs, probably written in a different
language. Input and output programs are formally verified to be consistent with provided language
syntax. Being an _embedded_ DSL, Nanopass does not need any intermediate code generators and the
passes are able to seamlessly interoperate with external Scheme code.

Np aims to be a highly portable implementation which relies solely on R7RS and SRFIs. Restrictions
of the target language demand certain changes in syntax and implementation, hence a new name.
Np is not a port of the original Nanopass framework, rather an adaptation to the R7RS world;
they are not entirely compatible and are not meant to be.

Please see [the project wiki](//github.com/ilammy/np/wiki) for information on how to build,
use, and help to develop Np.

Np is distributed under **[3-clause BSD license](LICENSE)**.
