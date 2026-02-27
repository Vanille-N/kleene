#import "/src/lib.typ" as kleene
#import kleene.prelude: *

= Comments

#let base = kleene.grammar(
  ..common.c-line-comment(),
  ..common.c-block-comment(),
  ..common.py-line-comment(),
  ..common.ml-block-comment(),
)

#let tests = kleene.grammar(
  c-line-comment: {
    yy(`//`, `// foo`)
    yy(```
    // comment

    ```)
  },
  c-block-comment: {
    yy(`/**/`, `/*       */`, `/* comment */`)
    yy(`/* * */`, `/* / */`)
    yy(```
    /* 
      * a block
      * comment
      */
    ```)
    yy(```
    /*
      * Not necessarily closed
    ```)
    yy(```
    /* They may be nested:
      /* like this */
        */
    ```)
  },
  py-line-comment: {
    yy(`#    comment `)
  },
  ml-block-comment: {
    yy(```
    (*   any (*
      including nested *)
      stuff *)
    ```)
  },
)

#kleene.test(kleene.extend(base, tests))

= Combinators

