#import "/src/lib.typ" as kleene

#let grammar = {
  import kleene.prelude: *
  kleene.grammar(
    v: {
      pat(`.*`)
      rw(x => (v: x))
      yy(`foo`)
    },
    w: {
      pat(`.*`)
      rw(x => (w: x))
      yy(`foo`)
    },
    z: {
      pat(`.*`)
      rw(x => (z: x))
      yy(`foo`)
    },
    sth: {
      pat(hint(1, (
        "v": <v>,
        "w": <w>,
        __: <z>,
      )))
      yy(`vbar`, `wbar`, `other`)
    },
  )
}

#grammar.sth

#kleene.test(grammar)

