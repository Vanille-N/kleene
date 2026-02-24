#import "/src/lib.typ" as kleene

#let grammar = {
  import kleene.prelude: *
  kleene.grammar(
    re1: pat(`[a-z]*`),
    re2: pat(`[a-z]?`),
    re3: pat(`[a-z]|`),
    re4: pat(`(a|b|)(|c|a)`),
    nre1: pat(`[a-z]+`),
    nre2: pat(`(a|b|)(c|a)`),
    star: pat(star("a")),
    fork: pat(fork("a", "")),
    seq: pat("", "", ""),
    lab1: pat(<re1>),
    lab2: pat(<nre1>),
    lab3: pat(<re4>, <star>),
    lab4: pat(<lab3>),
  )
}

#kleene.test(grammar)
