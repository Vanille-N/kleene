#import "/src/lib.typ" as kleene

#let arith = {
  import kleene.prelude: *
  kleene.grammar(
    int: {
      pat(iter(`[0-9]`))
      rw(cs => int(cs.join("")))
      yy(`5`)
      yy(`042`)
      yy(`400`)
      nn(`54a`)
    },
    hexdigit: {
      pat(`[0-9A-Fa-f]`)
      yy(`8`, `F`, `a`)
    },
    hex: {
      pat("0x", $$, iter(<hexdigit>))
      rw(cs => cs.flatten().join(""))
      yy(`0x42`, `0xF`)
    },
    identchar: {
      pat(`[a-z]`)
      pat(`[A-Z]`)
      yy(`a`, `F`)
    },
    ident: {
      pat(iter(<identchar>))
      rw(cs => cs.join(""))
      yy(`x`, `foo`)
    },
    comma: {
      pat(maybe(iter(" ")), ",", maybe(iter(" ")))
      rw(s => none)
      yy(`,`, `  , `)
    },
    maybehex: {
      pat(maybe(<hex>))
      yy(``, `0x42`)
      nn(`0x`)
    },
    value: {
      pat(fork(<hex>, <int>, <ident>))
      yy(`5`, `x`)
      nn(`5a`, `x4`, `0xz`)
    },
    list: {
      pat("[", iter(<value>, <comma>), maybe(<int>), "]")
      rw(((_, vals,last, _),) => vals.map(((i,_),) => i) + last)
      yy(
        `[1, 2, 3]`,
        `[42 , 54 ,    ]`,
        `[0x456,]`,
      )
      nn(
        `42`,
        `[42, 54a]`,
        `[42]`,
      )
    },
  )
}

#kleene.test(arith)

#kleene.parse(arith, <value>, "0x45")
