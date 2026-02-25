#import "/src/lib.typ" as kleene

#let grammar = {
  import kleene.prelude: *
  kleene.grammar(
    hexdigit: {
      pat(`[0-9A-F]`)
      rw(auto)

      pat(`[G-Z]`)
      err[Only letters in A-F are valid hexadecimal digits]
      pat(`[a-z]`)
      err[Use uppercase only in hexadecimal numbers]
      pat(`.`)
      err[Not a hexadecimal digit]
      pat(eof())
      err[Unexpected EOF]

      yy(`0`, `9`, `E`)
      nn(`Q`, `a`, `!`, ``)
    },
    hex: {
      pat(drop("0x"), iter(<hexdigit>))
      rw(ds => ds.flatten().join())

      yy(`0x42AF`)
      nn(`0x`, `0x42a`, `0x,`)
    }
  )
}

#kleene.test(grammar)
