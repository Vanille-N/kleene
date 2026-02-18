#import "/src/lib.typ" as kleene

#let grammar = {
  import kleene.prelude: *
  kleene.grammar(
    ident: {
      pat(`[a-zA-Z][a-zA-Z0-9_]*`)
      yy(`foo`)
      nn(`0`)
    },
    whitespace: {
      pat(iter(fork(`[ \n\t]`)))
      rw(none)
    },
    blank: {
      pat(maybe(<whitespace>))
      rw(none)
    },
    ..{
      let optional-spaces(symbol) = {
        pat(<blank>, symbol, <blank>)
        rw(none)
      }
      (
        lambda: optional-spaces("\\"),
        dot: optional-spaces("."),
        comma: optional-spaces(","),
      )
    },
    ident-list: {
      pat(<ident>, star(<comma>, <ident>))
      rw(ids => ids.flatten())
      yy(`x`)
    },
    atom: {
      pat(<ident>)
      rw(id => (id: id))
      pat(drop("("), $$, <blank>, <expr>, <blank>, drop(")"))
      yy(`x`, `(x)`)
      //yy(`((((x))))`)
      nn(`(x`)
      nn(`x y`)
    },
    app-expr: {
      pat(try(<atom>), iter(<whitespace>, <atom>))
      rw(atoms => (app: atoms.flatten()))
      pat(<atom>)
      yy(`x`, `x y z`, `x (y z)`)
    },
    lambda-expr: {
      pat(<lambda>, $$, <ident-list>, <dot>, <lambda-expr>)
      rw(((ids, body),) => (args: ids, body: body))
      pat(<app-expr>)
      yy(`\x.y`, `\x.\y.z`, `\x,y.(x ((\z.z) y))`)
    },
    expr: {
      pat(<lambda-expr>)
    }
  )
}

#kleene.test(grammar)

#let parse-lambda = kleene.parse.with(grammar, <expr>)

#let (ok, ans) = parse-lambda("\x. \y. y x")
#assert(ok)
#ans

#let (ok, ans) = parse-lambda("\x. \y y x")
#assert(not ok)
#ans

