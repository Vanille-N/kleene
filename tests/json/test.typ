#import "/src/lib.typ" as kleene

#let separated(sep) = (value) => {
  import kleene.prelude: *
  let fn((v,vs),) = (v,) + vs
  rewrite(fn)(value, star(sep, value))
}

#let grammar = {
  import kleene.prelude: *
  kleene.grammar(
    ws: {
      pat(`[ \n\t]*`)
      rw(none)
    },
    special: {
      pat("\\\\")
      rw(_ => "\\")
      pat("\\n")
      rw(_ => "\n")
      pat("\\t")
      rw(_ => "\t")
      pat("\\\"")
      rw(_ => "\"")
      pat(drop("\\u"), `[0-9a-fA-F]{4}`)
      rw(d => eval("\"\u{" + d + "}\""))
    },
    string: {
      pat(drop("\""), $$, star(fork(`[^\\"]+`, <special>)), drop("\""))
      rw(s => "" + s.join())
      yy(`""`, validate: (_,v) => if v != "" { repr(v) })
      yy(`"foo"`)
      yy(`"a\\b"`, `"a\"b"`, `"a\n"`, `"a\tb"`, `"a\uf1ecb"`)
      nn(`"a\"`)
    },
    int: {
      pat(`0[0-9]`)
      err[Numbers may not have a leading 0]
      pat(maybe("-"), `0|([1-9][0-9]*)`)
      rw(n => int(n.flatten().join()))
    },
    floatpt: {
      pat(drop("."), $$, `[0-9]+`)
      rw(d => float("0." + d))
    },
    exp: {
      pat(drop(`[eE]`), $$, maybe(`[+-]`), `[0-9]+`)
      rw(ex => calc.pow(10, int(ex.flatten().join())))
    },
    number: {
      pat(<int>, $$, maybe(<floatpt>), maybe(<exp>))
      rw(((i,f,e),) => {
        (i + f.at(0, default: 0)) * e.at(0, default: 1)
      })
      yy(`42`, `-30`, `0`, `1.01`, `0.000`, `1e07`, `-1.00E-8`)
      nn(`02`, `.42`, `-E2`, `-1e`, `.4`)
    },
    comma: pat(drop(",")),
    element: {
      pat(<ws>, <value>, <ws>)
    },
    elements: {
      pat(separated(<comma>)(<element>))
      rw(auto)
      pat()
      rw(_ => ())
    },
    array: {
      pat(drop("["), $$, <ws>, <elements>, drop("]"))
      rw(auto)
      yy(`[]`, `[ 1 ]`, `["a"]`, `[ 1 , 2 , 3 ]`, `["a", "b", "c"]`)
    },
    member: {
      pat(<ws>, <string>, <ws>, drop(":"), $$, <element>)
      yy(`"name": "lipu-tenpo-website"`)
      yy(`"version": "0.1.0"`)
      yy(`"lockfileVersion": 3`)
    },
    members: {
      pat(separated(<comma>)(<member>))
      rw(vs => vs.to-dict())
      pat()
      rw(_ => (:))
    },
    object: {
      pat(drop("{"), $$, <ws>, <members>, drop("}"))
      yy(`{    }`)
      yy(```
      { "foo": 1,
          "bar" : "baz",
       "quux": 0.1 }
      ```)
      yy(```
      {
        "foo": "foo",
        "bar": "bar",
        "baz": 3,
        "quux": true
      }
      ```)
      yy(```
      {
        "name": "lipu-tenpo-website",
        "version": "0.1.0",
        "lockfileVersion": 3,
        "requires": true
      }
      ```)
    },
    const: {
      pat("true")
      rw(_ => true)
      pat("false")
      rw(_ => false)
      pat("null")
      rw(_ => none)
    },
    value: {
      pat(<ws>, hint(1, (
        "{": <object>,
        "\"": <string>,
        "[": <array>,
        __: fork(<number>, <const>),
      )))
    }
  )
}

#kleene.test(grammar)

#let parse-json = kleene.parse.with(grammar, <element>)

#{
  for name in ("support-config", "support-package", "test-package", "package-lock") {
    let file = "examples/" + name + ".json"
    let theirs = std.json(file)
    let (ok, mine) = parse-json(read(file))

    assert(theirs == mine)
  }
}

