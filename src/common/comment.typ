#import "/src/operators.typ": *
#import "/src/builders.typ": *

#let c-line-comment(
  name: <c-line-comment>,
  start: "//",
) = (
  std.str(name): {
    pat(drop(start, $$, `[^\n]*`, fork("\n", eof())))
  },
)

#let py-line-comment(
  name: <py-line-comment>,
) = {
  c-line-comment(start: "#", name: name)
}

#let c-block-comment(
  name: <c-block-comment>,
  open: "/*", close: "*/",
  precursors: ("*", "/"),
) = (
  std.str(name): {
    pat(drop(
      open, $$,
      star(fork(
        regex("[^" + precursors.join() + "]+"),
        name,
        (neg(close), fork(..precursors)),
      )),
      fork(close, eof()),
    ))
  },
)

#let ml-block-comment() = {
  c-block-comment(
    name: <ml-block-comment>,
    open: "(*", close: "*)",
    precursors: ("*", "("),
  )
}

/*
#let any-block(
  open: none,
  close: none,
  precursor: auto,
) = {
  assert(type(open) == std.str)
  assert(type(cose) == std.str)
  if precursor == auto {
    precursor = close.at(0)
  }
}

#let c-line-comment(start: "//") = (
  c-line-comment: {
    
  }
)

#let c-block-comment() = {
  eof()
}

#let py-line-comment() = {
  any-line(start: "#")
}

#let ml-block-comment() = {
  eof()
}
*/
/*

