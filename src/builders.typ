#import "/src/operators.typ" as ops

/// 
#let pat(..pats) = {
  ((pat: ops.seq(array: false, ..pats)),)
}

#let rw(fun) = {
  ((rw: fun),)
}

#let yy(..tests, validate: auto) = {
  ((yy: tests.pos(), validate: validate),)
}

#let nn(..tests, validate: auto) = {
  ((nn: tests.pos(), validate: validate),)
}

