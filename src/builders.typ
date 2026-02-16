#import "/src/operators.typ" as ops

#let pat(..pats) = {
  ((pat: ops.seq(..pats)),)
}

#let tr(fun) = {
  ((tr: fun),)
}

#let yy(..tests) = {
  ((yy: tests.pos()),)
}

#let nn(..tests) = {
  ((nn: tests.pos()),)
}

#let combine(id, descr) = {
  let pats = ()
  let tr = auto
  let yy = ()
  let nn = ()
  if type(descr) != array {
    panic("Rule " + id + " is not valid")
  }
  for elt in descr {
    if type(elt) != dictionary {
      panic("Rule " + id + " is not valid")
    }
    if "pat" in elt {
      pats.push(elt.pat)
    } else if "tr" in elt {
      if tr != auto {
        panic("Multiple definitions of the transformer 'tr' in rule " + id)
      }
      tr = elt.tr
    } else if "yy" in elt {
      yy += elt.yy
    } else if "nn" in elt {
      nn += elt.nn
    } else {
      panic(elt) // TODO: improve error
    }
  }
  let pat = if pats.len() == 0 {
    panic("Must specify at least one match pattern 'pat' in rule " + id)
  } else if pats.len() == 1 {
    pats.at(0)
  } else {
    ops.fork(..pats)
  }
  (""+id: (pat: pat, tr: tr, yy: yy, nn: nn))
}

#let grammar(..rules) = {
  let rules = rules.named()
  if rules.len() == 0 {
    return (:)
  }
  for (id, descr) in rules {
    combine(id, descr)
  }
}
