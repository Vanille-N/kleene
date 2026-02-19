#import "/src/operators.typ" as ops

#let combine(id, descr) = {
  let rule = (pat: (), yy: (), nn: ())
  let dangling-pats = ()
  if type(descr) != array {
    panic("Rule " + id + " is not valid")
  }
  for elt in descr {
    if type(elt) != dictionary {
      panic("Rule " + id + " is not valid")
    }
    if "pat" in elt {
      dangling-pats.push(elt.pat)
    } else if "rw" in elt {
      if dangling-pats == () {
        panic("This instance of 'rw' does not apply to any patterns")
      }
      rule.pat.push(ops.rewrite(elt.rw)(ops.fork(..dangling-pats)))
      dangling-pats = ()
    } else if "yy" in elt {
      rule.yy.push(elt)
    } else if "nn" in elt {
      rule.nn.push(elt)
    } else {
      panic(elt) // TODO: improve error
    }
  }
  if dangling-pats != () {
    rule.pat.push(ops.fork(..dangling-pats))
  }
  if rule.pat != () {
    rule.pat = ops.fork(..rule.pat)
  }
  (""+id: rule)
}

/// Constructs a new grammar from its rules.
/// -> grammar
#let grammar(
  /// List of named rules, as constructed by @cmd:prelude:pat and its related functions.
  /// -> rule
  ..rules
) = {
  let rules = rules.named()
  if rules.len() == 0 {
    return (:)
  }
  for (id, descr) in rules {
    combine(id, descr)
  }
}

/// Edits a grammar by adding new rules and new cases to existing rules.
///
/// See: @sec-extend.
/// -> grammar
#let extend(
  /// Base grammar, takes precedence on rules that are defined by both.
  /// -> grammar
  g1,
  /// New rules and cases.
  /// -> grammar
  g2,
) = {
  let g = g1
  for (k, v2) in g2 {
    if k in g {
      let v = g.remove(k)
      v.pat += v2.pat
      v.yy += v2.yy
      v.nn += v2.nn
      g.insert(k, v)
    } else {
      g.insert(k, v2)
    }
  }
  g
}

/// Edits a grammar by substituting old rules for new ones.
///
/// See: @sec-patch.
/// -> grammar
#let patch(
  /// Base grammar.
  /// -> grammar
  g1,
  /// Patch, replaces rules already defined by `g1`.
  /// -> grammar
  g2,
) = {
  let g = g1
  for (k, v2) in g2 {
    if k in g {
      let v = g.remove(k)
      if v2.pat != () {
        v = v2
      } else {
        v.yy = v2.yy
        v.nn = v2.nn
      }
      g.insert(k, v)
    } else {
      g.insert(k, v2)
    }
  }
  g
}

/// Removes unit tests from a grammar.
/// Particularly useful when patching a grammar to remove those that would
/// no longer pass.
/// -> grammar
#let strip(
  /// Base grammar.
  /// -> grammar
  g1,
  /// Whether to remove positive tests.
  /// -> bool
  yy: true,
  /// Whether to remove negative tests.
  /// -> bool
  nn: true,
) = {
  let g = (:)
  for (k, v) in g1 {
    if yy {
      v.yy = ()
    }
    if nn {
      v.nn = ()
    }
    g.insert(k, v)
  }
  g
}

