/// Inserts a non-backtracking point.
/// Returns #typ.none.
///
/// See: @pat-commit.
/// -> pattern
#let commit() = (subparse, stack, input) => {
  (ok: true, backtrack: false, next: none, rest: input)
}

/// Matches the end of the stream.
/// Returns #typ.none.
///
/// See: @pat-eof.
/// -> pattern
#let eof() = (subparse, stack, input) => {
  if input == "" {
    (ok: true, backtrack: true, next: none, rest: input)
  } else {
    (ok: false, backtrack: true, msg: [Expected end of stream], next: none, stack: stack, rest: input)
  }
}

/// Recursively invokes another rule.
///
/// See: @pat-label.
/// -> pattern
#let label(
  /// Label of a rule defined elsewhere in the grammar.
  /// -> str
  lab,
) = (subparse, stack, input) => {
  subparse(std.label(lab), input)
}

/// Uses a regular expression to more efficiently match a string.
/// Returns #typ.t.string.
///
/// See: @pat-regex.
#let regex(
  /// String representing a regular expression using the syntax of the
  /// #link("https://typst.app/docs/reference/foundations/regex/")[standard library]
  /// -> str
  re,
) = (subparse, stack, input) => {
  let re = std.regex(re)
  if input.starts-with(re) {
    let match = input.find(re)
    let len = match.len()
    let rest = input.slice(len)
    let next = (
      ok: false, backtrack: true, msg: [No longer part of the regex match], stack: stack, rest: rest)
    (ok: true, backtrack: true, val: match, next: next, rest: rest)
  } else {
    (ok: false, backtrack: true, msg: [Regex does not match], next: none, stack: stack, rest: input)
  }
}

/// Helper to build sequences.
/// -> pattern
#let seq-aux(
  /// Patterns that need to be matched in order.
  /// -> array(pattern)
  pats,
  /// If true, forces the result to be an array even if it is of size 1.
  /// -> bool
  array: false,
) = (subparse, stack, input) => {
  let matches = ()
  let backtrack = true
  let input = input
  let next = none
  for pat in pats {
    let ans = subparse(pat, input)
    if not ans.backtrack {
      backtrack = false
    }
    if not ans.ok {
      return (: ..ans, backtrack: backtrack)
    }
    if ans.next != none {
      next = ans.next
    }
    if "val" in ans {
      matches.push(ans.val)
    }
    input = ans.rest
  }
  if (not array) and matches.len() == 1 {
    matches = matches.at(0)
  }
  (ok: true, backtrack: backtrack, val: matches, next: next, rest: input)
}

/// Matches a sequence of patterns in order.
/// Returns an #typ.t.array, except for possible optimizations where an implicit
/// invocation matches only one element.
///
/// See: @pat-seq.
/// -> pattern
#let seq(
  /// Sub-patterns.
  /// -> pattern
  ..pats,
  /// If true, forces the result to be an array even if it is of size 1.
  /// -> bool
  array: true,
) = {
  let pats = pats.pos()
  if (not array) and pats.len() == 1 {
    pats.at(0)
  } else {
    seq-aux(array: array, pats)
  }
}

/// Matches a string literal. Returns a #typ.t.str.
///
/// See: @pat-str.
/// -> pattern
#let str(
  /// Any string to match as-is.
  /// -> str
  string,
) = (subparse, stack, input) => {
  if input.starts-with(string) {
    (ok: true, backtrack: true, val: string, next: none, rest: input.slice(string.len()))
  } else {
    (ok: false, backtrack: true, msg: [Can't match string "#std.raw(string)"], next: none, stack: stack, rest: input)
  }
}

/// Matches 1 or more instances of the inner pattern.
/// Returns an #typ.t.array.
///
/// See: @pat-iter.
/// -> pattern
#let iter(
  /// List of patterns, implicitly cast to a @cmd:prelude:seq.
  /// -> pattern
  ..pats,
) = (subparse, stack, input) => {
  let matches = ()
  let count = 0
  let backtrack = true
  let input = input
  while true {
    let ans = subparse(seq(array: false, ..pats), input)
    if not ans.backtrack { backtrack = false }
    if not ans.ok {
      if count == 0 {
        return (: ..ans, backtrack: backtrack)
      } else {
        return (ok: true, backtrack: backtrack, val: matches, next: ans, rest: input)
      }
    }
    count += 1
    if "val" in ans {
      matches.push(ans.val)
    }
    if ans.ok and ans.rest == input {
      if matches.len() == 0 {
        return (: ..ans, backtrack: backtrack)
      } else {
        return (ok: true, backtrack: backtrack, val: matches, next: ans, rest: input)
      }
    }
    input = ans.rest
  }
}

/// Matches 0 or more instances of the inner pattern.
/// Returns an #typ.t.array.
///
/// See: @pat-star.
/// -> pattern
#let star(
  /// List of patterns, implicitly cast to a @cmd:prelude:seq.
  /// -> pattern
  ..pats,
) = (subparse, stack, input) => {
  let matches = ()
  let count = 0
  let backtrack = true
  let input = input
  while true {
    let ans = subparse(seq(array: false, ..pats), input)
    if not ans.backtrack { backtrack = false }
    if not ans.ok {
      return (ok: true, backtrack: backtrack, val: matches, next: ans, rest: input)
    }
    count += 1
    if "val" in ans {
      matches.push(ans.val)
    }
    if ans.ok and ans.rest == input {
      return (ok: true, backtrack: backtrack, val: matches, next: ans, rest: input)
    }
    input = ans.rest
  }
}

/// Branching choice between multiple possible subpatterns.
/// Returns the first match.
///
/// See: @pat-fork.
/// -> pattern
#let fork(
  /// List of ordered patterns between which a choice is made.
  /// -> pattern
  ..pats,
) = (subparse, stack, input) => {
  let latest-msg = (ok: false, backtrack: true, stack: stack, msg: [Empty rule], next: none, rest: input)
  let backtrack = true
  for pat in pats.pos() {
    let ans = subparse(pat, input)
    if not ans.backtrack { backtrack = false }
    if ans.ok {
      return (: ..ans, backtrack: backtrack)
    } else if not backtrack {
      return ans
    } else {
      latest-msg = ans
    }
  }
  latest-msg
}

/// Matches 0 or 1 instances of the inner pattern.
/// Returns an #typ.t.array, either empty or singleton.
///
/// See: @pat-maybe.
/// -> pattern
#let maybe(
  /// Implicitly cast to a @cmd:prelude:seq.
  /// -> pattern
  ..pat,
) = (subparse, stack, input) => {
  let ans = subparse(seq(array: false, ..pat), input)
  if ans.ok {
    (: ..ans, val: (ans.at("val", default: none),))
  } else if not ans.backtrack {
    ans
  } else {
    (ok: true, backtrack: true, val: (), next: ans, rest: input)
  }
}

/// Cancels out a @cmd:prelude:commit to re-enable backtracking.
/// Returns the inner match.
///
/// See: @pat-try
#let try(
  /// Implicitly cast to a @cmd:prelude:seq.
  /// -> pattern
  ..pats,
) = (subparse, stack, input) => {
  let ans = subparse(seq(array: false, ..pats), input)
  (: ..ans, backtrack: true)
}

/// Matches an arbitrary pattern, but returns #typ.none.
///
/// See: @pat-drop.
/// -> pattern
#let drop(
  /// Implicitly cast to a @cmd:prelude:seq.
  /// -> pattern
  ..pats,
) = (subparse, stack, input) => {
  let ans = subparse(seq(array: false, ..pats), input)
  if "val" in ans {
    let _ = ans.remove("val")
  }
  ans
}

/// Currified so that common rewriting patterns can be conveniently
/// written as standalone functions.
/// The type of the return value is that of the function.
/// If multiple patterns are given, they are implicitly
/// cast to a @cmd:prelude:seq.
///
/// See: @pat-rewrite.
/// -> function(..pattern) => pattern
#let rewrite(
  /// Pattern to rewrite with.
  /// - #typ.none: the value is dropped,
  /// - #typ.auto: identity transformation,
  /// - #typ.t.function: applies the given function.
  /// -> none | auto | function
  fun,
) = (..pats) => (subparse, stack, input) => {
  let ans = subparse(seq(array: false, ..pats), input)
  if "val" in ans {
    if fun == none {
      let _ = ans.remove("val")
    } else if fun == auto {
      // noop transform
    } else if type(fun) == function {
      ans.val = fun(ans.val)
    } else {
      panic(`transformation must be a function, none, or auto`)
    }
  }
  ans
}

#let peek(
  ..pats,
) = (subparse, stack, input) => {
  let ans = subparse(seq(array: false, ..pats), input)
  if ans.ok {
    (ok: true, backtrack: ans.backtrack, next: [Successful lookahead of length #{input.len() - ans.rest.len()}], stack: stack, rest: input)
  } else {
    ans
  }
}

#let neg(
  ..pats,
) = (subparse, stack, input) => {
  let ans = subparse(seq(array: false, ..pats), input)
  if ans.ok {
    (ok: false, backtrack: ans.backtrack, msg: [Lookahead of length #{input.len() - ans.rest.len()} should have failed], stack: stack, rest: input)
  } else {
    (ok: true, backtrack: ans.backtrack, next: [Lookahead of length #{input.len() - ans.rest.len()} failed as expected], stack: stack, rest: input)
  }
}
