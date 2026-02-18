#import "operators.typ" as ops

// Automatically reinterprets builtin types/values to operators.
#let auto-cast(
  // Pattern to interpret.
  // - #typ.t.function: native
  // - #typ.t.string: cast through 
  //
  // -> any
  pat
) = {
  if type(pat) == function {
    pat
  } else if type(pat) == label {
    ops.label(str(pat))
  } else if type(pat) == str {
    ops.str(pat)
  } else if type(pat) == array {
    ops.seq(..pat)
  } else if pat == $$ {
    ops.commit()
  } else if type(pat) == content and pat.func() == std.raw {
    ops.regex(pat.text)
  } else {
    panic("Cannot interpret an object of type " + str(type(pat)) + " as a parser")
  }
}

#let subparse(rules, stack) = (id, input) => {
  let stack = stack
  if type(id) == label {
    stack.push(id)
    let rule = rules.at(str(id))
    let latest-msg = (ok: false, backtrack: true, stack: stack, msg: [Empty rule], next: none, rest: input)
    let backtrack = true
    for group in rule.pat {
      let select = group.pat
      let transform = group.rw
      while type(select) != function {
        select = auto-cast(select)
      }
      let ans = select(subparse(rules, stack), stack, input)
      if not ans.backtrack { backtrack = false }
      if ans.ok {
        if "val" in ans {
          if transform == none {
            let _ = ans.remove("val")
          } else if transform == auto {
            // noop transform
          } else if type(transform) == function {
            ans.val = transform(ans.val)
          } else {
            panic("`rw` must be a function or none")
          }
        }
        return (: ..ans, backtrack: backtrack)
      } else if not backtrack {
        return ans
      } else {
        latest-msg = ans
      }
    }
    latest-msg
  } else {
    auto-cast(id)(subparse(rules, stack), stack, input)
  }
}

/// Initiate parsing.
/// Returns a boolean and a result.
/// The boolean indicates if the parsing is successful.
/// It also determines the type of the result:
/// - whatever type is returned by the last rewriting function in the case of a success,
/// - content that can be directly displayed for an error message in the case of a failure.
///
/// -> (bool, result)
#let parse(
  /// Typically constructed by @cmd:kleene:grammar.
  /// -> grammar
  rules,
  /// Indicates the entry point for the parsing.
  /// -> label
  pat,
  /// Input data to parse.
  /// -> str
  input,
) = {
  let ans = subparse(rules, ())(pat, input)
  if not ans.ok {
    import "ui.typ"
    (false, ui.error(input, pat, ans))
  } else if ans.rest != "" {
    import "ui.typ"
    (false, ui.incomplete(input, pat, ans))
     } else {
    (true, ans.at("val", default: none))
  }
}

