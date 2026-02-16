#import "operators.typ" as ops

#let auto-cast(pat) = {
  if type(pat) == function {
    pat
  } else if type(pat) == label {
    ops.label(pat)
  } else if type(pat) == str {
    ops.raw(pat)
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
    let select = rule.pat
    let transform = rule.tr
    let ans = auto-cast(select)(subparse(rules, stack), stack, input)
    if ans.ok {
      if "val" in ans {
        if transform == none {
          let _ = ans.remove("val")
        } else if transform == auto {
          // noop transform
        } else if type(transform) == function {
          ans.val = transform(ans.val)
        } else {
          panic("`tr` must be a function or none")
        }
      }
      ans
    } else {
      ans
    }
  } else {
    auto-cast(id)(subparse(rules, stack), stack, input)
  }
}

#let parse(rules, pat, input) = {
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

