#import "match.typ"

/// #property-priv()
/// Transforms a string to show invisible characters (spaces, linebreaks, tabs).
/// Returns the resulting text as a #typ.raw block.
/// -> content
#let show-invisible(
  /// Input text to transform.
  /// -> str
  line,
  /// By default, linebreaks (```typc "\n"```) are replaced by a substitute character.
  /// If this option is enabled, the formatted text will still have
  /// the original linebreaks in addition to the marker.
  /// -> bool
  preserve-linebreaks: false,
  /// Which color the special characters should be shown as.
  /// -> color
  dim-color: black.lighten(70%),
  /// Determines the substitution dictionary to use.
  /// - ```typc "special"```: `\n` and `\t`.
  /// - ```typc "unicode"```: `¤`, `»`, `␣`, `∅` denote respectively
  ///   linebreak, tab, space, eof.
  /// -> str
  mode: "special",
) = {
  let dicts = (
    special: (
      "\n": "\\n" + if preserve-linebreaks { "\n" } else { "" },
      "\t": "\\t",
      empty: "",
    ),
    unicode: (
      "\n": "¤" + if preserve-linebreaks { "\n" } else { "" },
      "\t": "»",
      " ": "␣",
      empty: "∅",
    ),
  )
  let dict = dicts.at(mode)
  if line.len() == 0 {
    set text(fill: gray)
    raw(dict.empty)
  }
  line.clusters().map(c => {
    if c in dict {
      set text(fill: dim-color)
      raw(dict.at(c))
    } else {
      raw(c)
    }
  }).join()
}

/// Runs the unit tests attached to a grammar
/// See @cmd:prelude:yy and @cmd:prelude:nn for details.
/// Runs all positive and negative tests, and formats them in a table.
/// -> content
#let check-unit-tests(
  /// The grammar to test, as constructed by @cmd:kleene:grammar.
  /// -> grammar
  grammar,
  /// Left recursion analysis
  /// -> dictionary
  lrec: none,
  /// Pass an array or a function to filter a subset of the tests.
  /// -> auto | array | function
  select: auto,
  /// Whether to display the final tally of passed/failed tests.
  /// -> bool
  total: true,
) = {
  import "parse.typ": parse
  let status(ok, expect: true, validated: auto, skip: false) = {
    let color = if skip == true {
      orange
    } else if validated == auto {
      if ok == expect {
        green
      } else {
        red
      }
    } else if validated == none {
      if ok == expect {
        green
      } else {
        yellow
      }
    } else {
      red
    }
    rect(width: 5mm, height: 5mm, fill: color)
  }
  let evaluate(ruleid, tt, expect: true, validate: auto) = {
    let input-box = box(fill: gray.lighten(90%), inset: 3pt)[#text(fill: blue)[#show-invisible(tt.text, mode: "unicode", preserve-linebreaks: true)]]

    if lrec != none and ruleid in lrec.dangerous {
      let incr = ("skip",)
      let arr = (
        table.cell[#input-box],
        status(false, skip: true),
        [_Skipped due to null cycle_],
      )
      return (incr, arr)
    }

    let incr = ()
    let (ok, ans) = parse(grammar, label(ruleid), tt.text)
    if ok == expect {
      incr.push("ok")
    } else {
      incr.push("err")
    }
     let validated = if validate != auto {
      incr.push("validation-required")
      if ok == expect {
        incr.push("validated")
        validate(tt.text, ans)
      }
    }

    let rowspan = 1
    let line1 = (
      status(ok, expect: expect),
      [#ans],
    )
    let line2 = if validated != none {
      incr.push("invalid")
      rowspan = 2
      (
        status(true, validated: validated),
        [Validation failed: #validated],
      )
    } else {
      ()
    }
    let arr = (
      table.cell(rowspan: rowspan)[#input-box],
      ..line1,
      ..line2,
    )
    (incr, arr)
  }
  let outcomes = (ok: 0, err: 0, validation-required: 0, validated: 0, invalid: 0, skip: 0)
  for (ruleid, rule) in grammar {
    if select != auto {
      if type(select) == array and ruleid not in select { continue }
      if type(select) == function and not select(ruleid) { continue }
    }
    if type(rule) != dictionary {
      panic(rule)
    }
    if rule.yy != () {
      table(columns: (2fr, auto, 5fr),
        table.header(
          [*#ruleid*], [],
          text(fill: green)[*examples*],
        ),
        ..for (yy, validate) in rule.yy {
          for tt in yy {
            let (incr, arr) = evaluate(ruleid, tt, expect: true, validate: validate)
            for i in incr { outcomes.at(i) += 1 }
            arr
          }
        }
      )
    }
    if rule.nn != () {
      table(columns: (2fr, auto, 5fr),
        table.header(
          [*#ruleid*], [],
          text(fill: red)[*counterexamples*],
        ),
        ..for (nn, validate) in rule.nn {
          for tt in nn {
            let (incr, arr) = evaluate(ruleid, tt, expect: false, validate: validate)
            for i in incr { outcomes.at(i) += 1 }
            arr
          }
        }
      )
    }
  }
  if total {
    box(table(columns: 4,
      [parsing], [#status(true)], [#status(false)], [#status(true, skip: true)],
      [#{outcomes.ok + outcomes.err + outcomes.skip}], [#{outcomes.ok}], [#{outcomes.err}], [#{outcomes.skip}],
    ))
    h(1cm)
    if outcomes.validated > 0 {
      box(table(columns: 4,
        [validation], [#status(true)], [#status(false, validated: none)], [#status(false)],
        [#{outcomes.validation-required}], [#{outcomes.validated - outcomes.invalid}], [#{outcomes.validation-required - outcomes.validated}], [#{outcomes.invalid}]
      ))
    }

  }
}

#let inverse-reachable-set(grammar, start) = {
  let one-step(pat) = {
    if "lab" in pat {
      (pat.lab,)
    } else if "pats" in pat {
      for pat in pat.pats {
        one-step(pat)
      }
    } else if "pat" in pat {
      one-step(pat.pat)
    } else {
      ()
    }
  }
  let step = (:)
  for (ruleid, rule) in grammar {
    step.insert(ruleid, one-step(rule.pat))
  }
  let reach = start
  while true {
    let new = ()
    for (ruleid, _) in grammar {
      if ruleid not in reach {
        for next in step.at(ruleid) {
          if next in reach {
            reach.push(ruleid)
            new.push(ruleid)
            break
          }
        }
      }
    }
    if new == () {
      break
    }
  }
  reach
}

#let check-empty(
  grammar,
) = {
  let _and(l, r) = {
    if l == false or r == false {
      false
    } else if l == true {
      r
    } else if r == true {
      l
    } else {
      none
    }
  }
  let _or(l, r) = {
    if l == true or r == true {
      true
    } else if l == false {
      r
    } else if r == false {
      l
    } else {
      none
    }
  }
  let known-nonempty(pat, known) = {
    if "lab" in pat {
      known.at(pat.lab, default: none)
    } else if pat.call == match.regex {
      let re = std.regex(pat.arg)
      if "".starts-with(re) {
        false
      } else {
        true
      }
    } else if pat.call in (match.star, match.commit, match.maybe, match.peek, match.neg, match.eof) {
      false
    } else if pat.call == match.str {
      pat.arg != ""
    } else if pat.call == match.fork {
      let ans = true
      for sub in pat.pats {
        ans = _and(ans, known-nonempty(sub, known))
      }
      ans
    } else if pat.call == match.seq {
      let ans = false
      for sub in pat.pats {
        ans = _or(ans, known-nonempty(sub, known))
      }
      ans
    } else if pat.call in (match.rewrite, match.iter, match.drop, match.try) {
      known-nonempty(pat.pat, known)
    } else {
      panic(pat)
    }
  }
  let emps = (:)
  while true {
    let new = ()
    for (id, rule) in grammar {
      if emps.at(id, default: none) == none {
        let nonempty = known-nonempty(rule.pat, emps)
        emps.insert(id, nonempty)
        new.push(id)
      }
    }
    if new == () {
      break
    }
  }
  emps
}

#let show-empty(grammar, nonempty) = {
  let tab = ((), (), ())
  for (id, _) in grammar {
    let idx = if nonempty.at(id) == none {
      1
    } else if nonempty.at(id) == true {
      0
    } else {
      2
    }
    tab.at(idx).push(id)
  }
  let titles = ([Provably nonempty], [Inconclusive], [Possibly empty])
  table(columns: 2,
    ..(
      for (title, rules) in titles.zip(tab) {
        if rules != () {
          ([*#title*], rules.map(i => raw("<" + i + ">")).join[, ])
        }
      }
    )
  )
}

#let check-leftrec(grammar, nonempty) = {
  let next-left(pat) = {
    if "lab" in pat {
      if nonempty.at(pat.lab) == true {
        ((pat.lab,), false)
      } else {
        ((pat.lab,), true)
      }
    } else if pat.call == match.regex {
      let re = std.regex(pat.arg)
      if "".starts-with(re) {
        ((), true)
      } else {
        ((), false)
      }
    } else if pat.call == match.str {
      if pat.arg != "" {
        ((), false)
      } else {
        ((), true)
      }
    } else if pat.call == match.fork {
      let ans = ()
      let after = true
      for sub in pat.pats {
        let (also, go-on) = next-left(sub)
        ans += also
        after = after or go-on
      }
      (ans, after)
    } else if pat.call == match.seq {
      let ans = ()
      for sub in pat.pats {
        let (also, go-on) = next-left(sub)
        ans += also
        if not go-on {
          return (ans, false)
        }
      }
      (ans, true)
    } else if pat.call in (match.commit,) {
      ((), true)
    } else if pat.call in (match.maybe, match.peek, match.neg, match.star) {
      let (ans, _) = next-left(pat.pat)
      (ans, true)
    } else if pat.call in (match.eof,) {
      ((), false)
    } else if pat.call in (match.rewrite, match.iter, match.drop, match.try) {
      next-left(pat.pat)
    } else {
      panic(pat)
    }
  }

  let next = (:)
  for (id, rule) in grammar {
    let (nl, _) = next-left(rule.pat)
    next.insert(id, nl)
  }
  let chains = ()
  for (id, nxs) in next {
    for nx in nxs {
      chains.push((id, nx))
    }
  }
  let cycles = ()
  while true {
    let new = ()
    for c in chains {
      if c.first() == c.last() {
        cycles.push(c)
      } else if c.last() in c.slice(0, -1) {
      } else {
        for nx in next.at(c.last()) {
          new.push((..c, nx))
        }
      }
    }
    chains = new
    if new == () {
      break
    }
  }
  (
    cycles: cycles,
    dangerous: inverse-reachable-set(grammar, cycles.flatten()),
  )
}

#let show-leftrec(lrec) = {
  if lrec.cycles != () {
    table(
      columns: 2,
      [*Null cycle detected*], lrec.cycles.at(0).map(id => raw("<" + id + ">")).join[ $->$ ],
    )
    [_The grammar is left-recursive: #raw("<" + lrec.cycles.at(0).at(0) + ">") can loop back
    to itself without consuming any input_]
  }
}

/// Runs the unit tests attached to a grammar and a number of sanity checks.
/// See @cmd:prelude:yy and @cmd:prelude:nn for details.
/// Runs all positive and negative tests, and formats them in a table.
/// -> content
#let test(
  /// The grammar to test, as constructed by @cmd:kleene:grammar.
  /// -> grammar
  grammar,
  /// Pass an array or a function to filter a subset of the tests.
  /// -> auto | array | function
  select: auto,
  /// Whether to display the final tally of passed/failed tests.
  /// -> bool
  total: true,
) = {
  let empty = check-empty(grammar)
  show-empty(grammar, empty)
  let lrec = check-leftrec(grammar, empty)
  show-leftrec(lrec)
  check-unit-tests(grammar, lrec: lrec, select: select, total: total)
}

#let extract-line(input, idx) = {
  let lines = input.split("\n")
  let cumul = 0
  for (lineid, line) in lines.enumerate() {
    if lineid < lines.len() - 1 {
      line += "\n"
    }
    cumul += line.len()
    if cumul >= idx {
      return (line, lineid + 1, idx - (cumul - line.len()))
    }
  }
}

#let error-box = rect.with(stroke: red)

#let show-span(input, idx, highlight-len: 1, msg: none) = {
  let (ctx, lineid, offset) = extract-line(input, idx)
  box({
    {
      set text(fill: gray)
      raw(str(lineid) + " | ")
    }
    show-invisible(ctx, mode: "unicode")
    linebreak()
    raw(" " * (str(lineid).len() + 3 + offset))
    {
      set text(fill: red)
      raw("^" * highlight-len + " ")
      msg
    }
    linebreak()
  })
}

#let show-span2(input, idx, highlight-pre: 1, highlight-post: 1, msg-pre: none, msg-post: none) = {
  let (ctx, lineid, offset) = extract-line(input, idx)
  box({
    {
      set text(fill: gray)
      raw(str(lineid) + " | ")
    }
    show-invisible(ctx, mode: "unicode")
    linebreak()
    let pre-len = highlight-pre
    let post-len = calc.max(0, calc.min(highlight-post, ctx.len() - offset))
    raw(" " * (str(lineid).len() + 2))
    {
      set text(fill: green)
      raw("~" * offset + "|")
    }
    {
      set text(fill: red)
      raw("^" * post-len + " ")
      msg-post
      if post-len == 0 {
        [ on next line]
      }
    }
    linebreak()
    {
      set text(fill: green)
      raw(" " * (str(lineid).len() + 3 + offset - 1))
      raw("| ")
      msg-pre
    }
  })
}

#let error-inner(input, ans) = {
  if "msg" not in ans {
    panic(ans)
  }
  if "stack" not in ans {
    panic(ans)
  }
  box[
    #show-span(input, input.len() - ans.rest.len(), msg: ans.msg) \
    While trying to parse: #{ans.stack.map(s => raw("<" + str(s) + ">")).join[ $->$ ]}.
  ]
}

#let error(input, ruleid, ans) = {
  error-box[
    #text(fill: red)[*Parsing error:*]
    The input does not match the expected format. \
    #error-inner(input, ans)
  ]
}

#let incomplete(input, ruleid, ans) = {
  error-box[
    #text(fill: red)[*Parsing error:*]
    The parser did not consume the entire input. \
    #show-span2(input, input.len() - ans.rest.len(), msg-post: [Surplus characters], highlight-post: ans.rest.len(), msg-pre: [Valid #raw("<" + str(ruleid) + ">")]) \
    #{
      while ans.next != none and ans.next.ok {
        ans.next = ans.next.next
      }
      if ans.next != none [
      Hint: halted due to the following: \
      #error-inner(input, ans.next)
    ]}
  ]
}
