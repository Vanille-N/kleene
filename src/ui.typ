#let test(rules, select: auto, total: true) = {
  import "parse.typ": parse
  let outcomes = (ok: 0, err: 0)
  let interpret(ok, expect: true) = {
    if ok == expect {
      "ok"
    } else {
      "err"
    }
  }
  let status(ok, expect: true) = {
    let color = if ok == expect {
      green
    } else {
      red
    }
    rect(width: 5mm, height: 5mm, fill: color)
  }
  let evaluate(ruleid, tt, expect: true) = {
    let (ok, ans) = parse(rules, label(ruleid), tt.text)
    let arr = (
      box(fill: gray.lighten(60%), inset: 3pt)[#text(fill: blue)[#tt]],
      status(ok, expect: expect),
      [#ans],
    )
    let incr = interpret(ok, expect: expect)
    (incr, arr)
  }
  for (ruleid, rule) in rules {
    if select != auto and ruleid not in select { continue }
    if rule.yy != () {
      table(columns: (2fr, auto, 5fr),
        table.header(
          [*#ruleid*], [],
          text(fill: green)[*examples*],
        ),
        ..for tt in rule.yy {
          let (incr, arr) = evaluate(ruleid, tt, expect: true)
          outcomes.at(incr) += 1
          arr
        }
      )
    }
    if rule.nn != () {
      table(columns: (2fr, auto, 5fr),
        table.header(
          [*#ruleid*], [],
          text(fill: red)[*counterexamples*],
        ),
        ..for tt in rule.nn {
          let (incr, arr) = evaluate(ruleid, tt, expect: false)
          outcomes.at(incr) += 1
          arr
        }
      )
    }
  }
  if total {
    box(table(columns: 2,
      [total], [#{outcomes.ok + outcomes.err}],
      [#status(true)], [#{outcomes.ok}],
      [#status(false)], [#{outcomes.err}]
    ))
  }
}

#let show-invisible(line, base-color: black, mode: "special") = {
  let dicts = (
    special: (
      "\n": "\\n",
      "\t": "\\t",
      empty: "",
    ),
    unicode: (
      "\n": "¤",
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
      set text(fill: base-color.lighten(70%))
      raw(dict.at(c))
    } else {
      raw(c)
    }
  }).join()
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
    #{ if ans.next != none [
      Hint: halted due to the following: \
      #error-inner(input, ans.next)
    ]}
  ]
}
