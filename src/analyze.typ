#import "match.typ"

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
    } else if pat.call in (match.error,) {
      true
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
        if nonempty != none {
          new.push(id)
        }
      }
    }
    if new == () {
      break
    }
  }
  emps
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
    } else if pat.call in (match.rewrite, match.iter, match.drop, match.try, match.error) {
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

