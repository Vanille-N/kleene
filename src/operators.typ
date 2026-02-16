#let commit() = (subparse, stack, input) => {
  (ok: true, backtrack: false, next: none, rest: input)
}

#let eof() = (subparse, stack, input) => {
  if input == "" {
    (ok: true, backtrack: true, next: none, rest: input)
  } else {
    (ok: false, backtrack: true, msg: [Expected end of stream], next: none, stack: stack, rest: input)
  }
}

#let label(lab) = (subparse, stack, input) => {
  subparse(lab, input)
}

#let regex(re) = (subparse, stack, input) => {
  let re = std.regex(re)
  if input.starts-with(re) {
    let match = input.find(re)
    let len = match.len()
    let rest = input.slice(len)
    (ok: true, backtrack: true, val: match, next: none, rest: rest)
  } else {
    (ok: false, backtrack: true, msg: [Regex does not match], next: none, stack: stack, rest: input)
  }
}

#let seq-aux(pats) = (subparse, stack, input) => {
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
  (ok: true, backtrack: backtrack, val: matches, next: next, rest: input)
}

#let seq(..pats) = {
  let pats = pats.pos()
  if pats.len() == 1 {
    pats.at(0)
  } else {
    seq-aux(pats)
  }
}

#let raw(string) = (subparse, stack, input) => {
  if input.starts-with(string) {
    (ok: true, backtrack: true, val: string, next: none, rest: input.slice(string.len()))
  } else {
    (ok: false, backtrack: true, msg: [Can't match string "#std.raw(string)"], next: none, stack: stack, rest: input)
  }
}

#let range(start, end) = (subparse, stack, input) => {
  if input.len() == 0 {
    return (ok: false, backtrack: true, msg: [End of input stream], stack: stack, next: none, rest: input)
  }
  if start <= input.at(0) and input.at(0) <= end {
    (ok: true, backtrack: true, val: input.at(0), next: none, rest: input.slice(1))
  } else {
    (ok: false, backtrack: true, msg: [Character is out of range], next: none, stack: stack, rest: input)
  }
}

#let exclude(..excl) = (subparse, stack, input) => {
  if input.len() == 0 {
    return (ok: false, backtrack: true, msg: [End of input stream], stack: stack, next: none, rest: input)
  }
  if input.at(0) not in excl.pos() {
    (ok: true, backtrack: true, val: input.at(0), next: none, rest: input.slice(1))
  } else {
    (ok: false, backtrack: true, msg: [Character is excluded], next: none, stack: stack, rest: input)
  }
}

#let iter(..pats) = (subparse, stack, input) => {
  let matches = ()
  let count = 0
  let backtrack = true
  let input = input
  while true {
    let ans = subparse(seq(..pats), input)
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

#let fork(..pats) = (subparse, stack, input) => {
  let latest-msg = ""
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

#let maybe(..pat) = (subparse, stack, input) => {
  let ans = subparse(seq(..pat), input)
  if ans.ok {
    (: ..ans, val: (ans.at("val", default: none),))
  } else if not ans.backtrack {
    ans
  } else {
    (ok: true, backtrack: true, val: (), next: ans, rest: input)
  }
}

#let try(..pats) = (subparse, stack, input) => {
  let ans = subparse(seq(..pats), input)
  (: ..ans, backtrack: true)
}

#let drop(..pats) = (subparse, stack, input) => {
  let ans = subparse(seq(..pats), input)
  if "val" in ans {
    let _ = ans.remove("val")
  }
  ans
}

