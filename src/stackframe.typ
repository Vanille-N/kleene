#let args(..vals) = (
  pos: vals.pos(),
  named: vals.named(),
)

#let pause(fun, args, env, cont) = {
  (
    fun: fun,
    args: args,
    env: env,
    cont: cont,
    __frame__: (),
  )
}

#let tailcall(fun, args) = {
  (
    fun: fun,
    args: args,
    env: none,
    cont: none,
    __frame__: (),
  )
}

#let resume(fun, args) = {
  let res = fun(..args.pos, ..args.named)
  if type(res) != dictionary or "__frame__" not in res {
    return res
  }
  let (fun, args, env, cont) = res
  let stack = ((cont, env),)
  let _fun = fun
  let _args = args
  let _ret = none
  let _iter0 = 9999
  while _iter0 > 0 and stack.len() > 0 {
    let _iter1 = 5
    while _iter1 > 0 and stack.len() > 0 {
      _iter1 -= 1
      if _ret == none {
        let res = _fun(.._args.pos, .._args.named)
        if type(res) != dictionary or "__frame__" not in res {
          _ret = (ret: res)
        } else {
          let (fun, args, env, cont) = res
          stack.push((cont, env))
          _fun = fun
          _args = args
        }
      } else {
        let frame = stack.pop()
        let (cont, env) = frame
        if cont == none {
          continue
        }
        let res = cont(_ret.ret, env)
        _ret = none
        if type(res) != dictionary or "__frame__" not in res {
          _ret = (ret: res)
        } else {
          let (fun, args, env, cont) = res
          stack.push((cont, env))
          _fun = fun
          _args = args
        }
      }
    }
  }
  if _ret != none {
    _ret.ret
  } else {
    panic("The parser seems to be taking a really long time. Check for infinite loops.")
  }
}

#let run(fun) = (..vals) => {
  let res = resume(fun, args(..vals))
  assert(type(res) != dictionary or "__frame__" not in res)
  res
}
