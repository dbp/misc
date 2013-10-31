#lang pyret

data Expr:
  | snum(n :: Number)
  | sadd(e1 :: Expr, e2 :: Expr)
  | slam(args :: List<String>, body :: Expr)
  | sid(s :: String)
  | sapp(fn :: Expr, args :: List<Expr>)
end

data Value:
  | vnum(n :: Number)
  | vlam(args :: List<String>, body :: Expr, env :: List<Pair>)
end

data Pair:
  | pair(a,b)
end

fun zip-index(_l :: List) -> List<Pair>:
  fun zi(l :: List, c :: Number):
    cases(List) l:
      | empty => empty
      | link(f, r) => link(pair(c, f), zi(r, c+1))
    end
  end
  zi(_l, 0)
end

fun interp(e :: Expr) -> Value:
  interp-env(e, [])
where:
  interp(snum(19)) is vnum(19)
  interp(sadd(snum(1),snum(1))) is vnum(2)
  interp(slam([], snum(1))) is vlam([], snum(1), [])
  interp(sapp(slam([], snum(1)), [])) is vnum(1)
  interp(sapp(slam(["x"], sadd(sid("x"),snum(1))), [snum(1)])) is vnum(2)
end

fun lookup(env :: List<Pair>, id :: String):
  cases(List) env:
    | empty => raise("lookup: " + id + " not bound")
    | link(f, r) =>
      if f.a == id: f.b else: lookup(r, id) end
  end
end

fun interp-env(e :: Expr, env :: List<Pair>) -> Value:
  cases(Expr) e:
    | snum(n) => vnum(n)
    | sadd(l, r) =>
      cases(Value) interp-env(l, env):
        | vlam(_,_,_) => raise("interp: adding a non-number")
        | vnum(n1) =>
          cases(Value) interp-env(r, env):
            | vlam(_,_,_) => raise("interp: adding a non-number")
            | vnum(n2) => vnum(n1 + n2)
          end
      end
    | slam(args, body) => vlam(args, body, env)
    | sid(i) => lookup(env, i)
    | sapp(fn, args) =>
      cases(Value) interp-env(fn, env):
        | vnum(_) => raise("interp: applying a non-function")
        | vlam(lamargs, body, lamenv) =>
          interp-env(body,
            for map2(name from lamargs, value from args.map(interp-env(_,env))):
              pair(name, value)
            end + lamenv)
      end
  end
end

fun cps(e :: Expr) -> is-slam:
  cases(Expr) e:
    | snum(n) => slam(["k"], sapp(sid("k"), [snum(n)]))
    | sadd(l, r) =>
      slam(["k"],
        sapp(cps(l),
          [slam(["v1"],
              sapp(cps(r),
                [slam(["v2"],
                  sapp(sid("k"),
                    [sadd(sid("v1"), sid("v2"))]))]))]))
    | slam(args, body) =>
      slam(["k"] + args,
        sapp(cps(body), [sid("k")]))
    | sid(i) => slam(["k"], sapp(sid("k"), [sid(i)]))
    | sapp(fn, _args) =>
      args = zip-index(_args)
      slam(["k"],
        args.foldr(fun(arg, base):
          sapp(cps(arg.b),
            [slam(["v" + tostring(arg.a)],
                base)])
        end,
          sapp(cps(fn),
            [sid("k")] + args.map(fun(a): sid("v" + tostring(a.a)) end))))
  end
end


fun interp-cps(e :: Expr) -> Value:
  interp(sapp(cps(e), [slam(["_arg_"], sid("_arg_"))]))
where:
  interp-cps(snum(19)) is vnum(19)
  interp-cps(sadd(snum(1),snum(1))) is vnum(2)
  interp-cps(sapp(slam([], snum(1)), [])) is vnum(1)
  interp-cps(sapp(slam(["x"], sid("x")), [snum(1)])) is vnum(1)
  interp-cps(sapp(slam(["x"], sadd(sid("x"),snum(1))), [snum(1)])) is vnum(2)
end