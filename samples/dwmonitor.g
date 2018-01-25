let(i3 := var("i3", TInt),
i5 := var("i5", TInt),
x9 := var("x9", TVect(T_Real(64), 2)),
x8 := var("x8", TVect(T_Real(64), 2)),
x7 := var("x7", TVect(T_Real(64), 2)),
x6 := var("x6", TVect(T_Real(64), 2)),
x4 := var("x4", TVect(T_Real(64), 2)),
x3 := var("x3", TVect(T_Real(64), 2)),
x2 := var("x2", TVect(T_Real(64), 2)),
x19 := var("x19", TVect(T_Real(64), 2)),
x18 := var("x18", TVect(T_Real(64), 2)),
x17 := var("x17", TVect(T_Real(64), 2)),
x14 := var("x14", TVect(T_Real(64), 2)),
x13 := var("x13", TVect(T_Real(64), 2)),
x10 := var("x10", TVect(T_Real(64), 2)),
x1 := var("x1", TVect(T_Real(64), 2)),
w1 := var("w1", TBool),
u8 := var("u8", TVect(T_Real(64), 2)),
u7 := var("u7", TVect(T_Real(64), 2)),
u6 := var("u6", TVect(T_Real(64), 2)),
u5 := var("u5", TVect(T_Real(64), 2)),
u4 := var("u4", TVect(T_Real(64), 2)),
u3 := var("u3", TVect(T_Real(64), 2)),
u2 := var("u2", TVect(T_Real(64), 2)),
u1 := var("u1", TVect(T_Real(64), 2)),
D := var("D", TPtr(T_Real(64)).aligned([ 16, 0 ])),
X := var("X", TPtr(T_Real(32)).aligned([ 16, 0 ])),
func(TInt, "transform", [ X, D ], 
   decl([ u1, u2, u3, u4, u5, u6, u7, u8, w1, x1, x10, x13, x14, x17, x18, x19, x2, x3, x4, x6, x7, x8, x9 ],
      chain(
         ivenv(
            assign(u5, Value(TVect(T_Real(64), 2), [ Value(T_Real(64), 0.0), Value(T_Real(64), 0.0) ])),
            assign(u2, vcvt_64f32f(addsub_4x32f(vdup(RealEPS(T_Real(32)), Value(TInt, 4)), vdup(nth(X, Value(TInt, 0)), Value(TInt, 4))))),
            assign(u1, Value(TVect(T_Real(64), 2), [ Value(T_Real(64), -1.0), Value(T_Real(64), 1.0) ])),
            loop(i5, [ 0 .. 2 ],
               chain(
                  assign(x6, addsub_2x64f(vdup(add(RealEPS(T_Real(64)), RealEPS(T_Real(64))), Value(TInt, 2)), vdup(nth(D, i5), Value(TInt, 2)))),
                  assign(x1, addsub_2x64f(Value(TVect(T_Real(64), 2), [ Value(T_Real(64), 0.0), Value(T_Real(64), 0.0) ]), u1)),
                  assign(x2, mul(x1, x6)),
                  assign(x3, mul(vushuffle_2x64f(x1, vparam([ 2, 1 ])), x6)),
                  assign(x4, neg(min(x3, x2))),
                  assign(u3, add(max(vushuffle_2x64f(x4, vparam([ 2, 1 ])), x3, x2), vdup(RealEPS(T_Real(64)), Value(TInt, 2)))),
                  assign(u5, add(u5, u3)),
                  assign(x7, addsub_2x64f(Value(TVect(T_Real(64), 2), [ Value(T_Real(64), 0.0), Value(T_Real(64), 0.0) ]), u1)),
                  assign(x8, mul(x7, u2)),
                  assign(x9, mul(vushuffle_2x64f(x7, vparam([ 2, 1 ])), u2)),
                  assign(x10, neg(min(x9, x8))),
                  assign(u1, add(max(vushuffle_2x64f(x10, vparam([ 2, 1 ])), x9, x8), vdup(RealEPS(T_Real(64)), Value(TInt, 2))))
               )
            ),
            assign(u6, Value(TVect(T_Real(64), 2), [ Value(T_Real(64), 0.0), Value(T_Real(64), 0.0) ])),
            loop(i3, [ 0 .. 1 ],
               chain(
                  assign(u8, vcvt_64f32f(addsub_4x32f(vdup(RealEPS(T_Real(32)), Value(TInt, 4)), vdup(nth(X, add(i3, Value(TInt, 1))), Value(TInt, 4))))),
                  assign(u7, vcvt_64f32f(addsub_4x32f(vdup(RealEPS(T_Real(32)), Value(TInt, 4)), vdup(nth(X, add(Value(TInt, 3), i3)), Value(TInt, 4))))),
                  assign(x14, add(u8, vushuffle_2x64f(u7, vparam([ 2, 1 ])))),
                  assign(x13, vushuffle_2x64f(x14, vparam([ 2, 1 ]))),
                  assign(u4, vshuffle_2x64f(min(x14, x13), max(x14, x13), vparam([ 1, 2 ]))),
                  assign(u6, vshuffle_2x64f(min(u4, u6), max(u4, u6), vparam([ 1, 2 ])))
               )
            ),
            assign(x17, addsub_2x64f(Value(TVect(T_Real(64), 2), [ Value(T_Real(64), 0.0), Value(T_Real(64), 0.0) ]), u6)),
            assign(x18, addsub_2x64f(Value(TVect(T_Real(64), 2), [ Value(T_Real(64), 0.0), Value(T_Real(64), 0.0) ]), u5)),
            assign(x19, cmpge_2x64f(x17, vushuffle_2x64f(x18, vparam([ 2, 1 ])))),
            assign(w1, sub(testc_4x32i(tcast(TVect(T_Int(32), 4), x19), vhex([ "0xffffffff", "0xffffffff", "0xffffffff", "0xffffffff" ])), testnzc_4x32i(tcast(TVect(T_Int(32), 4), x19), vhex([ "0xffffffff", "0xffffffff", "0xffffffff", "0xffffffff" ]))))
         ),
         creturn(w1)
      )
   )
))