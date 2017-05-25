func(TInt, "transform", [
         var("X", TPtr(T_Real(64)).aligned([ 16, 0 ])),
         var("D", TPtr(T_Real(64)).aligned([ 16, 0 ]))
     ], 
   decl([ var("q3", T_Real(64)),
          var("q4", T_Real(64)),
          var("s1", T_Real(64)),
          var("s4", TReal),
          var("s5", T_Real(64)),
          var("s6", TReal),
          var("s7", T_Real(64)),
          var("s8", T_Real(64)),
          var("w1", T_Real(64))],
      chain(
         assign(var.table.s5, V(0.0)),
         assign(var.table.s8, nth(X, V(0))),
         assign(var.table.s7, V(1.0)),
         loop(var("i5", TInt), [ 0 .. 2 ],
            chain(
               assign(var.table.s6, mul(var.table.s7, nth(var.table.D, var.table.i5))),
               assign(var.table.s5, add(var.table.s5, var.table.s6)),
               assign(var.table.s7, mul(var.table.s7, var.table.s8))
            )
         ),
         assign(var.table.s1, V(0.0)),
         loop(var("i3", TInt), [ 0 .. 1 ],
            chain(
               assign(var.table.q3, nth(X, add(var.table.i3, V(1)))),
               assign(var.table.q4, nth(X, add(V(3), var.table.i3))),
               assign(var.table.w1, sub(var.table.q3, var.table.q4)),
               assign(var.table.s4, cond(geq(var.table.w1, V(0)), var.table.w1, neg(var.table.w1))),
               assign(var.table.s1, cond(geq(var.table.s1, var.table.s4), var.table.s1, var.table.s4))
            )
         ),
         creturn(geq(var.table.s1, var.table.s5))
      )
   )
)

