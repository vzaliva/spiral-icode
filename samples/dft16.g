let(t160 := var("t160", TVect(TReal, 4)),
t159 := var("t159", TVect(TReal, 4)),
t158 := var("t158", TVect(TReal, 4)),
t157 := var("t157", TVect(TReal, 4)),
t156 := var("t156", TVect(TReal, 4)),
t155 := var("t155", TVect(TReal, 4)),
t154 := var("t154", TVect(TReal, 4)),
t153 := var("t153", TVect(TReal, 4)),
t152 := var("t152", TVect(TReal, 4)),
t151 := var("t151", TVect(TReal, 4)),
t150 := var("t150", TVect(TReal, 4)),
t149 := var("t149", TVect(TReal, 4)),
t148 := var("t148", TVect(TReal, 4)),
t147 := var("t147", TVect(TReal, 4)),
t146 := var("t146", TVect(TReal, 4)),
t145 := var("t145", TVect(TReal, 4)),
t144 := var("t144", TVect(TReal, 4)),
t143 := var("t143", TVect(TReal, 4)),
t142 := var("t142", TVect(TReal, 4)),
t141 := var("t141", TVect(TReal, 4)),
t140 := var("t140", TVect(TReal, 4)),
t139 := var("t139", TVect(TReal, 4)),
t138 := var("t138", TVect(TReal, 4)),
t137 := var("t137", TVect(TReal, 4)),
t136 := var("t136", TVect(TReal, 4)),
t135 := var("t135", TVect(TReal, 4)),
t134 := var("t134", TVect(TReal, 4)),
t133 := var("t133", TVect(TReal, 4)),
t132 := var("t132", TVect(TReal, 4)),
t131 := var("t131", TVect(TReal, 4)),
t130 := var("t130", TVect(TReal, 4)),
t129 := var("t129", TVect(TReal, 4)),
s184 := var("s184", TVect(TReal, 4)),
s183 := var("s183", TVect(TReal, 4)),
s182 := var("s182", TVect(TReal, 4)),
s181 := var("s181", TVect(TReal, 4)),
s180 := var("s180", TVect(TReal, 4)),
s179 := var("s179", TVect(TReal, 4)),
s178 := var("s178", TVect(TReal, 4)),
s177 := var("s177", TVect(TReal, 4)),
s176 := var("s176", TVect(TReal, 4)),
s175 := var("s175", TVect(TReal, 4)),
s174 := var("s174", TVect(TReal, 4)),
s173 := var("s173", TVect(TReal, 4)),
s172 := var("s172", TVect(TReal, 4)),
s171 := var("s171", TVect(TReal, 4)),
s170 := var("s170", TVect(TReal, 4)),
s169 := var("s169", TVect(TReal, 4)),
s168 := var("s168", TVect(TReal, 4)),
s167 := var("s167", TVect(TReal, 4)),
s166 := var("s166", TVect(TReal, 4)),
s165 := var("s165", TVect(TReal, 4)),
s164 := var("s164", TVect(TReal, 4)),
s163 := var("s163", TVect(TReal, 4)),
s162 := var("s162", TVect(TReal, 4)),
s161 := var("s161", TVect(TReal, 4)),
s160 := var("s160", TVect(TReal, 4)),
s159 := var("s159", TVect(TReal, 4)),
s158 := var("s158", TVect(TReal, 4)),
s157 := var("s157", TVect(TReal, 4)),
s156 := var("s156", TVect(TReal, 4)),
s155 := var("s155", TVect(TReal, 4)),
s154 := var("s154", TVect(TReal, 4)),
s153 := var("s153", TVect(TReal, 4)),
s152 := var("s152", TVect(TReal, 4)),
s151 := var("s151", TVect(TReal, 4)),
s150 := var("s150", TVect(TReal, 4)),
s149 := var("s149", TVect(TReal, 4)),
s148 := var("s148", TVect(TReal, 4)),
s147 := var("s147", TVect(TReal, 4)),
s146 := var("s146", TVect(TReal, 4)),
s145 := var("s145", TVect(TReal, 4)),
s144 := var("s144", TVect(TReal, 4)),
s143 := var("s143", TVect(TReal, 4)),
s142 := var("s142", TVect(TReal, 4)),
s141 := var("s141", TVect(TReal, 4)),
s140 := var("s140", TVect(TReal, 4)),
s139 := var("s139", TVect(TReal, 4)),
a46 := var("a46", TPtr(TVect(TReal, 4)).aligned([ 16, 0 ])),
a45 := var("a45", TPtr(TVect(TReal, 4)).aligned([ 16, 0 ])),
X := var("X", TPtr(TReal).aligned([ 16, 0 ])),
Y := var("Y", TPtr(TReal).aligned([ 16, 0 ])),
program(
   chain(
      func(TVoid, "init", [  ], 
         chain(
            chain(),
            chain()
         )
      ),
      func(TVoid, "transform", [ Y, X ], 
         decl([ a45, a46, s139, s140, s141, s142, s143, s144, s145, s146, s147, s148, s149, s150, s151, s152, s153, s154, s155, s156, s157, s158, s159, s160, s161, s162, s163, s164, s165, s166, s167, s168, s169, s170, s171, s172, s173, s174, s175, s176, s177, s178, s179, s180, s181, s182, s183, s184, t129, t130, t131, t132, t133, t134, t135, t136, t137, t138, t139, t140, t141, t142, t143, t144, t145, t146, t147, t148, t149, t150, t151, t152, t153, t154, t155, t156, t157, t158, t159, t160 ],
            chain(
               assign(a45, tcast(TPtr(TVect(TReal, 4)).aligned([ 16, 0 ]), X)),
               assign(s139, deref(a45)),
               assign(s140, deref(add(a45, Value(TInt, 1)))),
               assign(s141, vshuffle_4x32f(s139, s140, vparam([ 1, 3, 1, 3 ]))),
               assign(s142, vshuffle_4x32f(s139, s140, vparam([ 2, 4, 2, 4 ]))),
               assign(s143, deref(add(a45, Value(TInt, 4)))),
               assign(s144, deref(add(a45, Value(TInt, 5)))),
               assign(s145, vshuffle_4x32f(s143, s144, vparam([ 1, 3, 1, 3 ]))),
               assign(s146, vshuffle_4x32f(s143, s144, vparam([ 2, 4, 2, 4 ]))),
               assign(t129, add(s141, s145)),
               assign(t130, add(s142, s146)),
               assign(t131, sub(s141, s145)),
               assign(t132, sub(s142, s146)),
               assign(s147, deref(add(a45, Value(TInt, 2)))),
               assign(s148, deref(add(a45, Value(TInt, 3)))),
               assign(s149, vshuffle_4x32f(s147, s148, vparam([ 1, 3, 1, 3 ]))),
               assign(s150, vshuffle_4x32f(s147, s148, vparam([ 2, 4, 2, 4 ]))),
               assign(s151, deref(add(a45, Value(TInt, 6)))),
               assign(s152, deref(add(a45, Value(TInt, 7)))),
               assign(s153, vshuffle_4x32f(s151, s152, vparam([ 1, 3, 1, 3 ]))),
               assign(s154, vshuffle_4x32f(s151, s152, vparam([ 2, 4, 2, 4 ]))),
               assign(t133, add(s149, s153)),
               assign(t134, add(s150, s154)),
               assign(t135, sub(s149, s153)),
               assign(t136, sub(s150, s154)),
               assign(t137, add(t129, t133)),
               assign(t138, add(t130, t134)),
               assign(t139, sub(t129, t133)),
               assign(t140, sub(t130, t134)),
               assign(t141, sub(t131, t136)),
               assign(t142, add(t132, t135)),
               assign(t143, add(t131, t136)),
               assign(t144, sub(t132, t135)),
               assign(s155, vunpacklo_4x32f(t137, t141)),
               assign(s156, vunpackhi_4x32f(t137, t141)),
               assign(s157, vunpacklo_4x32f(t138, t142)),
               assign(s158, vunpackhi_4x32f(t138, t142)),
               assign(s159, vunpacklo_4x32f(t139, t143)),
               assign(s160, vunpackhi_4x32f(t139, t143)),
               assign(s161, vunpacklo_4x32f(t140, t144)),
               assign(s162, vunpackhi_4x32f(t140, t144)),
               assign(s163, vshuffle_4x32f(s155, s159, vparam([ 1, 2, 1, 2 ]))),
               assign(s164, vshuffle_4x32f(s155, s159, vparam([ 3, 4, 3, 4 ]))),
               assign(s165, vshuffle_4x32f(s156, s160, vparam([ 1, 2, 1, 2 ]))),
               assign(s166, vshuffle_4x32f(s156, s160, vparam([ 3, 4, 3, 4 ]))),
               assign(s167, vshuffle_4x32f(s157, s161, vparam([ 1, 2, 1, 2 ]))),
               assign(s168, vshuffle_4x32f(s157, s161, vparam([ 3, 4, 3, 4 ]))),
               assign(s169, vshuffle_4x32f(s158, s162, vparam([ 1, 2, 1, 2 ]))),
               assign(s170, vshuffle_4x32f(s158, s162, vparam([ 3, 4, 3, 4 ]))),
               assign(s171, sub(mul(Value(TVect(TReal, 4), [ Value(TReal, 1.0), Value(TReal, 0.70710678118654757), Value(TReal, 0.0), Value(TReal, -0.70710678118654757) ]), s165), mul(Value(TVect(TReal, 4), [ Value(TReal, 0.0), Value(TReal, 0.70710678118654757), Value(TReal, 1.0), Value(TReal, 0.70710678118654757) ]), s169))),
               assign(s172, add(mul(Value(TVect(TReal, 4), [ Value(TReal, 0.0), Value(TReal, 0.70710678118654757), Value(TReal, 1.0), Value(TReal, 0.70710678118654757) ]), s165), mul(Value(TVect(TReal, 4), [ Value(TReal, 1.0), Value(TReal, 0.70710678118654757), Value(TReal, 0.0), Value(TReal, -0.70710678118654757) ]), s169))),
               assign(t145, add(s163, s171)),
               assign(t146, add(s167, s172)),
               assign(t147, sub(s163, s171)),
               assign(t148, sub(s167, s172)),
               assign(s173, sub(mul(Value(TVect(TReal, 4), [ Value(TReal, 1.0), Value(TReal, 0.92387953251128674), Value(TReal, 0.70710678118654757), Value(TReal, 0.38268343236508978) ]), s164), mul(Value(TVect(TReal, 4), [ Value(TReal, 0.0), Value(TReal, 0.38268343236508978), Value(TReal, 0.70710678118654757), Value(TReal, 0.92387953251128674) ]), s168))),
               assign(s174, add(mul(Value(TVect(TReal, 4), [ Value(TReal, 0.0), Value(TReal, 0.38268343236508978), Value(TReal, 0.70710678118654757), Value(TReal, 0.92387953251128674) ]), s164), mul(Value(TVect(TReal, 4), [ Value(TReal, 1.0), Value(TReal, 0.92387953251128674), Value(TReal, 0.70710678118654757), Value(TReal, 0.38268343236508978) ]), s168))),
               assign(s175, sub(mul(Value(TVect(TReal, 4), [ Value(TReal, 1.0), Value(TReal, 0.38268343236508978), Value(TReal, -0.70710678118654757), Value(TReal, -0.92387953251128674) ]), s166), mul(Value(TVect(TReal, 4), [ Value(TReal, 0.0), Value(TReal, 0.92387953251128674), Value(TReal, 0.70710678118654757), Value(TReal, -0.38268343236508978) ]), s170))),
               assign(s176, add(mul(Value(TVect(TReal, 4), [ Value(TReal, 0.0), Value(TReal, 0.92387953251128674), Value(TReal, 0.70710678118654757), Value(TReal, -0.38268343236508978) ]), s166), mul(Value(TVect(TReal, 4), [ Value(TReal, 1.0), Value(TReal, 0.38268343236508978), Value(TReal, -0.70710678118654757), Value(TReal, -0.92387953251128674) ]), s170))),
               assign(t149, add(s173, s175)),
               assign(t150, add(s174, s176)),
               assign(t151, sub(s173, s175)),
               assign(t152, sub(s174, s176)),
               assign(t153, add(t145, t149)),
               assign(t154, add(t146, t150)),
               assign(t155, sub(t145, t149)),
               assign(t156, sub(t146, t150)),
               assign(a46, tcast(TPtr(TVect(TReal, 4)).aligned([ 16, 0 ]), Y)),
               assign(s177, vunpacklo_4x32f(t153, t154)),
               assign(deref(a46), s177),
               assign(s178, vunpackhi_4x32f(t153, t154)),
               assign(deref(add(a46, Value(TInt, 1))), s178),
               assign(s179, vunpacklo_4x32f(t155, t156)),
               assign(deref(add(a46, Value(TInt, 4))), s179),
               assign(s180, vunpackhi_4x32f(t155, t156)),
               assign(deref(add(a46, Value(TInt, 5))), s180),
               assign(t157, sub(t147, t152)),
               assign(t158, add(t148, t151)),
               assign(t159, add(t147, t152)),
               assign(t160, sub(t148, t151)),
               assign(s181, vunpacklo_4x32f(t157, t158)),
               assign(deref(add(a46, Value(TInt, 2))), s181),
               assign(s182, vunpackhi_4x32f(t157, t158)),
               assign(deref(add(a46, Value(TInt, 3))), s182),
               assign(s183, vunpacklo_4x32f(t159, t160)),
               assign(deref(add(a46, Value(TInt, 6))), s183),
               assign(s184, vunpackhi_4x32f(t159, t160)),
               assign(deref(add(a46, Value(TInt, 7))), s184)
            )
         )
      )
   )
))