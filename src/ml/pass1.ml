(* 1-st pass: Convert ML Ast to Coq IAst *)

open Core

open Ast
open Typetools
open IType
open IIntType
open IFloatType
open IArithType
open ITypes
open Ints
open Utils
open Option
open ExtrOcamlIntConv

exception CompileError1 of (string * Loc.t option)
let raise_CompileError1 msg = raise (CompileError1 (msg,None))

(* var_string -> type *)
let build_var_map l =
  match String.Map.of_alist l with
  | `Duplicate_key k -> raise_CompileError1 ("Duplicate variable '" ^ k ^ "' in 'let'" )
  | `Ok m -> m

(* var_string -> var_index *)
let build_var_index l =
  let il = List.mapi l ~f:(fun i (n,_) -> (n, z_of_int i)) in
  match String.Map.of_alist il with
  | `Duplicate_key k -> raise_CompileError1 ("Duplicate variable name '" ^ k ^ "' in 'let'" )
  | `Ok m -> m

let compile_int_type = function
  | Int8Type   -> IAst.Int8Type
  | Int16Type  -> IAst.Int16Type
  | Int32Type  -> IAst.Int32Type
  | Int64Type  -> IAst.Int64Type
  | UInt8Type  -> IAst.UInt8Type
  | UInt16Type -> IAst.UInt16Type
  | UInt32Type -> IAst.UInt32Type
  | UInt64Type -> IAst.UInt64Type
  | BoolType   -> IAst.BoolType

let compile_arith_type = function
  | I t   -> IAst.I (compile_int_type t)
  | F FloatType  -> IAst.F IAst.FloatType
  | F DoubleType -> IAst.F IAst.DoubleType

let rec compile_itype = function
  | A at          -> IAst.A (compile_arith_type at)
  | VoidType      -> IAst.VoidType
  | ArrType (t,l) -> IAst.ArrType (compile_itype t, z_of_int l)
  | VecType (t,l) -> IAst.VecType  (compile_arith_type t, z_of_int l)
  | PtrType (t,l) -> IAst.ArrType (compile_itype t, z_of_int l)

let compile_func (n:string) (al:IAst.rvalue list) =
  match n, al with
  | "cond",[a;b;c]             -> IAst.F_cond (a,b,c)
  | "min", a::b::c             -> IAst.F_min (a,b,c)
  | "max", a::b::c             -> IAst.F_max (a,b,c)
  | "add", [a;b]               -> IAst.F_add (a,b)
  | "sub", [a;b]               -> IAst.F_sub (a,b)
  | "mul", [a;b]               -> IAst.F_mul (a,b)
  | "div", [a;b]               -> IAst.F_div (a,b)
  | "geq", [a;b]               -> IAst.F_geq (a,b)
  | "abs", [a]                 -> IAst.F_abs a
  | "neg", [a]                 -> IAst.F_neg a
  | "vcvt_64f32f", [a;b]       -> IAst.F_vcvt_64f32f (a,b)
  | "addsub_4x32f", [a;b]      -> IAst.F_addsub_4x32f (a,b)
  | "addsub_2x64f", [a;b]      -> IAst.F_addsub_2x64f (a,b)
  | "vushuffle_2x64f", [a;b]   -> IAst.F_vushuffle_2x64f (a,b)
  | "vshuffle_2x64f" , [a;b;c] -> IAst.F_vshuffle_2x64f (a,b,c)
  | "vshuffle_4x32f" , [a;b;c] -> IAst.F_vshuffle_4x32f (a,b,c)
  | "vshuffle_8x32f" , [a;b;c] -> IAst.F_vshuffle_8x32f (a,b,c)
  | "vunpacklo_4x32f", [a;b]   -> IAst.F_vunpacklo_4x32f (a,b)
  | "vunpacklo_8x32f", [a;b]   -> IAst.F_vunpacklo_8x32f (a,b)
  | "vunpacklo_4x64f", [a;b]   -> IAst.F_vunpacklo_4x64f (a,b)
  | "vunpackhi_4x32f", [a;b]   -> IAst.F_vunpackhi_4x32f (a,b)
  | "vunpackhi_4x64f", [a;b]   -> IAst.F_vunpackhi_4x64f (a,b)
  | "vunpackhi_8x32f", [a;b]   -> IAst.F_vunpackhi_8x32f (a,b)
  | "cmpge_2x64f", [a;b]       -> IAst.F_cmpge_2x64f (a,b)
  | "testc_4x32i", [a;b]       -> IAst.F_testc_4x32i (a,b)
  | "testnzc_4x32i", [a;b]     -> IAst.F_testnzc_4x32i (a,b)
  | "vpermf128_4x64f", [a;b;c] -> IAst.F_vpermf128_4x64f (a,b,c)
  | "vpermf128_8x32f", [a;b;c] -> IAst.F_vpermf128_8x32f (a,b,c)
  | "vload_2l_4x32f", [a;b]    -> IAst.F_vload_2l_4x32f (a,b)
  | "vload_2h_4x32f", [a;b]    -> IAst.F_vload_2h_4x32f (a,b)
  | "vloadu_4x32f", [a]        -> IAst.F_vloadu_4x32f (a)
  | "vstore_2l_4x32f", [a;b]   -> IAst.F_vstore_2l_4x32f (a,b)
  | "vstore_2h_4x32f", [a;b]   -> IAst.F_vstore_2h_4x32f (a,b)
  | "vstoreu_4x32f", [a;b]     -> IAst.F_vstoreu_4x32f (a,b)
  | _,_ -> raise_CompileError1 (
               Format.asprintf "Unknonw function %s with %d arguments"
                               n (List.length al))

(* Generally follows C99 §6.4.4.1. Constants always unsigned *)
let type_and_value_of_hex l s : (IAst.inttype * BinNums.coq_Z) =
  let p = String.prefix s 2 in
  if p<>"0x" && p<>"0X" && p<>"-0x" && p<>"-0X" then
    raise (CompileError1 ("Hex value is expected with '0x' prefix in 'vhex'", Some l))
  else
    try
      let open BinNums in
      if String.suffix s 2 = "ul" then
        let z = z_of_hexstr (String.drop_suffix s 2) in
        match z with
        | Zpos _ | Z0 ->  (IAst.UInt64Type,z)
        | Zneg _ -> raise (CompileError1 (Format.asprintf "Invalid hex string \"%s\" in 'vhex'" s, Some l))
      else if String.suffix s 1 = "l" then
        let z = z_of_hexstr (String.drop_suffix s 1) in
        let t =
          if Int64Ex.in_range z then IAst.Int64Type
          else if Uint64Ex.in_range z then IAst.UInt64Type
          else raise (CompileError1 (Format.asprintf "Hex constant value does not fit biggest integer type \"%s\" in 'vhex'" s, Some l))
        in (t,z)
      else if String.suffix s 1 = "u" then
        let z = z_of_hexstr (String.drop_suffix s 1) in
        let t =
          if Uint32Ex.in_range z then IAst.UInt32Type
          else if Uint64Ex.in_range z then IAst.UInt64Type
          else raise (CompileError1 (Format.asprintf "Hex constant value does not fit biggest integer type \"%s\" in 'vhex'" s, Some l))
        in (t,z)
      else
        let z = z_of_hexstr s in
        let t =
          if Int32Ex.in_range z then IAst.Int32Type
          else if Uint32Ex.in_range z then IAst.UInt32Type
          else if Int64Ex.in_range z then IAst.Int64Type
          else if Uint64Ex.in_range z then IAst.UInt64Type
          else raise (CompileError1 (Format.asprintf "Hex constant value does not fit biggest integer type \"%s\" in 'vhex'" s, Some l))
        in (t,z)
    with
    | Failure _ -> raise (CompileError1 (Format.asprintf "Invalid hex string \"%s\" in 'vhex'" s, Some l))

let el_type_of_vhex l = function
  | 8  -> IAst.UInt8Type
  | 16 -> IAst.UInt16Type
  | 32 -> IAst.UInt32Type
  | 64 -> IAst.UInt64Type
  | bits -> raise (CompileError1 (Format.asprintf "Unexpected bit size %d for vhex" bits, Some l))

let typed_z_of_hex l t s =
  let v = z_of_hexstr s in
  match t with
  | IAst.UInt8Type -> if Uint8Ex.in_range v then v
                      else raise (CompileError1 (Format.asprintf "Hex constant \"%s\" does not fit 8 bit" s, Some l))
  | IAst.UInt16Type -> if Uint16Ex.in_range v then v
                       else raise (CompileError1 (Format.asprintf "Hex constant \"%s\" does not fit 16 bit" s, Some l))
  | IAst.UInt32Type -> if Uint32Ex.in_range v then v
                       else raise (CompileError1 (Format.asprintf "Hex constant \"%s\" does not fit 32 bit" s, Some l))
  | IAst.UInt64Type -> if Uint64Ex.in_range v then v
                       else raise (CompileError1 (Format.asprintf "Hex constant \"%s\" does not fit 64 bit" s, Some l))
  | _ -> raise (CompileError1 (Format.asprintf "Unsupported type for hex constant \"%s\" does not fit 32 bit" s, Some l))

let rec compile_lvalue vmap vindex (x:lvalue) =
  match x.lnode with
  | VarLValue v -> IAst.VarLValue (Map.find_exn vindex v)
  | LCast (t,lv) ->
     let lv = compile_lvalue vmap vindex lv in
     let t = compile_itype t in
     IAst.LCast (t,lv)
  | LDeref v -> IAst.LDeref (compile_rvalue vmap vindex v)
  | NthLvalue (v, i) -> IAst.NthLvalue (compile_lvalue vmap vindex v,
                                        compile_rvalue vmap vindex i)
and compile_rvalue vmap vindex rv =
  let compile_fconst (x:fconst) =
    match x.node with
    | FPLiteral (FloatType, x) -> IAst.FConst (IAst.FLiteral x)
    | FPLiteral (DoubleType, x) -> IAst.DConst (IAst.DLiteral x)
    | FloatEPS -> IAst.FConst IAst.FEPS
    | DoubleEPS -> IAst.DConst IAst.DEPS in
  let compile_iconst_z (x:iconst) =
    match x.node with
    | Int8Const   v -> Int8Ex.to_z   v
    | Int16Const  v -> Int16Ex.to_z  v
    | Int32Const  v -> Int32Ex.to_z  v
    | Int64Const  v -> Int64Ex.to_z  v
    | UInt8Const  v -> Uint8Ex.to_z  v
    | UInt16Const v -> Uint16Ex.to_z v
    | UInt32Const v -> Uint32Ex.to_z v
    | UInt64Const v -> Uint64Ex.to_z v
    | BoolConst   v -> z_of_bool v in
  let fconst_type (f:fconst) =
    match f.node with
    | FPLiteral (t,_) -> t
    | FloatEPS -> FloatType
    | DoubleEPS -> DoubleType in
  let iconst_type (c:iconst) = match c.node with
    | Int8Const   _ -> Int8Type
    | Int16Const  _ -> Int16Type
    | Int32Const  _ -> Int32Type
    | Int64Const  _ -> Int64Type
    | UInt8Const  _ -> UInt8Type
    | UInt16Const _ -> UInt16Type
    | UInt32Const _ -> UInt32Type
    | UInt64Const _ -> UInt64Type
    | BoolConst   _ -> BoolType in
  match rv.rnode with
  | VarRValue v -> IAst.VarRValue (Map.find_exn vindex v)
  | FunCallValue (n,a) ->
     let al = (List.map ~f:(compile_rvalue vmap vindex) a) in
     IAst.FunCallValue (compile_func n al)
  | FConst x -> compile_fconst x
  | IConst x -> IAst.IConst (compile_int_type (iconst_type x), compile_iconst_z x)
  | FConstArr (at, fl) ->
     let flt = List.map ~f:fconst_type fl in
     if List.for_all flt (eq_float_type at) then
       match List.hd_exn flt with
       | FloatType ->
          let compile_fconst_f x =
            (match compile_fconst x with
             | IAst.FConst x -> x
             | _ -> raise (CompileError1 ("Mix of float and double values in array", Some rv.rloc))) in
          let flc = List.map ~f:(compile_fconst_f) fl in
          IAst.FConstArr flc
       | DoubleType ->
          let compile_fconst_d x =
            (match compile_fconst x with
             | IAst.DConst x -> x
             | _ -> raise (CompileError1 ("Mix of float and double values in array", Some rv.rloc))) in
          let flc = List.map ~f:(compile_fconst_d) fl in
          IAst.DConstArr flc
     else
       raise (CompileError1 ("Mismatch between float array type and its value types", Some rv.rloc))
  | IConstArr (at, il) ->
     let ilt = List.map ~f:iconst_type il in
     if List.for_all ilt (eq_int_type at) then
       let ilc = List.map ~f:compile_iconst_z il in
       let t = compile_int_type (List.hd_exn ilt) in
       IAst.IConstArr (t, ilc)
     else
       raise (CompileError1 ("Mismatch between int array type and its value types\n", Some rv.rloc))
  | FConstVec (at, fl) ->
     let flt = List.map ~f:fconst_type fl in
     if List.for_all flt (eq_float_type at) then
       match List.hd_exn flt with
       | FloatType ->
          let compile_fconst_f x =
            (match compile_fconst x with
             | IAst.FConst x -> x
             | _ -> raise (CompileError1 ("Mix of float and double values in vector", Some rv.rloc))) in
          let flc = List.map ~f:(compile_fconst_f) fl in
          IAst.FConstVec flc
       | DoubleType ->
          let compile_fconst_d x =
            (match compile_fconst x with
             | IAst.DConst x -> x
             | _ -> raise (CompileError1 ("Mix of float and double values in vector", Some rv.rloc))) in
          let flc = List.map ~f:(compile_fconst_d) fl in
          IAst.DConstVec flc
     else
       raise (CompileError1 ("Mismatch between vector array type and its value types", Some rv.rloc))
  | IConstVec (at, il) ->
     let ilt = List.map ~f:iconst_type il in
     if List.for_all ilt (eq_int_type at) then
       let ilc = List.map ~f:compile_iconst_z il in
       let t = compile_int_type (List.hd_exn ilt) in
       IAst.IConstVec (t, ilc)
     else
       raise (CompileError1 ("Mismatch between int vector type and its value types\n", Some rv.rloc))
  | VHex sl ->
     let it = el_type_of_vhex rv.rloc (!Constants.vecLen/(List.length sl)) in
     let consts = List.map ~f:(typed_z_of_hex rv.rloc it) sl in
     IAst.IConstArr (it, consts)
  | RCast (t,rv) -> IAst.RCast (compile_itype t, compile_rvalue vmap vindex rv)
  | RDeref v -> IAst.RDeref (compile_rvalue vmap vindex v)
  | NthRvalue (v, i) -> IAst.NthRvalue (compile_rvalue vmap vindex v,
                                        compile_rvalue vmap vindex i)
  | VdupRvalue (v, il) ->
     match il.node with
     | Int32Const ic -> IAst.VdupRvalue (compile_rvalue vmap vindex v,
                                         compile_iconst_z il)
     | t -> raise (CompileError1 ("Int32 constant expected as type in VDUP" , Some v.rloc))

let pass1 valist body =
  let vmap = build_var_map valist in
  let vindex = build_var_index valist in
  let add_var s v =
    let open String.Set.Tree in
    if mem s v then raise_CompileError1 ("duplicate declaration of '" ^ v ^ "'")
    else add s v
  in
  let add_vars s vl = List.fold ~init:s ~f:add_var vl in

  let rec pass1 scope x =
    match x.node with
    | FunCallStmt (n,a) ->
       IAst.FunCallStmt (compile_func n (List.map ~f:(compile_rvalue vmap vindex) a))
    | Function (fn,fr,params,body) ->
       IAst.IFunction (fn,
                       compile_itype fr,
                       List.map ~f:(Map.find_exn vindex) params,
                       pass1 (add_vars scope params) body)
    | Decl (params,body) ->
       pass1 (add_vars scope params) body
    | Chain lbody -> IAst.Chain (List.map ~f:(pass1 scope) lbody)
    | Data (v,rl,body) ->
       IAst.Data (Map.find_exn vindex v,
                  List.map ~f:(compile_rvalue vmap vindex) rl,
                  pass1 (add_var scope v) body)
    | Loop (v,f,t,body) ->
       IAst.Loop (Map.find_exn vindex v,
                  Int64Ex.to_z f,
                  Int64Ex.to_z t,
                  pass1 (add_var scope v) body)
    | If (r,bt,bf) ->
       IAst.If (compile_rvalue vmap vindex r,
                pass1 scope bt,
                pass1 scope bf)
    | Skip -> IAst.Skip
    | Assign (l,r) -> IAst.Assign (compile_lvalue vmap vindex l,
                                   compile_rvalue vmap vindex r)
    | Return r -> IAst.Return (compile_rvalue vmap vindex r)
  in
  (* TODO: this lookup function is terribly inefficient. Re-implement using Map *)
  let rec var_index_find l v =
    match l with
    | [] -> None
    | (n,t)::ls -> if Map.find_exn vindex n = v
                   then Some (compile_itype t)
                   else var_index_find ls v in
  IAst.Program (var_index_find valist, pass1 String.Set.Tree.empty body)
