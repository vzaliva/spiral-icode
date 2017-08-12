(* 1-st pass: Convert ML Ast to Coq IAst *)

open Core

open Ast
open Typetools
open IType
open IIntType
open IFloatType
open IArithType
open Ints
open Utils
open Option
open ExtrOcamlIntConv

exception CompileError1 of (string * Loc.t option)
let raise_CompileError1 msg = raise (CompileError1 (msg,None))

let build_var_map l =
  match String.Map.of_alist l with
  | `Duplicate_key k -> raise_CompileError1 ("duplicate variable '" ^ k ^ "' in 'let'" )
  | `Ok m -> m

let build_var_index l =
  let il = List.mapi l ~f:(fun i (n,_) -> (n, z_of_int i)) in
  match String.Map.of_alist il with
  | `Duplicate_key k -> raise_CompileError1 ("duplicate variable name '" ^ k ^ "' in 'let'" )
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

(* TODO: placeholder!!! *)
let compile_func (n:string) (al:IAst.rvalue list) =
  IAst.F_neg (IAst.IConst (IAst.Int8Type, z_of_int 1))

(* TODO: placeholders *)
let z_of_Int8   (v:Int8Ex.t  ) = z_of_int 1
let z_of_Int16  (v:Int16Ex.t ) = z_of_int 1
let z_of_Int32  (v:Int32Ex.t ) = z_of_int 1
let z_of_Int64  (v:Int64Ex.t ) = z_of_int 1
let z_of_UInt8  (v:Uint8Ex.t ) = z_of_int 1
let z_of_UInt16 (v:Uint16Ex.t) = z_of_int 1
let z_of_UInt32 (v:Uint32Ex.t) = z_of_int 1
let z_of_UInt64 (v:Uint64Ex.t) = z_of_int 1
let z_of_Bool   (v:bool      ) = z_of_int 1

(* Generally follows C99 §6.4.4.1 *)
let type_and_value_of_hex l s : (IAst.inttype * BinNums.coq_Z) =
  if String.is_empty s then
    raise (CompileError1 ("Empty hex string in 'vhex'", Some l))
  else
    try
      let v = Uint64Ex.of_string s in
      let z = z_of_UInt64 v in
      if String.suffix s 1 = "ul" then
        (IAst.UInt64Type, z)
      else if String.suffix s 1 = "l" then
        begin
          if in_int64_range v then (IAst.UInt64Type, z)
          else (IAst.UInt64Type, z) (* always fits *)
        end
      else if String.suffix s 1 = "u" then
        begin
          if in_uint32_range v then (IAst.UInt32Type, z)
          else (IAst.UInt64Type, z) (* always fits *)
        end
      else
        begin
          if in_int32_range v then (IAst.Int32Type, z)
          else if in_uint32_range v then (IAst.UInt32Type, z)
          else if in_int64_range v then (IAst.UInt64Type, z)
          else (IAst.UInt64Type, z) (* always fits *)
        end
    with
    | Failure _ -> raise (CompileError1 (Format.asprintf "Invalid hex string \"%s\" in 'vhex'" s, Some l))


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
    | Int8Const   v -> z_of_Int8   v
    | Int16Const  v -> z_of_Int16  v
    | Int32Const  v -> z_of_Int32  v
    | Int64Const  v -> z_of_Int64  v
    | UInt8Const  v -> z_of_UInt8  v
    | UInt16Const v -> z_of_UInt16 v
    | UInt32Const v -> z_of_UInt32 v
    | UInt64Const v -> z_of_UInt64 v
    | BoolConst   v -> z_of_Bool   v in
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
     let consts = List.map ~f:(type_and_value_of_hex rv.rloc) sl in
     (* TODO: unify types of all vhex constants *)
     let it,_ = List.hd_exn consts in
     IAst.IConstArr (it, List.map ~f:snd consts)
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
                  z_of_Int64 f,
                  z_of_Int64 t,
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
  (* check top-level program structure *)
  ignore(
      let rec is_func x =
        match x.node with
        | Function _ -> ()
        | Chain x -> is_all_func x
        | _ -> raise (CompileError1 ("'program' must contain only function definitions", Some x.loc))
      and is_all_func = function
        | [] -> ()
        | (x::xs) -> is_func x ; is_all_func xs in
      match prog.node with
      | Chain body -> is_all_func body

      | Function _ -> ()
      | _ -> raise (CompileError1 ("'program' must contain only function definitions", Some prog.loc))
    );
  (* build list of used variables *)
  let used = pass1 [] String.Set.Tree.empty prog in
  ignore (check_never_decl vmap used)
