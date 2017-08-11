(*
1-st pass: Convert AST to IAst
*)

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

let build_var_index l =
  let il = List.mapi l ~f:(fun i (n,_) -> (n,i)) in
  match String.Map.Tree.of_alist il with
  | `Duplicate_key k -> raise_CompileError1 ("duplicate variable name '" ^ k ^ "' in 'let'" )
  | `Ok m -> m

let build_var_map l =
  match String.Map.Tree.of_alist l with
  | `Duplicate_key k -> raise_CompileError1 ("duplicate variable '" ^ k ^ "' in 'let'" )
  | `Ok m -> m

let rec compile_itype t : IAst.itype =
  let compile_arith_type = function
    | I Int8Type   -> IAst.I IAst.Int8Type
    | I Int16Type  -> IAst.I IAst.Int16Type
    | I Int32Type  -> IAst.I IAst.Int32Type
    | I Int64Type  -> IAst.I IAst.Int64Type
    | I UInt8Type  -> IAst.I IAst.UInt8Type
    | I UInt16Type -> IAst.I IAst.UInt16Type
    | I UInt32Type -> IAst.I IAst.UInt32Type
    | I UInt64Type -> IAst.I IAst.UInt64Type
    | I BoolType   -> IAst.I IAst.BoolType
    | F FloatType  -> IAst.F IAst.FloatType
    | F DoubleType -> IAst.F IAst.DoubleType in
  match t with
  | A at          -> IAst.A (compile_arith_type at)
  | VoidType      -> IAst.VoidType
  | ArrType (t,l) -> IAst.ArrType (compile_itype t, z_of_int l)
  | VecType (t,l) -> IAst.VecType  (compile_arith_type t, z_of_int l)
  | PtrType (t,l) -> IAst.ArrType (compile_itype t, z_of_int l)

(* TODO: placeholder!!! *)
let compile_func n al =
  IAst.FunCallValue (IAst.F_neg (IAst.IConst (IAst.Int8Type, z_of_int 1)))

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
    (match x.node with
     | FPLiteral (FloatType, x) -> IAst.FConst (IAst.FLiteral x)
     | FPLiteral (DoubleType, x) -> IAst.DConst (IAst.DLiteral x)
     | FloatEPS -> IAst.FConst IAst.FEPS
     | DoubleEPS -> IAst.DConst IAst.DEPS) in
  let fconst_type (f:fconst) =
     match f.node with
     | FPLiteral (t,_) -> t
     | FloatEPS -> FloatType
     | DoubleEPS -> DoubleType  in
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
     compile_func n al
  | FConst x -> compile_fconst x

  | IConst {node = (Int8Const   v) } -> IAst.IConst (IAst.Int8Type  , z_of_Int8   v)
  | IConst {node = (Int16Const  v) } -> IAst.IConst (IAst.Int16Type , z_of_Int16  v)
  | IConst {node = (Int32Const  v) } -> IAst.IConst (IAst.Int32Type , z_of_Int32  v)
  | IConst {node = (Int64Const  v) } -> IAst.IConst (IAst.Int64Type , z_of_Int64  v)
  | IConst {node = (UInt8Const  v) } -> IAst.IConst (IAst.UInt8Type , z_of_UInt8  v)
  | IConst {node = (UInt16Const v) } -> IAst.IConst (IAst.UInt16Type, z_of_UInt16 v)
  | IConst {node = (UInt32Const v) } -> IAst.IConst (IAst.UInt32Type, z_of_UInt32 v)
  | IConst {node = (UInt64Const v) } -> IAst.IConst (IAst.UInt64Type, z_of_UInt64 v)
  | IConst {node = (BoolConst   v) } -> IAst.IConst (IAst.BoolType  , z_of_Bool   v)

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
       ArrType (A (I at) , List.length il)
     else
       raise (CompileError1 ("Mismatch between int array type and its value types\n", Some rv.rloc))
  | FConstVec (at, fl) ->
     let flt = List.map ~f:fconst_type fl in
     if List.for_all flt (eq_float_type at) then
       VecType (F at , List.length fl)
     else
       raise (CompileError1 ("Mismatch between float vector type and its value types\n", Some rv.rloc))
  | IConstVec (at, il) ->
     let ilt = List.map ~f:iconst_type il in
     if List.for_all ilt (eq_int_type at) then
       VecType (I at , List.length il)
     else
       raise (CompileError1 ("Mismatch between int vector type and its value types\n", Some rv.rloc))
  | VHex sl ->
     let consts = List.map ~f:(iconst_of_hex rv.rloc) sl in
     let it = type_of_const (List.hd_exn consts).node in
     VecType (I it, List.length consts)
  | RCast (t,rv) ->
     let rt = rvalue_type vmap rv in
     if check_cast rt t then t
     else raise (CompileError1 (Format.asprintf "Illegal rvalue cast from %a to %a."
                                            pr_itype rt
                                            pr_itype t, Some rv.rloc));
  | RDeref v -> (match rvalue_type vmap v with
                 | PtrType (t,_) -> t
                 | t -> raise (CompileError1 (Format.asprintf "Dereferencing non-pointer type %a" pr_itype t, Some rv.rloc)))
  | NthRvalue (v, i) ->
     let it = rvalue_type vmap i in
     if not (is_integer it) then
       raise (CompileError1 (Format.asprintf "Invalid index type %a in NTH" pr_itype it, Some rv.rloc))
     else
       (match rvalue_type vmap v with
        | ArrType (t,_) | PtrType (t,_) -> t
        | t -> raise (CompileError1 (Format.asprintf "Invalid value type %a in NTH" pr_itype t, Some rv.rloc)))
  | VdupRvalue (v, il) ->
     match rvalue_type vmap v, il.node with
     | A vt, Int32Const ic -> let i = Int32Ex.to_int ic in
                               if is_power_of_2 i then VecType (vt, i)
                               else raise (CompileError1 ("Size in VDUP must be power of 2. Got: " ^ (string_of_int i), Some il.loc))
     | t,_ -> raise (CompileError1 (Format.asprintf "Invalid value type %a in VDUP" pr_itype t, Some v.rloc))



let pass1 valist body =
  let vindex = build_var_index valist in
  let vmap = build_var_map valist in
  let open String.Set.Tree in
  let add_var s v =
    if mem s v then raise_CompileError1 ("duplicate declaration of '" ^ v ^ "'")
    else add s v
  in
  let add_vars s vl = List.fold ~init:s ~f:add_var vl in

  let rec pass1 (fstack:(string * Ast.IType.t) list) u x =
    match x.node with
    | FunCallStmt (n,a) ->
       let al = (List.map ~f:(compile_rvalue vmap vindex) a) in
       compile_func n al
    | Function (fn,fr,params,body) ->
       pass1 ((fn,fr)::fstack) (add_vars u params) body
    | Decl (params,body) ->
       pass1 fstack (add_vars u params) body
    | Chain lbody ->
       List.fold ~f:(pass1 fstack) ~init:u lbody
    | Data (v,rl,body) ->
       ignore (List.map ~f:(check_vars_in_rvalue u) rl) ;
       pass1 fstack (add_var u v) body
    | Loop (v,f,t,body) ->
       if f>t then
         raise (CompileError1 (Printf.sprintf "Invalid loop index range: %s .. %s  " (Int64Ex.to_string f) (Int64Ex.to_string t), Some x.loc))
       else
         pass1 fstack (add_var u v) body
    | If (r,bt,bf) ->
       check_vars_in_rvalue u r ;
       union
         (pass1 fstack u bt)
         (pass1 fstack u bf)
    | Skip -> u
    | Assign (l,r) ->
       let rt = rvalue_type vmap r in
       let lt = lvalue_type vmap l in
       if not (check_coercion rt lt) then
         raise (CompileError1 (Format.asprintf "Incompatible types in assignment %a=[%a]."
                                           pr_itype lt
                                           pr_itype rt
               , Some x.loc));
       check_vars_in_lvalue u l;
       check_vars_in_rvalue u r;
       u
    | Return r -> check_vars_in_rvalue u r ;
                  (match List.hd fstack with
                   | None -> raise (CompileError1 ("Return outsude of function", Some x.loc))
                   | Some (fn,ft) ->
                      let at = rvalue_type vmap r in
                      if not (check_coercion at ft) then
                        raise (CompileError1 (Format.asprintf "Incompatible types in return from functon '%s'. Actual: %a. Expected: %a." fn  pr_itype at pr_itype ft, Some r.rloc))
                      else u)
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
