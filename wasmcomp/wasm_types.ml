(* Types *)

module Lib = Wasm_lib
module I32 = Wasm_i32

type ty_var = int
type ty_atom = string

type value_type = I32Type | I64Type | F32Type | F64Type
                | TyName of ty_name

(* type constructors *)
and ty_name =
  | TyVar of ty_var
  | TyAtom of ty_atom
  | TyFunc of ty_atom * (ty_name list)
  | TyConstr of ty_constr list * ty_name

(* subtyping constraints *)
and ty_constr =
  | Subtype of ty_var * ty_atom

type tyname_struct = value_type list

type stack_type = value_type list
type block_type = { constrs: ty_constr list; stack: stack_type }

module IntCompare = struct type t = int let compare = compare end
module StrCompare = struct type t = string let compare = compare end
module TynameCompare = struct type t = ty_name let compare = compare end
module ConstrCompare =
  struct type t = ty_constr
  let compare (Subtype (v1, a1)) (Subtype (v2, a2)) =
    let c1 = compare v1 v2 in
    let c2 = compare a1 a2 in
    if not (c1 = 0) then c1 else c2
end
module IdMap = Map.Make(IntCompare)
module IdSet = Set.Make(IntCompare)
module ConstrSet = Set.Make(ConstrCompare)
module AtomSet = Set.Make(StrCompare)
module TynameMap = Map.Make(TynameCompare)

let rec hoist_tyname_constrs (tyname: ty_name) : ty_constr list * ty_name =
  match tyname with
  | TyVar _ | TyAtom _ -> ([], tyname)
  | TyFunc (f, args) ->
    let (arg_constrs, args') =
      List.map hoist_tyname_constrs args |> List.split
    in
    (List.concat arg_constrs, TyFunc (f,args'))
  | TyConstr (constrs, tyname) -> (constrs, tyname)
;;

let rec get_tyname_constrs (tyname: ty_name) : ty_constr list =
  hoist_tyname_constrs tyname |> fst
;;

let subst_constr repl_map constr =
  match constr with
  | Subtype (v, a) when IdMap.mem v repl_map ->
    (* when this is called, vars should be mapped to vars *)
    let (TyVar v') = IdMap.find v repl_map in Subtype (v', a)
  | s -> s
;;

let rec subst_tyname (repl_map: ty_name IdMap.t) (tyname: ty_name) : ty_name =
  match tyname with
  | TyVar v when IdMap.mem v repl_map -> IdMap.find v repl_map
  | TyVar v -> TyVar v
  | TyAtom a -> TyAtom a
  | TyFunc (f, args) ->
    let args' = List.map (subst_tyname repl_map) args in
    TyFunc (f, args')
  | TyConstr (constrs, constr_tyname) ->
    let constrs' = List.map (subst_constr repl_map) constrs in
    let constr_tyname' = subst_tyname repl_map constr_tyname in
    TyConstr (constrs', constr_tyname')
;;

let subst_value_type (repl_map: ty_name IdMap.t) (vt: value_type) : value_type =
  match vt with
  | I32Type | I64Type | F32Type | F64Type -> vt
  | TyName tyname ->
    let tyname' = subst_tyname repl_map tyname in
    TyName tyname'
;;

let subst_stack_vars (repl_map: ty_name IdMap.t) (st: stack_type) : stack_type =
  List.map (subst_value_type repl_map) st
;;

(* like subst_tyname, except we can replace arbitrary tynames
 * instead of just vars *)
let rec transform_tyname (repl_map: ty_name TynameMap.t) (tyname: ty_name) : ty_name =
  match tyname with
  | _ when TynameMap.mem tyname repl_map -> TynameMap.find tyname repl_map
  | TyVar v -> TyVar v
  | TyAtom a -> TyAtom a
  | TyFunc (f, args) ->
    let args' = List.map (transform_tyname repl_map) args in
    TyFunc (f, args')
  | TyConstr (constrs, constr_tyname) ->
    let constr_tyname' = transform_tyname repl_map constr_tyname in
    TyConstr (constrs, constr_tyname')
;;

let transform_value_type (repl_map: ty_name TynameMap.t) (vt: value_type) : value_type =
  match vt with
  | I32Type | I64Type | F32Type | F64Type -> vt
  | TyName tyname ->
    let tyname' = transform_tyname repl_map tyname in
    TyName tyname'
;;

let get_tyvars_constr (constr: ty_constr) : ty_var list =
  match constr with
  | Subtype (v, _) -> [v]

let rec get_tyvars_tyname (name: ty_name) : ty_var list =
  match name with
  | TyVar v -> [v]
  | TyAtom a -> []
  | TyFunc (_, args) ->
    List.map get_tyvars_tyname args |> List.concat |>
    IdSet.of_list |> IdSet.elements
  | TyConstr (constrs, constr_tyname) ->
    let constr_vars = List.map get_tyvars_constr constrs in
    let tyname_vars = get_tyvars_tyname constr_tyname in
    List.concat (tyname_vars::constr_vars) |>
    IdSet.of_list |> IdSet.elements
;;

let get_tyvars_value_type (vt: value_type) : ty_var list =
  match vt with
  | I32Type | I64Type | F32Type | F64Type -> []
  | TyName tyname -> get_tyvars_tyname tyname
;;

let get_tyvars_stack (st: stack_type) : ty_var list =
  st |> List.map get_tyvars_value_type |> List.concat
;;

let truncate_stacks (st1: stack_type) (st2: stack_type) : (stack_type * stack_type) =
  let n  = min (List.length st1) (List.length st2) in
  let st1' = Lib.List.drop (List.length st1 - n) st1 in
  let st2' = Lib.List.drop (List.length st2 - n) st2 in
  (st1', st2')
;;

let stack_to_block_type (st: stack_type) : block_type = 
  let hoist_constraints vt = 
    match vt with
    | I32Type | I64Type | F32Type | F64Type -> ([], vt)
    | TyName tyname ->
      let (constrs, tyname') = hoist_tyname_constrs tyname in
      (constrs, TyName tyname')
  in
  let (constrs_lst, stack) = st |> List.map hoist_constraints |> List.split in
  let constrs = List.concat constrs_lst in
  { constrs; stack }
;;

(* make sure tyname constraints don't shadow each other *)
let make_tyname_unique ?(init_var=0) (tyname: ty_name) =
  let fresh_var = ref init_var in
  let rec go varmap name =
    match name with
    | TyVar v when IdMap.mem v varmap ->
      let TyVar v' = IdMap.find v varmap in TyVar v'
    | TyFunc (f, args) ->
      let args' = List.map (go varmap) args in
      TyFunc (f, args')
    | TyConstr (constrs, constr_name) ->
      let constr_vars = constrs |> List.map get_tyvars_constr |> List.concat in
      let constr_vars_inc = List.map (fun x -> x+1) constr_vars in
      fresh_var := List.fold_right max constr_vars_inc !fresh_var;
      let varmap' =
        constr_vars |> List.map (fun v -> (v, v + !fresh_var)) |> fun varlst ->
        let add_to_varmap (v,v') acc = IdMap.add v (TyVar v') acc in
        List.fold_right add_to_varmap varlst varmap
      in
      let constrs' = List.map (subst_constr varmap') constrs in
      let constr_name' = go varmap' constr_name in
      TyConstr (constrs', constr_name')
    | _ -> name
  in
  go IdMap.empty tyname
;;

let make_value_type_unique ?(init_var=0) (vt: value_type) : value_type =
  match vt with
  | I32Type | I64Type | F32Type | F64Type -> vt
  | TyName tyname ->
    let tyname' = make_tyname_unique ~init_var:init_var tyname in
    TyName tyname'
;;

let make_stack_unique ?(init_var=0) (st: stack_type) : stack_type =
  let go (fresh_var, acc) vt =
    let vt' = make_value_type_unique ~init_var:fresh_var vt in
    let tyvars = get_tyvars_value_type vt' in
    let tyvars_inc = List.map (fun x -> x+1) tyvars in
    let fresh_var' = List.fold_right max tyvars_inc fresh_var in
    (fresh_var', acc@[vt'])
  in
  let (_, st') = List.fold_left go (init_var, []) st in
  st'
;;

(* find the least var that is fresh in the block
 * this is used during generalization when creating fresh variables *)
let get_fresh_var_tyname (tyname: ty_name) =
  let vars = get_tyvars_tyname tyname in
  let m = List.fold_right max vars 0 in
  m + 1
;;

let get_fresh_var_value_type (vt: value_type) =
  match vt with
  | I32Type | I64Type | F32Type | F64Type -> 0
  | TyName name -> get_fresh_var_tyname name
;;

let get_fresh_var_stack (st: stack_type) =
  let get_fresh_vars = function
  | I32Type | I64Type | F32Type | F64Type -> 0
  | TyName name -> get_fresh_var_tyname name
  in
  List.fold_right max (List.map get_fresh_vars st) 0
;;

(* rewrite stack type so that it doesn't have any vars that
 * are the same from some other stack *)
let alpha_convert_stack (st1: stack_type) (st2: stack_type) : stack_type =
  let st1_fresh_var = get_fresh_var_stack st1 in
  make_stack_unique ~init_var:st1_fresh_var st2
;;

(* TODO: make this more easily parameterizable, since
 * this equality depends on the "theory" of the constraints *)
let constr_sets_equal vcs1 vcs2 =
  let atoms1 =
    ConstrSet.elements vcs1 |> List.map (fun (Subtype (_, a)) -> a)
    |> AtomSet.of_list
  in
  let atoms2 =
    ConstrSet.elements vcs2 |> List.map (fun (Subtype (_, a)) -> a)
    |> AtomSet.of_list
  in
  AtomSet.equal atoms1 atoms2
;;

(* check for alpha-equivalence of block types *)
let is_block_equiv (bt1: block_type) (bt2: block_type) : bool = 
  let build_constr_map constr acc =
    match constr with
    | Subtype (var, a) ->
      if IdMap.mem var acc
      then begin
        let varmap = IdMap.find var acc in
        let varmap' = ConstrSet.add constr varmap in
        IdMap.add var varmap' acc
      end
      else IdMap.add var (ConstrSet.singleton constr) acc
  in
  let vconstrs1 = List.fold_right build_constr_map bt1.constrs IdMap.empty in
  let vconstrs2 = List.fold_right build_constr_map bt2.constrs IdMap.empty in
  let rec compare_tynames t1 t2 =
    match t1, t2 with
    (* check if variable constraints are equal *)
    | TyVar v1, TyVar v2 ->
      let vcs1 = IdMap.find v1 vconstrs1 in
      let vcs2 = IdMap.find v2 vconstrs1 in
      constr_sets_equal vcs1 vcs2

    | TyAtom a1, TyAtom a2 when a1 = a2 -> true

    | TyFunc (f1, args1), TyFunc (f2, args2) when f1 = f2 ->
      List.combine args1 args2 |>
      List.map (fun (a1, a2) -> compare_tynames a1 a2) |>
      fun lst -> List.fold_right (&&) lst true

    | _ -> false
  in
  let compare_val_types v1 v2 =
    match v1, v2 with
    | I32Type, I32Type | I64Type, I64Type
    | F32Type, F32Type | F64Type, F64Type -> true
    | TyName t1, TyName t2 -> compare_tynames t1 t2
    | _ -> false
  in
  let rec compare_val_type_list s1 s2 =
    match s1, s2 with
    | [], [] -> true
    | v1::t1, v2::t2 ->
      if compare_val_types v1 v2
      then compare_val_type_list t1 t2
      else false
  in
  let (st1', st2') = truncate_stacks bt1.stack bt2.stack in
  compare_val_type_list st1' st2'
;;

type elem_type = AnyFuncType
type func_type = FuncType of stack_type * stack_type

let tyname_to_primitive (tyname: ty_name) : value_type =
  match tyname with
  | TyAtom "I32Type" -> I32Type
  | TyAtom "I64Type" -> I64Type
  | TyAtom "F32Type" -> F32Type
  | TyAtom "F64Type" -> F64Type
  | _ -> TyName tyname
;;

let tyname_to_func_type (tyname: ty_name) : func_type =
  match tyname with
  | TyFunc ("Func", [TyAtom f; TyFunc ("Args", args); TyFunc ("Ret", ret)]) ->
    let vargs = List.map tyname_to_primitive args in
    let vret = List.map tyname_to_primitive ret in
    FuncType (vargs, vret)

  | _ -> failwith "tyname_to_func_type: tyname is not a function type"
;;

type 'a limits = {min : 'a; max : 'a option}
type mutability = Immutable | Mutable
type table_type = TableType of Int32.t limits * elem_type
type memory_type = MemoryType of Int32.t limits
type global_type = GlobalType of value_type * mutability
type extern_type =
  | ExternFuncType of func_type
  | ExternTableType of table_type
  | ExternMemoryType of memory_type
  | ExternGlobalType of global_type


(* Attributes *)

let size = function
  | I32Type | F32Type -> 4
  | I64Type | F64Type -> 8


(* Subtyping *)

let match_limits lim1 lim2 =
  I32.ge_u lim1.min lim2.min &&
  match lim1.max, lim2.max with
  | _, None -> true
  | None, Some _ -> false
  | Some i, Some j -> I32.le_u i j

let match_func_type ft1 ft2 =
  ft1 = ft2

let match_table_type (TableType (lim1, et1)) (TableType (lim2, et2)) =
  et1 = et2 && match_limits lim1 lim2

let match_memory_type (MemoryType lim1) (MemoryType lim2) =
  match_limits lim1 lim2

let match_global_type gt1 gt2 =
  gt1 = gt2

let match_extern_type et1 et2 =
  match et1, et2 with
  | ExternFuncType ft1, ExternFuncType ft2 -> match_func_type ft1 ft2
  | ExternTableType tt1, ExternTableType tt2 -> match_table_type tt1 tt2
  | ExternMemoryType mt1, ExternMemoryType mt2 -> match_memory_type mt1 mt2
  | ExternGlobalType gt1, ExternGlobalType gt2 -> match_global_type gt1 gt2
  | _, _ -> false


(* Filters *)

let funcs =
  Lib.List.map_filter (function ExternFuncType t -> Some t | _ -> None)
let tables =
  Lib.List.map_filter (function ExternTableType t -> Some t | _ -> None)
let memories =
  Lib.List.map_filter (function ExternMemoryType t -> Some t | _ -> None)
let globals =
  Lib.List.map_filter (function ExternGlobalType t -> Some t | _ -> None)


(* String conversion *)

let string_of_constr = function
  | Subtype (v, a) -> Printf.sprintf "%d << %s" v a

let rec string_of_tyname = function
  | TyVar v -> string_of_int v
  | TyAtom a -> a
  | TyFunc (f, args) -> 
    let str_args = args |> List.map string_of_tyname |> String.concat "," in
    Printf.sprintf "%s(%s)" f str_args
  | TyConstr (constrs, constr_tyname) ->
    let str_constrs = constrs |> List.map string_of_constr |> String.concat "," in
    let str_constr_tyname = string_of_tyname constr_tyname in
    Printf.sprintf "%s.%s" str_constrs str_constr_tyname

let string_of_value_type = function
  | I32Type -> "i32"
  | I64Type -> "i64"
  | F32Type -> "f32"
  | F64Type -> "f64"
  | TyName tyname -> string_of_tyname tyname

let string_of_value_types = function
  | [t] -> string_of_value_type t
  | ts -> "[" ^ String.concat " " (List.map string_of_value_type ts) ^ "]"

let string_of_elem_type = function
  | AnyFuncType -> "anyfunc"

let string_of_limits {min; max} =
  I32.to_string_u min ^
  (match max with None -> "" | Some n -> " " ^ I32.to_string_u n)

let string_of_memory_type = function
  | MemoryType lim -> string_of_limits lim

let string_of_table_type = function
  | TableType (lim, t) -> string_of_limits lim ^ " " ^ string_of_elem_type t

let string_of_global_type = function
  | GlobalType (t, Immutable) -> string_of_value_type t
  | GlobalType (t, Mutable) -> "(mut " ^ string_of_value_type t ^ ")"

let string_of_stack_type ts =
  "[" ^ String.concat " " (List.map string_of_value_type ts) ^ "]"

let string_of_block_type b =
  let str_constrs = b.constrs |> List.map string_of_constr |> String.concat "," in
  Printf.sprintf "%s.%s" str_constrs (string_of_stack_type b.stack)

let string_of_func_type (FuncType (ins, out)) =
  string_of_stack_type ins ^ " -> " ^ string_of_stack_type out

let string_of_extern_type = function
  | ExternFuncType ft -> "func " ^ string_of_func_type ft
  | ExternTableType tt -> "table " ^ string_of_table_type tt
  | ExternMemoryType mt -> "memory " ^ string_of_memory_type mt
  | ExternGlobalType gt -> "global " ^ string_of_global_type gt
