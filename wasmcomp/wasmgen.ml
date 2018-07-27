(*  wasmgen.ml : transation of lambda terms into webassembly *)

module WAst = Wasm_ast
module WValues = Wasm_values
module WTypes = Wasm_types

open Lambda
open Wasm_source

let compile_structured_constant sc = 
  match sc with
  | Const_base (Const_int i) ->
    WAst.Const (WValues.I32Value.to_value (Int32.of_int i) @@ no_region) @@ no_region

let compile_primitive prim =
  match prim with
  | Paddint -> [WAst.Binary (WValues.I32 WAst.I32Op.Add) @@ no_region]
  | Psubint -> [WAst.Binary (WValues.I32 WAst.I32Op.Sub) @@ no_region]
  | Pmulint -> [WAst.Binary (WValues.I32 WAst.I32Op.Mul) @@ no_region]
  | _ -> []

let rec compile_expr expr = 
  match expr with
  | Lconst sc -> [compile_structured_constant sc]

  | Lprim (prim, args, _) ->
    let cargs = List.rev args |> List.map compile_expr |> List.flatten in
    let cfunc = compile_primitive prim in
    cargs @ cfunc

  | Lfunction { params = params; body = body } ->
    compile_expr body

  (* don't compile for now ... *)
  | _  -> []
;;

let compile_func var body =
  match body with
  | Lfunction { params = params; body = fbody } ->
    let f = { 
      (* give actual function type later *)
      WAst.ftype = (Int32.of_int (List.length params)) @@ no_region;
      (* set params as ints for now *)
      WAst.locals = List.map (fun _ -> WTypes.I32Type) params;
      WAst.body = compile_expr fbody;
    } in
    f @@ no_region

  | _ ->
    let f = { 
      (* give actual function type later *)
      WAst.ftype = (Int32.of_int 0) @@ no_region;
      (* set params as ints for now *)
      WAst.locals = [];
      WAst.body = compile_expr body;
    } in
    f @@ no_region
;;

let rec frange n f =
  match n with
  | 0 -> []
  | n' -> (f n)::(frange (n-1) f)
;;

let compile_implementation modulename expr =
  let rec extract_bindings f =
    match f with
    | Llet (_, _, var, varval, body) -> (var, varval) :: (extract_bindings body)
    | _ -> []
  in
  match expr with
  | Lprim (Psetglobal _, [bind_expr], _) ->
    let bindings = extract_bindings bind_expr in
    let ofuncs = List.map (fun (var, body) -> compile_func var body) bindings in
    let dummy_func_types =
      let nth_ftype n =
        let args = frange n (fun _ -> WTypes.I32Type) in
        WTypes.FuncType (args, [WTypes.I32Type]) @@ no_region
      in
      List.map nth_ftype [0;1;2;3;4;5]
    in
    { WAst.empty_module with
      types = dummy_func_types; funcs = ofuncs }
    @@ no_region

  | _ -> WAst.empty_module @@ no_region
;;



    
