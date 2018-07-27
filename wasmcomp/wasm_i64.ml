(* WebAssembly-compatible i64 implementation *)

module Int = Wasm_int

include Int.Make
  (struct
    include Int64
    let bitwidth = 64
  end)
