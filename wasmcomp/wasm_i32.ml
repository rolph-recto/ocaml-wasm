(* WebAssembly-compatible i32 implementation *)

module Int = Wasm_int

include Int.Make
  (struct
    include Int32
    let bitwidth = 32
  end)
