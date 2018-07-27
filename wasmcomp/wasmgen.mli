open Lambda
module WAst = Wasm_ast

val compile_implementation : string -> lambda -> WAst.module_

