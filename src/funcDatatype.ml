open Cil

let f file =  Cil.dumpFile defaultCilPrinter (stdout) "" (Frontc.parse file ())