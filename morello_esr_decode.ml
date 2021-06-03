let esr = Z.of_string Sys.argv.(1);;
let ec = Z.extract esr 26 8;;
let il = Z.extract esr 25 1;;
let iss = Z.extract esr 0 25;;
Printf.printf "EC %s IL %s ISS %s\n" (Z.format "%08b" ec) (Z.format "%b" il) (Z.format "%025b" iss);;
let instruction_abort () =
  let set = Z.extract esr 11 2 in
  let fnv = Z.extract esr 10 1 in
  let ea = Z.extract esr 9 1 in
  let s1ptw = Z.extract esr 7 1 in
  let ifsc = Z.extract esr 0 6 in
  let _ = Printf.printf "SET %s FnV %s EA %s S1PTW %s IFSC %s \n"
            (Z.format "%02b" set) (Z.format "%b" fnv) (Z.format "%b" ea) (Z.format "%b" s1ptw) (Z.format "%06b" ifsc) in
  print_endline
    (match Z.to_int ifsc with
     | 0b000000 -> "Address size fault, level 0"
     | 0b000001 -> "Address size fault, level 1"
     | 0b000010 -> "Address size fault, level 2"
     | 0b000011 -> "Address size fault, level 3"
     | 0b000100 -> "Translation fault, level 0"
     | 0b000101 -> "Translation fault, level 1"
     | 0b000110 -> "Translation fault, level 2"
     | 0b000111 -> "Translation fault, level 3"
     | 0b001001 -> "Access flag fault, level 1"
     | 0b001010 -> "Access flag fault, level 2"
     | 0b001011 -> "Access flag fault, level 3"
     | 0b001101 -> "Permission fault, level 1"
     | 0b001110 -> "Permission fault, level 2"
     | 0b001111 -> "Permission fault, level 3"
     | 0b010000 -> "Synchronous External abort, not on translation table walk"
     | 0b010100 -> "Synchronous External abort, level 0"
     | 0b010101 -> "Synchronous External abort, level 1"
     | 0b010110 -> "Synchronous External abort, level 2"
     | 0b010111 -> "Synchronous External abort, level 3"
     | 0b011000 -> "Synchronous parity or ECC error, not on translation table walk"
     | 0b011100 -> "Synchronous parity or ECC error, level 0"
     | 0b011101 -> "Synchronous parity or ECC error, level 1"
     | 0b011110 -> "Synchronous parity or ECC error, level 2"
     | 0b011111 -> "Synchronous parity or ECC error, level 3"
     | 0b101000 -> "Capability tag fault"
     | 0b101001 -> "Capability sealed fault"
     | 0b101010 -> "Capability bound fault"
     | 0b101011 -> "Capability permission fault"
     | 0b110000 -> "TLB conflict abort"
     | 0b110001 -> "Unsupported atomic hardware update fault"
     | _ -> "reserved")
;;
let data_abort () =
  let isv = Z.extract esr 24 1 in
  let sas = Z.extract esr 22 2 in
  let sse = Z.extract esr 21 1 in
  let srt = Z.extract esr 16 5 in
  let sf  = Z.extract esr 15 1 in
  let ar  = Z.extract esr 14 1 in
  let set = Z.extract esr 11 2 in
  let fnv = Z.extract esr 10 1 in
  let ea = Z.extract esr 9 1 in
  let cm = Z.extract esr 8 1 in
  let s1ptw = Z.extract esr 7 1 in
  let wnr = Z.extract esr 6 1 in
  let dfsc = Z.extract esr 0 6 in
  let _ = Printf.printf "ISV %s SAS %s SSE %s SRT %s SF %s AR %s SET %s FnV %s EA %s CM %s S1PTW %s WnR %s DFSC %s \n"
            (Z.format "%b" isv) (Z.format "%02b" sas) (Z.format "%b" sse) (Z.format "%05b" srt) (Z.format "%b" sf) (Z.format "%b" ar)
            (Z.format "%02b" set) (Z.format "%b" fnv) (Z.format "%b" ea) (Z.format "%b" cm) (Z.format "%b" s1ptw) (Z.format "%b" wnr) (Z.format "%06b" dfsc)
  in
  print_endline
    (match Z.to_int dfsc with
     | 0b000000 -> "Address size fault, level 0"
     | 0b000001 -> "Address size fault, level 1"
     | 0b000010 -> "Address size fault, level 2"
     | 0b000011 -> "Address size fault, level 3"
     | 0b000100 -> "Translation fault, level 0"
     | 0b000101 -> "Translation fault, level 1"
     | 0b000110 -> "Translation fault, level 2"
     | 0b000111 -> "Translation fault, level 3"
     | 0b001001 -> "Access flag fault, level 1"
     | 0b001010 -> "Access flag fault, level 2"
     | 0b001011 -> "Access flag fault, level 3"
     | 0b001101 -> "Permission fault, level 1"
     | 0b001110 -> "Permission fault, level 2"
     | 0b001111 -> "Permission fault, level 3"
     | 0b010000 -> "Synchronous External abort, not on translation table walk"
     | 0b010001 -> "Synchronous Tag Check fail"
     | 0b010100 -> "Synchronous External abort, level 0"
     | 0b010101 -> "Synchronous External abort, level 1"
     | 0b010110 -> "Synchronous External abort, level 2"
     | 0b010111 -> "Synchronous External abort, level 3"
     | 0b011000 -> "Synchronous parity or ECC error, not on translation table walk"
     | 0b011100 -> "Synchronous parity or ECC error, level 0"
     | 0b011101 -> "Synchronous parity or ECC error, level 1"
     | 0b011110 -> "Synchronous parity or ECC error, level 2"
     | 0b011111 -> "Synchronous parity or ECC error, level 3"
     | 0b100001 -> "Alignment fault"
     | 0b101000 -> "Capability tag fault"
     | 0b101001 -> "Capability sealed fault"
     | 0b101010 -> "Capability bound fault"
     | 0b101011 -> "Capability permission fault"
     | 0b101100 -> "Page table LC or SC permission violation fault"
     | 0b110000 -> "TLB conflict abort"
     | 0b110001 -> "Unsupported atomic hardware update fault"
     | 0b110100 -> "IMPLEMENTATION DEFINED fault (Lockdown)"
     | 0b110101 -> "IMPLEMENTATION DEFINED fault (Unsupported Exclusive or Atomic access)"
     | 0b111101 -> "Section Domain Fault"
     | 0b111110 -> "Page Domain Fault"
     | _ -> "reserved")
;;  
match Z.to_int ec with
| 0b000000 -> print_endline "Unknown reason"
| 0b000001 -> print_endline "Trapped WFI or WFE"
| 0b000011 -> print_endline "Trapped MCR or MRC access"
| 0b000100 -> print_endline "Trapped MCRR or MRRC access"
| 0b000101 -> print_endline "Trapped MCR or MRC access with (cproc==0b1110)"
| 0b000110 -> print_endline "Trapped LDC or STC access"
| 0b000111 -> print_endline "Trapped access to SVE, Advanced SIMD, or floating-point"
| 0b001100 -> print_endline "Trapped MRRC access with (cproc=0b1110)"
| 0b001110 -> print_endline "Illegal Execution state"
| 0b010011 -> print_endline "SMC instruction in AArch32"
| 0b010101 -> print_endline "SVC instruction in AArch64"
| 0b010110 -> print_endline "HVC instruction in AArch64"
| 0b010111 -> print_endline "SMC instruction in AArch64"
| 0b011000 -> print_endline "Trapped MSR, MRS or System instruction in AArch64"
| 0b011001 -> print_endline "Trapped access to SVE"
| 0b011111 -> print_endline "IMPLEMENTATION DEFINED exception to EL3"
| 0b100000 -> (print_endline "Instruction Abort from a lower Exception level"; instruction_abort ())
| 0b100001 -> (print_endline "Instruction Abort taken without change in Exception level"; instruction_abort ())
| 0b100010 -> print_endline "PC alignment fault exception"
| 0b100100 -> (print_endline "Data Abort from a lower Exception level"; data_abort ())
| 0b100101 -> (print_endline "Data Abort taken without change in Exception level"; data_abort ())
| 0b100110 -> print_endline "SP alignment fault exception"
| 0b101001 -> print_endline "Trapped access to Morello"
| 0b101010 -> print_endline "Trapped capability MSR or MRS"
| 0b101100 -> print_endline "Trapped floating-point exception from AArch64"
| 0b101111 -> print_endline "SError interrupt"
| 0b111100 -> print_endline "BRK in AArch64"
| i -> Printf.printf "Reserved for future use %s" (if i < 0x2d then "for synchronous exceptions" else "")
