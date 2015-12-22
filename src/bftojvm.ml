open Printf
let header = "
.class public BF
.super java/lang/Object

.method public <init>()V
    aload_0
    invokenonvirtual java/lang/Object/<init>()V
    return
.end method

.method public static main([Ljava/lang/String;)V
    .limit stack 10
    .limit locals 3

    ldc 15000
    istore_1

    ldc 30000
    newarray int
    astore_2

"
let footer = "
    return
.end method
"
let load = "\taload_2\n\tiload_1\n"

let move i = "\tiinc 1 " ^ i ^ "\n"

let modify i = load ^ "\tdup2
    iaload
    bipush " ^ i ^ "
    iadd
    iastore

"
let write = "
    getstatic java/lang/System/out Ljava/io/PrintStream;
" ^ load ^ "\tiaload 
    i2c
    invokevirtual java/io/PrintStream/print(C)V

"
let read = load ^ "\tgetstatic java/lang/System/in Ljava/io/InputStream;
    invokevirtual java/io/InputStream/read()I
    iastore

"

let loop_start i = "
    loop_" ^ i ^ "_start:
" ^ load ^ "
    iaload
    ifeq loop_" ^ i ^ "_end

"
let loop_end i = "
    goto loop_" ^ i ^ "_start
    loop_" ^ i ^ "_end:

"


type op = 
        | Move of int
        | Modify of int
        | Read
        | Write
        | LoopStart (* TODO Loop of op list *)
        | LoopEnd 

let rec optimize ops  = 
    match ops with 
    | Move (x) :: Move (y) :: tail -> 
                                optimize (Move (x + y) :: tail)
    | Modify (x) :: Modify (y) :: tail -> 
                                optimize (Modify (x + y) :: tail)
    | head :: tail -> head :: (optimize tail)
    | [] -> []






let gen ops =
    let loop = ref 0 in
    let generator s op =
        match op with
            | Move   (x) -> s ^ move (string_of_int x)
            | Modify (x) -> s ^ modify (string_of_int x)
            | Read       -> s ^ read
            | Write      -> s ^ write
            | LoopStart  -> loop := !loop + 1; s ^ (loop_start (string_of_int !loop))
            | LoopEnd    -> loop := !loop - 1; s ^ (loop_end   (string_of_int (!loop + 1)))
    in
    List.fold_left generator "" ops

let parse str =
    let match_op ch = 
        match ch with
        | '>' -> Some( Move(1) )
        | '<' -> Some( Move(-1) )
        | '+' -> Some( Modify(1) )
        | '-' -> Some( Modify(-1) )
        | ',' -> Some( Read )
        | '.' -> Some( Write )
        | '[' -> Some( LoopStart )
        | ']' -> Some( LoopEnd )
        | _ -> None
    in
    let chars = List.rev (Array.to_list(Array.init (String.length str) (String.get str))) in
    List.fold_left
        (fun l x -> match match_op x with
                | None -> l
                | Some y -> y :: l)
            [] chars

let () =
    Printf.printf "%s%s%s" header (gen (optimize (parse (input_line stdin)))) footer
