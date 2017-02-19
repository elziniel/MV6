(**************************************************************)
(* Language Tyrme: expr, lexing and parsing                   *)
(**************************************************************)

(* La definition de l'arbre syntactique des expression Tyrme se trouve
   dans ast.mli *)
open Ast


(* fonction de parsing: prends une expression de Tyrme et retourne
   l'arbre syntactique *)
let parse (s : string) : expr = Parser.main Lexer.token (Lexing.from_string s)


(**************************************************************)
(* Instructions of the MV                                     *)
(**************************************************************)

type instr =
| Halt
| Push
| Print
| Apply
| Acc of int
| Const of int
| Return of int
| Pop of int
| Branchif of int
| Branch of int
| Getblock of int
| Makeblock of int * int
| Closure of int * int
| Binop of int
| Str of string
| Add
| Sub
| Mul
| Div
| Eqi
| Cat


let string_of_instr : instr -> string = function
  | Halt -> "Halt"
  | Push -> "Push"
  | Print -> "Print"
  | Apply -> "Apply"
  | Acc n -> "Acc " ^ (string_of_int n)
  | Const n -> "Const " ^ (string_of_int n)
  | Return n -> "Return " ^ (string_of_int n)
  | Pop n -> "Pop " ^ (string_of_int n)
  | Branchif o -> "Branchif " ^ (string_of_int o)
  | Branch o -> "Branch " ^ (string_of_int o)
  | Getblock n -> "Getblock " ^ (string_of_int n)
  | Makeblock (t, n) -> "Makeblock (" ^ (string_of_int t) ^ ", " ^ (string_of_int n) ^ ")"
  | Closure (n, o) -> "Closure (" ^ (string_of_int n) ^ ", " ^ (string_of_int o) ^ ")"
  | Binop b -> "Binop " ^ (string_of_int b)
  | Str s -> "Str " ^ s
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Eqi -> "Eqi"
  | Cat -> "Cat"


(**************************************************************)
(* Asm                                                        *)
(**************************************************************)


(* Fonctions de lecture et d'ecriture d'entier 8bits et 32bits *)
let out_i8  (buf : out_channel) (i : int) : unit = output_char buf (char_of_int i)
let out_i32 (buf : out_channel) (i : int) : unit = output_binary_int buf i 

let in_i8   (buf : in_channel) : int = int_of_char (input_char buf)
let in_i32  (buf : in_channel) : int = input_binary_int buf


(* Fonction d'assemblage d'instruction *)
let assemble_instr (buf : out_channel) : instr -> unit = function
  | Halt -> out_i8 buf 0
  | Push -> out_i8 buf 1
  | Print -> out_i8 buf 2
  | Apply -> out_i8 buf 3
  | Acc n -> out_i8 buf 4; out_i32 buf n
  | Const n -> out_i8 buf 5; out_i32 buf n
  | Return n -> out_i8 buf 6; out_i32 buf n
  | Pop n -> out_i8 buf 7; out_i32 buf n
  | Branchif o -> out_i8 buf 8; out_i32 buf o
  | Branch o -> out_i8 buf 9; out_i32 buf o
  | Getblock n -> out_i8 buf 10; out_i32 buf n
  | Makeblock (t, n) -> out_i8 buf 11; out_i8 buf t; out_i32 buf n
  | Closure (n, o) -> out_i8 buf 12; out_i32 buf n; out_i32 buf o
  | Binop b -> out_i8 buf 13; out_i8 buf b
  | Str s -> out_i8 buf 14; out_i32 buf (String.length s);
    for i = 0 to String.length s - 1 do
      out_i8 buf (Char.code s.[i])
    done
  | Add -> out_i8 buf 15
  | Sub -> out_i8 buf 16
  | Mul -> out_i8 buf 17
  | Div -> out_i8 buf 18
  | Eqi -> out_i8 buf 19
  | Cat -> out_i8 buf 20


(* Fonction d'assemblage d'une liste d'instructions *)
let rec assemble (buf : out_channel) : instr list -> unit = function
  | [] -> ()
  | i::il -> assemble_instr buf i; assemble buf il


(* Ecrite pour vous: une fonction d'assemblage qui ecrit dans un fichier *)
let assemble_filename (name : string) (is : instr list) : unit = 
  let buf = open_out_bin name in
  begin
    assemble buf is;
    close_out buf
  end


(* fonction de desassemblage: stub *)
let rec disassemble (buf : in_channel) : instr list =
  (* Get the next char, and make sure to capture the end of the file *)
  let inc = (try Some (in_i8 buf) with | End_of_file -> None) in
  (* Test if there were a char *)
  match inc with
  | None -> []  (* Nope: end of the file *)
  | Some c ->     (* Yep ! Carry on *)
    let tmp = match c with
      | 0 -> Halt
      | 1 -> Push
      | 2 -> Print
      | 3 -> Apply
      | 4 -> let n = in_i32 buf in Acc (n)
      | 5 -> let n = in_i32 buf in Const (n)
      | 6 -> let n = in_i32 buf in Return (n)
      | 7 -> let n = in_i32 buf in Pop (n)
      | 8 -> let o = in_i32 buf in Branchif (o)
      | 9 -> let o = in_i32 buf in Branch (o)
      | 10 -> let n = in_i32 buf in Getblock (n)
      | 11 -> let t = in_i8 buf in let n = in_i32 buf in Makeblock (t, n)
      | 12 -> let n = in_i32 buf in let o = in_i32 buf in Closure (n, o) 
      | 13 -> let b = in_i8 buf in Binop (b)
      | 14 -> let s = String.make (in_i32 buf) 'a' in
	      for i = 0 to String.length s - 1 do
		s.[i] <- char_of_int (in_i8 buf)
	      done; Str s
      | 15 -> Add
      | 16 -> Sub
      | 17 -> Mul
      | 18 -> Div
      | 19 -> Eqi
      | 20 -> Cat
      | _ -> failwith "invalid byte-code"
    in tmp::disassemble buf


(* Ecrite pour vous: une fonction de desassemblage qui lit d'un fichier *)
let disassemble_filename (name : string) : instr list = 
  let buf = open_in_bin name in
  let insts = disassemble buf in
  let _ = close_in buf in
  insts


(**************************************************************)
(* Machine virtuelle                                          *)
(**************************************************************)

type tag = int

type mot = 
  | MotInt of int
  | PointString of string
  | PointBloc of (tag * (mot list))


let rec string_of_mot : mot -> string = function
  | MotInt i -> string_of_int i
  | PointString s -> s
  | PointBloc (t, l) ->
    let rec string_of_mot_aux l = match l with
      | [] -> ""
      | (MotInt i)::l2 -> string_of_int i ^ " " ^ string_of_mot_aux l2
      | (PointString s)::l2 -> s ^ " " ^ string_of_mot_aux l2
      | (PointBloc (t, l3))::l2 -> "(" ^ (string_of_int t) ^ ", [ " ^ string_of_mot_aux l3 ^ "]) " ^ string_of_mot_aux l2
    in "(" ^ (string_of_int t) ^ ", [ " ^ string_of_mot_aux l ^ "])"


type mv_state = {
  mutable acc: mot;
  code: instr list;
  mutable pc: int;
  stack: mot array;
  mutable sp: int
}


(* retourne l'accumulateur de l'etat donne en argument *)
let get_acc (s : mv_state) : mot = s.acc


(* Pour construire l'etat de la machine au debut de l'execution *)
let init (c : instr list) : mv_state = {
  code = c;
  stack = Array.make 1024 (MotInt 42);
  pc = 0;
  sp = -1;
  acc = MotInt 52
}


(* Peut-etre une fonction d'impression ? *)
let print_state (s : mv_state) : unit =
  print_string ("Stack:\n");
  (if (s.sp < 0) 
   then print_string "<empty>\n"
   else for i = 0 to s.sp do
       print_string ("#" ^ (string_of_int (s.sp - i)) ^ " -> " 
                     ^ (string_of_mot s.stack.(i)) ^ "\n")
     done);
  print_string ("Acc = " ^ (string_of_mot s.acc) ^ "\n");
  print_string ("PC = " ^ (string_of_int s.pc) ^ "\n\n")


let rec sublist (tab1:mot array) n (tab2:mot list) i = match n with
  | 0 -> tab2
  | _ -> sublist tab1 (n-1) (tab2@[tab1.(i)]) (i-1)


let list_of_mot = function
  | MotInt i -> failwith "MotInt"
  | PointString s -> failwith "PointString"
  | PointBloc (t, l) -> l


(* La fonction d'execution de la machine *)
let machine (s : mv_state) : mv_state =
  let idx = ref 0 in
  begin
    while s.pc < List.length s.code do
      idx := !idx + 1;
      print_string ("=== Step " ^ (string_of_int !idx) ^ " ===\n");
      print_string ("With PC = " ^ (string_of_int s.pc) ^ " (" ^ (string_of_instr (List.nth s.code s.pc)) ^ ")\n");
      begin match List.nth s.code s.pc with
      | Halt ->
	s.pc <- List.length s.code
      | Push ->
        s.sp <- s.sp + 1;
        s.stack.(s.sp) <- s.acc
      | Print ->
	print_string (string_of_mot s.acc)
      | Apply ->
	let s2 = s.stack.(s.sp) in
        s.stack.(s.sp) <- MotInt (s.pc + 1);
	let l = list_of_mot s.acc in
	s.pc <- (int_of_string (string_of_mot (List.nth l 0)) - 1);
        let rec apply_aux i = match i with
	  | 1 ->
	    s.sp <- s.sp + 1;
	    s.stack.(s.sp) <- (List.nth l 1)
	  | x ->
	    s.sp <- s.sp + 1;
	    s.stack.(s.sp) <- (List.nth l x);
	    apply_aux (i - 1)
	in apply_aux ((List.length l) - 1);
	s.sp <- s.sp + 1;
        s.stack.(s.sp) <- s2
      | Acc n ->
	s.acc <- s.stack.(s.sp - n)
      | Const n ->
        s.acc <- MotInt n
      | Return n ->
	s.sp <- s.sp - n;
	s.pc <- int_of_string (string_of_mot s.stack.(s.sp)) - 1;
	s.sp <- s.sp - 1
      | Pop n ->
        s.sp <- s.sp - n
      | Branchif o -> 
        s.pc <- s.pc + (if (s.acc != MotInt 0) then o - 1 else 0)
      | Branch o ->
        s.pc <- s.pc + o - 1
      | Getblock n ->
	s.acc <- let l = list_of_mot s.acc in
		 List.nth l n
      | Makeblock (t, n) ->
	s.acc <- PointBloc (t, sublist s.stack n [] s.sp);
        s.sp <- s.sp - n
      | Closure (n, o) ->
        s.acc <- PointBloc (88, sublist s.stack n [MotInt (s.pc + o)] s.sp)
      | Binop b -> begin match b with
	| 15 ->
          s.acc <- MotInt (int_of_string (string_of_mot s.stack.(s.sp)) + (int_of_string (string_of_mot s.acc)))
	| 16 ->
          s.acc <- MotInt (int_of_string (string_of_mot s.stack.(s.sp)) - (int_of_string (string_of_mot s.acc)))
	| 17 ->
          s.acc <- MotInt (int_of_string (string_of_mot s.stack.(s.sp)) * (int_of_string (string_of_mot s.acc)))
	| 18 ->
          s.acc <- MotInt (int_of_string (string_of_mot s.stack.(s.sp)) / (int_of_string (string_of_mot s.acc)))
	| 19 ->
          s.acc <- if s.stack.(s.sp) = s.acc then MotInt 1 else MotInt 0
	| 20 ->
          s.acc <- PointString ((string_of_mot s.stack.(s.sp)) ^ (string_of_mot s.acc))
	| _ -> failwith "invalid opcode"
      end; s.sp <- s.sp - 1
      | Str s2 ->
	s.acc <- PointString s2
      | _ -> failwith "Something is wrong with you"
      end;
      s.pc <- s.pc + 1;
      print_string ("New state is\n");
      print_state s;
    done;
    print_string ("\n" ^ (string_of_int !idx) ^ " steps in total\n\n")
  end; s


(* La fonction d'evaluation: retourne l'accumulateur a la fin de l'evaluation *)
let eval (c : instr list) : mot =
  let s = machine (init c) in get_acc s


(**************************************************************)
(* Compilation                                                *)
(**************************************************************)

(* Langage Tyrme *)
type value =
| Int of int
| Bool of bool
| String of string
| Unit

type binop = Add | Eq | And | Cat | App

type var = string

type expr =
| Const of value
| Binop of binop * expr * expr
| If of expr * expr * expr
| Let of var * expr * expr
| Letf of var * expr * expr
| Print of expr * expr
| Pair of expr * expr
| Fst of expr
| Snd of expr


(* Environnement *)
type env = (var * value) list

let empty_env = []


(* Interpretation *)
let rec interp : env * expr -> value = function
  | env, Const v -> v
  | env, Binop (b, e1, e2) ->
    (match b, interp (env, e1), interp (env, e2) with
    | Add, Int i, Int j -> Int (i + j)
    | Eq, Int i, Int j -> Bool (i = j)
    | And, Bool i, Bool j -> Bool (i && j)
    | Cat, String i, String j -> String (i ^ j)
    | App, String i, String j -> String (i ^ j)
    | _ -> failwith "type error")
  | env, If (e1, e2, e3) ->
    (match interp (env, e1) with
    | Bool true -> interp (env, e2)
    | Bool false -> interp (env, e3)
    | _ -> failwith "type error")
  | env, Let (s, e2, e3) ->
    let r = interp (env, e2) in
    let new_env = (s, r) :: env in
    interp (new_env, e3)

let repr : value -> int = function
  | Int i -> i
  | Bool true  -> 1
  | Bool false -> 0
  | _ -> failwith "type error"

let op : binop -> instr = function
  | Add -> Binop 15
  | Eq -> Binop 19
  | And -> Binop 15
  | Cat -> Binop 20
  | App -> Binop 15


let succ s = let c, v = s in (c, v + 1)

let env_succ e = List.map succ e

(* La fonction de compilation *)
let rec compil : env * expr -> instr list = function
  | env, Const v ->
    [Const (repr v)]
  | env, Binop (o, e1, e2) ->
    compil (env, e1) @
      [Push] @
      compil (env_succ env, e2) @
      [op o; Pop 1]
  | env, If (e1, e2, e3) ->
    let i2 = compil (env, e2) in
    let i3 = compil (env, e3) in
    compil (env, e1) @
      [Branchif (2 + List.length i3)] @
      i3 @
      [Branch (1 + List.length i2)] @
      i2
  | env, Let (v, e1, e2) ->
    let new_env = (v, 0) :: (List.map succ env) in
    compil (env, e1) @
      [Push] @
      compil (new_env, e2) @
      [Pop]
  | env, Letf (v, e1, e2) ->
    failwith "Letf"
  | env, Print (e1, e2) ->
    [Print e1] @
      compil (env, e2)
  | env, Pair (e1, e2) ->
    compil (env, e1) @
      [Push] @
      compil (env, e2) @
      [Push; Makeblock (80, 2)]
  | env, Fst e ->
    [Getblock 0]
  | env, Snd e ->
    [Getblock 1]





(* Pour lire le codex *)
let lire_codex () = 
  print_string (string_of_mot (eval (disassemble_filename "codex.tm")))
               
               
(* Exemple de compilation qui doit marcher et rendre la valeur 3 *)
let ex_compil () =
  print_string (string_of_mot (eval (compil (empty_env, parse "let x = 1 in x + 2"))));;