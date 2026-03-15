(* Minimal JSON parser for genesis file loading.
   Handles: objects, arrays, strings, numbers (int/float), booleans, null.
   No streaming, no error recovery — assumes well-formed input. *)

type t =
  | Object of (string * t) list
  | Array of t list
  | String of string
  | Number of float
  | Bool of bool
  | Null

(* ================================================================ *)
(* Lexer                                                             *)
(* ================================================================ *)

type token =
  | Lbrace | Rbrace | Lbracket | Rbracket
  | Colon | Comma | Tstring of string | Tnumber of float
  | Tbool of bool | Tnull

let is_ws c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

let tokenize s =
  let len = String.length s in
  let pos = ref 0 in
  let tokens = ref [] in
  while !pos < len do
    let c = s.[!pos] in
    if is_ws c then incr pos
    else match c with
    | '{' -> tokens := Lbrace :: !tokens; incr pos
    | '}' -> tokens := Rbrace :: !tokens; incr pos
    | '[' -> tokens := Lbracket :: !tokens; incr pos
    | ']' -> tokens := Rbracket :: !tokens; incr pos
    | ':' -> tokens := Colon :: !tokens; incr pos
    | ',' -> tokens := Comma :: !tokens; incr pos
    | '"' ->
      incr pos;
      let buf = Buffer.create 64 in
      while !pos < len && s.[!pos] <> '"' do
        if s.[!pos] = '\\' && !pos + 1 < len then begin
          incr pos;
          (match s.[!pos] with
           | '"' -> Buffer.add_char buf '"'
           | '\\' -> Buffer.add_char buf '\\'
           | '/' -> Buffer.add_char buf '/'
           | 'n' -> Buffer.add_char buf '\n'
           | 'r' -> Buffer.add_char buf '\r'
           | 't' -> Buffer.add_char buf '\t'
           | c -> Buffer.add_char buf '\\'; Buffer.add_char buf c);
          incr pos
        end else begin
          Buffer.add_char buf s.[!pos]; incr pos
        end
      done;
      if !pos < len then incr pos;
      tokens := Tstring (Buffer.contents buf) :: !tokens
    | 't' when !pos + 3 < len && String.sub s !pos 4 = "true" ->
      tokens := Tbool true :: !tokens; pos := !pos + 4
    | 'f' when !pos + 4 < len && String.sub s !pos 5 = "false" ->
      tokens := Tbool false :: !tokens; pos := !pos + 5
    | 'n' when !pos + 3 < len && String.sub s !pos 4 = "null" ->
      tokens := Tnull :: !tokens; pos := !pos + 4
    | '-' | '0'..'9' ->
      let start = !pos in
      if c = '-' then incr pos;
      while !pos < len && s.[!pos] >= '0' && s.[!pos] <= '9' do incr pos done;
      if !pos < len && s.[!pos] = '.' then begin
        incr pos;
        while !pos < len && s.[!pos] >= '0' && s.[!pos] <= '9' do incr pos done
      end;
      if !pos < len && (s.[!pos] = 'e' || s.[!pos] = 'E') then begin
        incr pos;
        if !pos < len && (s.[!pos] = '+' || s.[!pos] = '-') then incr pos;
        while !pos < len && s.[!pos] >= '0' && s.[!pos] <= '9' do incr pos done
      end;
      let num_str = String.sub s start (!pos - start) in
      tokens := Tnumber (float_of_string num_str) :: !tokens
    | _ -> incr pos
  done;
  List.rev !tokens

(* ================================================================ *)
(* Parser                                                            *)
(* ================================================================ *)

let parse_value tokens =
  let rec value = function
    | Lbrace :: rest -> parse_object [] rest
    | Lbracket :: rest -> parse_array [] rest
    | Tstring s :: rest -> (String s, rest)
    | Tnumber n :: rest -> (Number n, rest)
    | Tbool b :: rest -> (Bool b, rest)
    | Tnull :: rest -> (Null, rest)
    | _ -> (Null, [])
  and parse_object acc = function
    | Rbrace :: rest -> (Object (List.rev acc), rest)
    | Tstring key :: Colon :: rest ->
      let (v, rest) = value rest in
      let acc = (key, v) :: acc in
      (match rest with
       | Comma :: rest -> parse_object acc rest
       | Rbrace :: rest -> (Object (List.rev acc), rest)
       | _ -> (Object (List.rev acc), rest))
    | rest -> (Object (List.rev acc), rest)
  and parse_array acc = function
    | Rbracket :: rest -> (Array (List.rev acc), rest)
    | rest ->
      let (v, rest) = value rest in
      let acc = v :: acc in
      (match rest with
       | Comma :: rest -> parse_array acc rest
       | Rbracket :: rest -> (Array (List.rev acc), rest)
       | _ -> (Array (List.rev acc), rest))
  in
  value tokens

let parse s =
  let tokens = tokenize s in
  let (v, _) = parse_value tokens in
  v

(* ================================================================ *)
(* Accessors                                                         *)
(* ================================================================ *)

let get key = function
  | Object pairs -> List.assoc_opt key pairs
  | _ -> None

let to_string = function String s -> Some s | _ -> None
let to_float = function Number n -> Some n | _ -> None
let to_int = function Number n -> Some (int_of_float n) | _ -> None
let to_int64 = function Number n -> Some (Int64.of_float n) | _ -> None
let to_bool = function Bool b -> Some b | _ -> None

let to_object = function Object pairs -> Some pairs | _ -> None
