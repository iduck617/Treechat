open Core.Std
open Core_kernel
open Str
open Async.Std
open BPlusTree

(* types used for our chat server *)
type inet = Async_extra.Import.Socket.Address.Inet.t 
type client = {addr:inet; r:Reader.t; w:Writer.t}
type client_usr_list = (client * string) list 
type client_chat_list = (int * (string * string)) list 

(* initializations and definitions *)
let bTree = ref Hole
let client_usr_list_ref = ref []
let client_chat_list_ref = ref []
let session_id = ref 0
let split_spaces = Str.regexp "[ \t]+"

(* tail recursive helper function that inverts an association list *)
let rec inverse acc lst =
  match lst with 
  | [] -> acc 
  | (k,v)::t -> inverse (List.rev_append [(v,k)] acc) t

(* helper function that returns true if username is already
 * in use. *)
let usr_mem usr : bool =  
  let usr_lst = inverse [] !client_usr_list_ref in 
  List.Assoc.mem usr_lst usr 

(* find_username takes in a client and returns its username *)
let find_username client = 
  let new_list = List.map !client_usr_list_ref ~f:(fun (client,s) -> (client.addr,s)) in 
  match List.Assoc.find new_list client.addr with 
  | Some value -> value
  | None -> failwith "Invalid key"

(* find_sid takes in a client-client pair and returns their session id *)
let find_sid assoc_list tuple = 
  match List.Assoc.find assoc_list tuple with 
  | Some value -> value
  | None -> failwith "Invalid key"

(* recursive helper function that writes a list out one item
 * at a time *)
let rec write_lst client = function
  | [] -> Writer.write_line client.w "~OGMACO~"  
  | h::t -> 
    let () = Writer.write_line client.w h in
    Writer.flushed client.w >>> fun () -> 
    write_lst client t

(* accepts new user or asks client for unique username *)
let handle_new_user s client = 
  let delim = ' ' in 
  let s_lst = String.split s ~on:delim in
  match s_lst with 
  | u::usr::[] -> 
    if usr_mem usr then
      Writer.write_line client.w "Reprompt"
    else 
      client_usr_list_ref := List.rev_append !client_usr_list_ref [(client,usr)];
      Writer.write_line client.w "None"
  | _ -> failwith "Invalid username" 
    
(* send back list of users that have connected to server *)
let handle_list_command client = 
  let usr_lst = List.rev_map !client_usr_list_ref ~f:(fun (_,u) -> u) in 
  write_lst client usr_lst 

(* helper function that checks to see if there already exists a chat history
 * between the client and the given usr.
 * Returns: Some (usr1,usr2) if a history exists, and None otherwise. *)
let chat_exists client usr : (string * string) option = 
  match find_username client with 
  | client_usr -> 
    let chat_lst = inverse [] !client_chat_list_ref in 
    if List.Assoc.mem chat_lst (client_usr,usr) then 
      Some (client_usr,usr)
    else if List.Assoc.mem chat_lst (usr,client_usr) then
      Some (usr,client_usr)
    else 
      None
  
(* helper function that takes in a client and session id and returns 
 * the chat history associated with the session id to the client *)
let write_history client session_id = 
  match return_chat !bTree session_id with 
    | Some history_lst -> write_lst client history_lst
    | None -> failwith "No Chat History"

(* add message to correct session in BPlusTree, send back chat history *)
let handle_send_command s client = 
  (* see if this is part of an existing chat *)
  let delim = ' ' in 
  let s_lst = Str.bounded_split split_spaces s 3 in
  match s_lst with 
  | s::usr::msg::[] -> 
    begin
    (* get sender's username *)
    let client_usr = find_username client in
    let msg' = client_usr ^ ": " ^ msg in
    match chat_exists client usr with
    (* append msg to chat history (add_message session_id string) *)
    | Some (usr1,usr2) -> 
      (* get session id *)
      let clients_sid_list = inverse [] !client_chat_list_ref in 
      let s_id = find_sid clients_sid_list (usr1,usr2) in 

      (* add message to BPlusTree *)
      let () = bTree := add_message !bTree s_id [msg'] in

      (* write full chat history to sender *)
      write_history client s_id 

    (* create new session_id and add to BPlusTree *) 
    | None -> 
      (* create new session id *)
      session_id := !session_id + 1;
      let s_id = !session_id in 
      (* add (session_id, (client,client)) to chat list *)
      let () = client_chat_list_ref := List.rev_append [(s_id,(client_usr,usr))] !client_chat_list_ref in 

      (* add message to BPlusTree *)
      let () = bTree := add_message !bTree s_id [msg'] in
      
      (* write full chat history to sender *)
      write_history client s_id
    end
  | _ -> failwith "Invalid send command"

(* don't do anything *)
let handle_unknown_command () = ()

let parse_input s client : unit = 
  let lower_s = Core_string.lowercase s in 
  let first_char = Core_string.get lower_s 0 in 
  match first_char with
  | 'u' -> print_endline "new user"; handle_new_user s client 
  | 'l' -> print_endline "list connections"; handle_list_command client 
  | 's' -> print_endline "send message"; handle_send_command s client 
  | _ -> print_endline "unknown command"; handle_unknown_command ()
 
let run () =
  let handler addr r w =  
    (* check if this is already a registered client*)
    let client = {addr = addr; r = r; w = w} in
    let rec loop client : unit Deferred.t = 
      Reader.read_line client.r >>= fun res ->
        match res with 
        | `Eof -> return ()
        | `Ok line -> 
          if line = "" then 
            loop client
          else
            let () = parse_input line client in
            Writer.flushed w >>= fun () ->
            loop client
    in
    loop client
  in 
  let _ = 
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port 3110)
      handler in 
  Deferred.never ()

let _ = run () >>= fun () -> exit 0
let _ = Scheduler.go ()
