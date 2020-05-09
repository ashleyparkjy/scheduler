open Lwt
open Cohttp
open Cohttp_lwt_unix

exception BadUrl of int

let make_url semester subject class_num = 
  "https://classes.cornell.edu/api/2.0/search/classes.json?roster=" ^ semester ^ "&subject=" ^ subject ^ "&q=" ^ class_num

let get_json url =
  Client.get (Uri.of_string url) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  if code = 200 then
    Cohttp_lwt.Body.to_string body
  else raise(BadUrl code)

let runner semester subject class_num =
  let my_file = semester ^ "_" ^ subject ^ "_" ^ class_num ^ ".json" in
  let url = make_url semester subject class_num in
  let body = url |> get_json |> Lwt_main.run in
  let oc = open_out my_file in
  Printf.fprintf oc "%s\n" body;
  close_out oc;
  ()