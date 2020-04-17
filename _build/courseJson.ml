open Lwt
open Cohttp
open Cohttp_lwt_unix

let body subject =
  let my_url =  "https://classes.cornell.edu/api/2.0/search/classes.json?roster=SP20&subject=" ^ subject ^ "&acadCareer[]=UG&classLevels[]=1000" in
  Client.get (Uri.of_string my_url) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  if code =200 then
    begin
      Printf.printf "Response code: %d\n" code;
      ()
    end
  else raise (Failure ("Bad GET with " ^ (string_of_int code)));
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let runner subject =
  let body = Lwt_main.run (body subject) in
  print_endline ("Received body\n" ^ body)