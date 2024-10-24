open Parser

let _t : identifier = "e"

let file_to_string file =
  let s = really_input_string file (in_channel_length file) in
    close_in file;
    s

let read_all file_name =
  file_to_string (open_in file_name)

let main () =
  let len = (Array.length Sys.argv) in
  let argv = (Array.sub Sys.argv 1 (len-1)) in (* skip argv0 *)
    Array.iter (fun x -> print_string (read_all x)) argv

let _ = main ()
