let get_my_addr () =
    (Unix.gethostbyname (Unix.gethostname()) ).Unix.h_addr_list.(0) ;;


let main_server  host serv_fun =
    try
        let port =  80 in 
        let my_address = match host with None -> get_my_addr () | Some h ->
            Unix.inet_addr_of_string h in
        Printf.printf "Listening on %s:%i\n" (Unix.string_of_inet_addr
        my_address ) port; flush stdout;
        Unix.establish_server serv_fun (Unix.ADDR_INET(my_address, port))
        with
        Failure("int_of_string") -> Printf.eprintf "serv_up : bad port number\n"
;;

let uppercase_channel ic oc =
    try while true do    
        let s = input_line ic in 
        let r = String.uppercase s 
        in output_string oc (r^"\n") ; flush oc
    done
    with _ -> Printf.printf "End of text\n" ; flush stdout ; exit 0 ;;


let discovery_channel ic oc=
    try while true do
        let s = input_line ic in
        Printf.printf "je viens de recevoir : \"%s\"\n" s;flush stdout;
        let r =
            if s= "are you one of those ?"  then  "i might\n" else "you must have made a mistake\n" in 
        output_string oc r ; flush oc
         
    done
    with End_of_file -> Printf.printf "I am discovered !\n"; flush stdout; exit
    0;;

let go chan host=
    Unix.handle_unix_error (main_server host) chan ;;




(* Parsing of the command line*)
let host = ref None
;;
let speclist= 
    [
        ("-h",Arg.String (fun arg-> host:=Some arg )," : server -h 4.8.15.16 will
        bound the server on this address" );
        ("-d",Arg.Unit (fun () -> ()), " : server -d will do nothing...\n");
    ] 
        in
        Arg.parse speclist (fun (_:string)->()) "Some options:";;





go discovery_channel !host;;
Printf.printf "Service lancé\n"; flush stdout;;
