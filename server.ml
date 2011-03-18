let get_my_addr () =
    (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0) ;;


let main_server  serv_fun =
    try
        let port =  2203 in 
        let my_address = get_my_addr() in
        Printf.printf "ouverture sur %s:%i\n" (Unix.string_of_inet_addr
        my_address ) port; flush stdout;
       Unix.establish_server serv_fun (Unix.ADDR_INET(my_address, port))
    with
    Failure("int_of_string") -> Printf.eprintf "serv_up : bad port number\n"
;;

let uppercase_service ic oc =
    try while true do    
        let s = input_line ic in 
        let r = String.uppercase s 
        in output_string oc (r^"\n") ; flush oc
    done
    with _ -> Printf.printf "End of text\n" ; flush stdout ; exit 0 ;;


let go_uppercase_service () = 
    Unix.handle_unix_error main_server uppercase_service ;;


go_uppercase_service ();;
