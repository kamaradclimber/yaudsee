


module Server = struct
    (** gère le serveur *)
let get_my_addr ()=
(Unix.gethostbyname (Unix.gethostname()) ).Unix.h_addr_list.(0) ;;

let main_server  host port serv_fun =
    (** la fonction principale du serveur*)
    try
        let my_address = match host with 
        |None -> get_my_addr () (*TODO utiliser la fonction get_my_ips du module
        Utils qui est defini dans le code du client*)
        | Some h -> Unix.inet_addr_of_string h in
        Printf.printf "Listening on %s:%i\n" (Unix.string_of_inet_addr my_address ) port; flush stdout;
        Unix.establish_server serv_fun (Unix.ADDR_INET(my_address, port))
        with
        Failure("int_of_string") -> Printf.eprintf "serv_up : bad port number\n"
;;

let go chan host port=
    Unix.handle_unix_error (main_server host port) chan ;;


class handler matching_pattern fonction=
    (** Cette classe represente tous les handlers 
    object (self) 
        val f = fonction
        method 

    end


end;;


let uppercase_channel ic oc =
    (** La fonction de test qui met en majuscule les termes envoyés*)
    try while true do    
        let s = input_line ic in
        output_string oc ((String.uppercase s)^"\n") ;
        output_string oc ((String.lowercase s)^"\n") ;
        output_string oc ("END\n") ; flush oc
    done
    with _ -> Printf.printf "End of text\n" ; flush stdout ; exit 0 ;;


let discovery_channel ic oc=
    (* la fonction qui répond aux requetes de decouvertes envoyées pas les bons  client*)
    try while true do
        let s = input_line ic in
        Printf.printf "je viens de recevoir : \"%s\"\n" s;flush stdout;
        let r =
            if s= "are you one of those ?"  then  "i might\n" else "you must have made a mistake\n" in 
        output_string oc r ; flush oc
         
    done
    with End_of_file -> Printf.printf "I am discovered !\n"; flush stdout; exit
    0;;




(* Parsing of the command line*)
let port = ref 2203;;
let host = ref None
;;
let speclist= 
    [
        ("-h",Arg.String (fun arg-> host:=Some arg )," : server -h 4.8.15.16 will
        bound the server on this address" );
        ("-p",Arg.String (fun arg-> port:=int_of_string arg )," : server -p 2203 will
        bound the server on the port 2203 (default : 2203)" );
        ("-d",Arg.Unit (fun () -> ()), " : server -d will do nothing...\n");
    ] 
        in
        Arg.parse speclist (fun (_:string)->()) "Some options:";;




Server.go discovery_channel !host !port;;
Printf.printf "Service lancé\n"; flush stdout;;
