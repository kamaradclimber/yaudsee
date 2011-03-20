let get_my_addr () =
    (Unix.gethostbyname (Unix.gethostname()) ).Unix.h_addr_list.(0) ;;


let main_server  host serv_fun =
    try
        let port =  2203 in 
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
        let r =
            if s= "are you one of those ?"  then  "i might\n" else "you must have made a mistake\n" in 
        output_string oc r ; flush oc
         
    done
    with End_of_file -> Printf.printf "I am discovered !\n"; flush stdout; exit
    0;;

let go chan host=
    Unix.handle_unix_error (main_server host) chan ;;


let gen_num = let c = ref 0 in (fun () -> incr c; !c) ;;
exception Fin ;;

class connexion (sd,sa) = 
   object (self) 
     val s_descr = sd
     val s_addr = sa
     val mutable numero = 0
     initializer 
       numero <- gen_num();
       Printf.printf "TRACE.connexion : objet traitant %d créé\n" numero ;
       print_newline()
 
     method start () =  Thread.create (fun x -> self#run x ; self#stop x) ()
 
     method stop() = 
       Printf.printf "TRACE.connexion : fin objet traitant %d\n" numero ;
       print_newline () ;
       Unix.close s_descr
 
    method run () = 
      try 
        while true do
          let ligne =  my_input_line s_descr 
          in if (ligne = "") or (ligne = "\013") then raise Fin ;
             let result = "Je ne fais rien"^"\n"
             in ignore (ThreadUnix.write s_descr result 0 (String.length result))
        done
      with  
         Fin  -> () 
       | exn  -> print_string (Printexc.to_string exn) ; print_newline() 
   end ;;

class server addr p = 
   object (self)
     val port = p 
     val addr = addr
     val mutable sock = ThreadUnix.socket Unix.PF_INET Unix.SOCK_STREAM 0
 
     initializer 
         Unix.bind sock (Unix.ADDR_INET(addr,port)) ;
         Unix.listen sock 3
    
     method private client_addr = function 
         Unix.ADDR_INET(host,_) -> Unix.string_of_inet_addr host
       | _ -> "Unexpected client"
 
     method run () = 
       while(true) do 
         let (sd,sa) = ThreadUnix.accept sock in 
         let connexion = new connexion(sd,sa) 
         in Printf.printf "TRACE.serv: nouvelle connexion de %s\n\n"
                          (self#client_addr sa) ;
         ignore (connexion#start ())
       done
   end ;;



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



let _ = new server "192.168.0."


(*go discovery_channel !host;;*)
Printf.printf "Service lancé\n"; flush stdout;;
