(* This file is a client for Yaudse*)


(* 
 * What does it do ?
 * It will attempt first to find server on the local network 
 * and nothing else for now
 * *)

(* This is the example function of client opening connection and applying a
 * function over it*)
(*this function should disappeer and be replaced by a more geenric fucntion
 * this is copied from
 * http://caml.inria.fr/pub/docs/oreilly-book/html/book-ora187.html
 * *)

let main_client client_fun  =
    if Array.length Sys.argv < 3 
    then Printf.printf "usage :  client server port\n"
    else let server = Sys.argv.(1) in
    let server_addr =
        try  Unix.inet_addr_of_string server 
        with Failure("inet_addr_of_string") -> 
            try  (Unix.gethostbyname server).Unix.h_addr_list.(0) 
            with Not_found ->
                Printf.eprintf "%s : Unknown server\n" server ;
                exit 2
    in try 
        let port = int_of_string (Sys.argv.(2)) in
        let sockaddr = Unix.ADDR_INET(server_addr,port) in 
        let ic,oc = Unix.open_connection sockaddr
        in client_fun ic oc ;
        Unix.shutdown_connection ic
            with Failure("int_of_string") -> Printf.eprintf "bad port number";
            exit 2 ;;

(* The example function applied on the connection*)
let client_fun ic oc = 
    try
        while true do  
            print_string  "Request : " ;
            flush stdout ;
            output_string oc ((input_line stdin)^"\n") ;
            flush oc ;
            let r = input_line ic 
            in Printf.printf "Response : %s\n\n" r;
            if r = "END" then ( Unix.shutdown_connection ic ; raise Exit) ;
        done
    with 
    Exit -> exit 0
     | exn -> Unix.shutdown_connection ic ; raise exn  ;;


class connection addr port=
    object(self : 'a)
        val addr =addr
        val port = port
        val mutable ic = None
        method run (func: 'a -> in_channel -> out_channel -> unit)  =  
            begin
            let server = try Unix.inet_addr_of_string addr  with
            Failure("inet_addr_of_string") -> try (Unix.gethostbyname
            addr).Unix.h_addr_list.(0) with Not_found -> (Printf.eprintf "%s :
                Serveur Inconnu" addr; exit 2)  (*en theorie il faudrait
                ecrire du code qui permette a la connection de signaler que ca
                ne va pas sans planter tout le programme*)
                in
                let sockaddr = Unix.ADDR_INET(server,port) in
                let (icc,oc) = Unix.open_connection sockaddr in
                ic <- Some icc;
                func self icc oc
            end
        method stop () = match ic with None -> () | Some icc ->
            Unix.shutdown_connection icc
    end;;


class ['a]  pool n=
    object(self)
        val size = n
        val pool = Array.make n (None: 'a option)

        method hasFreeSpace () = Array.fold_right (fun el acc -> acc ||
        el=None) pool false
        method private findFreeSpace () = 
            begin 
                assert (self#hasFreeSpace ());
                let i = ref 0 in
                while (pool.(!i) <> None) do incr i done;
                !i
            end
        method put el =
           begin
               assert (self#hasFreeSpace ());
               let freeSpot = self#findFreeSpace () in
               pool.(freeSpot) <- Some el ;
            end
        method rem el=
            begin
                let i = ref 0 in
                while (!i<n && pool.(!i) <> Some el) do incr i done;
                if !i =n then failwith "Not_found" else pool.(!i) <- None
            end
        method private get i= 
            match pool.(i) with 
            |None -> failwith "Nothing_found"
            |Some a -> a
    end;;


class ['a] threadedPool n=
    (** On met dans le pool des threads dobjets qui repondent aux methode
     * run/stop*)
    object(self)
        inherit [Thread.t] pool n as super
        method add (el: 'a) arg = 
            begin
                let t = Thread.create (fun x -> el#run  x; el#stop x) arg in
                super#put t;
                Thread.id t
            end
        method del threadId = 
            begin
                let i = ref 0 in
                while (!i <n && Thread.id (super#get !i) <> threadId) do  incr i done;
                if !i = n then failwith "Not_found" else begin 
                    Thread.kill (super#get !i);
                    super#rem (super#get !i)
                end
            end
    end;;
(** La procédure pour ajouter une nouvelle connection dans le threadedPool est :
    * - de vérfier quil y a de la place avec la methode hasFreeSpace
    * - inserer une connection non demarrée avec add*)





let areYouAYaudseServer server_addr server_port =
(** This function asks a given host if he is a Yaudse server*)

    (* We first try to initialize a connection with the host*)
    let server_inet_addr = try Unix.inet_addr_of_string server_addr with
    Not_found -> (Printf.eprintf "%s : Unknown server\n" server_addr; exit 2) 
            in
            let socket = Unix.ADDR_INET (server_inet_addr, server_port) in
            try 

                let domain = Unix.domain_of_sockaddr socket in
                let sock = Unix.socket domain Unix.SOCK_STREAM 0 in 
                try 
                    Printf.printf "connection attempt initiated with %s\n" server_addr;
                    flush stdout;
                    Unix.connect sock socket ;
                    let ic,oc = (Unix.in_channel_of_descr sock ,
                    Unix.out_channel_of_descr sock) in

                    Printf.printf "connected to %s\n" server_addr;
                    (* Then we try to reduce the timeout time to go quicker on
                     * the discovery*)
                    Printf.printf "socket in timeout :%f\n" (Unix.getsockopt_float sock Unix.SO_RCVTIMEO);
                    Unix.setsockopt_float sock Unix.SO_RCVTIMEO 0.5;
                    Printf.printf "socket in timeout :%f\n" (Unix.getsockopt_float sock Unix.SO_RCVTIMEO);
                    Printf.printf "socket out timeout :%f\n" (Unix.getsockopt_float sock Unix.SO_RCVTIMEO);
                    Unix.setsockopt_float sock Unix.SO_RCVTIMEO 0.5;
                    Printf.printf "socket out timeout :%f\n" (Unix.getsockopt_float sock Unix.SO_RCVTIMEO);


                    (*This is the question asked to the host*)
                    Printf.printf "To %s:%i are you one of those ?\n" server_addr server_port; flush stdout;
                    output_string oc "are you one of those?\n"; flush oc;
                    let r = input_line ic in
                    Unix.shutdown_connection ic;
                    Printf.printf "connection closed with %s\n" server_addr;
                    (* it should only responds the right phrase*)
                    Printf.printf "%s:%i %b\n" server_addr server_port (r="i
                    might"); flush stdout;
                    r = "i might"

                    (*error handling*)
    with exn -> (Unix.close sock ; raise exn)	
                with 
                | Unix.Unix_error (err, fonction, stringarg) -> (Printf.printf "%s %s %s (%s:%i)   " (Unix.error_message err) fonction stringarg server_addr
                server_port; false)
;;


(* The discovery of all the host on a given network
 * if 4.8.15.16 is given, it will test all hosts in 4.8.15.16/24 but this might
 * change*)
let discovery_fun_sub_network my_addr port=
    let ip = Str.split (Str.regexp "\\.") my_addr in
    assert (List.length ip ==4);
    let prefixe = String.sub my_addr  0 (String.length my_addr - (String.length (List.nth ip 3))) in
    let range = Array.init 254 ( fun i -> prefixe^(string_of_int i) ) in

    List.filter (fun ip_address ->  areYouAYaudseServer ip_address port) (Array.to_list range)
;; 



(*fonction pour lancer le client de test*)
let go_client () = main_client client_fun ;;

discovery_fun_sub_network "192.168.1.2" 2203;;

go_client ();;
