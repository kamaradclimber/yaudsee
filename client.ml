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


class connection (addr, port)=
    object(self : 'a)
        val addr =addr
        val port = port
        val mutable ic = None

        method getAddr () = (addr : string)

        method run (func: 'a -> in_channel -> out_channel -> unit)  = 
            try 
                begin
                    (* Il faudrait mettre ici beaucoup plus
                     * de tests, de try catch
                     * pour gerer les differetns cas comme dans la
                     * fonction qui servait dexemple*)
                    let server = Unix.inet_addr_of_string addr  in
                    let sockaddr = Unix.ADDR_INET(server,port) in
                    let (icc,oc) = Unix.open_connection sockaddr in
                    ic <- Some icc;
                    func self icc oc
            end
    with _ -> ()

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


class thread fonction arg=
    object(self)
        val t = Thread.create fonction arg
        val mutable id = 0
        val mutable running = false
        val startTime = Sys.time ()
        initializer
            let idd = Thread.id t in
            id <- idd
        method isRunning () = running
        method kill () = (Thread.kill t; running <- false)
        method getId () = id
        method timeFromStart () = Sys.time () -. startTime
    end
;;



            




class ['a] threadedPool n=
    (** On met dans le pool des threads dobjets qui repondent aux methode
     * run/stop*)
    object(self)
        inherit [thread] pool n as super
        method add (el: 'a) arg = 
            begin
                let t = new thread (fun x -> 
                    el#run  x; el#stop ();
                    ) arg in
                super#put t;
                t#getId ()
    end
        method tryClean ()=
            for i=0 to n-1 do
                try 
                let el = super#get i in
                if el#timeFromStart () > 10. then el#kill ();
                if not (el#isRunning ()) then super#rem el
                with Failure("Nothing_found") -> ()
                done

            end;;
(** La procédure pour ajouter une nouvelle connection dans le threadedPool est :
    * - de vérfier quil y a de la place avec la methode hasFreeSpace
    * - inserer une connection non demarrée avec add*)






let discovery_network my_addr port=
        let ip = Str.split (Str.regexp "\\.") my_addr in
        assert (List.length ip ==4);
        let prefixe = String.sub my_addr  0 (String.length my_addr - (String.length (List.nth ip 3))) in
        let range = Array.init 255 ( fun i -> prefixe^(string_of_int i) ) in
        let peers = ref ([] :string list) in
        let p = new threadedPool 10 in

        let areYou connection ic oc=
            Printf.printf "la fonction areYou est executé \n"; flush stdout;
            try 
                output_string oc "are you one of those ?\n"; flush oc;
                let r = input_line ic in
                Printf.printf "test : %s\n" r;flush stdout;
                if r="i might" 
                then 
                    begin
                        peers := (connection#getAddr () ):: !peers;
                        Printf.printf "%s est un des notres !\n" (connection#getAddr ()); flush stdout
    end
                else 
                    ( Printf.printf "%s ne semble pas faire partie des notres !\n"
                    (connection#getAddr ());
                    flush stdout )
                with
                |exn -> (Printf.printf "erreur ?"; flush stdout)
                in
                let i = ref 0 in
                while !i < Array.length range do
                    while !i < Array.length range && p#hasFreeSpace () do
                        Printf.printf "%i lancé\n" !i; flush stdout;
                        let c = new connection (range.(!i), port) in
                        ignore(p#add c areYou);
                        incr i
                done;
                Unix.sleep 5;
                p#tryClean ();
                done;;


            




discovery_network "88.163.232.134" 2203;;

