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


class ['a] iterable =
    object (self)
        val  s = Queue.create ()
        method pop () = Queue.pop s
        method push (el: 'a) = Queue.push el s
        method is_empty () = Queue.is_empty s
        method copy () = 
            let n = new iterable in
            Queue.iter (fun el -> n#push el) s;
            n
    end;;

    let shift_right ip shift=
        let two = Big_int.big_int_of_int 2 in
        let power = Big_int.power_big_int_positive_int two shift in
        Big_int.div_big_int ip power ;;

let ipBigIntToString ip = 
    (Big_int.string_of_big_int (Big_int.mod_big_int (shift_right ip 0) (Big_int.power_big_int_positive_int (Big_int.big_int_of_int 2) 4))) ^ "." ^
    (Big_int.string_of_big_int (Big_int.mod_big_int (shift_right ip 4) (Big_int.power_big_int_positive_int (Big_int.big_int_of_int 2) 8))) ^ "." ^
    (Big_int.string_of_big_int (Big_int.mod_big_int (shift_right ip 8) (Big_int.power_big_int_positive_int (Big_int.big_int_of_int 2) 12))) ^"." ^
    (Big_int.string_of_big_int (Big_int.mod_big_int (shift_right ip 12) (Big_int.power_big_int_positive_int (Big_int.big_int_of_int 2) 16)))
;;


let shift_left ip shift=
        let two = Big_int.big_int_of_int 2 in
        let power = Big_int.power_big_int_positive_int two shift in
        Big_int.mult_big_int ip power ;;


class ipv4 s=
    (**les ips sont sous la forme x.x.x.x
     * pour le jour où on t veut passer en IPV6 il suffit de faire une classe
     * virtuelle/interface qui propose les meme fonctions cette classe*)
    object (self : 'a)
        val s = s
        val mutable bin = Big_int.zero_big_int
        initializer 
            let ip = Str.split (Str.regexp "\\.") s in
            assert (List.length ip ==4);
            bin <- List.fold_left (fun acc el-> Big_int.add_big_int 
            (shift_left acc 4 ) 
            (Big_int.big_int_of_string el)
            ) (Big_int.zero_big_int)    ip
        method get () = s
        method getP () = bin
        method isEqual (bi: 'a) = bin = bi#getP ()
        method isGreaterThan (bi:'a) = Big_int.gt_big_int bin (bi#getP ())
        method rangeTo (bi : 'a) =
            assert (not (self#isGreaterThan bi));
            let range = new iterable in
            let tmp = ref bin in
           while not (!tmp <> bi#getP ()) do
               range#push (  ipBigIntToString (!tmp));
               tmp := Big_int.succ_big_int !tmp
           done;
           range
    end;;



class connection (addr, port)=
    object(self : 'a)
        val addr =addr
        val port = port
        val mutable ic = None

        method getAddr () = (addr : ipv4)

        method run (func: 'a -> in_channel -> out_channel -> unit)  = 
            try 
                begin
                    (* Il faudrait mettre ici beaucoup plus
                     * de tests, de try catch
                     * pour gerer les differetns cas comme dans la
                     * fonction qui servait dexemple*)
                    let server = Unix.inet_addr_of_string (addr#get ())  in
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


        method doSomething fonction (iterable:  string iterable) port timeOut =
            let iter = iterable#copy () in 
            while not (iter#is_empty ()) do
                while not (iter#is_empty ()) && self#hasFreeSpace () do
                    let ip = iter#pop () in
                    Printf.printf "Lancement de %s\n" ip; flush stdout;
                    let ipv = new ipv4 ip in
                    let c = new connection (ipv, port) in
                    ignore(self#add c fonction);
            done;
            Unix.sleep timeOut;
            self#tryClean ()
                done


            end;;
(** La procédure pour ajouter une nouvelle connection dans le threadedPool est :
    * - de vérfier quil y a de la place avec la methode hasFreeSpace
    * - inserer une connection non demarrée avec add*)






let discovery_network my_addr port=
        let ip = Str.split (Str.regexp "\\.") my_addr in
        assert (List.length ip ==4);
        let prefixe = String.sub my_addr  0 (String.length my_addr - (String.length (List.nth ip 3))) in
        let range = new iterable in
        for i=0 to 254 do
            range#push (prefixe^(string_of_int i))
        done;
        let peers = new iterable in
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
                        peers#push (connection#getAddr () );
                        Printf.printf "%s est un des notres !\n"
                        ((connection#getAddr ())#get ()); flush stdout
    end
                else 
                    ( Printf.printf "%s ne semble pas faire partie des notres !\n"
                    ((connection#getAddr ())#get ());
                    flush stdout )
                with
                |exn -> (Printf.printf "erreur ?"; flush stdout)
                in
                p#doSomething areYou range port 5;
                range
;;


let ask question range port= 
    let p= new threadedPool 10 in 

    let ask_server question connection ic oc=
        Printf.printf "Je pose la question %s...." question; flush stdout;
        try 
            output_string oc ("Q&A "^question); flush oc;
            let line = ref "" in
            while !line <> "END" do
                line := input_line ic;
                Printf.printf "got answer from %s : %s\n" ((connection#getAddr
                ())#get ())
                !line; flush stdout;
        done
            with e -> Printf.printf "erreur from %s\n" ((connection#getAddr
            ())#get ())
            in
            p#doSomething (ask_server question) range port 5;
;;

    




let range = discovery_network "192.168.1.3" 2203;;

ask "test" range 2203;;

