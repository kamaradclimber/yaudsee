(* This file is a client for Yaudse*)


(* 
 * What does it do ?
 * It will attempt first to find server on the local network 
 * and nothing else for now
 * *)


open Utils
class connection (addr, port)=
    (** la classe qui gère les connections avec un serveur*)
    object(self : 'a)
        val addr =addr
        val port = port
        val mutable ic = None

        method getAddr () = (addr : Utils.IP.ipv4)

        method run (func: 'a -> in_channel -> out_channel -> unit)  = 
            try 
                begin
                    let server = Unix.inet_addr_of_string (addr#get ())  in
                    let sockaddr = Unix.ADDR_INET(server,port) in
                    let (icc,oc) = Unix.open_connection sockaddr in
                    ic <- Some icc;
                    func self icc oc
                end
            with _ -> ()

        method stop () = match ic with None -> () | Some icc -> Unix.shutdown_connection icc
                end;;


class ['a]  pool n=
    (** Gestion automatique d'un pool dobject qui effectuent des
     * actions...*)
    object(self)
        val size = n
        val pool = Array.make n (None: 'a option)

        method hasFreeSpace () = Array.fold_right (fun el acc -> acc || el=None) pool false
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
    (** Un wrapper pour les threads*)
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
    (** On met dans le pool des threads dobjets qui repondent aux methodes
     * run/stop*)
    object(self)
        inherit [thread] pool n as super
        method add (el: 'a) arg = 
            begin
                let t = new thread (fun x ->  el#run  x; el#stop () ) arg in
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


        method doSomething fonction (iterable:  string Utils.iterable) port timeOut =
            let iter = iterable#copy () in 
            while not (iter#is_empty ()) do
                while not (iter#is_empty ()) && self#hasFreeSpace () do
                    let ip = iter#pop () in
                    Printf.printf "Lancement de %s\n" ip; flush stdout;
                    let ipv = new Utils.IP.ipv4 ip in
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




module Action = struct

    let discovery_network my_addr port=
        let ip = Str.split (Str.regexp "\\.") my_addr in
        assert (List.length ip ==4);
        let prefixe = String.sub my_addr  0 (String.length my_addr - (String.length (List.nth ip 3))) in
        let range = new Utils.iterable in
        for i=0 to 254 do range#push (prefixe^(string_of_int i))  done;
        let peers = new Utils.iterable in
        let p = new threadedPool 100 in

        let areYou connection ic oc=
            Printf.printf "la fonction areYou est executé \n"; flush stdout;
            try 
                output_string oc "DISC are you one of those ?\n"; flush oc;
                let r = input_line ic in
                Printf.printf "test : %s\n" r;flush stdout;
                if r="i might" 
                then 
                    begin
                        peers#push ((connection#getAddr ())#get () );
                        Printf.printf "%s est un des notres !\n" ((connection#getAddr ())#get ()); flush stdout
    end
                else 
                    ( Printf.printf "%s ne semble pas faire partie des notres !\n" ((connection#getAddr ())#get ()); flush stdout )
                with
                |exn -> (Printf.printf "erreur ?"; flush stdout)
                in
                p#doSomething areYou range port 5;
                peers
    ;;


    let ask question range port= 
        let p= new threadedPool 100 in 

        let ask_server question connection ic oc=
            Printf.printf "Je pose la question %s...." question; flush stdout;
            try 
                output_string oc (""^question^"\n"); flush oc;
                let line = ref "" in
                while !line <> "END" do
                    line := input_line ic;
                    Printf.printf "got answer from %s : %s\n" ((connection#getAddr ())#get ()) !line; flush stdout;
        done
            with e -> Printf.printf "erreur from %s\n" ((connection#getAddr ())#get ())
                in
                p#doSomething (ask_server question) range port 5;
    ;;

                    end
    ;;



    let range = new Utils.iterable;;

    File.readAndIfDoesNotExist 
    "ips.txt" 
    (fun ligne -> range#push ligne)
    (fun fileName -> 
        let r  = Action.discovery_network "192.168.1.3" 2203 in
        let fichier = open_out_gen [Open_append; Open_creat; Open_wronly] 0o777 fileName in
        while not (r#is_empty ()) do
            let ip = r#pop () in
            output_string fichier (ip^"\n");
            range#push ip
                done;
                flush fichier;
                close_out fichier)
    ;;

    (**Cette fonction nous permet d'utiliser la bonne adresse pour le serveur mais il
     * faudrait mieux lutiliser*)
    Utils.IP.get_my_ips ();;


    let req = (Printf.printf "requete: ";flush stdout ; input_line stdin) in
    Action.ask req range 2203;;

