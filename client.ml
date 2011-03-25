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
  module File = struct
      (** le module pour lire et ecrire dans des fichiers*)
      let read_apply file f = 
          (**lire ligne par ligne et appliquer sur chacune dentre elle la fonction
           * f*)
          try 
              let fichier = open_in_bin file in
              while true do
                  f (input_line fichier)
              done
          with 
          |Sys_error _ -> failwith("No such file")
          |End_of_file -> ()
          | _          -> failwith "error dans read_apply"

      ;;

      let readAndIfDoesNotExist file f_exists fdoesnot=
          if Sys.file_exists file 
          then read_apply file f_exists 
          else fdoesnot file

  end;;


      module Utils = struct

          class ['a] iterable =
              (** La classe qui stocke les elements (essentielement des IP pour le moments qu'on peut interroger*)
              (** Pour l'utiliser il faut copier l'objet puis le parcourir, car la lecture est destructive*)
              (** En théorie il faudrait gérer un jour une lecture non destructive et  proposer un itérateur *)
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

    module IP = struct

          (**Trois fonctions devraient exister dans la version 3.12 d'OCaml mais quon
           * redefinit ici en attendant le passage a la version suivante*)
          let shift_right ip shift=
              (** décalage d'un big int vers la droite (sorte de division)*)
              let two = Big_int.big_int_of_int 2 in
              let power = Big_int.power_big_int_positive_int two shift in
              Big_int.div_big_int ip power ;;

          let ipBigIntToString ip = 
              (** convertit un big int representant une ip en string de la forme
               * x.x.x.x*)
              (Big_int.string_of_big_int (Big_int.mod_big_int (shift_right ip 0) (Big_int.power_big_int_positive_int (Big_int.big_int_of_int 2) 4))) ^ "." ^
              (Big_int.string_of_big_int (Big_int.mod_big_int (shift_right ip 4) (Big_int.power_big_int_positive_int (Big_int.big_int_of_int 2) 8))) ^ "." ^
              (Big_int.string_of_big_int (Big_int.mod_big_int (shift_right ip 8) (Big_int.power_big_int_positive_int (Big_int.big_int_of_int 2) 12))) ^"." ^
              (Big_int.string_of_big_int (Big_int.mod_big_int (shift_right ip 12) (Big_int.power_big_int_positive_int (Big_int.big_int_of_int 2) 16)))
          ;;

          let shift_left ip shift=
              (** décalage vers la gauche dun big int*)
              let two = Big_int.big_int_of_int 2 in
              let power = Big_int.power_big_int_positive_int two shift in
              Big_int.mult_big_int ip power ;;


          let get_my_ips () = 
              let _ = Unix.system "touch ips.tmp && hostname -I >>ips.tmp" in
              let  l= ref "" in
              File.read_apply "ips.tmp" (fun ligne -> l := ligne);
              let ips = Str.split (Str.regexp " +") !l in
              ips
          ;;



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
                      bin <- List.fold_left (fun acc el-> Big_int.add_big_int (shift_left acc 4 ) (Big_int.big_int_of_string el) ) (Big_int.zero_big_int) ip
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
              end
    end

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
                          output_string oc "are you one of those ?\n"; flush oc;
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
                          output_string oc ("Q&A "^question); flush oc;
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

