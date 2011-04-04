
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
            (*TODO changer l'implémentation en list ref pour améliorer un peu
             * l'utilisation du truc, en particulier la lecture non
             * destructive*)
            object (self)
                val  mutable s = Queue.create ()
                method pop () = Queue.pop s
                method push (el: 'a) = Queue.push el s
                method is_empty () = Queue.is_empty s
                method copy () = 
                    let n = new iterable in
                    Queue.iter (fun el -> n#push el) s;
                    n
                method iter f=(**Cette fonction n'est pas destructive*)
                    let c=Queue.copy s in
                    Queue.iter f s;
                    s <- c
                method fold f (acc:bool)= (**lecture non destructive*)
                    let c = Queue.copy s in
                    let a = Queue.fold f  acc s in
                    s <- c;
                    a
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
