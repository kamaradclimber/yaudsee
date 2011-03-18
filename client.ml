


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


let areYouAYaudseServer server_addr server_port =


    let server_inet_addr = try Unix.inet_addr_of_string server_addr with
    Not_found -> (Printf.eprintf "%s : Unknown server\n" server_addr; exit 2) 
            in
            let socket = Unix.ADDR_INET (server_inet_addr, server_port) in
            try 

                let domain = Unix.domain_of_sockaddr socket in
                let sock = Unix.socket domain Unix.SOCK_STREAM 0 in 
                try 
                    Printf.printf "connection initiated with %s\n" server_addr;
                    flush stdout;
                    Unix.connect sock socket ;
                    let ic,oc = (Unix.in_channel_of_descr sock ,
                    Unix.out_channel_of_descr sock) in

                    Printf.printf "socket in timeout :%f\n" (Unix.getsockopt_float sock Unix.SO_RCVTIMEO);
                    Unix.setsockopt_float sock Unix.SO_RCVTIMEO 0.5;
                    Printf.printf "socket in timeout :%f\n" (Unix.getsockopt_float sock Unix.SO_RCVTIMEO);
                    Printf.printf "socket out timeout :%f\n" (Unix.getsockopt_float sock Unix.SO_RCVTIMEO);
                    Unix.setsockopt_float sock Unix.SO_RCVTIMEO 0.5;
                    Printf.printf "socket out timeout :%f\n" (Unix.getsockopt_float sock Unix.SO_RCVTIMEO);

                    Printf.printf "To %s:%i are you one of those ?\n" server_addr server_port; flush stdout;

                    output_string oc "are you one of those?\n"; flush oc;
                    let r = input_line ic in
                    Unix.shutdown_connection ic;
                    Printf.printf "connection closed with %s\n" server_addr;
                    flush stdout;
                    r = "i might"
    with exn -> (Unix.close sock ; raise exn)	
                with 
                | Unix.Unix_error (err, fonction, stringarg) -> (Printf.printf "%s %s %s (%s:%i)   " (Unix.error_message err) fonction stringarg server_addr
                server_port; false)
;;



let discovery_fun_sub_network my_addr port=
    let ip = Str.split (Str.regexp "\\.") my_addr in
    assert (List.length ip ==4);
    let prefixe = String.sub my_addr  0 (String.length my_addr - (String.length (List.nth ip 3))) in
    let range = Array.init 254 ( fun i -> prefixe^(string_of_int i) ) in

    Array.iter (fun ip_address -> Printf.printf "%s : %b\n" (ip_address) (areYouAYaudseServer
    ip_address port)) range
;; 




let go_client () = main_client client_fun ;;

discovery_fun_sub_network "192.168.1.2" 2203;;

go_client ();;
