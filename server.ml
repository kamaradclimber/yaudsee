open Unix;;

let me  = inet_addr_of_string "192.168.1.2" ;;
let mySocket = ADDR_INET (me,2203);;

let working _ out = 
    output_string out "Hello Bob !"
;;


let server = establish_server working mySocket;;
