open Unix;;

let distSocket = ADDR_INET (inet_addr_of_string "192.168.1.2", 2203);;

let (inC, outC) = open_connection distSocket;;

while true do
    Printf.printf "%s " (input_line inC);
done
