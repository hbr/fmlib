let handle_request request =
    let path =
        let open Fmlib_std.Option in
        if String.starts_with ~prefix:"GET " request then
            let* end_pos = String.index_from_opt request 4 ' ' in
            let length = end_pos - 4 in
            Some (String.sub request 4 length)
        else
            None
    in
    let print =
        Printf.sprintf
            "HTTP/1.1 %i OK\r\nContent-Type: text/%s\r\nContent-Length: %i\r\n\r\n%s"
    in
    match path with
    | Some path ->
        if path = "/fmlib/webapp/single_page.js" then
            print 200 "javascript" (String.length Assets.js) Assets.js
        else if
            path = "/fmlib/webapp/"
            ||
            path = "/fmlib/webapp/single_page.html"
        then
            print 200 "html" (String.length Assets.html) Assets.html
        else if
            path = "/favicon.ico"
        then
            print 200 "plain" 0 ""
        else
            "HTTP/1.1 404 Not found\r\nContent-Length: 10\r\n\r\nNot found!"
    | None ->
        "HTTP/1.1 400 Bad Request\r\nContent-Length: 12\r\n\r\nBad Request!"


let () =
    let port = 8080 in
    let address = Unix.inet_addr_loopback in
    let sockaddr = Unix.ADDR_INET (address, port) in

    let server = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt server SO_REUSEADDR true;
    Unix.bind server sockaddr;
    Unix.listen server 5;

    Printf.printf
        "Server is listening at http://%s:%d\nUse path: <%s>\n%!"
        (Unix.string_of_inet_addr address)
        port
        "/fmlib/webapp/single_page.html"
    ;

    while true do
        let (client_socket, _) = Unix.accept server in
        let buffer = Bytes.create 1024 in
        let _ = Unix.read client_socket buffer 0 1024 in
        let request = Bytes.to_string buffer in
        let response = handle_request request in
        let _ =
            Unix.write
                client_socket
                (Bytes.of_string response)
                0
                (String.length response)
        in
        Unix.close client_socket
    done
