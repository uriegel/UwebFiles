open Session
open Request

System.IO.Directory.SetCurrentDirectory "/media/speicher/projekte/UwebFiles"

printfn "Starting Test Server"

let request (requestSession: RequestSession) =
    async {
        match requestSession.Url with
        | "/bild" ->
            do! Files.serveFile requestSession "assets/bild.jpg"
            return true
        | _ -> return false
    }

let configuration = Configuration.create {
    Configuration.createEmpty() with 
        Port = 9865
        Requests = [ request ]
}

let server = Server.create configuration 
server.start ()
stdin.ReadLine() |> ignore
server.stop ()