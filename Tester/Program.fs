open Session
open Request

printfn "Starting Test Server"

let request (requestSession: RequestSession) =
    async {
        match requestSession.Url with
        | "/bild" ->
            do! Files.serveFile requestSession "assets/bild.jpg"
            return true
        | "/film" ->
            do! Files.serveFile requestSession "assets/film.html"
            return true
        | "/filmfile" -> 
            do! Files.serveFile requestSession "/media/speicher/projekte/UwebServer/webroot/Reitbeteiligung/Convoy.mp4"
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