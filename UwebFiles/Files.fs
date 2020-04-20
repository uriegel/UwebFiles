module Files
open System
open System.IO
open System.IO.Compression
open ResponseData
open Session

let asyncSendStream responseData (stream: Stream) (contentType: string) lastModified = 
    async {
        let mutable headers = Map.empty
        let mutable streamToSend = stream
        if responseData.requestData.header.contentEncoding.Value <> ContentEncoding.None &&
            (contentType.StartsWith ("application/javascript", StringComparison.CurrentCultureIgnoreCase)            
            || contentType.StartsWith ("text/", StringComparison.CurrentCultureIgnoreCase)) then
            
            let ms = new MemoryStream ()
            use compressedStream = 
                match responseData.requestData.header.contentEncoding.Value with    
                | ContentEncoding.Deflate ->
                    headers <- headers.Add("Content-Encoding", "deflate") 
                    new DeflateStream (ms, CompressionMode.Compress, true) :> Stream
                | ContentEncoding.GZip ->
                    headers <- headers.Add("Content-Encoding", "gzip")
                    new GZipStream (ms, CompressionMode.Compress, true) :> Stream
                | _ -> null

            do! stream.CopyToAsync compressedStream |> Async.AwaitTask
            compressedStream.Close();
            ms.Position <- 0L 
            
            streamToSend <- ms

        if lastModified <> "" then headers <- headers.Add("Last-Modified", lastModified)     
        // TODO:
        let noCache = false
        if noCache then
            headers <- headers.Add("Cache-Control", "no-cache,no-store")
            headers <- headers.Add("Expires", (DateTime.Now.Subtract(TimeSpan(1, 0, 0))).ToUniversalTime().ToString "r")   

        headers <- ResponseHeaders.initialize headers contentType (Some (int streamToSend.Length))

        if contentType.StartsWith ("application/javascript", StringComparison.CurrentCultureIgnoreCase) 
            || contentType.StartsWith ("text/css", StringComparison.CurrentCultureIgnoreCase)
            || contentType.StartsWith ("text/html", StringComparison.CurrentCultureIgnoreCase) then
                headers <- headers.Add("Expires", DateTime.Now.ToUniversalTime().ToString "r")

        let headerBytes = Response.createHeaderOk responseData headers None
        do! responseData.requestData.session.networkStream.AsyncWrite (headerBytes, 0, headerBytes.Length)
        
        if responseData.requestData.header.method <> Method.Head then
            let bytes = Array.zeroCreate 8192

            let mutable dataToSend = true
            while dataToSend do 
                let! read = streamToSend.AsyncRead (bytes, 0, bytes.Length)
                if read <> 0 then
                    do! responseData.requestData.session.networkStream.AsyncWrite (bytes, 0, read)
                else
                    dataToSend <- false
    }

let asyncSendFile (file: string) (responseData: ResponseData) = async {
    let file = 
        if not (Path.IsPathRooted file) then
            Path.Combine (Directory.GetCurrentDirectory (), file)
        else
            file
        
    let fi = FileInfo file
    //let noCache = Server.Configuration.NoCacheFiles.Contains(file.ToLower());
    let noCache = false
    if not noCache then
        // TODO: AppCache
        ()

    let isModifiedSince = 
        match responseData.requestData.header.Header "If-Modified-Since" with
        | None -> None
        | Some a when a = "" -> None
        | Some v -> 
            let pos = v.IndexOf ";"
            Some (if pos <> -1 then v.Substring (0, pos) else v)
        
    let modified =  
        match isModifiedSince with
        | Some v ->
            let ifModifiedSince = Convert.ToDateTime v
            let fileTime = fi.LastWriteTime.AddTicks -(fi.LastWriteTime.Ticks % TimeSpan.FromSeconds(1.0).Ticks)
            let diff = fileTime - ifModifiedSince
            diff > TimeSpan.FromMilliseconds 0.0
        | None -> true

    if modified then
        let contentType = 
            match fi.Extension with
            | ".html" 
            | ".htm" -> "text/html; charset=UTF-8"
            | ".css" -> "text/css; charset=UTF-8"
            | ".js" -> "application/javascript; charset=UTF-8"
            | ".appcache" -> "text/cache-manifest"
            | _ ->  MimeTypes.get fi.Extension
       
        let dateTime = fi.LastWriteTime
        let lastModified = dateTime.ToUniversalTime().ToString "r"

        use stream = File.OpenRead file
        do! asyncSendStream responseData stream contentType lastModified
    else
        do! Response.asyncSend304 responseData
}

let serveFile (requestSession: RequestSession) file = async {
    let requestData = requestSession.RequestData :?> RequestData.RequestData
    let responseData = create requestData
    do! asyncSendFile file responseData
}
    
// let asyncSendFile (file: string) responseData = async {
//     if file.EndsWith (".mp4", StringComparison.InvariantCultureIgnoreCase)
//         || file.EndsWith (".mkv", StringComparison.InvariantCultureIgnoreCase)
//         || file.EndsWith (".mp3", StringComparison.InvariantCultureIgnoreCase)
//         || file.EndsWith (".wav", StringComparison.InvariantCultureIgnoreCase) then
//         do! asyncSendRange file responseData
//     else
//         do! asyncInternalSendFile file responseData
// }
