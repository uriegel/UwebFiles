module Files
open System
open System.IO
open System.IO.Compression
open ResponseData
open Session

let asyncSendNotFound (requestSession: RequestSession) = async {
    let requestData = requestSession.RequestData :?> RequestData.RequestData
    let responseData = ResponseData.create requestData
    do! Response.asyncSendNotFound responseData 
}

let asyncSend304 (requestSession: RequestSession) = async {
    let requestData = requestSession.RequestData :?> RequestData.RequestData
    let responseData = ResponseData.create requestData
    do! Response.asyncSend304 responseData 
}

let private asyncSendStream responseData (stream: Stream) (contentType: string) lastModified  = 
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

        match lastModified with
        | Some lastModified -> headers <- headers.Add("Last-Modified", lastModified)     
        | None -> ()
        // TODO:
        let noCache = false
        if noCache then
            headers <- headers.Add("Cache-Control", "no-cache,no-store")
            headers <- headers.Add("Expires", (DateTime.Now.Subtract(TimeSpan(1, 0, 0))).ToUniversalTime().ToString "r")   

        headers <- ResponseHeaders.initialize headers contentType (Some streamToSend.Length)

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

// TODO: to fsharptools
let (|Long|_|) (str: string option) =
    match str with
    | Some str -> 
        match System.Int64.TryParse str with
        | true,int -> Some int
        | _ -> None
    | _ -> None        

let private asyncSendRange responseData (stream: Stream) (contentType: string) lastModified = async {

    let getRange () =
        match responseData.requestData.header.Header "Range" with
        | Some range ->
            match range |> String.indexOf "bytes=" with
            | Some pos -> 
                let rangeString = range |> String.substring (pos + 6)
                match rangeString |> String.indexOfChar '-' with
                | Some 0 -> 
                    match Some (rangeString |> String.substring 1) with
                    | Long i -> Some (0L, i)
                    | _ -> None
                | Some minusPos when minusPos = rangeString.Length - 1 ->                     
                    match Some (rangeString |> String.substring2 0 minusPos) with
                    | Long i -> Some (i, stream.Length - 1L)
                    | _ -> None
                | Some minusPos ->
                    match Some (rangeString |> String.substring2 0 minusPos), Some (rangeString |> String.substring (minusPos + 1)) with
                    | Long s, Long e -> Some (s, e)
                    | _ -> None
                | None -> None
            | None -> None
        | None -> None

    match getRange () with
    | Some (s, e) -> 
        let contentLength = e - s + 1L

        let mutable headers = Map.empty
        headers <- headers.Add ("ETag", "\"0815\"")
        headers <- headers.Add ("Accept-Ranges", "bytes")
        headers <- headers.Add ("Content-Range", sprintf "bytes %d-%d/%d" s e stream.Length)
        headers <- headers.Add ("Keep-Alive", "timeout=5, max=99")   
        headers <- headers.Add ("Connection", "Keep-Alive")   
        headers <- headers.Add ("Content-Type", contentType)        
        headers <- ResponseHeaders.initialize headers contentType (Some contentLength)

        let headerBytes = Response.createHeader responseData headers 206 "Partial Content" None
        do! responseData.requestData.session.networkStream.AsyncWrite (headerBytes, 0, headerBytes.Length)
        
        let bytes = Array.zeroCreate 8192

        stream.Seek (s, SeekOrigin.Begin) |> ignore
        let mutable dataToSend = true
        let mutable completeRead = 0L
        while dataToSend do 
            let bytesToRead = int (min (int64 bytes.Length) (contentLength - completeRead))
            let! read = stream.AsyncRead (bytes, 0, bytesToRead)
            if read <> 0 then
                completeRead <- completeRead + int64 read
                do! responseData.requestData.session.networkStream.AsyncWrite (bytes, 0, read)
                if completeRead = contentLength then
                    dataToSend <- false
            else
                dataToSend <- false
    | None -> do! asyncSendStream responseData (stream: Stream) (contentType: string) lastModified
}


let private asyncSendStreamOrRange responseData (stream: Stream) (contentType: string) lastModified  = 
    match contentType with
    // TODO: //         || file.EndsWith (".mkv", StringComparison.InvariantCultureIgnoreCase)
    | "video/mp4"  
    | "audio/mpeg"
    | "audio/wav" -> asyncSendRange responseData stream contentType lastModified
    | _ -> asyncSendStream responseData stream contentType lastModified

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
        let lastModified = Some (dateTime.ToUniversalTime().ToString "r")

        use stream = File.OpenRead file
        do! asyncSendStreamOrRange responseData stream contentType lastModified
    else
        do! Response.asyncSend304 responseData
}

let serveStream requestSession stream contentType lastModified = async {      
    let requestData = requestSession.RequestData :?> RequestData.RequestData
    let responseData = create requestData
    do! asyncSendStreamOrRange responseData stream contentType lastModified
}

let serveFile requestSession file = async {
    let requestData = requestSession.RequestData :?> RequestData.RequestData
    let responseData = create requestData
    do! asyncSendFile file responseData
}
    
