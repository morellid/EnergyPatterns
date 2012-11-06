namespace EnergyPatterns.RemoteAmmeter

open Energon.Measuring
open Energon.Extech380803

open System
open System.Net
open System.Text
open Energon.Measuring
open System.Collections.Generic


type WebListener(startCallback:(unit -> string), stopCallback:(unit -> string)) =
  let address = "http://+:80/Temporary_Listen_Addresses/"
  let listener = new System.Net.HttpListener()
  let rec GetContextCallback(result:IAsyncResult) =
    let context = listener.EndGetContext(result)
    let request = context.Request
    let relPath = request.Url.PathAndQuery.Substring("/Temporary_Listen_Addresses/".Length )
    let spl = List.toArray ["/"; "?"]
    let tags = relPath.Split(spl, StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun (s:string) -> 
        printf "%s\n" s |> ignore
        System.Web.HttpUtility.UrlDecode(s) 
        )
    let op = tags.[0]
    let sb = new StringBuilder()
    match op with 
      | "start" -> sb.Append(startCallback()) |> ignore
      | "stop" -> sb.Append(stopCallback()) |> ignore
      | _ -> printf "undefined op\n"
    let response = context.Response
    let buffer = System.Text.Encoding.UTF8.GetBytes(sb.ToString())
    response.ContentLength64 = int64( buffer.Length) |> ignore
    use outputStream = response.OutputStream
    outputStream.Write(buffer, 0, buffer.Length);
    listener.BeginGetContext(new AsyncCallback(GetContextCallback), null) |> ignore
  do
    listener.Prefixes.Add(address);

  member x.start() =
    listener.Start()
    listener.BeginGetContext(new AsyncCallback(GetContextCallback), null);

  member x.stop() =
    listener.Stop()



/// <summary>An helper class that handles an experiment where the load is run remotely, with remote sensors</summary>
type Server() = 
    let ammeter = new Extech380803Sensor("Extech", DataType.Watt, 2.)

    let start() = 
        printf "received start/n"
        ammeter.Start()
        "OK"

    let stop() =
        printf "received stop/n"
        ammeter.Stop()
        let avg = Seq.averageBy (fun (r:Reading) -> float r.Value ) ammeter.Results
        System.String.Format("{0}", avg)

    let w = new WebListener(start, stop)

    member x.Start() =
        w.start()

    member x.Stop() =
        w.stop()


/// <summary> This class should be used on the remote side, in the case that we are measuring a remote process, with remote sensors </summary>



