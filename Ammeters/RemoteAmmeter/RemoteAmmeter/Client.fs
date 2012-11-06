namespace EnergyPatterns.RemoteAmmeter

open System
open System.Net
open System.Text
open Energon.Measuring
open System.Collections.Generic

type Client(ip:string) =
    let wc = new WebClient()
    let address = System.String.Format(@"http://{0}/Temporary_Listen_Addresses", ip)
    member x.start() =
        let addr = System.String.Format(@"{0}/{1}", address, "start")
        try
            wc.DownloadString(addr) 
        with 
            | _ -> "KO"
    member x.stop(par:seq<string>) =
        let sb = new System.Text.StringBuilder()
        sb.AppendFormat(@"{0}/stop", address) |> ignore
        par |> Seq.iter (fun s -> sb.AppendFormat(@"/{0}", System.Web.HttpUtility.UrlEncode(s)) |> ignore)
        let addr = sb.ToString()
        try
            wc.DownloadString(addr)
        with 
            | _ -> "KO"

