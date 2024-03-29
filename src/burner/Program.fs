module burner.App

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open FSharp.Control.Tasks

open Microsoft.AspNetCore.Http

open burner.EnergyController
open burner.Config

// ---------------------------------
// Models
// ---------------------------------

type VzloggerInput = {
    data: VzloggerData list
} and VzloggerData = {
    uuid: System.Guid
    tuples: float list list
}

// ---------------------------------
// Web app
// ---------------------------------

let getLatestValue (vzloggerInput:VzloggerInput) id =
    let needed = vzloggerInput.data
                 |> List.choose (fun x -> if(x.uuid = id) then Some x else None)
                 |> List.collect (fun x -> x.tuples)
                 |> List.sortByDescending (fun x -> x.Head)
                 |> List.tryHead
    match needed with
    | Some [x; y] -> Some (x,y)
    | _ -> None

let pushHandler =
    fun (next: HttpFunc) (ctx:HttpContext) ->
        task {
            use streamreader = new StreamReader(ctx.Request.Body)
            let! data = streamreader.ReadToEndAsync()
            printfn "%s" data
            printfn ""

            let inputId = System.Guid.Parse(configuration.["Input"])
            let outputId = System.Guid.Parse(configuration.["Output"])

            let values = unjson<VzloggerInput> data

            let inputValue = getLatestValue values inputId
            let outputValue = getLatestValue values outputId

            match inputValue with
            | Some (x,y) -> energyDataProcessor.Post (EnergyController.Input {TimeStamp = int64 x
                                                                              Value = y})
            | _ -> ()

            match outputValue with
            | Some (x,y) -> energyDataProcessor.Post (EnergyController.Output {TimeStamp = int64 x
                                                                               Value = y})
            | _ -> ()

            return! Successful.OK "" next ctx
        }

let webApp =
    choose [
        POST >=> choose [
            route "/push" >=> pushHandler
        ]
        setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder : CorsPolicyBuilder) =
    builder.WithOrigins("http://localhost:8080")
           .AllowAnyMethod()
           .AllowAnyHeader()
           |> ignore

let configureApp (app : IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<Microsoft.AspNetCore.Hosting.IWebHostEnvironment>()
    (match env.IsDevelopment() with
    | true  -> app.UseDeveloperExceptionPage()
    | false -> app.UseGiraffeErrorHandler errorHandler)
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseGiraffe(webApp)

let configureServices (services : IServiceCollection) =
    services.AddCors()    |> ignore
    services.AddGiraffe() |> ignore

let configureLogging (builder : ILoggingBuilder) =
    let filter (l : LogLevel) = l.Equals LogLevel.Error
    builder.AddFilter(filter).AddConsole().AddDebug() |> ignore

[<EntryPoint>]
let main _ =
    printfn "Input: %s" configuration.["Input"]
    printfn "Output: %s" configuration.["Output"]

    let contentRoot = Directory.GetCurrentDirectory()
    let webRoot     = Path.Combine(contentRoot, "WebRoot")
    WebHostBuilder()
        .UseKestrel()
        .UseContentRoot(contentRoot)
        .UseIISIntegration()
        .UseWebRoot(webRoot)
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .UseUrls("http://0.0.0.0:5000/")
        .Build()
        .Run()
    0