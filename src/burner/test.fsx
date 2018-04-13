#r @"C:\Users\brase\.nuget\packages\newtonsoft.json\11.0.2\lib\netstandard2.0\Newtonsoft.Json.dll"
#r @"C:\Users\brase\.nuget\packages\fable.jsonconverter\1.0.7\lib\netstandard1.6\Fable.JsonConverter.dll"
//#r @"C:\Program Files\dotnet\sdk\NuGetFallbackFolder\microsoft.netcore.app\2.0.0\ref\netcoreapp2.0\System.Reflection.TypeExtensions.dll"

open System.IO
open System.Text

open Newtonsoft.Json

type VzloggerInput = {
    data: VzloggerData list
} and VzloggerData = {
    uuid: System.Guid
    tuples: (float list) list
}

type bla = {
    tuples: (float list) list
}

let private converter = Fable.JsonConverter()

let unjson<'T> json =
        let a = JsonConvert.DeserializeObject(json, typeof<'T>, converter) :?> 'T
        a

let mkjson a =
        let json = JsonConvert.SerializeObject(a, converter)
        json

let string = File.ReadAllText("C:\\Users\\brase\\code\\fs\\burner\\src\\burner\\data.json")

let tuples = "{\"tuples\": [[1521807504748.0, 3689.7689999999998]]}"
let values = unjson<VzloggerData> string