namespace Barnacle.Base
open System.IO
open System
open Newtonsoft.Json
open Newtonsoft.Json.Linq

// Custom converter for int option
type OptionIntConverter() =
    inherit JsonConverter()
    
    override _.CanConvert(objectType) =
        objectType = typedefof<int option>
    
    override _.WriteJson(writer: JsonWriter, value: obj, serializer: JsonSerializer) =
        match value :?> int option with
        | Some v -> writer.WriteValue(v)
        | None -> writer.WriteNull()
    
    override _.ReadJson(reader: JsonReader, objectType: Type, existingValue: obj, serializer: JsonSerializer) =
        let token = JToken.Load(reader)
        match token.Type with
        | JTokenType.Null -> None :> obj
        | JTokenType.Integer -> Some (token.Value<int>()) :> obj
        | _ -> raise (JsonSerializationException("Expected null or integer for int option"))

// Custom converter for float array option
type OptionFloatArrayConverter() =
    inherit JsonConverter()
    
    override _.CanConvert(objectType) =
        objectType = typedefof<float array option>
    
    override _.WriteJson(writer: JsonWriter, value: obj, serializer: JsonSerializer) =
        match value :?> float array option with
        | Some arr -> serializer.Serialize(writer, arr)
        | None -> writer.WriteNull()
    
    override _.ReadJson(reader: JsonReader, objectType: Type, existingValue: obj, serializer: JsonSerializer) =
        let token = JToken.Load(reader)
        match token.Type with
        | JTokenType.Null -> None :> obj
        | JTokenType.Array -> Some (token.ToObject<float array>(serializer)) :> obj
        | _ -> raise (JsonSerializationException("Expected null or float array for float array option"))

// Define Keyframe type
type Keyframe = {
    time: float
    [<JsonConverter(typeof<OptionFloatArrayConverter>)>]
    scale: float array option
    [<JsonConverter(typeof<OptionFloatArrayConverter>)>]
    rotation: float array option
    [<JsonConverter(typeof<OptionFloatArrayConverter>)>]
    translation: float array option
}

// Define Transform type
type transform = {
    keyframes: Keyframe array
}

// Add these type definitions before the Barnacle type
type MeshPrimitive = {
    vertices: float array
    indices: int array
}

type Primitive =
    | Mesh of MeshPrimitive
    | MeshURI of string
    | Sphere of float

// Custom converter for Primitive array
type PrimitiveArrayConverter() =
    inherit JsonConverter()
    
    override _.CanConvert(objectType) =
        objectType = typedefof<Primitive array>
    
    override _.WriteJson(writer: JsonWriter, value: obj, serializer: JsonSerializer) =
        let primitives = value :?> Primitive array
        writer.WriteStartArray()
        for prim in primitives do
            match prim with
            | Mesh m ->
                writer.WriteStartObject()
                writer.WritePropertyName("type")
                writer.WriteValue("mesh")
                writer.WritePropertyName("vertices")
                serializer.Serialize(writer, m.vertices)
                writer.WritePropertyName("indices")
                serializer.Serialize(writer, m.indices)
                writer.WriteEndObject()
            | MeshURI uri ->
                writer.WriteStartObject()
                writer.WritePropertyName("type")
                writer.WriteValue("mesh")
                writer.WritePropertyName("uri")
                writer.WriteValue(uri)
                writer.WriteEndObject()
            | Sphere r ->
                writer.WriteStartObject()
                writer.WritePropertyName("type")
                writer.WriteValue("sphere")
                writer.WritePropertyName("radius")
                writer.WriteValue(r)
                writer.WriteEndObject()
        writer.WriteEndArray()
    
    override _.ReadJson(reader: JsonReader, objectType: Type, existingValue: obj, serializer: JsonSerializer) =
        let token = JToken.Load(reader)
        let arr = token :?> JArray
        let primitives = 
            arr |> Seq.map (fun item ->
                let obj = item :?> JObject
                let primType = obj.["type"].Value<string>()
                match primType with
                | "mesh" when obj.ContainsKey("uri") ->
                    MeshURI (obj.["uri"].Value<string>())
                | "mesh" ->
                    Mesh {
                        vertices = obj.["vertices"].ToObject<float array>(serializer)
                        indices = obj.["indices"].ToObject<int array>(serializer)
                    }
                | "sphere" ->
                    Sphere (obj.["radius"].Value<float>())
                | _ -> raise (JsonSerializationException("Unknown primitive type"))
            ) |> Seq.toArray
        primitives :> obj

// Material type
type Material =
    | Diffuse of float array  // albedo

type MaterialArrayConverter() =
    inherit JsonConverter()
    
    override _.CanConvert(objectType) =
        objectType = typedefof<Material array>
    
    override _.WriteJson(writer: JsonWriter, value: obj, serializer: JsonSerializer) =
        let materials = value :?> Material array
        writer.WriteStartArray()
        for mat in materials do
            match mat with
            | Diffuse albedo ->
                writer.WriteStartObject()
                writer.WritePropertyName("type")
                writer.WriteValue("diffuse")
                writer.WritePropertyName("albedo")
                serializer.Serialize(writer, albedo)
                writer.WriteEndObject()
        writer.WriteEndArray()
    
    override _.ReadJson(reader: JsonReader, objectType: Type, existingValue: obj, serializer: JsonSerializer) =
        let token = JToken.Load(reader)
        let arr = token :?> JArray
        let materials = 
            arr |> Seq.map (fun item ->
                let obj = item :?> JObject
                let matType = obj.["type"].Value<string>()
                match matType with
                | "diffuse" -> Diffuse (obj.["albedo"].ToObject<float array>(serializer))
                | _ -> raise (JsonSerializationException("Unknown material type"))
            ) |> Seq.toArray
        materials :> obj

// Light type
type Light = {
    emission: float array
    twoSided: bool
}

type LightArrayConverter() =
    inherit JsonConverter()
    
    override _.CanConvert(objectType) =
        objectType = typedefof<Light array>
    
    override _.WriteJson(writer: JsonWriter, value: obj, serializer: JsonSerializer) =
        let lights = value :?> Light array
        writer.WriteStartArray()
        for light in lights do
            writer.WriteStartObject()
            writer.WritePropertyName("type")
            writer.WriteValue("diffuse")
            writer.WritePropertyName("emission")
            serializer.Serialize(writer, light.emission)
            writer.WritePropertyName("two-sided")
            writer.WriteValue(light.twoSided)
            writer.WriteEndObject()
        writer.WriteEndArray()
    
    override _.ReadJson(reader: JsonReader, objectType: Type, existingValue: obj, serializer: JsonSerializer) =
        let token = JToken.Load(reader)
        let arr = token :?> JArray
        let lights = 
            arr |> Seq.map (fun item ->
                let obj = item :?> JObject
                let lightType = obj.["type"].Value<string>()
                match lightType with
                | "diffuse" -> {
                    emission = obj.["emission"].ToObject<float array>(serializer)
                    twoSided = obj.["two-sided"].Value<bool>()
                  }
                | _ -> raise (JsonSerializationException("Unknown light type"))
            ) |> Seq.toArray
        lights :> obj

// Define Instance type with converters for optional int fields
type Instance = {
    primitive: int
    [<JsonConverter(typeof<OptionIntConverter>)>]
    material: int option
    [<JsonConverter(typeof<OptionIntConverter>)>]
    light: int option
}

// Define Node type
type node = {
    instance: obj // Supports both array and single int
    children: int array
    [<JsonConverter(typeof<OptionIntConverter>)>]
    transform: int option // Index into transforms array
}

// Define Barnacle type without rootInstances
type Barnacle = {
    nodes: node array
    transforms: transform array
    instances: Instance array
    [<JsonConverter(typeof<PrimitiveArrayConverter>)>]
    primitives: Primitive array
    [<JsonConverter(typeof<MaterialArrayConverter>)>]
    materials: Material array
    [<JsonConverter(typeof<LightArrayConverter>)>]
    lights: Light array
}

type Parser(filePath: string) =
    let barnacle = 
        try
            let json = File.ReadAllText(filePath, System.Text.Encoding.UTF8)
            Some (JsonConvert.DeserializeObject<Barnacle>(json))
        with
        | ex -> 
            printfn "Failed to parse file: %s" ex.Message
            None

    member _.Barnacle = barnacle

    member private this.ParsePrimitives (primitives: Primitive array) =
        printfn "Primitive Details:"
        primitives 
        |> Array.iteri (fun i prim ->
            printfn "Primitive %d:" i
            match prim with
            | Mesh m ->
                printfn "  Type: Mesh"
                printfn "  Vertex Count: %d" (Array.length m.vertices / 3)
                printfn "  Vertices: [%s]" (String.Join(", ", m.vertices))
                printfn "  Triangle Count: %d" (Array.length m.indices / 3)
                printfn "  Indices: [%s]" (String.Join(", ", m.indices))
            | MeshURI uri ->
                printfn "  Type: Mesh (External)"
                printfn "  URI: %s" uri
            | Sphere r ->
                printfn "  Type: Sphere"
                printfn "  Radius: %.1f" r
            printfn ""
        )

    member private this.ParseMaterials (materials: Material array) =
        printfn "Material Details:"
        materials 
        |> Array.iteri (fun i mat ->
            printfn "Material %d:" i
            match mat with
            | Diffuse albedo ->
                printfn "  Type: Diffuse"
                printfn "  Albedo: [%.2f, %.2f, %.2f]" albedo.[0] albedo.[1] albedo.[2]
            printfn ""
        )

    member private this.ParseLights (lights: Light array) =
        printfn "Light Details:"
        lights 
        |> Array.iteri (fun i light ->
            printfn "Light %d:" i
            printfn "  Type: Diffuse"
            printfn "  Emission: [%.1f, %.1f, %.1f]" light.emission.[0] light.emission.[1] light.emission.[2]
            printfn "  Two-sided: %b" light.twoSided
            printfn ""
        )

    member this.Display() =
        match barnacle with
        | Some b ->
            printfn "Nodes Count: %d" (Array.length b.nodes)
            printfn "Transforms Count: %d" (Array.length b.transforms)
            printfn "Instances Count: %d" (Array.length b.instances)
            printfn "Materials Count: %d" (Array.length b.materials)
            printfn "Lights Count: %d" (Array.length b.lights)

            // Display Nodes
            printfn "\nNode Details:"
            b.nodes 
            |> Array.iteri (fun i node ->
                printfn "Node %d:" i
                let instanceStr = 
                    match node.instance with
                    | :? array<obj> as arr -> sprintf "Array [%s]" (String.Join(", ", arr))
                    | :? System.Collections.IEnumerable as seq when not (seq :? array<obj>) -> 
                        let items = seq |> Seq.cast<obj> |> Seq.map string |> String.concat ", "
                        sprintf "Sequence [%s]" items
                    | single -> sprintf "Single: %A" single
                printfn "  Instance: %s" instanceStr
                let children = node.children |> Option.ofObj |> Option.defaultValue [||]
                printfn "  Children: [%s]" (String.Join(", ", children))
                printfn "  Transform: %s" (node.transform |> Option.map string |> Option.defaultValue "None")
                printfn ""
            )

            // Display Transforms
            printfn "Transform Details:"
            b.transforms 
            |> Array.iteri (fun i transform ->
                printfn "Transform %d:" i
                transform.keyframes 
                |> Array.iteri (fun j keyframe ->
                    printfn "  Keyframe %d:" j
                    printfn "    Time: %.1f" keyframe.time
                    keyframe.scale |> Option.iter (fun s -> printfn "    Scale: [%s]" (String.Join(", ", s)))
                    keyframe.rotation |> Option.iter (fun r -> printfn "    Rotation: [%s]" (String.Join(", ", r)))
                    keyframe.translation |> Option.iter (fun t -> printfn "    Translation: [%s]" (String.Join(", ", t)))
                )
                printfn ""
            )

            // Display Instances
            printfn "Instance Details:"
            b.instances 
            |> Array.iteri (fun i instance ->
                printfn "Instance %d:" i
                printfn "  Primitive: %d" instance.primitive
                instance.material |> Option.iter (fun m -> printfn "  Material: %d" m)
                instance.light |> Option.iter (fun l -> printfn "  Light: %d" l)
                printfn ""
            )

            // Call integrated parsing methods
            this.ParsePrimitives b.primitives
            this.ParseMaterials b.materials
            this.ParseLights b.lights
        | None -> 
            printfn "No data to display"