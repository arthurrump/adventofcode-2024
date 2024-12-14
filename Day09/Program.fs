open System.IO

let diskMap =
    File.ReadAllText("input.txt").Trim().ToCharArray()
    |> Array.map (fun ch -> int ch - int '0')

let compact disk =
    let disk = Array.copy disk
    for i = 0 to Array.length disk - 1 do
        if disk[i] = ValueNone then
            let swap = Array.findIndexBack ValueOption.isSome disk
            if swap > i then
                disk[i] <- disk[swap]
                disk[swap] <- ValueNone
    disk

let part1 () =
    let disk =
        [| for i, d in Array.indexed diskMap do
            if i % 2 = 0
            then for _ = 1 to d do ValueSome (i / 2)
            else for _ = 1 to d do ValueNone |]
    compact disk
    |> Seq.indexed
    |> Seq.sumBy (fun (i, d) -> bigint (i * ValueOption.defaultValue 0 d))

// printfn "Part 1: %A" (part1 ())

let diskFileMap =
    diskMap
    |> Array.indexed
    |> Array.map (fun (i, len) ->
        let file = if i % 2 = 0 then ValueSome (i / 2) else ValueNone
        file, len
    )

let compactFile i (diskFileMap: (int voption * int) array) =
    let file, len = diskFileMap[i]
    if ValueOption.isSome file then
        let insert = Array.tryFindIndex (fun (f, l) -> f = ValueNone && l >= len) diskFileMap
        match insert with
        | Some index when index < i ->
            if snd diskFileMap[index] = len then
                let res = Array.copy diskFileMap
                res[index] <- (file, len)
                res[i] <- (ValueNone, len)
                res
            else
                let res = Array.insertAt index diskFileMap[i] diskFileMap
                res[index + 1] <- (ValueNone, snd res[index + 1] - len)
                res[i + 1] <- (ValueNone, len)
                res
        | _ ->
            diskFileMap
    else
        diskFileMap

let compactDisk diskFileMap =
    let mutable diskFileMap = diskFileMap
    for i = Array.length diskFileMap - 1 downto 0 do
        diskFileMap <- compactFile i diskFileMap
    diskFileMap

let part2 () =
    compactDisk diskFileMap
    |> Array.fold (fun (index, sum) (file, len) ->
        match file with
        | ValueNone ->
            (index + len, sum)
        | ValueSome fileId ->
            let sum' = List.sum [ for i in index .. index + len - 1 -> bigint i * bigint fileId ]
            (index + len, sum + sum')
    ) (0, bigint 0)
    |> snd

printfn "Part 2: %A" (part2 ())
