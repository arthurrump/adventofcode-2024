open System.IO

let diskMap =
    File.ReadAllText("input.txt").Trim().ToCharArray()
    |> Array.map (fun ch -> int ch - int '0')

let fileCount = (diskMap.Length + 1) / 2

let part1Smart () =
    Seq.init (Array.sum diskMap) bigint
    |> Seq.mapFold (fun (dmFrontIndex, dmFrontInIndex, dmBackIndex, dmBackInIndex) i ->
        let fileId =            
            if dmFrontIndex % 2 = 0 
            then dmFrontIndex / 2
            else dmBackIndex / 2
        if dmFrontIndex < dmBackIndex then
            let dmFrontIndex', dmFrontInIndex' = 
                if dmFrontInIndex < diskMap[dmFrontIndex] - 1
                then dmFrontIndex, dmFrontInIndex + 1
                else dmFrontIndex + 1, 0
            let dmBackIndex', dmBackInIndex' =
                if dmFrontIndex % 2 = 0 then
                    dmBackIndex, dmBackInIndex
                else
                    if dmBackInIndex < diskMap[dmBackIndex] - 1
                    then dmBackIndex, dmBackInIndex + 1
                    else dmBackIndex - 2, 0
            Some (i * bigint fileId), (dmFrontIndex', dmFrontInIndex', dmBackIndex', dmBackInIndex')
        elif dmFrontIndex = dmBackIndex && dmBackInIndex < diskMap[dmBackIndex] then
            Some (i * bigint fileId), (dmFrontIndex, dmFrontInIndex, dmBackIndex, dmBackInIndex + 1)
        else None, (dmFrontIndex, dmFrontInIndex, dmBackIndex, dmBackInIndex)
    ) (0, 0, diskMap.Length - 1, 0)
    |> fst
    |> Seq.takeWhile (Option.isSome)
    |> Seq.choose id
    |> Seq.sum

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

printfn "Part 1: %A" (part1 ())
