// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
#indent "off"

open System;
open System.IO;
open System.Text.RegularExpressions
open CoenM.ImageHash

// Define a function to construct a message to print
let from whom =
	sprintf "from %s" whom
;;

type File = {
	path: string;
	name: string;
	ts: string;
	ext: string;
	size: int64;
	sidecar: string option;
}

let list_dir (dir:string) =
	let pattern = Regex(@"^(?<ts>\d{8}_\d{6})_\w{8}\.(?<ext>\w+)$") in
	let files =
		Directory.GetFiles(dir)
		|> Seq.choose (fun path ->
			let fn = System.IO.Path.GetFileName(path) in
			match pattern.Match(fn) with
			|null -> None
			|m -> Some((fn, path, m))
		)
		|> Seq.map (fun (fn, path, m) -> {
			File.name = fn;
			path = path;
			ts = m.Groups.["ts"].Value;
			ext = m.Groups.["ext"].Value;
			size = FileInfo(path).Length;
			sidecar =
				let sidecar_name = System.IO.Path.GetFileNameWithoutExtension(path) + ".json" in
				if File.Exists(sidecar_path) then Some(sidecar_path) else None
		})
		|> Seq.filter (fun f -> f.ext <> "json")
		|> Seq.sortBy (fun f -> f.name)
		|> List.ofSeq
	in
	files
;;

let get_sets (files: File list) =
	files
	|> List.groupBy (fun f -> (f.ts, f.ext))
	|> List.map (fun ((ts, ext), files) -> (ext, files))
;;

type Dupe = {
	small_dupe: File;
	other: File;
	similarity: float;
}


let get_dupes_jpg (files: File List) =
	let hasher = CoenM.ImageHash.HashAlgorithms.PerceptualHash() in
	let hashes =
		files
		|> List.map (fun f ->

			printf "Hashing %s..." f.path;
			use s = File.OpenRead(f.path) in
			let hash = hasher.Hash(s) in
			printfn "..done";
			(f, hash)
		)
		in
	let dupes = seq {
		for (dupe_candidate, dupe_hash) in hashes do
			let dupes_of = hashes |> List.choose (fun (other_candidate, other_hash) ->
				if dupe_candidate = other_candidate then None
				else
					let similarity = CompareHash.Similarity(dupe_hash, other_hash) in
					let dupe_size = dupe_candidate.size in
					let other_size = other_candidate.size in
					if similarity > 98.0 && dupe_size < other_size/4L then
						Some {
							Dupe.small_dupe = dupe_candidate;

							other = other_candidate;
							similarity=similarity
						}
					else None
			) in
			match dupes_of with
			|[d] -> Some(d)
			|[] -> None
			|many ->
				let sims = many
					|> Seq.map (fun d -> d.similarity)
					|> Seq.map (sprintf "%.2f")
					|> String.concat "," in
				eprintfn "warning: got multiple dupe candidates for %s.\n%s" dupe_candidate.name sims;
				None
		done
	}
	|> Seq.choose (id)
	in
	dupes
;;


let get_dupes (ext:string) (files: File list) =
	match ext with
	| "jpg" -> get_dupes_jpg files
	| _ -> Seq.empty
	// if ext is jpg:
	// get hashes of all in set
	// compare each hash with all other hashes
	//   - if similar, and size < other size / 2, then yield a dupe of this.

	// if ext is mp4:
	// if set is size 2
	// if smallest size < other size / 10, then yield dupe.
;;

[<EntryPoint>]
let main argv =
	let dirs = match argv with
		| [||] -> ["."]
		| dirs -> List.ofArray dirs
		in
	let get_dupes dir =
		let files = list_dir dir in
		let sets = get_sets files in
		let dupes =
			Seq.collect (fun (ext, files) -> get_dupes ext files ) sets
			|> List.ofSeq in
		dupes in
	let dupes =
		dirs
		|> Seq.collect get_dupes
		|> List.ofSeq
		in
	for d in dupes do
		let MB i = float(i)*10E-6 in
		eprintfn "Found a dupe:";
		eprintfn "\t%s, %.1fM, probs dupe of" d.small_dupe.name (MB d.small_dupe.size);
		eprintfn "\t%s, %.1fM." d.other.name (MB d.other.size);
	done;
	Console.Error.Flush();
	Console.Out.Flush();
	for d in dupes do
		printfn "%s" d.small_dupe.path;
		match d.small_dupe.sidecar with
			| Some(s) -> printfn "%s" s
			| _ -> ();
	done;
	0 // return an integer exit code
;;