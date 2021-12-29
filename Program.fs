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
	let pattern = Regex(@"^(?<ts>[a-zA-Z0-9]+_\d+).*\.(?<ext>\w+)$") in
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
				let sidecar_path = System.IO.Path.Join(dir, sidecar_name) in
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
	|> List.map (fun ((ts, ext), files) ->
		eprintfn "%s:" ts;
		for f in files do
			eprintfn "\t%s" f.name;
		done;
		(ext, files)
	)
;;

type DupeDetails =
	| JpgDupe of similarity:float
	| Mp4Dupe of duration:TimeSpan * dateCreated:DateTime
	with
		static member toString d = match d with
			| JpgDupe(s) -> sprintf "%.2f" s
			| Mp4Dupe (s,t) -> sprintf "%A, %A" s t
	end

type Dupe = {
	small_dupe: File;
	other: File;
	details: DupeDetails;
}

let get_dupes_base (identity: (File -> Option<'I>)) (is_dupe: (('I * 'I) -> Option<DupeDetails>)) (files: File list) =
	let identities =
		match files with
		| [f] -> []
		| _ ->
			files
			|> List.choose (
				fun f ->
					match identity f with
					| Some(id) -> Some (f, id)
					| None -> None
			)
	in
	let dupes =
		identities
		|> Seq.choose (fun (dupe_candidate, dupe_hash) ->
			let dupes_of = identities |> List.choose (fun (other_candidate, other_hash) ->
				if dupe_candidate = other_candidate then None
				else
					let dupe_details = is_dupe(dupe_hash, other_hash) in
					let dupe_size = dupe_candidate.size in
					let other_size = other_candidate.size in
					match dupe_details with
					| Some(d) when dupe_size < other_size/2L ->	Some {
							Dupe.small_dupe = dupe_candidate;
							other = other_candidate;
							details = d;
						}
					| Some(d) when dupe_size < other_size ->
						eprintfn "deets of \n\t%s and %s match but \n\t%d\n\t%d" dupe_candidate.name other_candidate.name dupe_size other_size;
						None
					| _ -> None
			) in
			match dupes_of with
			|[d] -> Some(d)
			|[] -> None
			|many ->
				let desc = many
					|> Seq.map (fun d -> DupeDetails.toString d.details)
					|> String.concat "," in
				eprintfn "warning: got multiple dupe candidates for %s.\n%s" dupe_candidate.name desc;
				None
		)
	in dupes
;;

let get_dupes_jpg () =
	let hasher = CoenM.ImageHash.HashAlgorithms.PerceptualHash() in
	get_dupes_base (
		fun f ->
			eprintf "Hashing %s..." f.path;
			use s = File.OpenRead(f.path) in
			let hash = hasher.Hash(s) in
			eprintfn "..done";
			Some hash
	) (
		fun (dupe_hash, other_hash) ->
			let similarity = CompareHash.Similarity(dupe_hash, other_hash) in
			if similarity >= 98.0 then
				Some(JpgDupe(similarity))
			else None
	)
;;


open MetadataExtractor
open MetadataExtractor.Formats.QuickTime

let As<'T> (x: Object): ('T option) =
	if x :? 'T then
		Some(x :?> 'T)
		else None

let get_dupes_mp4 =
	get_dupes_base (
		fun f ->
			use stream = File.Open(f.path, FileMode.Open) in
			let dirs = QuickTimeMetadataReader.ReadMetadata stream in
			let qtmeta = dirs |> Seq.choose (As<QuickTimeMovieHeaderDirectory>) |> Seq.tryHead in
			match qtmeta with
			| None ->
				eprintfn "Warning: no QtMovieHeaderDir in %s" f.name;
				None
			| Some(qtmeta) ->
				let duration = qtmeta.GetObject (QuickTimeMovieHeaderDirectory.TagDuration) :?> System.TimeSpan in
				let created = qtmeta.GetDateTime (QuickTimeMovieHeaderDirectory.TagCreated) in
				Some (duration, created)

	) (
		fun (dupe_hash, other_hash) ->
			let dupe_duration, dupe_date = dupe_hash in
			let other_duration, other_date = other_hash in
			let duration_diff = dupe_duration - other_duration in
			if Math.Abs(duration_diff.TotalSeconds) < 0.1 && dupe_date = other_date  then
				Some(Mp4Dupe(dupe_duration, dupe_date))
			else None
	)
;;

let get_dupes (ext:string) (files: File list) =
	match ext.ToLower() with
	// | "jpg" -> get_dupes_jpg files
	| "jpg" -> get_dupes_jpg () files
	| "mp4" -> get_dupes_mp4 files
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