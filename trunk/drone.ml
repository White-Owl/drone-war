class drone =
	object
		val mutable source_text = ""
		val mutable drone_name = ""

		method get_source = source_text
		method get_drone_name = drone_name

		method load file_name =
			drone_name <- Filename.chop_extension (Filename.basename file_name);
			let chan_in = Pervasives.open_in file_name in
				source_text <- IO.read_all (IO.input_channel(chan_in));
				Pervasives.close_in chan_in;
end;;
