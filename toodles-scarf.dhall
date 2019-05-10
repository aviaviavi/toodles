-- import scarf dhall definitions at the top
-- then, you'll specify a list of distributions (one per platform)
-- `URI` can point to a local tar archive or a remote one
let platforms = ../scarf-server/scarf/example-packages/scarf.dhall

in  { name =
		"toodles"
	, author =
		"Avi Press"
	, copyright =
		"2019 Avi Press"
	, license =
		"MIT"
	, version =
		"1.2.1"
	, distributions =
		[ { platform =
			  platforms.mac
		  , simpleExecutableInstall =
			  "toodles"
		  , uri =
			  "./toodles-1.2.1-mac.tar.gz"
		  , includes =
			  [ "web" ]
		  }
		, { platform =
			  platforms.linux_x86_64
		  , simpleExecutableInstall =
			  "toodles"
		  , uri =
			  "./toodles-1.2.1-linux.tar.gz"
		  , includes =
			  [ "web" ]
		  }
		]
	}
