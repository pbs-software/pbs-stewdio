.onAttach <- function(lib,pkg)
{
	pkg_info = utils::sessionInfo( package="PBSstewdio" )$otherPkgs$PBSstewdio
	if( is.character( pkg_info$Packaged ) )
		pkg_date <- strsplit( pkg_info$Packaged, " " )[[1]][1]
	else
		pkg_date  <- date()
	
	#userguide_path <- system.file( "doc/PBSawatea.pdf", package = "PBSawatea" )
	year <- substring(date(),nchar(date())-3,nchar(date()))

	packageStartupMessage("
-----------------------------------------------------------
PBS Stewdio ", pkg_info$Version, " -- Copyright (C) 2024-",year," Fisheries and Oceans Canada

Packaged on ", pkg_date, "
Pacific Biological Station, Nanaimo

All available PBS packages can be found at
https://github.com/pbs-software
-----------------------------------------------------------

")
}
# No Visible Bindings
# ===================
if(getRversion() >= "2.15.1") utils::globalVariables(names=c(
	"dirBuild", "dirMtex",  "dirRcmd", "dirRepo",
	"package",
	"Rpaths"
	), package="PBSstewdio")
