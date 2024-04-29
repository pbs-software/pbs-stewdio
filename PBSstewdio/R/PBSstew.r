## stew---------------------------------2024-04-25
## Starts the primary GUI interface
## Authors: Jon T. Schnute, Rowan Haigh
## ---------------------------------------------RH
stew <- function(pkg="PBSstewdio", wdf="stewWin.txt") #, pathfile="ADpaths.txt")
{
#	.initOptions()
#	if (!is.null(pathfile) && file.exists(pathfile))
#		readADpaths(pathfile)
#	else pathfile="ADpaths.txt"
#	pkg="PBSpkg"

	## Perhaps rename to something else -- too similar to .PBSadmb
#	assign("PBSadmb",list(pkg=pkg,call=match.call(),args=args(admb),useCols=NULL),envir=.PBSadmbEnv)

	pdir <- system.file(package=pkg)          ## package directory
	wdir <- paste(pdir,"/win",sep="")         ## window description file directory
	#pdir <- "C:/Users/haighr/Files/Projects/R/Source/pbs-software/pbs-stewdio/trunk/PBSstewdio"  ## just fornow
	#wdir <- "C:/Users/haighr/Files/Projects/R/Source/pbs-software/pbs-stewdio/trunk/PBSstewdio/inst/win"  ## just fornow
#	edir <- paste(pdir,"/examples",sep="")    ## examples directory
	tdir <- tempdir()                         ## temporary working directory
	twdf <- paste(tdir,"/",wdf,sep="")        ## temporary window description file
	twdf <- convSlashes(twdf,os="unix")

	stripExt <- function(x) { return(sub("[.].{1,3}$", "", x)) }

	win.filename <- paste(wdir,wdf,sep="/")
	temp <- readLines(win.filename)
	
	## insert examples into window description file menuitems
#	etpl <- basename(Sys.glob(file.path(edir,"*.tpl"))) ## TPL files in examples directory
#	eprf <- stripExt(etpl)                              ## strip off extensions
#	enew <- character(0)
#	edir <- gsub( "\\\\", "/", edir )
#	for (i in eprf) 
#		enew=c(enew,paste("menuitem label=",i," function=doAction action=\"copyFiles(`",
#			i,".`,srcdir=`",edir,"`); convOS(paste(`",i,"`,c(`.tpl`,`.dat`,`.pin`,`.r`),sep=``))\"",sep=""))
#	temp <- gsub("@nitems",length(eprf),temp)
#	temp <- gsub("@menuitems",paste(enew,collapse="\n\t"),temp)
#
#	temp = gsub("@wdf",twdf,temp)
#	temp = gsub("@pathfile",pathfile,temp)
	
	## create the window (from temp string)
	temp <- unlist( strsplit(temp, "\n" ) )
	## createWin(twdf, TRUE)
	writeLines(temp,con=twdf)
	createWin(twdf)

	## set some values
#	.load.prefix.droplist()
#	loadOptionsGUI( atcall(.PBSadmb) )
#	isOK <- .win.checkADopts()
#
#	if( isOK == FALSE && .Platform$OS.type == "windows" ) {
#		cat( "\nADMB or MinGW are not installed in the default location.\n" )
#		cat( "If needed, follow install instructions from GUI menu <Install>\n" )
#		cat( "If ADMB is installed on your system, you can manually set the ADMB or MinGW path values\n" )
#		cat( "in the GUI to point to your own installations.\n" )
#	}

	#TODO need centralized window variable init (is it done anywhere?)
#	setWinVal( list( currentdir.values = getwd() ) )
#	setWinVal( list( currentdir = getwd() ) )
#	#setWinVal( list( optfile = optfile ) )
#	setWinVal( list( optfile = pathfile ) )
	#setPBSext("bat", '"C:/Windows/notepad.exe" %f')  ## open bat file in editor
	setPBSext("bat", '"C:/Apps/UltraEdit/Uedit32.exe" %f')  ## open bat file in editor
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~pkg

.win.batch <- function(winName="PBSstew")
{
	winval = getWinVal(winName=winName)
	bats   = c("Paths.bat", "PathCheck.bat", "Check.bat", "Build.bat")
	batbat = paste0(winval$dirBat, "/", winval$preBat, bats)
	batbat = c(paste0(winval$dirBat,"/Rcopy.bat"), batbat)
	openFile(batbat)
}
.win.check <- function(winName="PBSstew")
{
	getWinVal(winName=winName, scope="L")
	if (basename(repo)!=package)
		repo = file.path(repo,package)
	file.copy(from=file.path(dirRepo,package), to=dirBuild, recursive=T, copy.date=T)
	check  = paste0(dirBat, "/", preBat, "Check")
	cmd    = paste0(check, " ", package)
	print(cmd) ## abandoned for now
}
stew()