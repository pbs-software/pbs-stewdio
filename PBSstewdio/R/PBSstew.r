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

.updateGUI <- function()  ## update the main GUI  ## (RH 240430)
{
	getWinVal(winName="stewPaths",scope="L")  ## because the window lives in memory once it's created
	Rpathlist = lapply(1:4,function(i){Rpaths[i,"path"]})
	names(Rpathlist) = if (dim(Rpaths)[2]>1) Rpaths[,"name"] else  rownames(Rpaths)
	setWinVal(Rpathlist,"PBSstew")
}
.win.paths <- function(winName="PBSstew")  ## (RH 240430)
{
	getWinVal(winName=winName, scope="L")
	Rpaths = data.frame(path=c(dirRcmd,dirMtex,dirRepo,dirBuild), row.names=c("dirRcmd","dirMtex","dirRepo","dirBuild"))
	#tput(Rpaths)
	pathWin = c("window name=stewPaths title=\"Paths for PBSstewdio Package Building\" onclose=.updateGUI",
		"grid 2 1 sticky=W",
		"  object name=Rpaths width=\"70\"",
		"  grid 1 3 sticky=E",
		"    entry name=Rfile value=Rpaths.txt width=10 label=filename edit=F noeditbg=powderblue mode=character",
		"    button text=save bg=green function=doAction action=\"getWinVal(winName=\`stewPaths\`,scope=\`L\`); write.table(Rpaths, \`Rpaths.txt\`, col.names=FALSE)\"",
		"    button text=load bg=gold function=doAction action=\"Rpaths=read.table(\`Rpaths.txt\`, col.names=c(\`name\`,\`path\`)); setWinVal(list(Rpaths=matrix(Rpaths$path,ncol=1)),\`stewPaths\`)\""
	)
	createWin(pathWin, astext=TRUE)
#browser();return()
}
.win.check <- function(winName="PBSstew")  ## (RH 240430)
{
	#path.save = shell("path", intern=T)
	#on.exit(shell(paste0("set ",path.save)))  ## doesn't appear to be enacted
	path.save = Sys.getenv("PATH")
	on.exit(Sys.setenv(PATH=path.save))

	getWinVal(winName=winName, scope="L")
	if (basename(dirRepo)!=package)
		dirRepo = file.path(dirRepo, package)
	if (!dir.exists(dirRepo))
		stop (paste0("Repo: '",dirRepo,"' does not exists"))
	if (!dir.exists(dirBuild))
		stop (paste0("Build: '",dirBuild,"' does not exists"))
	if (!dir.exists(dirRcmd))
		stop (paste0("Rcmd: '",dirRcmd,"' does not exists"))
	file.copy(from=dirRepo, to=dirBuild, overwrite=T, recursive=T, copy.date=T)
	check  = paste0(dirRcmd, "/R CMD check ")
	Rcmd   = paste0(check, file.path(dirBuild, package))
	path.check =  paste0(c("PATH=.",convSlashes(c(dirRcmd,dirMtex))),collapse=";")
	Sys.setenv(PATH=path.check)
	#cmd = paste0("set ", path.check, " & ", Rcmd)  ## setting path directly does not work
#browser();return()
	results = shell(Rcmd, intern=T)
}
#.win.batch <- function(winName="PBSstew") [deprecated]
#{
#	winval = getWinVal(winName=winName)
#	bats   = c("Paths.bat", "PathCheck.bat", "Check.bat", "Build.bat")
#	batbat = paste0(winval$dirBat, "/", winval$preBat, bats)
#	batbat = c(paste0(winval$dirBat,"/Rcopy.bat"), batbat)
#	openFile(batbat)
#}
stew()