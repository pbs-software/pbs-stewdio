## stew---------------------------------2024-05-02
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
	.loadGUI()
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
	#setPBSext("bat", '"C:/Apps/UltraEdit/Uedit32.exe" %f')  ## open bat file in editor
	mess = paste0("setPBSext(\"bat\", '\"", getWinVal(winName="PBSstew")$dirEdit, "\" %f')")
	eval(parse(text=mess))
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~stew

#.flush.cat-----------------------------2011-09-30
# Flush the cat down the console (from PBStools)
.flush.cat = function(...)
{
	cat(...); flush.console(); invisible()
}

## rebug--------------------------------2024-05-09
##  Reflect and debug: print out objects for clues when debugging.
## -----------------------------------------------RH
rebug <- function(label, object, delim=c("=","~"), browse=FALSE)
{
	## browse option may be used in future
	delims = rep(delim,2)[1:2]
	.flush.cat(rep(delim[1],50), "<start>", "\n", sep="")
	.flush.cat(label, "\n", rep("-",nchar(label)),"\n", sep="")
	print(object)
	.flush.cat(rep(delim[2],50), "<end>", "\n\n", sep="")
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~rebug

## Dot functions 
##----------------------------------------------------------
.loadGUI <- function()  ## load pathways from Rpaths.txt to GUI; forget error checking  ## (RH 240508)
{
	if (!file.exists("Rpaths.txt"))
		return(invisible(paste0("No 'Rpaths.txt' in ", getwd())))
	Rpaths   = read.table("Rpaths.txt", col.names=c("name","path"))
#	isBuild  = is.element(Rpaths$name,"dirBuild")
#	dirBuild = Rpaths$path[isBuild]
#	package  = getWinVal(winName="PBSstew")$package
#	if (basename(dirBuild)==package) {
#		mess = paste0("Specified Build directory in 'Rpaths.txt':\n\n", dirBuild, "\n\nincludes package package name '", package, "'\n\nSuggest set Build directory in 'Rpaths.txt' to one level above.")
#		showAlert(mess); stop(mess)
#	}
	Rpathlist = lapply(1:nrow(Rpaths),function(i){Rpaths[i,"path"]})
	names(Rpathlist) = if (dim(Rpaths)[2]>1) Rpaths[,"name"] else  rownames(Rpaths)
	setWinVal(Rpathlist,"PBSstew")
	invisible("OK")
}
##----------------------------------------------------------
.updateGUI <- function()  ## update the main GUI; forget error checking (RH 240508)
{
	getWinVal(winName="stewPaths",scope="L")  ## because the window lives in memory once it's created
#	isBuild  = is.element(rownames(Rpaths),"dirBuild")
#	dirBuild = Rpaths$path[isBuild]
#	package  = getWinVal(winName="PBSstew")$package
#	if (basename(dirBuild)==package) {
#		mess = paste0("\nSpecified Build directory in pathway GUI:\n", dirBuild, "\nincludes package package name '", package, "'\nSuggest set Build directory to one level above.")
#		closeWin(name="stewPaths")
#		stop(mess)  ## showAlert cause infinite loop
#	}
	Rpathlist = lapply(1:nrow(Rpaths),function(i){Rpaths[i,"path"]})
	names(Rpathlist) = if (dim(Rpaths)[2]>1) Rpaths[,"name"] else  rownames(Rpaths)
	setWinVal(Rpathlist,"PBSstew")
}
##----------------------------------------------------------
.win.paths <- function(winName="PBSstew")  ## (RH 240502)
{
	getWinVal(winName=winName, scope="L")
	Rpaths = data.frame(path=c(dirRcmd,dirMtex,dirEdit,dirRepo,dirBuild), row.names=c("dirRcmd","dirMtex","dirEdit","dirRepo","dirBuild"))
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
##----------------------------------------------------------
.win.check <- function(winName="PBSstew") ## (RH 240508)
{
	#path.save = shell("path", intern=T)
	#on.exit(shell(paste0("set ",path.save)))  ## doesn't appear to be enacted
	path.save = Sys.getenv("PATH")
	on.exit(Sys.setenv(PATH=path.save))

	getWinVal(winName=winName, scope="L")
	## Set of directory checks
	if (basename(dirRepo)==package) {
		mess = paste0("Specified Repo directory:\n\n", dirRepo, "\n\nincludes package name '", package, "'\n\nSet Repo directory to one level above.")
		showAlert(mess); stop(mess)
	}
	if (basename(dirBuild)==package) {
		mess = paste0("Specified Build directory:\n\n", dirBuild, "\n\nincludes package package name '", package, "'\n\nSet Build directory to one level above.")
		showAlert(mess); stop(mess)
	}
	if (!dir.exists(dirRepo)) {
		mess = paste0("Repo: '",dirRepo,"'\ndoes not exist.")
		showAlert(mess); stop(mess)
	}
	if (!dir.exists(dirBuild)) {
		mess = paste0("Build: '",dirBuild,"'\ndoes not exist.")
		showAlert(mess); stop(mess)
	}
	if (!dir.exists(dirRcmd)) {
		mess = paste0("Rcmd: '",dirRcmd,"'\ndoes not exist.")
		showAlert(mess); stop(mess)
	}
	if (dir.exists(file.path(dirBuild,package)))
		doCopy = getYes(paste0("'", package, "' exists in \n", dirBuild,"\nDo you want to overwrite?"))
	if (doCopy) {
		.flush.cat("Copying '", package, "'\n\tfrom: ", dirRepo, "\n\tto:   ", dirBuild, "\n", sep="")
		file.copy(from=file.path(dirRepo, package), to=dirBuild, overwrite=TRUE, recursive=TRUE, copy.date=TRUE)
	}
#browser();return()
	if (getWinAct("PBSstew")[1]!="build") {
		check  = paste0(dirRcmd, "/R CMD check ")
		if (cran)
			check = paste0(check, " --as-cran ")
		Rcmd   = paste0(check, file.path(dirBuild, package))
		path.check =  paste0(c("PATH=.",convSlashes(c(dirRcmd,dirMtex))),collapse=";")
		Sys.setenv(PATH=path.check)
		.flush.cat("Checking package '", package, "'\n\t*** may take a few minutes (wait for return of R console) ***\n", sep="")
		results.check = shell(Rcmd, intern=T)
		tput(results.check)
		print(results.check)
	}
}
##----------------------------------------------------------
.win.build <- function(winName="PBSstew") ## (RH 240508)
{
	path.save = Sys.getenv("PATH")
	on.exit(Sys.setenv(PATH=path.save))

	getWinVal(winName=winName, scope="L")
	if (!dir.exists(dirBuild)) {
		mess = paste0("Build: '",dirBuild,"'\ndoes not exist.")
		showAlert(mess); stop(mess)
	}
	if (!dir.exists(file.path(dirBuild,package))) {
		## Grab the package from the Repo via .win.check()
		setWinAct(winName="PBSstew", action="build")  ## redundant to GUI button push but need it for debugging
		.win.check()
	}
	if (!dir.exists(dirRcmd)) {
		mess = paste0("Rcmd: '",dirRcmd,"'\ndoes not exist.")
		showAlert(mess); stop(mess)
	}
	if (any(btype)) {
		bbtype = names(btype)[btype]
		cwd = getwd()
		on.exit(setwd(cwd), add = TRUE)
		for (b in bbtype) {
			if (b=="src")
				build  = paste0(dirRcmd, "/R CMD build --no-build-vignettes --compact-vignettes ")
			if (b=="bin")
				build  = paste0(dirRcmd, "/R CMD INSTALL --build --compact-docs --compile-both ")
			#Rcmd = paste0(build, file.path(dirBuild, package))  ## places build in user's cwd instead of dirBuild
			Rcmd = paste0(build, package)
			.flush.cat(Rcmd, "\n\tin: ", dirBuild, "\n")
#browser();return()
			setwd(dirBuild)  ## goto the user-specified build directory for package creation
			path.build =  paste0(c("PATH=.",convSlashes(c(dirRcmd,dirMtex))),collapse=";")
			Sys.setenv(PATH=path.build)
			results.build = shell(Rcmd, intern=T)
			tput(results.build)
			print(results.build)
		} ## end b loop bbtype
	} ## end if any btype
}
##----------------------------------------------------------
.win.edit = function(winName="PBSstew")  ## (RH 240509)
{
	getWinVal(winName=winName, scope="L")
	epath = switch (etype, 'repo'=file.path(dirRepo,package), 'build'=file.path(dirBuild,package), ".")
	#shell(paste0("dir ", convSlashes(epath), " /s /b /oe"), intern = TRUE)  ## alternative but too complicated
	files = list.files(path=epath, full.names=TRUE, recursive=TRUE)  ## get full path names to avoid confusion
	if (length(files)==0) {
		mess = paste0("No files detected in\n'", epath, "'")
		showAlert(mess); stop(mess)
	}
	if (file.exists(file.path(getwd(), stw))) {
		inpat  = readLines(file.path(getwd(), stw), warn=FALSE)
		outpat =  gsub("(\\,)?\\s+","|", gsub("\\\"","",inpat))
		if (length(outpat)>1)
			outpat = paste0(outpat,collapse="|")
		if (debug) rebug("outpat",outpat)
#browser();return()
		good = c(grep(outpat, files, value=TRUE))
		if (length(good)==0) {
			mess = paste0("User's input pattern from:\n'", file.path(getwd(), stw), "'\ndid not match any files.\n")
			showAlert(mess); stop(mess)
		}
	} else {
		bad.pattern = "\\.pdf$|\\.sty$|\\.bak$|\\.tar\\.gz$|\\.zip$|\\.bup|Copy|\\.rda$|\\.rds$|\\.gif$|\\.dll$|\\.o$"
		bad   = c(grep(bad.pattern, files, value=TRUE))
		good  = setdiff(files, bad)
	}
	fnams = basename(good)
	if (length(fnams)==0) {
		mess = paste0("No files detected in\n'", epath, "'")
		showAlert(mess); stop(mess)
	}
	ncol = ceiling(length(fnams)/25)  ## number columns in GUI when displaying file names (based on 30 files per column)
	if (debug) {
		rebug("epath", epath)
		rebug("files",files)
		rebug("fnams",fnams)
		rebug("ncol",ncol)
	}

	ext  = sapply(strsplit(fnams,split="\\."),function(x){if (length(x)==1) "misc" else rev(x)[1]})
	fdf  = data.frame(name=fnams, ext=ext, path=good)
	fdf  = fdf[order(fdf$ext),]  ## file name data frame
	tput(fdf) ## easiest way to transfer to .win.edit.file()
	fls  =  split(fdf,fdf$ext)   ## file name list by extension
	ford = rev(sort(sapply(fls,nrow)))

	## Subfunction to allocate files to column (RH 240509)
	allocator = function(x, percol=25) {
		xcum = cumsum(x)
		xcol = rep(0,length(x))
		ncol = 0
		for (i in 1:length(xcum)) {
			if (xcum[i] >= percol) {
				ncol = ncol + 1
#print(c(xcum[i], percol, ncol))
				xcol[i] = ncol
				percol = percol * ncol
			} else {
				xcol[i] = ncol
			}
		}
		names(xcol) = names(x)
		return(xcol)
	}
	fmap = allocator(ford)
#browser();return()

	#fmap = round(cumsum(ford) / (floor(sum(ford) / ncol)))
	#remap=.su(fmap); names(remap)=remap; remap[1:length(remap)]=1:length(remap)  ## need to revise fmap when #files/ext >> 30
	#fmap[1:length(fmap)] = remap[as.character(fmap)] ## need element-wise to retain names
	fgrp = split(ford, fmap) ## use revised fmap
	#ncol = length(remap)  ## number columns in GUI when displaying file names (based on 30 files per column)
	ncol = length(unique(fmap))  ## number columns in GUI when displaying file names (based on 25 files per column)
	if (debug) {
		rebug("ext", ext)
		rebug("fdf",fdf)
		rebug("ford",ford)
		rebug("fmap",fmap)
		rebug("fgrp",fgrp)
		rebug("ncol",ncol)
	}
#browser();return()

	## Start constructing fileWin
	fileWin = c(
		paste0("window name=editFiles title=\"Files in ", epath, "\""),
		paste0("grid 1 ", ncol, " sticky=NW byrow=F") )
	for (g in 1:length(fgrp)) {
		fg = fgrp[[g]]
		fileWin = c(fileWin,
			paste0("  grid ", length(fg) + ifelse(g==ncol,1,0), " 1 sticky=NW byrow=F") )  ## add an extra row for the Edit button
#browser();return()
		for (f in 1:length(fg)) {
			fext = names(fg)[f]
			fnam = fls[[fext]][,"name"]
#if(fext=="misc"){browser();return()}
			fileWin = c(fileWin,
				"    grid 2 1 sticky=W byrow=F",
				paste0("      label text=\"", fext, " files\" font=\"10 bold\" sticky=NW fg=blue"),
				paste0("      vector names=", fext, "files mode=logical vertical=T sticky=NW length=", length(fnam), " labels=\"", paste0(fnam,collapse=" "), "\" values=\"", paste0(rep("F",length(fnam)),collapse=" "), "\" vecnames=\"", paste0(fnam,collapse=" "), "\"")
				)
			if (g==ncol && f==length(fg)) {
				fileWin = c(fileWin,
					"    grid 2 1 sticky=SE pady=\"10 0\"",
					"      label text=\"Select files to edit\\nand press edit button\"",
					"      button text=edit font=\"bold 10\" bg=gold name=editnow sticky=SE function=.win.edit.file"
				)
			}
		} ## end f loop (# files in group)
	} ## end g loop (# groups)
#browser();return()
	if (debug) rebug("fileWin", fileWin)
	createWin(fileWin, astext=TRUE)
}
##----------------------------------------------------------
.win.edit.file = function(winName="editFiles")  ## (RH 240509)
{
	getWinVal(winName=winName, scope="L")
	allfiles = ls(pattern="files$")
	lstfiles = list()
	for (a in 1:length(allfiles)) {
		aa = allfiles[a]
		lstfiles[[aa]] = get(aa)
	}
	if (any(sapply(lstfiles,any))) {
		afiles = lstfiles[sapply(lstfiles,any)]
		efiles = unlist(lapply(afiles, function(x) {names(x)[x]} ))
		tget(fdf)  ## from .win.edit()
		unpackList(getWinVal(winName="PBSstew")[c("etype","dirEdit","dirBuild","dirRepo","package")])
		epath = switch (etype, 'repo'=file.path(dirRepo,package), 'build'=file.path(dirBuild,package), ".")
		#pfiles = file.path(epath, fdf$path[is.element(fdf$name,efiles)])
		pfiles = fdf$path[is.element(fdf$name,efiles)]  ## now has full path from '.win.edit()'
		cmd = paste0(getWinVal(winName="PBSstew")$dirEdit, " ", paste0(pfiles, collapse=" "))
#browser();return()
		shell(cmd, intern=T)
	} else {
		message ("WARNING: No files selected")
	}
}
#stew()