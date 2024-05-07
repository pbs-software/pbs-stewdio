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
	#setPBSext("bat", '"C:/Windows/notepad.exe" %f')  ## open bat file in editor
	setPBSext("bat", '"C:/Apps/UltraEdit/Uedit32.exe" %f')  ## open bat file in editor
	invisible()
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~stew

#.flush.cat-----------------------------2011-09-30
# Flush the cat down the console (from PBStools)
.flush.cat = function(...)
{
	cat(...); flush.console(); invisible()
}
.loadGUI <- function()  ## load pathways from Rpaths.txt to GUI  ## (RH 240506)
{
	if (!file.exists("Rpaths.txt"))
		return(invisible(paste0("No 'Rpaths.txt' in ", getwd())))
	Rpaths = read.table("Rpaths.txt", col.names=c("name","path"))
	isBuild = is.element(Rpaths$name,"dirBuild")
	if ( basename(Rpaths$path[isBuild])==getWinVal(winName="PBSstew")$package ) {
		Rpaths$path[isBuild] = dirname(Rpaths$path[isBuild])
		message ("'Build' path has been changed to exclude the package name ", getWinVal()$package)
	}
	Rpathlist = lapply(1:nrow(Rpaths),function(i){Rpaths[i,"path"]})
	names(Rpathlist) = if (dim(Rpaths)[2]>1) Rpaths[,"name"] else  rownames(Rpaths)
	setWinVal(Rpathlist,"PBSstew")
	invisible("OK")
}
.updateGUI <- function()  ## update the main GUI (RH 240506)
{
	getWinVal(winName="stewPaths",scope="L")  ## because the window lives in memory once it's created
	isBuild = is.element(rownames(Rpaths),"dirBuild")
	if ( basename(Rpaths$path[isBuild])==getWinVal(winName="PBSstew")$package ) {
		Rpaths$path[isBuild] = dirname(Rpaths$path[isBuild])
		message ("'Build' path has been changed to exclude the package name ", getWinVal()$package)
	}
	Rpathlist = lapply(1:nrow(Rpaths),function(i){Rpaths[i,"path"]})
	names(Rpathlist) = if (dim(Rpaths)[2]>1) Rpaths[,"name"] else  rownames(Rpaths)
	setWinVal(Rpathlist,"PBSstew")
}
.win.paths <- function(winName="PBSstew")
{
	getWinVal(winName=winName, scope="L")
	Rpaths = data.frame(path=c(dirRcmd,dirMtex,dirEdit,dirRepo,dirBuild), row.names=c("dirRcmd","dirMtex","dirEdit","dirRepo","dirBuild"))
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
.win.check <- function(winName="PBSstew") ## (RH 240501)
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
	.flush.cat("Copying ", package, "\n\tfrom: ", dirRepo, "\n\tto:   ", dirBuild, "\n", sep="")
	file.copy(from=dirRepo, to=dirBuild, overwrite=T, recursive=T, copy.date=T)
	if (getWinAct("PBSstew")[1]=="check") {
		check  = paste0(dirRcmd, "/R CMD check ")
		if (cran)
			check = paste0(check, " --as-cran ")
		Rcmd   = paste0(check, file.path(dirBuild, package))
		path.check =  paste0(c("PATH=.",convSlashes(c(dirRcmd,dirMtex))),collapse=";")
		Sys.setenv(PATH=path.check)
		.flush.cat("Checking package ", package, "\n", sep="")
		results.check = shell(Rcmd, intern=T)
		tput(results.check)
		print(results.check)
	}
}
.win.build <- function(winName="PBSstew") ## (RH 240501)
{
	path.save = Sys.getenv("PATH")
	on.exit(Sys.setenv(PATH=path.save))

	getWinVal(winName=winName, scope="L")
	if (!dir.exists(dirBuild))
		stop (paste0("Build: '",dirBuild,"' does not exists"))
	if (!dir.exists(file.path(dirBuild,package))) {
		## Grab it from the repo via .win.check()
		setWinAct("PBSstew","build")  ## redundant to GUI button push but need it for debugging
		.win.check()
	}
	if (!dir.exists(dirRcmd))
		stop (paste0("Rcmd: '",dirRcmd,"' does not exists"))
	if (any(btype)) {
		bbtype = names(btype)[btype]
		for (b in bbtype) {
			if (b=="src")
				build  = paste0(dirRcmd, "/R CMD build --no-build-vignettes --compact-vignettes ")
			if (b=="bin")
				build  = paste0(dirRcmd, "/R CMD INSTALL --build --compact-docs --compile-both ")
			#Rcmd = paste0(build, file.path(dirBuild, package))  ## places build in user's cwd instead of dirBuild
			Rcmd = paste0(build, package)
			.flush.cat(Rcmd, "\n\tin: ", dirBuild, "\n")
#browser();return()
			cwd = getwd()
			on.exit(setwd(cwd), add = TRUE)
			setwd(dirBuild)  ## goto the user-specified build directory for package creation
			path.build =  paste0(c("PATH=.",convSlashes(c(dirRcmd,dirMtex))),collapse=";")
			Sys.setenv(PATH=path.build)
			results.build = shell(Rcmd, intern=T)
			tput(results.build)
			print(results.build)
		} ## end b loop bbtype
	} ## end if any btype
}
.win.edit = function(winName="PBSstew")  ## (RH 240507)
{
	getWinVal(winName=winName, scope="L")
	epath = switch (etype, 'repo'=dirRepo, 'build'=file.path(dirBuild,package), ".")
	files = list.files(path=epath, full.names=F, recursive=T)
	bad = c(grep("\\.pdf$|\\.sty$|\\.bak$|\\.tar\\.gz$|\\.zip$|\\.bup|Copy",files,value=T))
	good = setdiff(files, bad)
	fnams  = basename(good)
	#shell(paste0("dir ", convSlashes(epath), " /s /b /oe"), intern = TRUE)  ## alternative but too complicated
	ext = sapply(strsplit(fnams,split="\\."),function(x){if (length(x)==1) "misc" else rev(x)[1]})
	fdf = data.frame(name=fnams, ext=ext, path=good)
	fdf = fdf[order(fdf$ext),]  ## file name data frame
	tput(fdf) ## easiest way to transfer to .win.edit.file()
	fls =  split(fdf,fdf$ext)   ## file name list by extension
	ford = rev(sort(sapply(fls,nrow)))
	fgrp = split(ford, round(cumsum(ford) / (floor(sum(ford) / 3))) ) ## Use 3 columns

	## Start constructing fileWin
	fileWin = c(
		paste0("window name=editFiles title=\"Files in ", epath, "\""),
		"grid 1 3 sticky=NW byrow=F")
	for (g in 1:length(fgrp)) {
		fg = fgrp[[g]]
		fileWin = c(fileWin,
			paste0("  grid ", length(fg)+ifelse(g==3,1,0), " 1 sticky=NW byrow=F"))
		for (f in 1:length(fg)) {
			fext = names(fg)[f]
			fnam = fls[[fext]][,"name"]
#if(fext=="misc"){browser();return()}
			fileWin = c(fileWin,
				"    grid 2 1 sticky=W byrow=F",
				paste0("      label text=\"", fext, " files\" font=\"10 bold\" sticky=NW fg=blue"),
				paste0("      vector names=", fext, "files mode=logical vertical=T sticky=NW length=", length(fnam), " labels=\"", paste0(fnam,collapse=" "), "\" values=\"", paste0(rep("F",length(fnam)),collapse=" "), "\" vecnames=\"", paste0(fnam,collapse=" "), "\"")
				)
			if (g==3 && f==length(fg)) {
				fileWin = c(fileWin,
					"    grid 2 1 sticky=SE pady=\"10 0\"",
					"      label text=\"Select files to edit\\nand press edit button\"",
					"      button text=edit font=\"bold 10\" bg=gold name=editnow sticky=SE function=.win.edit.file"
				)
			}
		}
	}
	createWin(fileWin, astext=TRUE)
}
.win.edit.file = function(winName="editFiles")  ## (RH 240507)
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
		epath = switch (etype, 'repo'=dirRepo, 'build'=file.path(dirBuild,package), ".")
		pfiles = file.path(epath, fdf$path[is.element(fdf$name,efiles)])
		cmd = paste0(getWinVal(winName="PBSstew")$dirEdit, " ", paste0(pfiles, collapse=" "))
#browser();return()
		shell(cmd, intern=T)
	} else {
		message ("WARNING: No files selected")
	}
}
#stew()