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
	pkg.choices = grep("^PBS",list.dirs(.libPaths(), full.names=F, recursive=F), value=T)
	if (length(pkg.choices)==0)
		pkg.choices = c("PBSadmb","PBSddesolve","PBSmapping", "PBSmodelling")
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
.updateGUI <- function()  ## update the main GUI; forget error checking (RH 240514)
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
	## Rpaths no longer an 'object' in GUI
	dirlist = ls(pattern="^dir")
	Rpaths  = data.frame(sapply(dirlist,function(x){get(x)})); colnames(Rpaths)="path"
	Rpathlist = lapply(1:nrow(Rpaths),function(i){Rpaths[i,"path"]})
	names(Rpathlist) = if (dim(Rpaths)[2]>1) Rpaths[,"name"] else  rownames(Rpaths)
	setWinVal(Rpathlist,"PBSstew")
}
##----------------------------------------------------------
.win.paths <- function(winName="PBSstew")  ## (RH 240516)
{
	getWinVal(winName=winName, scope="L")
	Rpaths = data.frame(path=c(dirRepo,dirBuild,dirProj,dirRcmd,dirMtex,dirEdit), row.names=c("dirRepo","dirBuild","dirProj","dirRcmd","dirMtex","dirEdit"))
	insert = character()
	for (i in 1:nrow(Rpaths)) {
		ii = rownames(Rpaths)[i]
		iii = Rpaths[i,1]
		insert = c(insert, "  grid 1 2 sticky=E")
		insert = c(insert, paste0("    entry name=", ii, " value=\"", iii, "\" label=\"", ii, "\" width=75 mode=character func=setGUIoptions action=", ii))
		insert = c(insert, paste0("    button text=\">\" func=doAction action=\"select", ifelse(ii %in% "dirEdit", "File","Dir"), "(usewidget=\`", ii, "\`)\""))
	}
	pathWin = c("window name=stewPaths title=\"Paths for PBSstewdio Package Building\" onclose=.updateGUI",
		paste0("grid ", nrow(Rpaths)+1, " 1 sticky=W"),
		#"  object name=Rpaths width=\"75\"",
		insert,
		"  grid 1 3 sticky=E",
		"    entry name=Rfile value=Rpaths.txt width=10 label=filename edit=F noeditbg=powderblue mode=character",
		"    button text=save bg=green function=doAction action=\"winval= getWinVal(winName=\`stewPaths\`); zdir= grep(\`^dir\`,names(winval),value=T); Rpaths= do.call(\`rbind\`, lapply(winval[zdir], data.frame, stringsAsFactors=FALSE)); write.table(Rpaths, \`Rpaths.txt\`, col.names=FALSE)\"",
		"    button text=load bg=gold function=doAction action=\"Rpaths=read.table(\`Rpaths.txt\`, col.names=c(\`name\`,\`path\`)); Rpathlist= lapply(1:nrow(Rpaths),function(i){Rpaths[i,\`path\`]}); names(Rpathlist)= Rpaths[,\`name\`]; setWinVal(Rpathlist,\`stewPaths\`)\""
	)
#browser();return()
	createWin(pathWin, astext=TRUE)
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
.win.collect = function(winName="PBSstew")  ## (RH 240515)
{
	getWinVal(winName=winName, scope="L")
	#epath = switch (etype, 'repo'=file.path(dirRepo,package), 'build'=file.path(dirBuild,package), ".")
	epath = switch (etype, 'repo'=file.path(dirRepo,package), 'build'=file.path(dirBuild,package), 'proj'=file.path(dirProj,project), ".")
	#shell(paste0("dir ", convSlashes(epath), " /s /b /oe"), intern = TRUE)  ## alternative but too complicated
	files = list.files(path=epath, full.names=TRUE, recursive=TRUE)  ## get full path names to avoid confusion
	if (length(files)==0) {
		mess = paste0("No files detected in\n'", epath, "'")
		showAlert(mess); stop(mess)
	}
	stwpath = normalizePath(dirname(stw), winslash="/", mustWork=F)
	stwbase = basename(stw)
	stwfile = file.path(stwpath,stwbase)
	if (stwpath!="" && stwbase!="" && file.exists(stwfile)) {
		usestw = TRUE
	} else {
		usestw = FALSE
		mess = paste0("User-specified stw file cannot be found:\n\tstwpath = '", stwpath, "'\n\tstwbase = '", stwbase, "'\n\tstwfile = '", stwfile, "'\nAll files displayed (not subset)\n")
		message (mess)
	}
	if (usestw) {
		inpat  = readLines(stwfile, warn=FALSE)
		outpat =  gsub("(\\,)?\\s+","|", gsub("\\\"","",inpat))
		if (length(outpat)>1)
			outpat = paste0(outpat,collapse="|")
		if (debug) rebug("outpat",outpat)
		good = c(grep(outpat, files, value=TRUE))
		if (length(good)==0) {
			mess = paste0("User's input pattern from:\n'", file.path(getwd(), stw), "'\ndid not match any files.\n")
			showAlert(mess); stop(mess)
		}
	} else {
		bad.pattern = "\\.pdf$|\\.sty$|\\.bak$|\\.tar\\.gz$|\\.zip$|\\.bup|Copy|\\.rda$|\\.rds$|\\.gif$|\\.dll$|\\.o$|\\.lnk$"
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
		paste0("window name=collect title=\"Files in ", epath, "\""),
		paste0("grid 1 ", ncol, " sticky=NW byrow=F") )
	for (g in 1:length(fgrp)) {
		fg = fgrp[[g]]
		fileWin = c(fileWin,
			paste0("  grid ", length(fg) + ifelse(g==ncol,1,0), " 1 sticky=NW byrow=F") )  ## add an extra row for the Edit button
		for (f in 1:length(fg)) {
			fext = names(fg)[f]
			fnam = fls[[fext]][,"name"]
#if(fext=="misc"){browser();return()}
			fileWin = c(fileWin,
				"    grid 2 1 sticky=W byrow=F",
				paste0("      label text=\"", fext, " files\" font=\"10 bold\" sticky=NW fg=blue"),
				paste0("      vector names=", fext, "files mode=logical vertical=T sticky=NW length=", length(fnam), " labels=\"'", paste0(fnam,collapse="' '"), "'\" values=\"", paste0(rep("F",length(fnam)),collapse=" "), "\" vecnames=\"'", paste0(fnam,collapse="' '"), "'\"")
				)
#browser();return()
			if (g==ncol && f==length(fg)) {
				fileWin = c(fileWin,
					"    grid 2 1 sticky=SE pady=\"10 0\"",
					"      label text=\"Select files to edit and/or run\"",
					"      grid 1 3 sticky=SE",
					"        button text=edit font=\"bold 10\" bg=rosybrown1 name=editnow sticky=SE function=.win.edit",
					"        button text=convert font=\"bold 10\" bg=gold name=runnow sticky=SE function=.win.convert",
					"        button text=source  font=\"bold 10\" bg=green name=runnow sticky=SE function=.win.source"
				)
			}
		} ## end f loop (# files in group)
	} ## end g loop (# groups)
	if (debug) rebug("fileWin", fileWin)
	createWin(fileWin, astext=TRUE)
}
##----------------------------------------------------------
.win.grab = function(winName="collect")  ## (RH 240515) Common code for edit convert, and source
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
		tget(fdf)  ## from .win.collect()
		getWinVal(winName="PBSstew", scope="L")
		epath = switch (etype, 'repo'=file.path(dirRepo,package), 'build'=file.path(dirBuild,package), 'proj'=file.path(dirProj,project), ".")
		tput(epath) ## should only have to determine this once
		pfiles = fdf$path[is.element(fdf$name,efiles)]  ## now has full path from '.win.edit()'
		if (debug) rebug("pfiles",pfiles)
	} else {
		pfiles = ""
		message ("WARNING: No files selected")
	}
	return(pfiles)
}
##----------------------------------------------------------
.win.edit = function(winName="collect")  ## (RH 240515)
{
	pfiles = .win.grab()
	if (all(pfiles=="")) {
		invisible(return("No files to edit"))
	}
	getWinVal(winName="PBSstew", scope="L")
	cmd = paste0(dirEdit, " ", paste0(pfiles, collapse=" "))
	shell(cmd, intern=T)
	invisible(return(cmd))
}
##----------------------------------------------------------
.win.convert = function(winName="collect")  ## (RH 240510)
{
	pfiles = .win.grab()
	if (all(pfiles=="")) {
		invisible(return("No files selected"))
	}
	getWinVal(winName="PBSstew", scope="L")
	valid.exts = c("Rd")
	tget(fdf)   ## from .win.collect()
	tget(epath) ## from .win.grab()
	exts = fdf$ext[match(pfiles,fdf$path)]
	if (!any(exts %in% valid.exts)){
		mess = "No valid extensions for 'convert' action"
		stop(mess)
	}
	#tmpdir = normalizePath(tempdir(), winslash="/", mustWork=FALSE)
	outdir = dirname(epath) ## put results into build or repo
	for (ext in unique(exts)) {
		exfiles = pfiles[grep(ext,exts)]
		if (debug) rebug("exfiles",exfiles)
		if (ext=="Rd") {
			mess = paste0("Converting Rd file(s): '", paste0(basename(exfiles), collapse="' '"), "' to PDF in directory:\n   '", outdir, "'\nSystem will attempt to open PDF file(s) for you.\n\n")
			.flush.cat(mess)
			ex0  = sub(paste0("\\.",ext), "", basename(exfiles))
			ex1  = file.path(outdir, paste0(ex0,".pdf"))
			cmds = paste0(dirRcmd, "/R CMD Rd2pdf --batch --no-preview --force --title=", ex0, " --output=", ex1, " ",  exfiles)
			for (cmd in cmds)
				shell(cmd, intern=T)
			openFile(ex1)
#browser();return()
		} else {
			next  ## don't bother sending a message
			mess = paste0("'.", ext, "' : support for converting files only available for extensions: '.Rd'\n")
			.flush.cat(mess)
		}
	} ## end ext loop
	invisible(return(cmds))
}
##----------------------------------------------------------
.win.source = function(winName="collect")  ## (RH 240515)
{
	pfiles = .win.grab()
	if (all(pfiles=="")) {
		invisible(return("No files selected"))
	}
	getWinVal(winName="PBSstew", scope="L")
	valid.exts = c("R","r")
	tget(fdf)   ## from .win.collect()
	tget(epath) ## from .win.grab()
	exts = fdf$ext[match(pfiles,fdf$path)]
	if (!any(exts %in% valid.exts)){
		mess = "No valid extensions for 'source' action"
		stop(mess)
	}
	#tmpdir = normalizePath(tempdir(), winslash="/", mustWork=FALSE)
	outdir = dirname(epath) ## put results into build or repo
#browser();return()
	for (ext in unique(exts)) {
		exfiles = pfiles[grep(ext,exts)]
		if (debug) rebug("exfiles",exfiles)
		if (ext %in% c("r","R")) {
			## Construct a function to run at start-up
			first =  c(
				".First <- function() {",
				"   require(\"graphics\", quietly = TRUE)",
				"   require(\"PBSmodelling\", quietly = TRUE)",
				"   assign(\"clr.rgb\", list( black=c(0,0,0), ",
				"   red=c(255,0,0), green=c(0,255,0), blue=c(0,0,255),",
				"   yellow=c(255,255,0), magenta=c(255,0,255), forest=c(0,128,0),",
				"   cyan=c(0,255,255), pumpkin=c(255,128,0), eggplant=c(64,0,128),",
				"   rose=c(253,179,193), mango=c(255,209,143), sky=c(198,250,253),",
				"   canary=c(255,255,195), lettuce=c(193,255,193), fog=c(223,223,223) ), pos=1 )",
				"   assign(\"clr\",lapply(clr.rgb,function(v) {grDevices:::rgb(v[1],v[2],v[3],maxColorValue=255) }),pos=1)",
				"   options(stringsAsFactors=FALSE, help_type=\"html\")",
				"   grDevices:::ps.options(horizontal=FALSE,family=\"NimbusSan\",paper=\"special\")",
				"   grDevices:::windows(width=6.5,height=9,record=TRUE,buffered=TRUE)",
				"   graphics:::frame()",
				"   grDevices:::bringToTop(-1)",
				"   print(\"Scroll using up-arrow key to source your functions\")",
				"}" )
#browser();return()
			if (debug) rebug("first",first)
			writeLines(first, con=file.path(outdir,".First.r"))
			writeLines(paste0("source(\"", basename(exfiles), "\")"), con=file.path(outdir,".RHistory"))
			file.copy(from=exfiles, to=outdir, overwrite=FALSE, copy.date=TRUE)
			cmd0 = paste0("cd /d ", outdir, " && ", dirRcmd, "/R CMD BATCH ", file.path(outdir,".First.r"))
			cmd1 = paste0("cd /d ", outdir, " && ", dirRcmd, "/Rgui.exe")
			shell(cmd0, wait=T, mustWork=F)
			shell(cmd1, wait=F)
		} else {
			next  ## don't bother sending a message
			mess = paste0("'.", ext, "' : support for sourcing files only available for extensions: '.[rR]'\n")
			.flush.cat(mess)
		}
	} ## end ext loop
}
##----------------------------------------------------------
.win.make.pkg = function(winName="PBSstew")  ## (RH 240518)
{
	getWinVal(winName=winName, scope="L")
	epath = switch (etype, 'repo'=file.path(dirRepo,package), 'build'=file.path(dirBuild,package), ".")
	if (dir.exists(epath)) {
		mess = paste0("Package '", package, "' already exists!\n   Specify a package name that does not exist in:\n'", dirname(epath),"'")
		showAlert(mess); stop(mess)
	}
#browser();return()
	#package.skeleton(name = package, path=dirname(epath)) ## this function tries to be smart and ends up dumb
	ppath = file.path(dirname(epath), package)
	dir.create(ppath)
	for (i in c("R","data","inst","man","vignettes"))
		dir.create(file.path(ppath, i))
	for (j in c("doc","win"))
		dir.create(file.path(ppath, "inst", j))
	readme = c(
		"* Edit the help file skeletons in 'man', possibly combining help files for multiple functions.",
		"* Edit the package 'DESCRIPTION'.",
		"* Edit the exports in 'NAMESPACE', and add necessary imports.",
		"* Put any C/C++/Fortran code in 'src'.",
		"* If you have compiled code, add a useDynLib() directive to 'NAMESPACE'.",
		"* Run R CMD build to build the package tarball.",
		"* Run R CMD check to check the package tarball.",
		"",
		"Read \"Writing R Extensions\" for more information."
	)
	description = c(
		paste0("Package: ", package),
		"Type: Package",
		"Title: What the Package Does (Short Line)",
		"Version: 1.0",
		paste0("Date: ", substr(Sys.time(),1,10)),
		"Author: Who wrote it",
		"Maintainer: Who to complain to <yourfault@somewhere.net>",
		"Description: More about what it does (maybe more than one line).",
		"License: What license is it under?"
	)
	namespace = c(
		"# Packages declared in the 'Depends' field should not also be in the 'Imports' field (from R-exts.pdf).",
		"import( methods, PBSmodelling )",
		"# importFrom(\"<somepackage>\", \"<func1>\", \"<func2>\")",
		paste0("# Export all non-dot names from ", package),
		"exportPattern(\"^[^\\.]\")",
		"# R now requires documentation for all exported objects,",
		"#  even dot functions; not desirable to keep them in the NAMESPACE.",
		"# export(.win.func)"
	)
	writeLines(namespace, con=file.path(ppath,"NAMESPACE"))
	writeLines(description, con=file.path(ppath,"DESCRIPTION"))
	writeLines(readme, con=file.path(ppath,"Read-and-delete-me"))
	invisible()
}
