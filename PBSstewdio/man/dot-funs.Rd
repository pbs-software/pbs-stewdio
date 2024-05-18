\name{dot-funs}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{.flush.cat}
\alias{.loadGUI}
\alias{.updateGUI}
\alias{.win.paths}
\alias{.win.build}
\alias{.win.check}
\alias{.win.collect}
\alias{.win.grab}
\alias{.win.edit}
\alias{.win.convert}
\alias{.win.source}
\alias{.win.make.pkg}
\title{
  Miscellaneous Dot Functions
}
\description{
  An assortment of dot functions to facilitate running \pkg{PBSstewdio}.
}
\usage{
.flush.cat(...)
.loadGUI()
.updateGUI()
.win.paths(winName="PBSstew")
.win.build(winName="PBSstew")
.win.check(winName="PBSstew")
.win.collect(winName="PBSstew")
.win.grab(winName="collect")
.win.edit(winName="collect")
.win.convert(winName="collect")
.win.source(winName="collect")
.win.make.pkg(winName="PBSstew")
}
\arguments{
  \item{...}{\code{dots} -- assortment of text strings to send to the R console}
  \item{winName}{\code{character} -- name of a GUI window}
}
%- maybe also 'usage' for other objects documented here.
\details{
  Utility functions that juggle GUIs like hot tamales.
}
\author{
  \href{mailto:schnutej@shaw.ca}{Jon T. Schnute}, Scientist Emeritus, Nanaimo BC\cr

  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-05-18}
}
\note{
  Function \code{.flush.cat} (flush the cat down the console) 
  has been lifted verbatim from \pkg{PBStools}.

  Function:\cr
  \code{.win.collect} collects and displays files from target directory;\cr
  \code{.win.grab} grabs checked files from the collection;\cr
  \code{.win.edit} edits the checked files;\cr
  \code{.win.convert} converts checked files with extension \code{'.Rd'};\cr
  \code{.win.source} opens a new R session for checked \code{'.[Rr]'} files.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
In package \pkg{PBSstewdio}:\cr
  \code{\link[PBSstewdio]{stew}}\cr
In package \pkg{PBSmodelling}:\cr
  \code{getWinAct},
  \code{getWinVal},
  \code{setWinVal},
  \code{setGUIoptions}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{utilities}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{hidden}
\concept{GUI}
% Use only one concept per line.
