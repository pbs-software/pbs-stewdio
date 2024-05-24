\name{runExpr}
\alias{runExpr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Run an Expression
}
\description{
  Run an R expression (presented as a character string, not a true
  expression) for evaluation and execution.
}
\usage{
runExpr(expr, green.light, outnam="results", guitext="expression", add2expr="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expr}{\code{character} -- a text string that can be evaluated using:
    \code{eval(parse(text=expr))}}
  \item{green.light}{\code{logical} -- if \code{TRUE}, user can keep pressing buttons;
    if \code{FALSE}, user should not be able to launch processes while one is in play}
  \item{outnam}{\code{character} -- output name for expression's internal messages}
  \item{guitext}{\code{character} -- text appearing in GUI that denotes process in play}
  \item{add2expr}{\code{character} -- additional commands to execute when GUI button
    <Start> is pushed}
}
\details{
  Creates a GUI that details the process to run.
  The user launches the process by pushing the <Start> button,
  which contains a variety of R commands to execute using
  a built-in function called \code{'doAction'}.
  When the process has completed, the GUI dispays the processing time.
  The user needs to push the <Close> button to reset the original environment.
}
\value{
  Nothing explicity although internal feedback is provided by
  any \code{shell} (\code{system}) commands that were executed.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-05-24}
}
\note{
  This function is geared to running \code{shell} commands at present.

  When an R function, say \code{afunc()}, has been called, \code{afunc}
  runs through all lines of code within it.
  If \code{afunc} calls \code{runExpr()}, the latter creates a GUI named \code{'green'},
  which sits apart from the function environment well after \code{afunc} has been
  executed. The GUI knows nothing about objects that were present while the function
  environment was active. Therefore, it's necessary to store objects in a stable, 
  pre-specified environment (like \code{'.PBSmodEnv'}) to be accessible by the GUI.
  Further, any commands that need to be executed by the GUI should be 
  transferred from \code{afunc} to the GUI, which can run the commands using the
  built-in function \code{'doAction'}.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
In package \pkg{PBSstewdio}:\cr
  \code{\link[PBSstewdio]{stew}},
  \code{\link[PBSstewdio]{rebug}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{GUI}
% \concept{ ~cpt2 }
% Use only one concept per line.
