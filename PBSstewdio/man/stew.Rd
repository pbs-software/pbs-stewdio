\name{stew}
\alias{stew}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Run Package GUI
}
\description{
  Create a GUI to check and build packages.
}
\usage{
stew(pkg="PBSstewdio", wdf="stewWin.txt")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pkg}{\code{character} -- package name (defaults to \code{'PBSstewdio'})}
  \item{wdf}{\code{character} -- name of the \emph{window description file} that creates the GUI}
}
\details{
  Opens GUI named \code{'PBSstew'} for manipulating package checking and building.
}
\value{
  Nada
}
\author{
  \href{mailto:schnutej@shaw.ca}{Jon T. Schnute}, Scientist Emeritus, Nanaimo BC\cr

  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-05-01}
}
\note{
  Function and windows description file (\code{stewWin.txt}) are 
  undergoing developmental changes.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
  \code{\link[PBSstewdio]{stew}}
}
\examples{
\dontrun{
stew()
}
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
