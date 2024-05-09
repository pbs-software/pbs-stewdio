\name{rebug}
\alias{rebug}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Reflect Objects on Console
}
\description{
  Reflect and debug: print out objects for clues when debugging.
}
\usage{
rebug(label, object, delim=c("=","~"), browse=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{label}{\code{character} -- name of object}
  \item{object}{\code{object} -- an R object to print to console}
  \item{delim}{\code{character} -- delimiter symbols for framing the start and end of the reflected object}
  \item{browse}{\code{logical} -- if \code{TRUE}, this may do something in future}
}
\details{
  To help the debugging process, a user can see various objects generated internally to functions.
}
\value{
  Nada, object details are simply printed to the R console.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-05-09}
}
\note{
  Printing of objects seems to be slow.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
  \code{\link[PBSstewdio]{stew}}
}
\examples{
rebug("swiss", swiss)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{debugging}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{clarify}
% \concept{ ~cpt2 }
% Use only one concept per line.
