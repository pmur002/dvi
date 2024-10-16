\name{readDVI}
\alias{readDVI}
\title{
  Read DVI File
}
\description{
  Read a DVI file (produced by LaTeX) into R. 
}
\usage{
readDVI(f, engine=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{A character value giving the name of a DVI file.}
  \item{engine}{The TeX engine that was used to produce the DVI file.
    Should be one of \code{"pdfTeX"}, \code{"LuaTeX"}, \code{"XeTeX"},
    or \code{"upTeX"} (case is ignored). If \code{NULL} (the default),
    will be guessed from the DVI file.}
}
\value{
  A \code{"DVI"} object.
}
\author{
  Paul Murrell
}
\examples{
readDVI(system.file("DVI", "test-pdftex.dvi", package="dvi"))
readDVI(system.file("DVI", "test-luatex.dvi", package="dvi"))
readDVI(system.file("DVI", "test-xetex.xdv", package="dvi"))
readDVI(system.file("DVI", "test-uptex.dvi", package="dvi"))
}
\keyword{ dplot }
