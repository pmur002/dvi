\name{readDVI}
\alias{readDVI}
\title{
  Read DVI File
}
\description{
  Read a DVI file (produced by LaTeX) into R. 
}
\usage{
readDVI(f)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{A character value giving the name of a DVI file.}
}
\value{
  A \code{"DVI"} object.
}
\author{
  Paul Murrell
}
\examples{
readDVI(system.file("DVI", "test-pdf.dvi", package="dvi"))
readDVI(system.file("DVI", "test-lua.dvi", package="dvi"))
readDVI(system.file("DVI", "test-xe.xdv", package="dvi"))
readDVI(system.file("DVI", "test-up.dvi", package="dvi"))
}
\keyword{ dplot }
