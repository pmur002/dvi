
library(dvi)

## Guess the right engine
pdfEngine <- readDVI(system.file("DVI", "test-pdftex.dvi", package="dvi"))
stopifnot(attr(pdfEngine, "engine") == "pdftex" &&
          attr(pdfEngine, "guessEngine"))

luaEngine <- readDVI(system.file("DVI", "test-luatex.dvi", package="dvi"))
stopifnot(attr(luaEngine, "engine") == "luatex" &&
          attr(luaEngine, "guessEngine"))

xeEngine <- readDVI(system.file("DVI", "test-xetex.xdv", package="dvi"))
stopifnot(attr(xeEngine, "engine") == "xetex" &&
          attr(xeEngine, "guessEngine"))

## Specify the right engine
upEngine <- readDVI(system.file("DVI", "test-uptex.dvi", package="dvi"),
                    engine="upTeX")
stopifnot(attr(upEngine, "engine") == "uptex" &&
          !attr(upEngine, "guessEngine"))

## Specify the wrong engine
tools::assertWarning(readDVI(system.file("DVI", "test-pdftex.dvi",
                                         package="dvi"),
                             engine="luatex"))

