
engines <- tolower(c("pdfTeX",
                     "LuaTeX",
                     "XeTeX",
                     "upTeX"))

commentEngine <- function(comment) {
    string <- paste(blockValue(comment), collapse="")
    whichEngine <- sapply(engines,
                          function(x) grepl(x, string, ignore.case=TRUE))
    if (any(whichEngine)) {
        engines[whichEngine]
    } else {
        engines[c(1, 4)]
    }
}
