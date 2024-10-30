
opCodes <- function(dvi) {
    sapply(dvi,
           function(op) {
               blockValue(op$blocks$op.opcode)
           })
}

opParams <- function(dvi) {
    lapply(dvi,
           function(op) {
               ## Remove code (first component of blocks)
               nocode <- op$blocks[-1]
               params <- lapply(nocode,
                                function(p) {
                                    blockValue(p)
                                })
               names <- names(nocode)
               if (length(names)) {
                   names(params) <- gsub("op.opparams[.]?", "", names)
               }
               params
           })
}

readDVI <- function(file) {
    ## Create a list that is known to be too long
    ops <- vector("list", file.size(file))
    done <- FALSE
    offset <- 0
    length <- 1
    guessEngine <- FALSE
    while (!done) {
        op <- readFormat(file, opFormat, offset=offset)
        if (length == 1 &&
            blockValue(op$blocks$op.opcode) != 247) {
            warning("DVI file does not start with a 'pre' op")
        }
        if (blockValue(op$blocks$op.opcode) == 249) {
            done <- TRUE
        }
        offset <- offset + op$nbytes
        ops[[length]] <- op
        length <- length + 1
    }
    result <- ops[1:(length - 1)]
    class(result) <- "DVI"
    result
}


