
readDVI <- function(f, engine=NULL) {
    ## Create a list that is known to be too long
    ops <- vector("list", file.size(f))
    done <- FALSE
    offset <- 0
    length <- 1
    guessEngine <- FALSE
    if (is.null(engine) || length(engine) < 1) {
        guessEngine <- TRUE
    } else {
        engine <- tolower(engine)
        if (length(engine) > 1) {
            warning("Only first 'engine' used")
            engine <- engine[1]
        }
        whichEngine <- match(engine, engines)
        if (is.na(whichEngine)) {
            warning(paste0("Unknown 'engine' (", engine, "); {dvi} will guess"))
            guessEngine <- TRUE
        }
    }
    while (!done) {
        op <- readFormat(f, opFormat, offset=offset)
        if (length == 1 &&
            blockValue(op$blocks$op.opcode) != 247) {
            warning("DVI file does not start with a 'pre' op")
        }
        if (blockValue(op$blocks$op.opcode) == 247) {
            engineGuess <-
                commentEngine(op$blocks$op.opparams.comment.string)
            if (guessEngine) {
                engine <- engineGuess[1]
            } else {
                if (!(engine %in% engineGuess)) {
                    warning(paste0("'engine' (", engine, ") does not match ",
                                   "comment (",
                                   paste(engineGuess, collapse=" or "),
                                   ")"))
                }
            }
        }
        if (blockValue(op$blocks$op.opcode) == 249) {
            done <- TRUE
        }
        offset <- offset + op$nbytes
        ops[[length]] <- op
        length <- length + 1
    }
    result <- ops[1:(length - 1)]
    attr(result, "engine") <- engine
    attr(result, "guessEngine") <- guessEngine
    class(result) <- "DVI"
    result
}


