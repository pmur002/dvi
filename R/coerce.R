
## Coerce hexView structure to simpler R list

opCode <- function(op) {
    blockValue(op$blocks$op.opcode)
}

opParams <- function(op) {
    ## Remove code (first component of blocks)
    nocode <- op$blocks[-1]
    params <- lapply(nocode,
                     function(p) {
                         blockValue(p)
                     })
    names(params) <- gsub("op.opparams.", "", names(nocode), fixed=TRUE)
    params
}

opList <- function(dvi) {
    lapply(dvi,
           function(op) {
               list(code=opCode(op),
                    params=opParams(op))
           })
}
