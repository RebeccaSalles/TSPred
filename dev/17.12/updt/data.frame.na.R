###############################################################
## data.frames with filled NA's
data.frame.na <- function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE,
    stringsAsFactors = FALSE)
{
    data.row.names <- if (check.rows && is.null(row.names))
        function(current, new, i) {
            if (is.character(current))
                new <- as.character(new)
            if (is.character(new))
                current <- as.character(current)
            if (anyDuplicated(new))
                return(current)
            if (is.null(current))
                return(new)
            if (all(current == new) || all(current == ""))
                return(new)
            stop(gettextf("mismatch of row names in arguments of 'data.frame', item %d",
                i), domain = NA)
        }
    else function(current, new, i) {
        if (is.null(current)) {
            if (anyDuplicated(new)) {
                warning("some row.names duplicated: ", paste(which(duplicated(new)),
                  collapse = ","), " --> row.names NOT used")
                current
            }
            else new
        }
        else current
    }
    object <- as.list(substitute(list(...)))[-1L]
    mrn <- is.null(row.names)
    x <- list(...)
    n <- length(x)
    if (n < 1L) {
        if (!mrn) {
            if (is.object(row.names) || !is.integer(row.names))
                row.names <- as.character(row.names)
            if (any(is.na(row.names)))
                stop("row names contain missing values")
            if (anyDuplicated(row.names))
                stop("duplicate row.names: ", paste(unique(row.names[duplicated(row.names)]),
                  collapse = ", "))
        }
        else row.names <- integer(0L)
        return(structure(list(), names = character(0L), row.names = row.names,
            class = "data.frame"))
    }
    vnames <- names(x)
    if (length(vnames) != n)
        vnames <- character(n)
    no.vn <- !nzchar(vnames)
    vlist <- vnames <- as.list(vnames)
    nrows <- ncols <- integer(n)
    for (i in seq_len(n)) {
        xi <- if (is.character(x[[i]]) || is.list(x[[i]]))
            as.data.frame(x[[i]], optional = TRUE, stringsAsFactors = stringsAsFactors)
        else as.data.frame(x[[i]], optional = TRUE)
        nrows[i] <- .row_names_info(xi)
        ncols[i] <- length(xi)
        namesi <- names(xi)
        if (ncols[i] > 1L) {
            if (length(namesi) == 0L)
                namesi <- seq_len(ncols[i])
            if (no.vn[i])
                vnames[[i]] <- namesi
            else vnames[[i]] <- paste(vnames[[i]], namesi, sep = ".")
        }
        else {
            if (length(namesi))
                vnames[[i]] <- namesi
            else if (no.vn[[i]]) {
                tmpname <- deparse(object[[i]])[1L]
                if (substr(tmpname, 1L, 2L) == "I(") {
                  ntmpn <- nchar(tmpname, "c")
                  if (substr(tmpname, ntmpn, ntmpn) == ")")
                    tmpname <- substr(tmpname, 3L, ntmpn - 1L)
                }
                vnames[[i]] <- tmpname
            }
        }
        if (missing(row.names) && nrows[i] > 0L) {
            rowsi <- attr(xi, "row.names")
            nc <- nchar(rowsi, allowNA = FALSE)
            nc <- nc[!is.na(nc)]
            if (length(nc) && any(nc))
                row.names <- data.row.names(row.names, rowsi,
                  i)
        }
        nrows[i] <- abs(nrows[i])
        vlist[[i]] <- xi
    }
    nr <- max(nrows)
    for (i in seq_len(n)[nrows < nr]) {
        xi <- vlist[[i]]
        if (nrows[i] > 0L) {
            xi <- unclass(xi)
            fixed <- TRUE
            for (j in seq_along(xi)) {
                ### added NA fill to max length/nrow
                xi1 <- xi[[j]]
                if (is.vector(xi1) || is.factor(xi1))
                  xi[[j]] <- c(xi1, rep(NA, nr - nrows[i]))
                else if (is.character(xi1) && class(xi1) == "AsIs")
                  xi[[j]] <- structure(c(xi1, rep(NA, nr - nrows[i])),
                    class = class(xi1))
                else if (inherits(xi1, "Date") || inherits(xi1,
                  "POSIXct"))
                  xi[[j]] <- c(xi1, rep(NA, nr - nrows[i]))
                else {
                  fixed <- FALSE
                  break
                }
            }
            if (fixed) {
                vlist[[i]] <- xi
                next
            }
        }
        stop("arguments imply differing number of rows: ", paste(unique(nrows),
            collapse = ", "))
    }
    value <- unlist(vlist, recursive = FALSE, use.names = FALSE)
    vnames <- unlist(vnames[ncols > 0L])
    noname <- !nzchar(vnames)
    if (any(noname))
        vnames[noname] <- paste("Var", seq_along(vnames), sep = ".")[noname]
    if (check.names)
        vnames <- make.names(vnames, unique = TRUE)
    names(value) <- vnames
    if (!mrn) {
        if (length(row.names) == 1L && nr != 1L) {
            if (is.character(row.names))
                row.names <- match(row.names, vnames, 0L)
            if (length(row.names) != 1L || row.names < 1L ||
                row.names > length(vnames))
                stop("row.names should specify one of the variables")
            i <- row.names
            row.names <- value[[i]]
            value <- value[-i]
        }
        else if (!is.null(row.names) && length(row.names) !=
            nr)
            stop("row names supplied are of the wrong length")
    }
    else if (!is.null(row.names) && length(row.names) != nr) {
        warning("row names were found from a short variable and have been discarded")
        row.names <- NULL
    }
    if (is.null(row.names))
        row.names <- .set_row_names(nr)
    else {
        if (is.object(row.names) || !is.integer(row.names))
            row.names <- as.character(row.names)
        if (any(is.na(row.names)))
            stop("row names contain missing values")
        if (anyDuplicated(row.names))
            stop("duplicate row.names: ", paste(unique(row.names[duplicated(row.names)]),
                collapse = ", "))
    }
    attr(value, "row.names") <- row.names
    attr(value, "class") <- "data.frame"
    value
}