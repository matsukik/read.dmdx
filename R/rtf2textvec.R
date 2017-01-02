#' Convert RTF file into a plain-text charactor vector
#'
#' Convert RTF file format into a charactor vector, with each element containing stripped out texts form each line in the file.
#'
#' @param file A name of the file
#' @param warn logical. If \code{FALSE} (Default), messages form internally used
#' functions (e.g., readLines) will be muted.
#' @param encoding encoding to be used.
#'
#' @return A character vector with each element containing stripped out texts form each line in the file.
#' @export
#'
#' @importFrom stringi stri_replace_all_fixed stri_match_all
#' @importFrom stats na.omit
#'
rtf2textvec <- function(file, warn = FALSE, encoding = "unknown")
{
  lines <- readLines(file, warn = warn, encoding = encoding)
  if(substring(lines[1], first = 1, last=5) != "{\\rtf") stop("'file' is not an RTF file.")

  lines <- lines[-grep("^[{][\\]f[^;]", lines)]
  lines <- gsub("[{][\\]pntxt[^}]+|[{][\\][*][\\]bkmk[^}]+[}]", "", lines)

  if(encoding == "UTF-8")
  {
    lines <- gsub("\\\\u8221[\\][\']94|\\\\u8221[\\][\']81[\\][\']68|\\\\u8221$", "\"", lines)
    lines <- gsub("\\\\u8220[\\][\']93|\\\\u8220[\\][\']81[\\][\']67|\\\\u8220$", " \"", lines)

    .utf8conv <- function(x)
    {
      matches <- stri_match_all(x, regex="\\\\u(-*\\d+)(\\\\'[0-9a-z]+)*")[[1]]
      if(any(is.na(matches[[1]])))
      {
        return(x)
      } else {
        .intToUtf8 <- function(x)
        {
          x <- as.numeric(as.character(x))
          if(x < 0) x <- 65536 + x
          intToUtf8(x)
        }
        matches[,2] <- sapply(matches[,2], .intToUtf8)
        stri_replace_all_fixed(x, pattern=matches[,1], replacement=matches[,2], vectorize_all=FALSE)
      }
    }
    lines <- unlist(lapply(lines, .utf8conv))
    collapse <- ifelse(any(grepl("\\\\uc2", lines)), "", " ")

    lines <- gsub("\\\\uc2", "", lines)
    lines <- gsub("^([\\][\'][0-9a-z]+){2,}[ ]*", "", lines)
  } else {
    collapse <- ""
  }

  lines <- gsub("[\\][\']94", "\"", lines)
  lines <- gsub("[\\][\']93", " \"", lines)

  par.lines <- grep("^[\\]par", lines)
  lines.end <- grep("[\\]themedata", lines)
  if(length(lines.end) == 0){lines.end <- length(lines)}
  par.lines <- par.lines[-which(par.lines == lines.end)]
  par.end <- c(par.lines[2:length(par.lines)] - 1, lines.end-1)
  res <- res1 <- lines[par.lines]
  for(i in 1:length(par.lines))
  {
    rp <- par.lines[i]
    lp <- par.end[i]
    tmp <- res1[i] <- paste(lines[rp:lp], collapse = collapse)
    tmp <- gsub("[\\]tab[ ]*", "\t", tmp)
    tmp <- gsub("[\\][^\\ '<>,.]+[ ]*", "", tmp)
    tmp <- gsub("[{}]", "", tmp)
    tmp <- gsub("\t", " ", tmp)
    res[i] <- trimws(tmp)
    if(res[i] == "") res[i] <- NA
  }
  res <- c(na.omit(res))
  res
}
