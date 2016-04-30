#' Convert RTF file into a plain-text charactor vector
#'
#' Convert RTF file format into a charactor vector, with each element containing stripped out texts form each line in the file.
#'
#' @param file A name of the file
#' @param warn logical. If \code{FALSE} (Default), messages form internally used
#' functions (e.g., readLines) will be muted.
#'
#' @return A character vector with each element containing stripped out texts form each line in the file.
#' @export
#'
rtf2textvec <- function(file, warn = FALSE)
{
  lines <- readLines(file, warn = warn)
  lines <- gsub("[\\][\']9[34]", "\"", lines)
  lines <- gsub("OLE_LINK1}", "", lines)
  lines <- gsub("[\\]tab", " ", lines)
  if(substring(lines[1], first = 1, last=5) != "{\\rtf") stop("'file' is not an RTF file.")
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
    tmp <- res1[i] <- paste(lines[rp:lp], collapse = "")
    tmp <- gsub("[\\][^\\ '<>,.]+[ ]?", "", tmp)
    res[i] <- gsub("[{}.()]", "", tmp)
    if(res[i] == "") res[i] <- NA
  }
  res <- c(na.omit(res))
  res
}
