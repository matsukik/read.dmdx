#' Read DMDX item file
#'
#' Read DMDX item file in either RTF format or ASCII format, and convert it into a list contaning the header information and data frame
#'
#' @param file A name of file
#' @param colnames names of columns for the data.frame to be returned.
#' @param CorrectAnswers logical. If \code{TURE}, the signs (+, -, ^, =) will
#' be removed from the item number and converted into a new column 'Correct Answers
#' with the corresponding values ("Yes", "No", \code{NA}, and "Any" respectively).
#' Default to \code{FALSE}
#' @param warn logical. If \code{FALSE} (Default), messages form internally used
#' functions (e.g., readLines) will be muted.
#' @param separator logical. If \code{FALSE} (Default), frame separator '/' will be omitted from the output data.frame.
#' @param encoding encoding to be used.
#'
#' @return A list containing:
#' \itemize{
#'   \item{header (A list of header items. Each <> are treated as separate items)}
#'   \item{itemdat (A data frame with each column corresponding to item number,
#'   item from each frame marked by the divider /, and timing and other indicators.)}
#' }
#' @export
#'

read.dmdx <- function(file, colnames, CorrectAnswers = FALSE, warn = FALSE, separator = FALSE, encoding = "unknown")
{
  lines <- readLines(file, warn = warn, encoding = encoding)
  #unicode <- (encoding == "UTF-8")
  if(substring(lines[1], first = 1, last=5) == "{\\rtf")
  {
    lines <- rtf2textvec(file, warn = warn, encoding = encoding)
  } else {
    lines <- gsub("\t", " ", lines)
    lines <- gsub("[\u201c\u201d]", "\"", lines)
  }


  .parse_header <- function(x){
    m <- gregexpr("\\<(\\w+)\\s([^\\>]+)\\>", x)
    parray <- regmatches(x, m)[[1]]
    parray <- gsub("(\\w+)\\s(.+)", "\\1=\\2",  parray)
    parray <- strsplit(parray, "=")
    res <- lapply(parray, "[[", 2)
    names(res) <- lapply(parray, "[[", 1)
    res
  }

  header <- .parse_header(lines[1])

  sections <- grep("^[$]", lines)
  newlines <- grep("^\\s?$", lines)
  instloc <- grep("^0", lines)
  for(i in instloc)
  {
    if(regexpr("[;]\\s*$", lines[i]) == -1)
    {
      j = i;
      while(regexpr("[;]\\s*$", lines[j]) == -1)
      {
        j = j + 1;
        instloc <- c(instloc, j);
      }
    }
  }

  items <- lines[-c(1,sections, newlines, instloc)]
  .itemsplit <- function(x, separator = FALSE)
  {

    reg <- ifelse(separator, "[^ \"';]+|\"([^\"]*)\"|'([^']*)'|(<[^>]+>)", "[^ \"'/;]+|\"([^\"]*)\"|'([^']*)'|(<[^>]+>)")
    m <- gregexpr(reg, x)
    res <- regmatches(x, m)[[1]]
    return(gsub("[\"']", "", res))
  }

  itemlist <- lapply(items, .itemsplit, separator=separator)
  itemlist <- lapply(itemlist, trimws)
  itemdat <- data.frame(do.call("rbind", itemlist), stringsAsFactors = FALSE)
  if(!missing(colnames)) colnames(itemdat) <- colnames
  if(CorrectAnswers){
    cor.ans <- c("+"="Yes", "-"="No", "^"=NA, "="="Any")
    this.ans <- substring(itemdat[,1], 1, 1)
    itemdat$CorrectAnswers <- cor.ans[this.ans]
    tmp <- gsub("^[\\+\\=\\^\\-]", "", itemdat[,1])
    itemdat[,1] <- as.numeric(tmp)
  }
  list(header=header, itemdat=itemdat)
}
