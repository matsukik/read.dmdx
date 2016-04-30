#' Read DMDX item file
#'
#' Read DMDX item file in either RTF format or ASCII format, and convert it into a list contaning the header information and data frame
#'
#' @param file A name of the file
#' @param acc logical. If \code{TURE}, negative reaction time
#' (DMDX's way of marking the trials with incorrect response) will
#' be converted to positive, and then new column containing response accuracy information
#' will be added.(column named \code{ACC}); Default to \code{FALSE}
#' @param item.order logical. If \code{TRUE}, column containing item order will be produced. Default to \code{FALSE}
#' @param read.max maximum number of lines to read at a time during initial parsing of the data file.
#' @param warn logical. If \code{FALSE} (Default), messages form internally used
#' functions (e.g., readLines, scan) will be muted.
#'
#'
#' @return A data.frame containing following columns:
#' \itemize{
#'  \item{sbj, subject number within the data file}
#'  \item{date, data colletion dat}
#'  \item{time, data collection time}
#'  \item{machine, name of the data collection machine}
#'  \item{refresh, reflecsh rate used}
#'  \item{id, ID assigned to the subject}
#'  \item{RT, response latency}
#'} and optionally,
#' \itemize{
#'  \item{ACC, accuracy of responses}
#'  \item{item.order, order of the items}
#'}
#'
#' @export
#'
read.azk <- function(file, acc = FALSE, item.order = FALSE, read.max = 20000, warn = FALSE)
{
  fp <- file(file, open="r")
  f.end <- 0
  ast.line <- c()
  while((cl <- length(fc <- readLines(fp, read.max, warn=warn))) > 0 )
  {
    f.end <- f.end+cl;
    ast.line <- c(ast.line, grep("\\*+", fc))
  }
  close(fp)

  sbj.line <- ast.line + 1
  end.line <- c(ast.line[-1] - 2, f.end)
  data <- list()
  for(i in 1:length(ast.line))
  {
    s.tmp <- scan(file, skip=ast.line[i], what=character(), nlines =1, quiet = !warn)
    s.tmp <- gsub(",$","", s.tmp)
    header <- scan(file, skip=sbj.line[i], what=character(), nlines=1, quiet = !warn)
    what <- list(Item=numeric(),RT=numeric())
    if(length(header) > 2) what$COT <- numeric()
    d.list <- scan(file, skip=sbj.line[i]+1, what=what,
                   nlines=(end.line[i]-sbj.line[i]-1), comment.char="!", quiet = !warn)
    if(acc){
      d.list$ACC <- as.numeric(d.list$RT>0)
      d.list$RT <- abs(d.list$RT)
    }
    if(item.order){
        d.list$item.order <- 1:length(d.list$RT)
    }
    data[[i]] <- data.frame(sbj=s.tmp[2], date=s.tmp[3], time=s.tmp[4],
                            machine=s.tmp[6], refresh=s.tmp[8], id=s.tmp[10],
                            d.list)
  }
  do.call("rbind", data)
}
