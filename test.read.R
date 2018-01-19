# https://gist.github.com/hadley/6353939

read_file1 <- function(path) {
  paste0(paste0(readLines(path), collapse = "\n"), "\n")
}

read_file2 <- function(path) {
  size <- file.info(path)$size
  readChar(path, size, useBytes = TRUE)
}

read_file3 <- function(path) {
  size <- file.info(path)$size
  rawToChar(readBin(path, "raw", size))
}

read_file4 <- function(path, chunk_size = 1e4) {
  con <- file(path, "rb", raw = TRUE)
  on.exit(close(con))
  
  # Guess approximate number of chunks
  n <- file.info(path)$size / chunk_size
  chunks <- vector("list", n)
  
  i <- 1L
  chunks[[i]] <- readBin(con, "raw", n = chunk_size)
  while(length(chunks[[i]]) == chunk_size) {
    i <- i + 1L
    chunks[[i]] <- readBin(con, "raw", n = chunk_size)
  }
  
  rawToChar(unlist(chunks, use.names = FALSE))
}

library(Rcpp)
sourceCpp("read-file.cpp")

library(Rcpp)
sourceCpp("read-file.cpp")

path <- file.path(R.home("doc"), "COPYING")
file.info(path)$size / 1024

stopifnot(identical(read_file1(path), read_file2(path)))
stopifnot(identical(read_file1(path), read_file3(path)))
stopifnot(identical(read_file1(path), read_file4(path)))
stopifnot(identical(read_file1(path), read_file_cpp2(path)))
stopifnot(identical(read_file1(path), read_file_cpp2(path)))

microbenchmark(
  readLines = read_file1(path),   
  readChar = read_file2(path),   
  readBin = read_file3(path),
  chunked_read = read_file4(path),
  Rcpp = read_file_cpp1(path),
  Rcpp2 = read_file_cpp2(path)
)
