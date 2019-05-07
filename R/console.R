# Control messaging to console

.onAttach <- function(...) {
  default_log_set()
}

#' @name console-methods
#' @title Methods for printing to console
#' @description Print to console using colours.
#' @param x Character
#' @param ... Objects to print
#' @export
char <- function(x) {
  crayon::green(encodeString(x, quote = "'"))
}

#' @rdname console-methods
#' @export
stat <- function(...) {
  crayon::blue(...)
}

#' @rdname console-methods
#' @export
func <- function(x) {
  crayon::red(paste0(x, '()'))
}

#' @rdname console-methods
#' @export
cat_line <- function(...) {
  cat(paste0(..., "\n"), sep = "")
}
