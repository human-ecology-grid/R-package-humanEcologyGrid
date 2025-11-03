check_errors <- function(expr) {
  tryCatch({ suppressWarnings(expr); TRUE }, error = function(...) FALSE)
}
