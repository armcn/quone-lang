.make_variant <- function(type, tag, fields = list()) {
  structure(c(list(`__type__` = type, `__tag__` = tag), fields),
            class = c(paste0('tl_', type, '_', tag), paste0('tl_', type), 'tl_value'))
}

None <- function() {
  .make_variant('Option', 'None')
}

Some <- function(field1) {
  .make_variant('Option', 'Some', list(field1 = field1))
}

#' @param x [Integer]
#' @return [Integer]
inc <- function(x) {
  x + 1L

}

#' @param f [Integer -> Integer]
#' @param opt [Option Integer]
#' @return [Option Integer]
map_option <- function(f, opt) {
  local({
    .tmp_0 <- opt
    if (identical(.tmp_0$`__tag__`, 'None')) {
      None()

    } else if (identical(.tmp_0$`__tag__`, 'Some')) {
      x <- .tmp_0$field1
      Some(f(x))

    } else {
      stop('Non-exhaustive match')
    }

  })

}

result <- map_option(inc, Some(41L))

