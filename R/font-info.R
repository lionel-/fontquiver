
#' @export
font_info <- function(font) {
  files <- font_regularise_files(font)
  info <- at_bottom(files, spread_attributes, "base")
  gather_tree(info, c("variant", "style"))
}

#' @export
font_info_files <- function(font) {
  info <- font_info(font)
  getter <- font_getter(font)

  fnames <- c("sans", "serif", "mono", "symbol")
  fnames <- stats::setNames(fnames, fnames)

  filter_file <- function(...) {
    filtered <- filter_first(info, ..., ~!is.na(family))
    if (nrow(filtered) == 1) {
      font <- do.call(getter, filtered[c("variant", "style")])
      font$file
    } else {
      NA
    }
  }

  families <- lapply(fnames, function(r_family) {
    list(
      plain = filter_file(~family == r_family, ~italic == FALSE, ~bold == FALSE),
      italic = filter_file(~family == r_family, ~italic == TRUE, ~bold == FALSE),
      bold = filter_file(~family == r_family, ~italic == FALSE, ~bold == TRUE),
      bolditalic = filter_file(~family == r_family, ~italic == TRUE, ~bold == TRUE)
    )
  })

  families
}

font_regularise_files <- function(files) {
  if (is.character(files)) {
    files <- get(paste0("font_", files, "_files"))
  }
  stopifnot(is.list(files))
  files
}

font_getter <- function(font) {
  files <- font_regularise_files(font)
  attr(files, "getter")
}
