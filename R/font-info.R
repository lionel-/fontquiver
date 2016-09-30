#' Font metadata
#'
#' This returns a data frame containing metadata about fonts. The
#' columns "variant" and "style" correspond to the properties of the
#' font as reported by fontconfig. These are suitable as parameters of
#' a font getter function such as \code{\link{font_bitstream_vera}()}.
#' The columns "family", "italic" and "bold" provide information in
#' terms of R nomenclature of fonts. The column "base" gives the base
#' filename of the font.
#'
#' @param font A string giving the name of the font
#'   (e.g. \code{"bitstream_vera"}).
#' @seealso \code{\link{font_info_files}()}
#' @export
font_info <- function(font) {
  files <- font_regularise_files(font)
  info <- at_bottom(files, spread_attributes, "base")
  gather_tree(info, c("variant", "style"))
}

#' Font files
#'
#' Returns a list of font files. The files are organised by R
#' nomenclature of families (\code{"sans"}, \code{"serif"},
#' \code{"mono"}, and \code{"symbol"}) and fonts (\code{"plain"},
#' \code{"italic"}, \code{"bold"}, and \code{"bolditalic"}). When a
#' font does not have a combination, \code{NA} is reported.
#'
#' @inheritParams font_info
#' @examples
#' font_info_files("bitstream_vera")$mono$bold
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
