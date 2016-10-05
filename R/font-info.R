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
#' @param fontset A string giving the name of a set of fonts
#'   (e.g. \code{"Bitstream Vera"}).
#' @seealso \code{\link{font_families}()}, \code{\link{font_variants}()}
#' @export
font_info <- function(fontset) {
  font <- str_standardise(fontset, sep = "_")
  files <- font_regularise_files(font)
  info <- at_bottom(files, spread_attributes, "base")
  gather_tree(info, c("variant", "style"))
}

#' Font families
#'
#' Returns families of font files for a given font set. The files are
#' organised by R nomenclature of families (\code{"sans"},
#' \code{"serif"}, \code{"mono"}, and \code{"symbol"}) and faces
#' (\code{"plain"}, \code{"italic"}, \code{"bold"}, and
#' \code{"bolditalic"}). When a font does not have a combination,
#' \code{NA} is reported.
#'
#' @inheritParams font_info
#' @examples
#' font_families("Bitstream Vera")$mono$bold
#' @export
font_families <- function(fontset) {
  info <- font_info(fontset)
  props <- font_props(fontset)
  getter <- props$getter

  filter_file <- function(...) {
    filtered <- filter_first(info, ..., ~!is.na(family), ~!is.na(face))
    if (nrow(filtered)) {
      do.call(getter, filtered[c("variant", "style")])
    } else {
      NULL
    }
  }

  fnames <- set_names(c("sans", "serif", "mono", "symbol"))
  families <- lapply(fnames, function(r_family) {
    family <- list(
      plain = filter_file(~family == r_family, ~face == "plain"),
      italic = filter_file(~family == r_family, ~face == "italic"),
      bold = filter_file(~family == r_family, ~face == "bold"),
      bolditalic = filter_file(~family == r_family, ~face == "bolditalic"),
      symbol = filter_file(~family == r_family, ~face == "symbol")
    )
    structure(family, class = "font_family")
  })

  structure(families, class = "font_families")
}

#' Font family
#'
#' @inheritParams font_info
#' @param family One of \code{"sans"}, \code{"serif"}, \code{"mono"}
#'   or \code{"symbol"}.
#' @export
font_family <- function(fontset, family) {
  families <- font_families(fontset)
  family <- str_standardise(family, sep = "")
  families[[family]]
}

font_regularise_files <- function(files) {
  if (is.character(files)) {
    files <- get(paste0("font_", files, "_files"))
  }
  stopifnot(is.list(files))
  files
}

#' Font variants
#'
#' @inheritParams font_info
#' @export
font_variants <- function(fontset) {
  props <- font_props(fontset)
  variants <- names(props$files)

  variants <- Map(function(variant, variant_name) {
    styles <- set_names(names(variant))
    styles <- lapply(styles, props$getter, variant = variant_name)
    structure(styles, class = "font_styles")
  }, props$files, names(props$files))

  structure(variants, class = "font_variants")
}

#' Font variant
#'
#' @inheritParams font_info
#' @param variant Font variant, according to Fontconfig's
#'   nomenclature.
#' @export
font_variant <- function(fontset, variant) {
  variant <- str_standardise(variant, sep = "-")
  variants <- font_variants(fontset)
  variants[[variant]]
}
