#' @references \url{https://fedorahosted.org/liberation-fonts/}
#' @include fontset.R
fontset_liberation <- function(variant = "sans", style = "regular") {
  if (system.file("fonts", package = "fontLiberation") == "") {
    stop("fontLiberation must be installed", call. = FALSE)
  }
  font_get("Liberation", variant, style, pkg = "fontLiberation")
}

fontset_liberation_files <- list(
  sans = list(
    regular = font_sans("LiberationSans-Regular", face = "plain", weight = 80),
    italic = font_sans("LiberationSans-Italic", face = "italic", weight = 80),
    bold = font_sans("LiberationSans-Bold", face = "bold", weight = 200),
    `bold-italic` = font_sans("LiberationSans-BoldItalic", face = "bolditalic", weight = 200)
  ),
  `mono` = list(
    regular = font_mono("LiberationMono-Regular", face = "plain", weight = 80),
    oblique = font_mono("LiberationMono-Italic", face = "italic", weight = 80),
    bold = font_mono("LiberationMono-Bold", face = "bold", weight = 200),
    `bold-italic` = font_mono("LiberationMono-BoldItalic", face = "bolditalic", weight = 200)
  ),
  serif = list(
    regular = font_serif("LiberationSerif-Regular", face = "plain", weight = 80),
    italic = font_serif("LiberationSerif-Italic", face = "italic", weight = 80),
    bold = font_serif("LiberationSerif-Bold", face = "bold", weight = 200),
    `bold-italic` = font_serif("LiberationSerif-BoldItalic", face = "bolditalic", weight = 200)
  )
)

fontset_register("Liberation",
  getter = fontset_liberation,
  files = fontset_liberation_files
)
