#' @references \url{http://dejavu-fonts.org}
#' @include utils.R
fontset_dejavu <- function(variant = "sans", style = "book") {
  if (system.file("fonts", package = "fontHeavy") == "") {
    stop(call. = FALSE,
      "fontHeavy must be installed from Github. Please run:\n",
      "  install.packages('devtools')\n",
      "  devtools::install_github('lionel-/fontHeavy')"
    )
  }
  font_get("DejaVu", variant, style, pkg = "fontHeavy")
}

fontset_dejavu_files <- list(
  sans = list(
    book = font_sans("ttf/DejaVuSans", face = "plain", weight = 80),
    oblique = font_sans("ttf/DejaVuSans-Oblique", face = "italic", weight = 80),
    bold = font_sans("ttf/DejaVuSans-Bold", face = "bold", weight = 200),
    `bold-oblique` = font_sans("ttf/DejaVuSans-BoldOblique", face = "bolditalic", weight = 200),
    `extra-light` = font_sans("ttf/DejaVuSans-ExtraLight", weight = 40)
  ),
  `sans-condensed` = list(
    book = font_extra("ttf/DejaVuSansCondensed", face = "plain", weight = 80),
    oblique = font_extra("ttf/DejaVuSansCondensed-Oblique", face = "italic", weight = 80),
    bold = font_extra("ttf/DejaVuSansCondensed-Bold", face = "bold", weight = 200),
    `bold-oblique` = font_extra("ttf/DejaVuSansCondensed-BoldOblique", face = "bolditalic", weight = 200)
  ),
  `sans-mono` = list(
    book = font_mono("ttf/DejaVuSansMono", face = "plain", weight = 80),
    oblique = font_mono("ttf/DejaVuSansMono-Oblique", face = "italic", weight = 80),
    bold = font_mono("ttf/DejaVuSansMono-Bold", face = "bold", weight = 200),
    `bold-oblique` = font_mono("ttf/DejaVuSansMono-BoldOblique", face = "bolditalic", weight = 200)
  ),
  serif = list(
    book = font_serif("ttf/DejaVuSerif", face = "plain", weight = 80),
    italic = font_serif("ttf/DejaVuSerif-Italic", face = "italic", weight = 80),
    bold = font_serif("ttf/DejaVuSerif-Bold", face = "bold", weight = 200),
    `bold-italic` = font_serif("ttf/DejaVuSerif-BoldItalic", face = "bolditalic", weight = 200)
  ),
  `serif-condensed` = list(
    book = font_extra("ttf/DejaVuSerifCondensed", face = "plain", weight = 80),
    italic = font_extra("ttf/DejaVuSerifCondensed-Italic", face = "italic", weight = 80),
    bold = font_extra("ttf/DejaVuSerifCondensed-Bold", face = "bold", weight = 200),
    `bold-italic` = font_extra("ttf/DejaVuSerifCondensed-BoldItalic", face = "bolditalic", weight = 200)
  ),
  `math-tex-gyre` = list(
    regular = font_extra("ttf/DejaVuMathTexGyre", face = "plain", weight = 80)
  )
)

fontset_register("DejaVu",
  getter = fontset_dejavu,
  files = fontset_dejavu_files
)
