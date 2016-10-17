#' @references \url{http://users.teilar.gr/~g1951d/}
#' @include fontset.R
fontset_symbola <- function(variant = "symbols", style = "regular") {
  font <- font_get("Symbola", variant, style, pkg = "fontquiver")
  font$name <- "Symbola"
  font$fullname <- "Symbola"
  font
}

fontset_symbola_files <- list(
  symbols = list(
    regular = symbol("Symbola", face = "symbol", weight = 80)
  )
)

fontset_register("Symbola",
  getter = fontset_symbola,
  files = fontset_symbola_files
)
