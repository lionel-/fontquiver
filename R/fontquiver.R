
check_font_family <- function(face, style, files) {
  faces <- names(files)
  if (!face %in% faces) {
    stop(call. = FALSE,
      "Available faces:\n",
      paste(names(faces), collapse = ", ")
    )
  }

  if (!style %in% names(files[[face]])) {
    stop(call. = FALSE,
      "Available styles for face ", face, ":\n",
      paste(names(files[[face]]), collapse = ", ")
    )
  }
}

check_font_ext <- function(ext) {
  if (!ext %in% c("ttf", "svg", "woff")) {
    stop("`ext` should be one of: 'ttf', 'svg', 'woff'",
      call. = FALSE)
  }
}

font_file <- function(base, name, ext) {
  dir <- paste0(base, "-fonts")
  file <- paste(name, ext, sep = ".")
  path <- file.path(dir, file)
  system.file("fonts", path, package = "fontquiver")
}

font_version <- function(font) {
  file <- system.file("fonts", paste0(font, "-VERSION"),
    package = "fontquiver")
  readChar(file, file.info(file)$size - 1)
}
