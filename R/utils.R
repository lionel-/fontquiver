
check_font_family <- function(face, style, files) {
  faces <- names(files)
  if (!face %in% faces) {
    stop(call. = FALSE,
      "Available faces:\n",
      paste(faces, collapse = ", ")
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
  if (!ext %in% c("ttf", "woff")) {
    stop("`ext` must be either 'ttf' or 'woff'", call. = FALSE)
  }
}

check_font_exists <- function(font) {
  dir <- system.file("fonts", paste0(font, "-fonts"), package = "fontquiver")
  dir.exists(dir)
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

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

str_standardise <- function(name) {
  gsub(" ", "-", tolower(name))
}

str_trim_ext <- function(path) {
  sub("\\..+$", "", path)
}
