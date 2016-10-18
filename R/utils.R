
font_get <- function(fontset, variant, style, pkg) {
  variant <- str_standardise(variant, sep = "-")
  style <- str_standardise(style, sep = "-")

  props <- fontset_props(fontset)
  check_font_family(variant, style, props$files)

  std_name <- str_standardise(fontset, sep = "-")
  base <- props$files[[variant]][[style]]
  ttf <- concat_font(std_name, base, "ttf", package = pkg)
  woff <- concat_font(std_name, base, "woff", package = pkg)
  version <- font_version(std_name, package = pkg)
  name <- paste(fontset, str_prettify(variant), sep = " ")
  fullname <- paste(name, str_prettify(style), sep = " ")

  structure(class = "font", list(
    ttf = ttf,
    woff = woff,
    fontset = fontset,
    name = name,
    fullname = fullname,
    variant = variant,
    style = style,
    weight = attr(base, "weight"),
    family = attr(base, "family"),
    face = attr(base, "face"),
    package = pkg,
    version = version
  ))
}

check_font_family <- function(face, style, files) {
  face <- str_standardise(face, sep = "-")
  style <- str_standardise(style, sep = "-")

  if (!face %in% names(files)) {
    stop(call. = FALSE,
      "Available variants:\n",
      paste(str_prettify(names(files)), collapse = ", ")
    )
  }

  if (!style %in% names(files[[face]])) {
    styles <- names(files[[face]])
    styles <- paste(str_prettify(styles), collapse = ", ")
    stop(call. = FALSE, "Available styles for variant ", face, ":\n", styles)
  }
}

check_font_exists <- function(font, package) {
  dir <- system.file("fonts", paste0(font, "-fonts"), package = package)
  dir.exists(dir)
}

concat_font <- function(base, name, ext, package) {
  dir <- paste0(base, "-fonts")
  filename <- paste(name, ext, sep = ".")
  path <- file.path(dir, filename)

  file <- system.file("fonts", path, package = package)
  if (file == "") {
    stop("Internal error: cannot find font", filename)
  }
  file
}

font_version <- function(font, package) {
  file <- system.file("fonts", paste0(font, "-VERSION"), package = package)
  readChar(file, file.info(file)$size - 1)
}

#' Splice fonts and font collections
#'
#' \code{splice_fonts()} Reduces its arguments to a flat list. It
#' accepts indinstinctly \code{font} objects, lists of \code{font}
#' objects (obtained with \code{\link{fonts}()}), or collections of
#' fonts produced by \code{\link{font_variants}()} or
#' \code{\link{font_families}()}. Duplicate fonts are removed from the
#' result.
#'
#' @param ... Fonts or collections of fonts. See
#'   \code{\link{font_families}()} and \code{\link{font_variants}()}
#'   for creating collections of fonts. You can also supply lists of
#'   individual fonts as returned by \code{\link{fonts}()}.
#' @export
#' @examples
#' splice_fonts(font("Bitstream Vera", "Sans", "Oblique"), font_faces("Bitstream Vera", "mono"))
splice_fonts <- function(...) {
  fonts <- list(...)

  known_classes <- c(
    "font_families", "font_variants","font_faces",
    "font_styles", "font", "list"
  )
  unknown <- setdiff(vapply_chr(fonts, class), known_classes)
  if (length(unknown)) {
    stop("Unknown classes: ", paste(unknown, collapse = ", "), call. = FALSE)
  }

  meta <- keep(fonts, inherits, c("font_families", "font_variants"))
  coll <- keep(fonts, inherits, c("font_faces", "font_styles"))
  file <- keep(fonts, inherits, "font")
  list <- keep(fonts, is_bare_list)

  meta <- flatten(lapply(meta, flatten))
  coll <- flatten(coll)
  list <- flatten(list)

  if (!all(vapply_lgl(list, inherits, "font"))) {
    stop("Lists can contain only `font` objects", call. = FALSE)
  }

  fonts <- compact(c(meta, coll, file, list))
  names(fonts) <- vapply_chr(fonts, `[[`, "fullname")
  fonts
}

# R Utils ------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

str_standardise <- function(s, sep = "-") {
  gsub(" ", sep, tolower(s))
}

str_prettify <- function(str) {
  if (is.null(str)) return(NULL)
  vapply_chr(str, function(s) {
    s <- strsplit(s, "-|_| ")[[1]]
    paste0(toupper(substring(s, 1, 1)), substring(s, 2), collapse=" ")
  })
}

str_trim_ext <- function(path) {
  sub("\\..+$", "", path)
}

gather_tree <- function(x, depth) {
  if (is.character(depth)) {
    depth <- as.list(depth)
  } else if (is.numeric(depth)) {
    depth <- rep(list(NULL), depth)
  } else {
    stop("`depth` should be character or numeric", call. = FALSE)
  }

  if (length(depth) > 1) {
    x <- gather_tree_recurse(x, depth)
  }
  bind_rows(x, .id = depth[[1]])
}

gather_tree_recurse <- function(x, depth) {
  if (length(depth) > 1) {
    lapply(x, gather_tree_recurse, depth = depth[-1])
  } else {
    bind_rows(x, .id = depth[[1]])
  }
}

at_bottom <- function(.x, .f, ...) {
  if (is.list(.x)) {
    lapply(.x, at_bottom, .f, ...)
  } else {
    .f(.x, ...)
  }
}

spread_attributes <- function(x, id) {
  stopifnot(is.atomic(x))

  attrs <- attributes(unclass(x))
  stopifnot(all(lengths(attrs) == length(x)))

  attrs <- as.data.frame(attrs, stringsAsFactors = FALSE)
  df <- cbind(unclass(x), attrs, stringsAsFactors = FALSE)
  names(df)[[1]] <- id

  df
}

set_names <- function(x, nm = x) {
  stats::setNames(x, nm)
}

# Simple reimplementations to avoid dplyr dependency -----------------

bind_rows <- function(x, .id = NULL) {
  df <- do.call("rbind", unname(x))

  if (!is.null(.id)) {
    lengths <- vapply(x, nrow, numeric(1))
    id <- rep(names(x), lengths)
    df <- cbind(id, df, stringsAsFactors = FALSE)
    names(df)[[1]] <- .id
  }

  df
}

filter <- function(df, ...) {
  fs <- list(...)
  stopifnot(all(vapply(fs, inherits, logical(1), "formula")))
  fs <- lapply(fs, `[[`, 2)

  bind_call <- function(lhs, rhs, fun = "&") {
    call(fun, lhs, rhs)
  }
  pattern <- Reduce(bind_call, fs[-1], fs[[1]])
  args <- list(df, pattern, substitute(), drop = FALSE)

  # Assumes all formula envs are identical
  env <- as.environment(df)
  parent.env(env) <- environment(..1)
  do.call("[", args, envir = env)
}

filter_first <- function(df, ...) {
  filtered <- filter(df, ...)

  if (nrow(filtered) > 1) {
    filtered[1, , drop = FALSE]
  } else {
    filtered
  }
}

vapply_chr <- function(.x, .f, ...) {
  vapply(.x, .f, character(1), ..., USE.NAMES = FALSE)
}
vapply_lgl <- function(.x, .f, ...) {
  vapply(.x, .f, logical(1), ...)
}
lapply_if <- function(.x, .p, .f, ...) {
  matches <- vapply_lgl(.x, .p)
  .x[matches] <- lapply(.x[matches], .f, ...)
  .x
}
compact <- function(x) {
  Filter(length, x)
}
flatten <- function(.x) {
  unlist(.x, FALSE, FALSE)
}
keep <- function(.x, .p, ...) {
  .x[vapply_lgl(.x, .p, ...)]
}
is_bare_list <- function(.x) {
  is.list(.x) && !is.object(.x)
}
