#' @export
#' @title font description to be used by graphical devices
#' @format an object that describes a font
#' @description Summarize a font package.
#' @field get_data get a data.frame describing a font
#' @field get_id get font id
#' @field get_families get a list to be used by a graphic device.
#' @field set_data,set_id convenience function to be used when creating a
#' font description object.
#' @details
#' \code{font_desc} is only there to provide common structure and behavior
#' for a given font description.
#'
#' @examples
#' # construct a \code{font_desc} object
#' font_desc_ <- font_desc_bitstream_vera$new()
#' font_desc_$get_families()
#' font_desc_$get_data()
#'
#' font_desc_dejavu$new()$get_families()
#' font_desc_dejavu$new()$get_data()
#' font_desc_liberation$new()$get_families()
#' font_desc_liberation$new()$get_data()
#' @importFrom R6 R6Class
font_desc <- R6Class(
  "font_desc",
  public = list(

    get_data = function(){
      private$content
    },
    set_id = function(fontid){
      private$id <- fontid
      self
    },
    get_id = function(){
      private$id
    },
    set_version = function(version){
      private$version <- version
      self
    },
    get_version = function(){
      private$version
    },
    print = function(){
      if(!is.null(private$content))
        print(private$content)
      else warning("uninitialized font description")
    } ,
    set_data = function(df){

      stopifnot( "family" %in% names(df) )
      stopifnot( "style" %in% names(df) )
      stopifnot( "bold" %in% names(df) )
      stopifnot( "italic" %in% names(df) )
      stopifnot( "base" %in% names(df) )

      stopifnot( all( c("sans", "serif", "mono") %in% df$family ) )
      stopifnot( all( c("regular", "bold", "italic", "bolditalic") %in% df$style ) )

      private$content <- df
      self
    },

    get_families = function(ext = "ttf", as_vector = FALSE) {
      families_variations <- c("sans", "serif", "mono")
      stopifnot(all( families_variations %in% private$content$family ))

      ldata <- split(private$content, private$content$family)[families_variations]
      out <- lapply(ldata, function(x) {
        ref_result <- setNames( rep(NA, 4), c("regular", "bold", "italic", "bolditalic") )
        existings <- intersect( x$style, names(ref_result) )
        id <- match( existings, names(ref_result) )
        ttf_files <- paste0(x$base[id], ".", ext)
        y <- system.file( file.path("fonts", private$fontdir), package = private$package )
        y <- paste0(y, "/", ttf_files)
        y <- ifelse( file.exists(y), y, NA )
        ref_result[id] <- y
        ref_result
      })
      if( as_vector ) out <- as.character(unlist(out))
      out
    }
  ),
  private = list(
    content = NULL,
    fontdir = NULL,
    id = NULL,
    version = NULL,
    package = NULL
  )
)

#' @export
#' @rdname font_desc
#' @details
#'
#' \code{font_desc_bitstream_vera} return details about Bitstream Vera
#' font families and styles.
font_desc_bitstream_vera <- R6Class(
  "font_bitstream_vera_spec",
  inherit = font_desc,
  public = list(
    initialize = function() {

      self$set_data ( data.frame(
        family = rep( c( "sans", "serif", "mono" ), each = 4 ),
        style = rep(c( "regular", "bold", "italic", "bolditalic"), 3 ),
        bold = rep(c(FALSE, TRUE, FALSE, TRUE), 3 ),
        italic = rep(c(FALSE, FALSE, TRUE, TRUE), 3 ),
        base = c( "Vera", "VeraBd", "VeraIt", "VeraBI",
                  "VeraSe", "VeraSeBd", NA, NA,
                  "VeraMono", "VeraMoBd", "VeraMoIt", "VeraMoBI"),
        stringsAsFactors = FALSE
      ) )$set_id("bitstream_vera")

      private$fontdir <- "bitstream-vera-fonts"
      private$package <- "fontBitstreamVera"
      self$set_version(packageVersion("fontBitstreamVera"))
    }
  )
)

#' @export
#' @rdname font_desc
#' @details
#'
#' \code{font_desc_dejavu} return details about DejaVu
#' font families and styles.
font_desc_dejavu <- R6Class(
  "font_desc_dejavu",
  inherit = font_desc,
  public = list(
    initialize = function() {

      self$set_data ( data.frame(
        family = c("sans", "sans", "sans", "sans", "sans", "sanscondensed", "sanscondensed",
                   "sanscondensed", "sanscondensed", "mono", "mono", "mono",
                   "mono", "serif", "serif", "serif", "serif", "serifcondensed",
                   "serifcondensed", "serifcondensed", "serifcondensed", "mathtexgyre"
        ),
        style = c("regular", "bold", "italic", "bolditalic", "extralight", "regular",
                  "bold", "italic", "bolditalic", "regular", "bold", "italic",
                  "bolditalic", "regular", "bold", "italic", "bolditalic", "regular",
                  "bold", "italic", "bolditalic", "regular"),
        bold = c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE,
                 FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
                 FALSE, TRUE, FALSE),
        italic = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE,
                   FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE,
                   TRUE, TRUE, FALSE),
        base = c("DejaVuSans", "DejaVuSans-Bold", "DejaVuSans-Oblique", "DejaVuSans-BoldOblique",
                 "DejaVuSans-ExtraLight", "DejaVuSansCondensed", "DejaVuSansCondensed-Bold",
                 "DejaVuSansCondensed-Oblique", "DejaVuSansCondensed-BoldOblique",
                 "DejaVuSansMono", "DejaVuSansMono-Bold", "DejaVuSansMono-Oblique",
                 "DejaVuSansMono-BoldOblique", "DejaVuSerif", "DejaVuSerif-Bold",
                 "DejaVuSerif-Italic", "DejaVuSerif-BoldItalic", "DejaVuSerifCondensed",
                 "DejaVuSerifCondensed-Bold", "DejaVuSerifCondensed-Italic", "DejaVuSerifCondensed-BoldItalic",
                 "DejaVuMathTexGyre"),
        stringsAsFactors = FALSE
      ) )$set_id("dejavu")

      private$fontdir <- "dejavu-fonts/ttf"
      private$package <- "fontHeavy"
      self$set_version(packageVersion("fontHeavy"))
    }
  )
)



#' @export
#' @rdname font_desc
#' @details
#'
#' \code{font_desc_dejavu} return details about Liberation
#' font families and styles.
font_desc_liberation <- R6Class(
  "font_desc_liberation",
  inherit = font_desc,
  public = list(
    initialize = function() {

      self$set_data ( data.frame(
        family = rep( c( "sans", "serif", "mono" ), each = 4 ),
        style = rep(c( "regular", "bold", "italic", "bolditalic"), 3 ),
        bold = rep(c(FALSE, TRUE, FALSE, TRUE), 3 ),
        italic = rep(c(FALSE, FALSE, TRUE, TRUE), 3 ),
        base = c( "LiberationSans-Regular", "LiberationSans-Bold", "LiberationSans-Italic", "LiberationSans-BoldItalic",
                  "LiberationSerif-Regular", "LiberationSerif-Bold", "LiberationSerif-Italic", "LiberationSerif-BoldItalic",
                  "LiberationMono-Regular", "LiberationMono-Bold", "LiberationMono-Italic", "LiberationMono-BoldItalic"),
        stringsAsFactors = FALSE
      ) )$set_id("liberation")

      private$fontdir <- "liberation-fonts"
      private$package <- "fontHeavy"
      self$set_version(packageVersion("fontHeavy"))
    }
  )
)


# families <- sapply( font_dejavu_files, length )
# names(families) <- gsub(pattern = "-", replacement = "", names(families))
# families <- unlist( purrr::map2(names(families), families, function(x, y) rep(x, y)) )
# dput(families)
# style <- setNames( unlist( lapply( font_dejavu_files, names ) ), NULL )
# dput(style)
# bold <- c(F, T, F, T, F, F, T, F, T, F, T, F, T, F, T, F, T, F, T, F, T, F)
# dput(bold)
# italic <- c(F, F, T, T, F, F, F, T, T, F, F, T, T, F, F, T, T,F, F, T, T,F)
# dput(italic)
#

#' @import purrr
#' @title include fonts as as CSS dependency
#' @description include fonts for families \code{sans}, \code{serif}, \code{mono}
#' as as CSS dependency.
#' @param font_description a \code{font_desc} object
#' @export
font_desc_html_dependency <- function(font_description =
                                     font_desc_bitstream_vera$new() ) {
  if (!requireNamespace("htmltools")) {
    stop("htmltools is not installed", call. = FALSE)
  }

  css_dir <- file.path(tempdir(), "fontquiver")
  if (!dir.exists(css_dir)) {
    dir.create(css_dir)
  }
  families_ <- c("sans", "serif", "mono")
  styles_ <- c("regular", "bold", "italic", "bolditalic")

  id <- font_description$get_id()

  fd_df <- font_description$get_data()
  # filter to only graphic device families
  fd_df <- fd_df[fd_df$family %in% families_, ]
  # filter to only graphic device styles
  fd_df <- fd_df[fd_df$style %in% styles_, ]
  # order the same way than get_families()
  order_ <- order(factor(fd_df$family, levels = families_ ) ,
        factor(fd_df$style, levels = styles_ ) )
  fd_df <- fd_df[order_, ]

  fd_df$file_ttf <- font_description$get_families(ext = "ttf", as_vector = TRUE)
  fd_df$file_woff <- font_description$get_families(ext = "woff", as_vector = TRUE)
  fd_df$family_id <- paste( rep(id, nrow(fd_df) ), fd_df$family, sep = "-")

  args <- list( id = fd_df$family_id,
    file = fd_df$base,
    bold = fd_df$bold, italic = fd_df$italic )

  css <- pmap_chr(args, get_css_font_face)
  css_file <- tempfile(id, css_dir, fileext = ".css")
  writeLines(css, css_file, useBytes = TRUE)

  fd_df <- fd_df[ !is.na( fd_df$base ), ]
  to <- file.path(css_dir, basename(fd_df$file_ttf))
  file.copy(fd_df$file_ttf, to)
  to <- file.path(css_dir, basename(fd_df$file_woff))
  file.copy(fd_df$file_woff, to)

  htmltools::htmlDependency(
    name = id,
    version = packageVersion("fontquiver"),
    src = css_dir,
    stylesheet = basename(css_file)
  )
}

get_css_font_face <- function(id, file, bold, italic) {
  if( is.na(file) ) return ("")
  paste( c(
    "@font-face {\n",
    sprintf("font-family: '%s';\n", id),
    ifelse(italic, "font-style: italic;\n", ""),
    ifelse(bold, "font-weight: bold;\n", ""),
    sprintf("src: url('%s.ttf') format('truetype'), url('%s.woff') format('woff');\n", file, file),
    "}\n"
  ), collapse = "")
}




