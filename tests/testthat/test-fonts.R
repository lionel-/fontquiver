
context("Fontset Files")

check_fontset_files <- function(files, font_getter) {
  Map(function(styles, variant) {
    fonts <- lapply(names(styles), font_getter, variant = variant)
    not_found <- fonts == ""
    info <- paste(variant, ": ", names(styles)[not_found], collapse = ", ", sep = "")
    expect_false(any(not_found), info)
  }, files, names(files))
}

test_that("Bitstream Vera files are found", {
  check_fontset_files(fontset_bitstream_vera_files, fontset_bitstream_vera)
})

test_that("DejaVu files are found", {
  if (check_font_exists("dejavu", "fontDejaVu")) {
    check_fontset_files(fontset_dejavu_files, fontset_dejavu)
  }
})

test_that("Liberation files are found", {
  check_fontset_files(fontset_liberation_files, fontset_liberation)
})

test_that("Symbola file is found", {
  check_fontset_files(fontset_symbola_files, fontset_symbola)
})
