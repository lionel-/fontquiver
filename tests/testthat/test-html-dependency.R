
test_that("throw when malformed list is supplied", {
  fonts <- list(fontset_bitstream_vera(), mtcars)
  expect_error(htmlFontDependency(fonts), "can contain only")
})

test_that("throw when non-font object is supplied", {
  expect_error(htmlFontDependency(mtcars), "Unknown class")
})

test_that("font collections are spliced", {
  variants <- font_variants("Bitstream Vera")
  families <- font_families("Bitstream Vera")
  styles <- font_styles("Bitstream Vera", "Sans Mono")
  faces <- font_faces("Bitstream Vera", "mono")

  expect_length(splice_fonts(variants), 10)
  expect_length(splice_fonts(families), 10)
  expect_length(splice_fonts(styles), 4)
  expect_length(splice_fonts(faces), 4)
  expect_length(splice_fonts(variants, families, styles, faces), 28)
})
