
# fontquiver 0.1.0.9000

- Add Liberation and Symbola fonts

- Removed `font_bitstream_vera()` and `font_dejavu()` getters. They
  are replaced by a family of generic font getters: `font()` to
  retrieve the font for a given variant and style, and `fonts()` to
  retieve all fonts defined in a fontset.

- Add `font_families()` and `font_faces()` to retrieve font files
  organised according to R's nomenclature of fonts. Conversely there is
  now `font_variants()` and `font_styles()` which organise fonts
  according to fontconfig's categories. Also, `font_symbol()`
  allows to retrieve a symbol font defined by a fontset (currently
  only the Symbola fontset defines one).

- `htmlFontDependency()` now takes fonts and collections of fonts (as
  returned by `font_families()` and friends) as argument.

# fontquiver 0.1.0

Initial release

- Add `font_bitstream_vera()` and `font_dejavu()`. The latter require
  installing the fontHeavy package from Github:
  `devtools::install_github("lionel-/fontHeavy")`.

- Implement `htmlFontDependency()` to include font dependency in web
  pages or applications.
