
# fontquiver

fontquiver installs a set of fonts with permissive licences. It is
useful for packages that needs controlled versions of fonts.


## Installation

Get the development version from github with:

```{r}
# install.packages("devtools")
devtools::install_github("lionel-/fontquiver")
```


## Usage

### Fonts installed in the R library

fontquiver is an interface to fonts installed in the R library. It
provides convenient and structured access, for instance, to the
Bitstream Vera font family installed with the
[fontBitstreamVera](https://cran.r-project.org/package=fontBitstreamVera)
package, or the Liberation family that comes with
[fontLiberation](https://cran.r-project.org/package=fontLiberation).
The fonts too heavy to be distributed on CRAN can be accessed by
fontquiver through Github-only packages such as
[fontDejaVu](https://github.com/lionel-/fontDejaVu). These collections
of font are referred to as `fontset` in fontquiver. Call
`fontset_list()` to check which fontsets are currently installed in
your library.

Fonts installed in the R library can be useful for a variety of
purposes. They can be used in web applications with the
`htmlFontDependency()` tool. They are also helpful to create
reproducible outputs. An example of this is the `vdiffr` package which
relies on fontquiver to create SVGs that are reproducible across
platforms. Without fontquiver fonts, the SVGs generated by svglite
would have slight differences depending on the versions of the system
fonts used to compute text metrics.


### Categories of fonts

The standard categories of fonts in R are the `sans`, `serif`, and
`mono` families and the `plain`, `italic`, `bold`, and `bolditalic`
faces.However, font nomenclatures are extremely rich and go well
beyond those 12 categories. For example the DéjàVu set contains a font
whose variant and style are `Serif Condensed` and `Extra Light`. For
this reason, fontquiver uses the categories provided by the font util
`fc-scan` from
[Fontconfig](https://www.freedesktop.org/wiki/Software/fontconfig/).
The terms "variants" and "styles" refer to those categories while
"families" and "faces" refer to R's categories.

To check which variants and styles are available for a given fontset:

```{r}
fontset_variants("DejaVu")
fontset_styles("Bitstream Vera", variant = "Serif")
```


### Font getters

fontquiver provides several getters to access font objects. They all
take a fontset name as argument. They may also take `variant`/`style`
or `family`/`face` arguments.

`font()` takes a fontest, a variant and a style, and returns an atomic
font object. Those objects contain fields such as `ttf`, `fullname`,
or `version`:

```{r}
font("Bitstream Vera", "Sans", "Roman")
font("Bitstream Vera", "Serif", "Bold")$ttf
```

The other getters return collections of fonts. `font_variants()`
returns a tree of lists with the outer list containing all variants of
a fontset and the inner lists containing all styles for a given
variant. Similarly, `font_families()` returns a tree of fonts
structured according to families and faces:

```{r}
font_variants("Liberation")
font_families("Liberation")
```

If you need a specific variant or family, `font styles()` and
`font_faces()` return a list of fonts:

```{r}
font_styles("DejaVu", "Sans Condensed")
font_faces("DejaVu", "Mono")
```


## Applications

### Web dependency on an installed font

The `htmlFontDependency()` tool takes any font object or collection of
font objects. It copies the relevant fonts in `woff` format to a
temporary directory and creates a CSS linking to those fonts.

```{r}
# install.packages("htmltools")

# Create font dependency
liberation <- font_families("Liberation")
mono <- font_styles("DejaVu", "Sans Mono")
html_dep <- htmlFontDependency(liberation, mono)

span_mono <- htmltools::tags$span(
  style = "font-family:'Deja Vu Sans Mono'; font-style:italic;",
  "Text rendered with monospace italic font"
)
span_bold <- htmltools::tags$span(
  style = "font-family:'Bitstream Vera Sans'; font-weight:700;",
  "Text rendered with sans bold font"
)

# Add font dependency to an HTML object and print
text <- htmltools::div(span_mono, span_bold, html_dep)
htmltools::html_print(text)
```


### User fonts in svglite

You can supply fontquiver fonts to `svglite`:

```{r}
svglite::stringSVG(user_fonts = font_families("Liberation"), {
  plot(1:10)
})
```

See the fonts vignette in the svglite package for more about this.
