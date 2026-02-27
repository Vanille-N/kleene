#let new-sections = state("new-sections", (:))

#let is-outline = state("is-outline", false)

#let tagged(hl) = (sec) => {
  context {
    if is-outline.get() {
      hl(sec)
    } else {
      sec
    }
  }
}
#let styles = (
  new: it => [#highlight(fill: green.transparentize(70%), it)#text(fill: green, super[*(+)*])],
  major: it => [#highlight(fill: orange.transparentize(70%), it)#text(fill: orange, super[*(!!)*])],
  minor: it => [#highlight(fill: yellow.transparentize(70%), it)#text(fill: yellow, super[*(!)*])],
  breaking: it => [#highlight(fill: red.transparentize(70%), it)#text(fill: red, super[*(#sym.arrow.zigzag)*])]
)

#let new = tagged(styles.new)
#let minor = tagged(styles.minor)
#let major = tagged(styles.major)
#let breaking = tagged(styles.breaking)

