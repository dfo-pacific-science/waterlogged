#' Color-blind Friendly Palette for NTSERP lab
#'
#' A function that asks for the number of colors (groups) needed and then gives a visually tested color palette for use in figures.
#' @param n_groups the number of colors (groups) required
#' @param full_palette logical; if TRUE, the full palette is returned (not recommended for plots)
#' @return list of colors suitable for colorblind viewing
#' @export
#'
## EXAMPLES
# NTSERP_palette(n_groups = 3)
# NTSERP_palette(full_palette = TRUE)

NTSERP_palette <- function(n_groups=0, full_palette = FALSE) {

  n <- n_groups

  if(full_palette == TRUE) {

    a <- c("#000000", "#233d4d","#5A4E45", "#915e3d", "#C86F35","#fe7f2d", "#FE9234","#fda53a","#FDB840", "#fcca46", "#E6C855","#cfc664", "#B8C473","#a1c181","#91B884", "#81ae86","#71A588", "#619b8a")
    message("WARNING: This is the full palette and colors are not easily distinguished from each other, especially for people with color-blindness. Use caution if using this full palette and consider testing combinations via Viz Palette at https://projects.susielu.com/viz-palette. YOU'VE BEEN WARNED.")
    return(a)
  }

  if(n == 2) {
    a <- c("#233d4d", "#fe7f2d")
    b <- c("#233d4d", "#a1c181")
    c <- c("#fe7f2d","#619b8a")
    d <- c("#233d4d", "#FDB840")

    palettes <- list(a,b,c,d)
  }

  if(n == 3) {
    a <- c("#233d4d", "#fe7f2d","#a1c181")
    b <- c("#233d4d", "#fe7f2d","#619b8a")
    c <- c("#233d4d", "#FDB840","#619b8a")

    palettes <- list(a,b,c)

  }

  if(n == 4){
    a <- c("#233d4d","#fe7f2d","#a1c181","#619b8a")
  }

  if(n == 5) {
    a <- c("#233d4d","#915e3d", "#fe7f2d", "#a1c181", "#619b8a")
  }

  if(n == 6) {
    a <- c("#233d4d", "#915e3d", "#fe7f2d", "#FDB840", "#a1c181","#619b8a")
  }

  if(n == 7) {
    a <- c("#000000", "#233d4d", "#915e3d", "#fe7f2d", "#FDB840", "#a1c181","#619b8a")
  }

  if(n <= 3) {

    message("Here's are some suggested palettes for ", n, " groups. All palettes tested for visual perception via Viz Palette by Suzie Lu.")
    return(palettes)

  }
  if(n>3 & n<=7) {
    message("Here's a suggested palette for ", n, " groups. All palettes tested for visual perception via Viz Palette by Suzie Lu.")
    return(a)
  }

  if(n > 7) {
    message("Uh oh! That's too many groups! You're on your own for this one. Set full_palette to TRUE and use Viz Palette to test more combos. Good luck, cowboy!")
  }
}
