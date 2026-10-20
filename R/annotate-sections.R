
#' Colour regions of a ggplot panel
#'
#' @param x_cutoff Optional numeric x cutoff.
#' @param y_cutoff Optional numeric y cutoff.
#' @param fill Vector of two or four colours.
#' @param alpha Background opacity, between 0 and 1.
#' @param show_lines Draw cutoff lines?
#' @param line_colour Cutoff line colour.
#' @param line_width Cutoff line width in mm.
#' @param line_type Cutoff line type.
#'
#' @return A ggplot2 layer.
#' @export
annotate_sections  <- function(
  x_cutoff = NULL,
  y_cutoff = NULL,
  fill = NULL,
  alpha = 0.2,
  show_lines = FALSE,
  line_colour = "grey40",
  line_width = 0.5,
  line_type = "dashed"
) {
  validate_regions(
    x_cutoff, y_cutoff, fill, alpha,
    line_width, line_colour, line_type,
    show_lines
  )

  n <- if (
    !is.null(x_cutoff) &&
    !is.null(y_cutoff)
  ) 4L else 2L

  if (is.null(fill)) {
    fill <- if (n == 4L) {
      c("#F9D5D3", "#D6EAF8",
        "#FCF3CF", "#D5F5E3")
    } else {
      c("#F9D5D3", "#D6EAF8")
    }
  }

  ggplot2::layer(
    data = data.frame(.region = 1L),
    mapping = NULL,
    stat = "identity",
    geom = GeomSections,
    position = "identity",
    inherit.aes = FALSE,
    show.legend = FALSE,
    params = list(
      x_cutoff = x_cutoff,
      y_cutoff = y_cutoff,
      fill = fill,
      alpha = alpha,
      show_lines = show_lines,
      line_colour = line_colour,
      line_width = line_width,
      line_type = line_type
    )
  )
}
