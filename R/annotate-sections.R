
#' Annotate rectangular background sections
#'
#' Divide the plotting panel into two or four
#' coloured sections using explicit cutoffs.
#'
#' @param x_cutoff Optional numeric x cutoff.
#' @param y_cutoff Optional numeric y cutoff.
#' @param fill Two or four background colours.
#' @param alpha Background opacity (0 to 1).
#' @param show_lines Draw dividing lines?
#' @param line_colour Dividing line colour.
#' @param line_width Dividing line width in mm.
#' @param line_type Dividing line type.
#'
#' @return A ggplot2 layer.
#'
#' @export
annotate_sections <- function(
  x_cutoff = NULL,
  y_cutoff = NULL,
  fill = NULL,
  alpha = 0.2,
  show_lines = FALSE,
  line_colour = "grey40",
  line_width = 0.5,
  line_type = "dashed"
) {
  validate_cutoff(x_cutoff, "x_cutoff")
  validate_cutoff(y_cutoff, "y_cutoff")
  validate_alpha(alpha)

  validate_lines(
    show_lines, line_colour,
    line_width, line_type
  )

  if (is.null(x_cutoff) && is.null(y_cutoff)) {
    stop(
      "Supply x_cutoff, y_cutoff, or both.",
      call. = FALSE
    )
  }

  n <- if (
    !is.null(x_cutoff) &&
    !is.null(y_cutoff)
  ) 4L else 2L

  if (is.null(fill)) {
    fill <- if (n == 4L) {
      c(
        "#F9D5D3",
        "#D6EAF8",
        "#FCF3CF",
        "#D5F5E3"
      )
    } else {
      c("#F9D5D3", "#D6EAF8")
    }
  }

  validate_colours(fill, n)

  ggplot2::layer(
    data = data.frame(.section = 1L),
    # Mapped (not passed as params) so scale
    # transformations apply, as in geom_vline().
    # NULL cutoffs are dropped from the mapping.
    mapping = ggplot2::aes(
      xintercept = !!x_cutoff,
      yintercept = !!y_cutoff
    ),
    stat = "identity",
    geom = GeomAnnotationsections,
    position = "identity",
    inherit.aes = FALSE,
    show.legend = FALSE,
    params = list(
      fill = fill,
      alpha = alpha,
      show_lines = show_lines,
      line_colour = line_colour,
      line_width = line_width,
      line_type = line_type
    )
  )
}

# Internal geom for explicit section annotations
GeomAnnotationsections <- ggplot2::ggproto(
  "GeomAnnotationsections",
  ggplot2::Geom,

  required_aes = character(0),

  default_aes = ggplot2::aes(
    xintercept = NA_real_,
    yintercept = NA_real_
  ),

  draw_key = ggplot2::draw_key_blank,

  draw_panel = function(
    data, panel_params, coord,
    fill,
    alpha = 0.2,
    show_lines = FALSE,
    line_colour = "grey40",
    line_width = 0.5,
    line_type = "dashed"
  ) {
    check_sections_coord(coord)

    # Cutoffs are now in transformed scale space
    x_cutoff <- data$xintercept[1]
    y_cutoff <- data$yintercept[1]

    pos <- section_positions(
      if (is.na(x_cutoff)) NULL else x_cutoff,
      if (is.na(y_cutoff)) NULL else y_cutoff,
      fill, coord, panel_params
    )

    draw_section_grob(
      x_cutoff = pos$x,
      y_cutoff = pos$y,
      fill = pos$fill,
      alpha = alpha,
      show_lines = show_lines,
      line_colour = line_colour,
      line_width = line_width,
      line_type = line_type
    )
  }
)
