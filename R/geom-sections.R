
#' Automatically colour rectangular group sections
#'
#' Determine two or four rectangular sections from
#' the positions of grouped observations.
#'
#' Groups must be separated by gaps along the
#' relevant x and/or y axes.
#'
#' @inheritParams ggplot2::geom_point
#' @param orientation "auto", "x", or "y".
#'   For four groups, a 2 x 2 layout is required.
#' @param alpha Background opacity (0 to 1).
#' @param show_lines Draw dividing lines?
#' @param line_colour Dividing line colour.
#' @param line_width Dividing line width in mm.
#' @param line_type Dividing line type.
#'
#' @return A ggplot2 layer.
#'
#' @export
geom_sections <- function(
  mapping = NULL,
  data = NULL,
  ...,
  orientation = "auto",
  alpha = 0.2,
  show_lines = FALSE,
  line_colour = "grey40",
  line_width = 0.5,
  line_type = "dashed",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  orientation <- match.arg(
    orientation, c("auto", "x", "y")
  )

  validate_alpha(alpha)

  validate_lines(
    show_lines, line_colour,
    line_width, line_type
  )

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = Geomsections,
    position = "identity",
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    params = list(
      orientation = orientation,
      alpha = alpha,
      show_lines = show_lines,
      line_colour = line_colour,
      line_width = line_width,
      line_type = line_type,
      na.rm = na.rm,
      ...
    )
  )
}

# Internal geom for automatically calculated sections
Geomsections <- ggplot2::ggproto(
  "Geomsections",
  ggplot2::Geom,

  required_aes = c("x", "y", "fill"),

  default_aes = ggplot2::aes(),

  draw_key = ggplot2::draw_key_rect,

  draw_panel = function(
    data, panel_params, coord,
    orientation = "auto",
    alpha = 0.2,
    show_lines = FALSE,
    line_colour = "grey40",
    line_width = 0.5,
    line_type = "dashed",
    na.rm = FALSE
  ) {
    check_sections_coord(coord)

    if (nrow(data) == 0L) {
      return(grid::nullGrob())
    }

    # Remove missing observations
    valid <- is.finite(data$x) &
      is.finite(data$y) &
      !is.na(data$fill) &
      !is.na(data$group)

    if (!all(valid)) {
      if (!na.rm) {
        warning(
          "Removed ", sum(!valid),
          " observations with missing values.",
          call. = FALSE
        )
      }

      data <- data[valid, , drop = FALSE]
    }

    if (nrow(data) == 0L) {
      return(grid::nullGrob())
    }

    # Convert ggplot2's group IDs to characters
    group_ids <- as.character(data$group)

    # Ensure each group has exactly one fill colour
    fill_by_group <- split(
      data$fill, group_ids
    )

    consistent <- vapply(
      fill_by_group,
      function(x) length(unique(x)) == 1L,
      logical(1)
    )

    if (!all(consistent)) {
      stop(
        "Each group must have exactly one ",
        "fill colour.",
        call. = FALSE
      )
    }

    group_colours <- vapply(
      fill_by_group,
      function(x) unique(x)[1],
      character(1)
    )

    # Compute cutoffs using the input coordinates
    section_data <- data.frame(
      x = data$x,
      y = data$y,
      group = group_ids
    )

    sections <- compute_sections(
      section_data,
      orientation = orientation
    )

    # Match colours to the spatial section order
    fills <- unname(
      group_colours[sections$groups]
    )

    # Convert cutoffs to panel coordinates
    x_pos <- panel_cutoff(
      sections$x_cutoff,
      "x", coord, panel_params
    )

    y_pos <- panel_cutoff(
      sections$y_cutoff,
      "y", coord, panel_params
    )

    # Draw sections using the shared renderer
    draw_section_grob(
      x_cutoff = x_pos,
      y_cutoff = y_pos,
      fill = fills,
      alpha = alpha,
      show_lines = show_lines,
      line_colour = line_colour,
      line_width = line_width,
      line_type = line_type
    )
  }
)
