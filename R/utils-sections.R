
# Validate a single numeric cutoff
validate_cutoff <- function(x, name) {
  if (is.null(x)) return(invisible(NULL))

  if (!is.numeric(x) ||
      length(x) != 1L ||
      !is.finite(x)) {
    stop(
      name, " must be one finite numeric value.",
      call. = FALSE
    )
  }

  invisible(NULL)
}

# Validate alpha
validate_alpha <- function(alpha) {
  if (!is.numeric(alpha) ||
      length(alpha) != 1L ||
      !is.finite(alpha) ||
      alpha < 0 || alpha > 1) {
    stop(
      "alpha must be a number between 0 and 1.",
      call. = FALSE
    )
  }

  invisible(NULL)
}

# Validate colour vectors
validate_colours <- function(fill, n) {
  if (!is.character(fill) ||
      length(fill) != n ||
      anyNA(fill)) {
    stop(
      "fill must contain exactly ", n,
      " colours.",
      call. = FALSE
    )
  }

  tryCatch(
    grDevices::col2rgb(fill),
    error = function(e) {
      stop(
        "fill contains an invalid colour.",
        call. = FALSE
      )
    }
  )

  invisible(NULL)
}

# Validate line settings
validate_lines <- function(
  show_lines, line_colour,
  line_width, line_type
) {
  if (!is.logical(show_lines) ||
      length(show_lines) != 1L ||
      is.na(show_lines)) {
    stop(
      "show_lines must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.numeric(line_width) ||
      length(line_width) != 1L ||
      !is.finite(line_width) ||
      line_width < 0) {
    stop(
      "line_width must be non-negative.",
      call. = FALSE
    )
  }

  if (!is.character(line_colour) ||
      length(line_colour) != 1L ||
      is.na(line_colour)) {
    stop(
      "line_colour must be one colour.",
      call. = FALSE
    )
  }

  tryCatch(
    grDevices::col2rgb(line_colour),
    error = function(e) {
      stop("Invalid line_colour.", call. = FALSE)
    }
  )

  valid_types <- c(
    "blank", "solid", "dashed", "dotted",
    "dotdash", "longdash", "twodash"
  )

  if (!is.character(line_type) ||
      length(line_type) != 1L ||
      is.na(line_type) ||
      !line_type %in% valid_types) {
    stop("Invalid line_type.", call. = FALSE)
  }

  invisible(NULL)
}

# Restrict the first version to Cartesian coordinates
check_sections_coord <- function(coord) {
  if (!inherits(coord, "CoordCartesian")) {
    stop(
      "Section layers currently require ",
      "Cartesian coordinates.",
      call. = FALSE
    )
  }

  invisible(NULL)
}

# Convert a cutoff from data coordinates into
# normalised panel coordinates.
#
# The cutoff is transformed by the coordinate
# system and clipped to the panel.
panel_cutoff <- function(
  cutoff, axis, coord, panel_params
) {
  if (is.null(cutoff)) return(NULL)

  value <- data.frame(
    x = if (axis == "x") cutoff else NA_real_,
    y = if (axis == "y") cutoff else NA_real_
  )

  transformed <- coord$transform(
    value, panel_params
  )

  # coord_flip() moves data x onto the panel's y
  # axis and vice versa
  if (inherits(coord, "CoordFlip")) {
    axis <- setdiff(c("x", "y"), axis)
  }

  position <- transformed[[axis]][1]

  if (!is.finite(position)) {
    stop(
      "Cutoff could not be transformed ",
      "into panel coordinates.",
      call. = FALSE
    )
  }

  max(0, min(1, position))
}

# Convert data-space cutoffs to panel positions.
#
# Fills arrive in data order (bottom-left,
# bottom-right, top-left, top-right). Under
# coord_flip() the axes swap, so the cutoffs swap
# and the off-diagonal quadrants trade places.
section_positions <- function(
  x_cutoff, y_cutoff, fill, coord, panel_params
) {
  x_pos <- panel_cutoff(
    x_cutoff, "x", coord, panel_params
  )

  y_pos <- panel_cutoff(
    y_cutoff, "y", coord, panel_params
  )

  if (!inherits(coord, "CoordFlip")) {
    return(list(x = x_pos, y = y_pos, fill = fill))
  }

  if (length(fill) == 4L) {
    fill <- fill[c(1L, 3L, 2L, 4L)]
  }

  list(x = y_pos, y = x_pos, fill = fill)
}
