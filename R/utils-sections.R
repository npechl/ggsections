
validate_regions <- function(
  x_cutoff, y_cutoff, fill, alpha,
  line_width, line_colour, line_type,
  show_lines
) {
  if (is.null(x_cutoff) && is.null(y_cutoff)) {
    stop(
      "Supply x_cutoff, y_cutoff, or both.",
      call. = FALSE
    )
  }

  for (value in list(x_cutoff, y_cutoff)) {
    if (!is.null(value) &&
        !(is.numeric(value) &&
          length(value) == 1L &&
          is.finite(value))) {
      stop(
        "Cutoffs must be finite numeric scalars.",
        call. = FALSE
      )
    }
  }

  n <- if (
    !is.null(x_cutoff) &&
    !is.null(y_cutoff)
  ) 4L else 2L

  if (!is.null(fill)) {
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
        stop("Invalid fill colour.", call. = FALSE)
      }
    )
  }

  if (!is.numeric(alpha) ||
      length(alpha) != 1L ||
      !is.finite(alpha) ||
      alpha < 0 || alpha > 1) {
    stop("alpha must be between 0 and 1.",
         call. = FALSE)
  }

  if (!is.numeric(line_width) ||
      length(line_width) != 1L ||
      !is.finite(line_width) ||
      line_width < 0) {
    stop("line_width must be non-negative.",
         call. = FALSE)
  }

  if (!is.logical(show_lines) ||
      length(show_lines) != 1L ||
      is.na(show_lines)) {
    stop("show_lines must be TRUE or FALSE.",
         call. = FALSE)
  }

  if (!is.character(line_colour) ||
      length(line_colour) != 1L ||
      is.na(line_colour)) {
    stop("line_colour must be one colour.",
         call. = FALSE)
  }

  tryCatch(
    grDevices::col2rgb(line_colour),
    error = function(e) {
      stop("Invalid line_colour.", call. = FALSE)
    }
  )

  if (length(line_type) != 1L ||
      is.na(line_type) ||
      !(line_type %in% c(
        "blank", "solid", "dashed", "dotted",
        "dotdash", "longdash", "twodash"
      ))) {
    stop("Invalid line_type.", call. = FALSE)
  }

  invisible(TRUE)
}
