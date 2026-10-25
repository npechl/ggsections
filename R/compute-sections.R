
# Split group identifiers into two sets according
# to their mean position along an axis.
split_region_groups <- function(data, axis) {
  centres <- tapply(
    data[[axis]],
    data$group,
    mean
  )

  ordered <- names(sort(centres))
  n <- length(ordered)

  list(
    lower = ordered[seq_len(n / 2L)],
    upper = ordered[
      seq.int(n / 2L + 1L, n)
    ]
  )
}

# Compute the gap between two sets of groups.
region_axis_gap <- function(
  data, axis, lower, upper
) {
  a <- data[
    data$group %in% lower,
    axis
  ]

  b <- data[
    data$group %in% upper,
    axis
  ]

  left_edge <- max(a)
  right_edge <- min(b)

  list(
    gap = right_edge - left_edge,
    cutoff = (left_edge + right_edge) / 2
  )
}

#' Compute rectangular group sections
#'
#' @keywords internal
compute_sections <- function(
  data,
  orientation = "auto"
) {
  orientation <- match.arg(
    orientation, c("auto", "x", "y")
  )

  required <- c("x", "y", "group")

  if (!all(required %in% names(data))) {
    stop(
      "data must contain x, y and group.",
      call. = FALSE
    )
  }

  if (!is.numeric(data$x) ||
      !is.numeric(data$y)) {
    stop(
      "x and y must be numeric.",
      call. = FALSE
    )
  }

  if (any(!is.finite(data$x)) ||
      any(!is.finite(data$y)) ||
      anyNA(data$group)) {
    stop(
      "Coordinates and groups must be complete.",
      call. = FALSE
    )
  }

  data$group <- as.character(data$group)

  groups <- unique(data$group)
  n <- length(groups)

  if (!n %in% c(2L, 4L)) {
    stop(
      "geom_sections() requires exactly ",
      "2 or 4 groups.",
      call. = FALSE
    )
  }

  xsplit <- split_region_groups(data, "x")
  ysplit <- split_region_groups(data, "y")

  xgap <- region_axis_gap(
    data, "x",
    xsplit$lower, xsplit$upper
  )

  ygap <- region_axis_gap(
    data, "y",
    ysplit$lower, ysplit$upper
  )

  if (n == 2L) {
    if (orientation == "auto") {
      xspan <- diff(range(data$x))
      yspan <- diff(range(data$y))

      gx <- if (xspan > 0) {
        xgap$gap / xspan
      } else {
        -Inf
      }

      gy <- if (yspan > 0) {
        ygap$gap / yspan
      } else {
        -Inf
      }

      orientation <- if (gx >= gy) {
        "x"
      } else {
        "y"
      }
    }

    selected <- if (orientation == "x") {
      xgap
    } else {
      ygap
    }

    if (selected$gap <= 0) {
      stop(
        "Groups overlap along the ",
        "selected axis.",
        call. = FALSE
      )
    }

    sides <- if (orientation == "x") {
      xsplit
    } else {
      ysplit
    }

    return(list(
      x_cutoff = if (orientation == "x") {
        selected$cutoff
      } else NULL,
      y_cutoff = if (orientation == "y") {
        selected$cutoff
      } else NULL,
      groups = c(sides$lower, sides$upper)
    ))
  }

  # Four-group case: require a 2 x 2 layout.
  if (xgap$gap <= 0 || ygap$gap <= 0) {
    stop(
      "Groups cannot be separated into ",
      "four non-overlapping quadrants.",
      call. = FALSE
    )
  }

  quadrants <- c(
    intersect(xsplit$lower, ysplit$lower),
    intersect(xsplit$upper, ysplit$lower),
    intersect(xsplit$lower, ysplit$upper),
    intersect(xsplit$upper, ysplit$upper)
  )

  if (length(quadrants) != 4L ||
      length(unique(quadrants)) != 4L) {
    stop(
      "Groups do not form a valid ",
      "2 x 2 layout.",
      call. = FALSE
    )
  }

  list(
    x_cutoff = xgap$cutoff,
    y_cutoff = ygap$cutoff,
    groups = quadrants
  )
}
