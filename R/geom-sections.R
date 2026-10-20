
#' @keywords internal
#' @importFrom ggplot2 Geom draw_key_blank
GeomSections <- ggplot2::ggproto(
  "GeomSections",
  Geom,

  required_aes = character(0),

  default_aes = ggplot2::aes(),

  draw_key = draw_key_blank,

  draw_panel = function(
    data, panel_params, coord,
    x_cutoff = NULL,
    y_cutoff = NULL,
    fill,
    alpha = 0.2,
    show_lines = FALSE,
    line_colour = "grey40",
    line_width = 0.5,
    line_type = "dashed"
  ) {
    if (!inherits(coord, "CoordCartesian")) {
      stop(
        "annotation_regions() requires ",
        "Cartesian coordinates.",
        call. = FALSE
      )
    }

    # Convert cutoffs to panel-relative coordinates.
    # NA on the unused axis is intentional.
    positions <- coord$transform(
      data.frame(
        x = if (is.null(x_cutoff)) NA_real_
            else x_cutoff,
        y = if (is.null(y_cutoff)) NA_real_
            else y_cutoff
      ),
      panel_params
    )

    clamp <- function(x) {
      max(0, min(1, x))
    }

    xs <- if (is.null(x_cutoff)) {
      c(0, 1)
    } else {
      c(0, clamp(positions$x[1]), 1)
    }

    ys <- if (is.null(y_cutoff)) {
      c(0, 1)
    } else {
      c(0, clamp(positions$y[1]), 1)
    }

    # Order: left-to-right, then bottom-to-top.
    cells <- expand.grid(
      ix = seq_len(length(xs) - 1L),
      iy = seq_len(length(ys) - 1L)
    )

    rects <- lapply(seq_len(nrow(cells)), function(i) {
      ix <- cells$ix[i]
      iy <- cells$iy[i]

      xmin <- xs[ix]
      xmax <- xs[ix + 1L]
      ymin <- ys[iy]
      ymax <- ys[iy + 1L]

      grid::rectGrob(
        x = (xmin + xmax) / 2,
        y = (ymin + ymax) / 2,
        width = xmax - xmin,
        height = ymax - ymin,
        default.units = "npc",
        gp = grid::gpar(
          fill = grDevices::adjustcolor(
            fill[i], alpha.f = alpha
          ),
          col = NA
        )
      )
    })

    if (show_lines) {
      line_gp <- grid::gpar(
        col = line_colour,
        lwd = line_width * 72.27 / 25.4,
        lty = line_type
      )

      if (!is.null(x_cutoff) &&
          positions$x[1] >= 0 &&
          positions$x[1] <= 1) {
        rects <- append(rects, list(
          grid::segmentsGrob(
            x0 = positions$x[1],
            x1 = positions$x[1],
            y0 = 0,
            y1 = 1,
            default.units = "npc",
            gp = line_gp
          )
        ))
      }

      if (!is.null(y_cutoff) &&
          positions$y[1] >= 0 &&
          positions$y[1] <= 1) {
        rects <- append(rects, list(
          grid::segmentsGrob(
            x0 = 0,
            x1 = 1,
            y0 = positions$y[1],
            y1 = positions$y[1],
            default.units = "npc",
            gp = line_gp
          )
        ))
      }
    }

    do.call(grid::grobTree, rects)
  }
)
