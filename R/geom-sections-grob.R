
# Draw coloured rectangular sections
draw_section_grob <- function(
  x_cutoff = NULL,
  y_cutoff = NULL,
  fill,
  alpha = 0.2,
  show_lines = FALSE,
  line_colour = "grey40",
  line_width = 0.5,
  line_type = "dashed"
) {
  xs <- if (is.null(x_cutoff)) {
    c(0, 1)
  } else {
    c(0, x_cutoff, 1)
  }

  ys <- if (is.null(y_cutoff)) {
    c(0, 1)
  } else {
    c(0, y_cutoff, 1)
  }

  # expand.grid() gives the desired ordering:
  # bottom-left, bottom-right, top-left, top-right
  cells <- expand.grid(
    ix = seq_len(length(xs) - 1L),
    iy = seq_len(length(ys) - 1L)
  )

  grobs <- vector("list", nrow(cells))

  for (i in seq_len(nrow(cells))) {
    ix <- cells$ix[i]
    iy <- cells$iy[i]

    xmin <- xs[ix]
    xmax <- xs[ix + 1L]
    ymin <- ys[iy]
    ymax <- ys[iy + 1L]

    grobs[[i]] <- grid::rectGrob(
      x = (xmin + xmax) / 2,
      y = (ymin + ymax) / 2,
      width = xmax - xmin,
      height = ymax - ymin,
      default.units = "npc",
      gp = grid::gpar(
        fill = grDevices::adjustcolor(
          fill[i],
          alpha.f = alpha
        ),
        col = NA
      )
    )
  }

  # Optional dividing lines
  if (show_lines) {
    gp <- grid::gpar(
      col = line_colour,
      lwd = line_width * 72.27 / 25.4,
      lty = line_type
    )

    if (!is.null(x_cutoff) &&
        x_cutoff > 0 &&
        x_cutoff < 1) {
      grobs <- append(grobs, list(
        grid::segmentsGrob(
          x0 = x_cutoff,
          x1 = x_cutoff,
          y0 = 0,
          y1 = 1,
          default.units = "npc",
          gp = gp
        )
      ))
    }

    if (!is.null(y_cutoff) &&
        y_cutoff > 0 &&
        y_cutoff < 1) {
      grobs <- append(grobs, list(
        grid::segmentsGrob(
          x0 = 0,
          x1 = 1,
          y0 = y_cutoff,
          y1 = y_cutoff,
          default.units = "npc",
          gp = gp
        )
      ))
    }
  }

  do.call(grid::grobTree, grobs)
}
