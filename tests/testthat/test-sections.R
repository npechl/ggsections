df <- data.frame(
  x = c(1, 2, 3, 10, 11, 12),
  y = c(1, 2, 3, 1, 2, 3),
  g = rep(c("a", "b"), each = 3),
  h = rep(c("p", "q", "r"), 2)
)

# Panel positions and fills as drawn by layer i
drawn <- function(p, i = 1L) {
  b <- ggplot2::ggplot_build(p)
  layer <- b$plot$layers[[i]]
  data <- b$data[[i]]
  coord <- b$layout$coord
  pp <- b$layout$panel_params[[1]]
  if (inherits(layer$geom, "GeomAnnotationsections")) {
    x <- data$xintercept[1]
    y <- data$yintercept[1]
    section_positions(
      if (is.na(x)) NULL else x,
      if (is.na(y)) NULL else y,
      layer$aes_params$fill %||% layer$geom_params$fill,
      coord, pp
    )
  } else {
    s <- compute_sections(
      data.frame(x = data$x, y = data$y, group = data$fill)
    )
    section_positions(s$x_cutoff, s$y_cutoff, s$groups, coord, pp)
  }
}

test_that("annotate cutoffs respect scale transformations", {
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    annotate_sections(x_cutoff = 5) +
    ggplot2::scale_x_log10()
  pp <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  expected <- (log10(5) - pp$x.range[1]) / diff(pp$x.range)
  expect_equal(drawn(p)$x, expected)

  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    annotate_sections(x_cutoff = 3) +
    ggplot2::scale_x_reverse()
  expect_gt(drawn(p, 2L)$x, 0.5)
})

test_that("coord_flip swaps axes and quadrant fills", {
  q <- data.frame(
    x = c(1, 9, 1, 9), y = c(1, 1, 9, 9),
    g = c("red", "blue", "green", "black")
  )
  p <- ggplot2::ggplot(q, ggplot2::aes(x, y, fill = g)) +
    geom_sections() +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_flip()
  expect_no_error(ggplot2::ggplotGrob(p))
  # Panel bottom-right is data low-x, high-y
  expect_equal(drawn(p)$fill, c("red", "green", "blue", "black"))

  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    annotate_sections(x_cutoff = 5) +
    ggplot2::coord_flip()
  pos <- drawn(p)
  expect_null(pos$x)
  expect_true(pos$y > 0 && pos$y < 1)
})

test_that("sections follow fill, not other discrete aesthetics", {
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = g, colour = h)) +
    geom_sections() +
    ggplot2::geom_point()
  expect_no_error(ggplot2::ggplotGrob(p))
})
