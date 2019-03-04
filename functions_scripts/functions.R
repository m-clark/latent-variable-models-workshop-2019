theme_plotly = function(vis, MB=FALSE) {
  if(! inherits(vis, 'plotly'))  stop('vis is not a plotly object.')
  vis <- vis %>%
    plotly::layout(xaxis = list(zeroline=FALSE,
                                showgrid=FALSE),
                   yaxis = list(zeroline=FALSE,
                                showgrid=FALSE),
                   plot_bgcolor='transparent',
                   paper_bgcolor='transparent')
  if(!MB) {
    vis <- vis %>%
      plotly::config(displayModeBar=FALSE)
  }
  
  vis
}


theme_blank = function(vis, MB=FALSE) {
  if(! inherits(vis, 'plotly'))  stop('vis is not a plotly object.')
  
  a <- list(
    title = '',
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  vis <- vis %>%
    plotly::layout(xaxis = a,
                   yaxis = a,
                   plot_bgcolor='transparent',
                   paper_bgcolor='transparent')
  if(!MB) {
    vis <- vis %>%
      plotly::config(displayModeBar=FALSE)
  }
  
  vis
}


theme_trueMinimal = function(font_size = 12,
                             font_family = "",
                             center_axis_labels = FALSE){
  if (center_axis_labels) {
    axis_just <- 0.5
  }
  else {
    axis_just <- 1
  }
  ggplot2::theme(
    text = ggplot2::element_text(
      family = font_family,
      face = "plain",
      colour = "gray46",
      size = font_size,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      lineheight = 0.9,
      margin = ggplot2::margin(),
      debug = FALSE
    ),
    # axis.text.x = ggplot2::element_text(),
    # axis.text.y = ggplot2::element_text(),
    axis.title.x = ggplot2::element_text(hjust = axis_just),
    axis.title.y = ggplot2::element_text(hjust = axis_just),
    title = ggplot2::element_text(colour='gray33', size = font_size*1.25),
    legend.key = ggplot2::element_rect(fill='transparent', colour = NA),
    legend.background = ggplot2::element_rect(fill='transparent', colour = NA),
    panel.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = "transparent", colour = NA)
  )
}

# helper function for visualization
show_digit = function(arr784, col = gray(12:1 / 12), ...) {
  image(matrix(as.matrix(arr784), nrow = 28)[, 28:1], col = col, ...)
}

# load image files
load_image_file = function(filename) {
  ret = list()
  f = file(filename, 'rb')
  readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  n    = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  nrow = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  ncol = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  x = readBin(f, 'integer', n = n * nrow * ncol, size = 1, signed = FALSE)
  close(f)
  data.frame(matrix(x, ncol = nrow * ncol, byrow = TRUE))
}

# load label files
load_label_file = function(filename) {
  f = file(filename, 'rb')
  readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  n = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  y = readBin(f, 'integer', n = n, size = 1, signed = FALSE)
  close(f)
  y
}