course_colors <- c("#FF9400", "#0072B2", "#43b284", "#dd5129", "#030A10", "#8C8984", "gray80")

theme_tokupon_course <- function() {
  course_theme <- ggplot2::theme()
  course_theme <- 
    list(course_theme, scale_color_manual(values = course_colors))
  course_theme <-
    list(course_theme, scale_fill_manual(values = course_colors))
  course_theme
  }

tokupon_course_pal <- function() {
  scales::manual_pal(course_colors)
}

scale_colour_tokupon <- function(...) ggplot2::discrete_scale("colour", "tokuopon", tokupon_course_pal(), ...)

scale_fill_tokupon <- function(...) ggplot2::discrete_scale("fill", "tokuopon", tokupon_course_pal(), ...)

# scales::show_col(colours = course_colors)
