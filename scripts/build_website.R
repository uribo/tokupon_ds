on_ci <- isTRUE(as.logical(Sys.getenv("CI")))

# Build website --------------
build_website <- function() {
  cli::cli_h2("Building website")
  site_output <- "_site"
  unlink(site_output, recursive = TRUE)
  cli::cli_alert_info("Running quarto to build website")
  quarto::quarto_render(as_job = FALSE)
}

build_website()

if (!on_ci) servr::httd("_site")
