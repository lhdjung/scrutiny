# Installs scrutiny 0.6.1 from CRAN into `lib`, for the scripts that compare it
# with the working tree. Once a newer version is on CRAN, 0.6.1 is in the
# archive, so both locations are tried. The one that doesn't have it fails
# with a 404, which is expected and therefore kept quiet.
install_scrutiny_0_6_1 <- function(lib) {
  for (dir in c("", "Archive/scrutiny/")) {
    url <- paste0(
      "https://cran.r-project.org/src/contrib/",
      dir,
      "scrutiny_0.6.1.tar.gz"
    )
    try(
      suppressWarnings(install.packages(
        url,
        lib = lib,
        repos = NULL,
        type = "source",
        quiet = TRUE
      )),
      silent = TRUE
    )
    if (dir.exists(file.path(lib, "scrutiny"))) {
      return(invisible(lib))
    }
  }
  cli::cli_abort("Could not install scrutiny 0.6.1.")
}
