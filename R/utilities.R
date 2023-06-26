.file_name_rnw_to_rmd <- function(rnw_file) {
    filename <- file_path_sans_ext(rnw_file)
    paste0(filename, ".Rmd")
}
