#' @name Rnw2RmdPandoc
#'
#' @title Translation from '.Rnw' to '.Rmd' using pandoc and custom
#'     filters
#'
#' @description `Rnw2RmdPandoc()` uses pandoc and custom filters to
#'     convert from LaTeX (Sweave) to markdown.
#'
#' @param input character(1) path to an `.Rnw` (Sweave) file.
#'
#' @param output character(1) path to the output file
#'
#' @param ... additional arguments passed to [pandoc_convert].
#'
#' @param options character vector of options (in the style of
#'     [system], with key/value pairs as separate elements of the
#'     character vector) passed to [pandoc_convert]. `Rnw2RmdPandoc()`
#'     adds filters to translate LaTeX commands introduced by
#'     BiocStyle.
#'
#' @details `Rnw2RmdPandoc()` requires that `pandoc` is
#'     installed. Conversion is far from perfect; the more complicated
#'     the LaTeX, the less perfect the conversion. Current major
#'     limitations include:
#'
#' @details
#' - The generated Rmd file does not include front-matter 'yaml',
#'   e.g., title, author, and output format.
#' - Cross-references are not handled.
#'
#' @details Closely examine the generated markdown file for
#'     `{=latex}`; this indicates LaTeX code that has not be translated.
#'
#' @examples
#' rnw <- system.file(package = "Rnw2Rmd", "example", "Rnw2Rmd-test.Rnw")
#' cat(readLines(rnw), sep = "\n")
#'
#' rmd <- Rnw2RmdPandoc(rnw, tempfile(fileext = ".Rmd"), verbose = TRUE)
#' cat(readLines(rmd), sep = "\n")
#'
#' @importFrom tools file_path_sans_ext
#'
#' @importFrom rmarkdown pandoc_convert
#'
#' @export
Rnw2RmdPandoc <-
    function(input, output = NULL, ..., options = NULL)
{
    stopifnot(
        file.exists(input),
        is.null(output) || dir.exists(dirname(output)),
        is.null(output) || !file.exists(output)
    )

    if (is.null(output)) {
        output <- .file_name_rnw_to_rmd(input)
        stopifnot(!file.exists(output))
    }

    lua_filters <- dir(
        system.file(package = "Rnw2Rmd", "lua"),
        full.names = TRUE
    )
    lua_filter_flags <- unlist(Map(
        function(filter) c("--lua-filter", filter), lua_filters
    ), use.names = FALSE)

    options <- c(options, lua_filter_flags)

    pandoc_convert(
        input,
        output = output,
        from = "latex+raw_tex",
        to = "markdown",
        options = options,
        ...
    )

    output
}
