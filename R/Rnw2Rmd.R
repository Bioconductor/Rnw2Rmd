#' First pass at conversion from Rnw to Rmd
#'
#' This function will attempt to replace most of the `Rnw` syntax with the `Rmd`
#' syntax. It is not by all means complete but gets the user as close as
#' possible to having a fully functional RMarkdown vignette.
#'
#' @param from character(1) The file path to the Rnw vignette
#'
#' @param to character(1) The output file path to the generated RMarkdown file.
#'   If missing, the file extension will be replaced from `Rnw` to `Rmd`
#'
#' @param validate logical(1) Whether to use `available.packages` to verify
#'   the repository location of packages that are denoted with `Rpackage`.
#'
#' @return Called for the side effect of creating an RMarkdown document from the
#'   `Rnw` one in the same location (default).
#'
#' @importFrom tools file_ext
#' @export
Rnw2Rmd <- function(from, to, validate = TRUE) {
    if (missing(from) || !file.exists(from))
        stop("'from' input must be a valid file")
    fileext <- file_ext(from)
    if (!identical(tolower(fileext), "rnw"))
        stop("'from' file must have an 'Rnw' extension")
    if (missing(to))
        to <- .file_name_rnw_to_rmd(from)

    from_vig <- readLines(from)
    ## isolate head
    yml_head <- .yaml_front_matter(from_vig)
    ## clean up body
    vig_body <- .remove_meta_head_tags(from_vig)
    vig_body <- .remove_rnw_head(vig_body)
    vig_body <- .remove_BiocStyle_tex(vig_body)

    ## fix escaped underscores
    vig_body <- gsub("\\_", "_", fixed = TRUE, vig_body)
    ## remove stray %%
    vig_body <- gsub("%%", "", fixed = TRUE, vig_body)
    ## replace funky sections
    vig_body <- gsub("(\\\\section\\*+\\{)([^\\}]+)(\\})", "## \\2", vig_body)
    # Use mdsr mods
    vig_body <- .Rnw2RmdMods(vig_body)
    temp_body <- tempfile(fileext = ".Rnw")
    writeLines(vig_body, temp_body)

    out_body <- Rnw2RmdPandoc(input = temp_body)

    new_body <- readLines(out_body)
    new_body <- .clean_up_blanks(new_body)

    new_vig <- c(yml_head, "", new_body)
    ## write to file
    writeLines(new_vig, con = to)
}

# from mdsr::Rnw2Rmd (archived on CRAN as of 2022-12-22)
# modified for Rnw2RmdPandoc
.Rnw2RmdMods <- function(x)  {
    x <- gsub("(\\\\chapter\\{)([^\\}]+)(\\})", "# \\2", x)
    x <- gsub("(\\\\citep\\{)([^\\}]+)(\\})", "[@\\2]", x)
    x <- gsub("(\\\\cite\\{)([^\\}]+)(\\})", "@\\2", x)
    x <- gsub("(\\\\ref\\{)([^\\}]+)(\\})", "\\\\@ref(\\2)", x)
    x <- gsub("(\\\\label\\{)([^\\}]+)(\\})", "{#\\2}", x)
    x <- gsub(
        "(\\\\index\\{)([^\\}]+)(\\})(\\{)([^\\}]+)(\\})\\%",
        "\\\\index{\\2}{\\5}",
        x
    )
    x <- gsub("\\\\item", "- ", x)
    x <- gsub("(\\\\href\\{)([^\\}]+)(\\})(\\{)([^\\}]+)(\\})", "[\\5](\\2)", x)
    x <- gsub("(\\\\url\\{)([^\\}]+)(\\})", "(\\2)", x)
    x <- gsub("{\\\\tt ([a-zA-Z0-9. _()=]*)} ", "`\\1` ", x, perl = TRUE)
    x
}

.tag.exists <- function(tag, text, withOpen = TRUE, withBracket = FALSE) {
    append <- ""
    if (withBracket)
        append <- "\\[.*\\]"
    if (withOpen)
        append <- paste(append, "\\{", sep = "")
    tag_template <- paste0("\\\\{{word}}", append)
    pat <- whisker::whisker.render(tag_template, data = c(word = tag))
    any(grepl(pat, x = text))
}

.identify_front <- function(text) {
    date <- "`r format(Sys.time(), '%B %d, %Y')`"
    ptext <- paste0(text, collapse = " ")
    title <- author <- NULL
    if (.tag.exists("title", text))
        title <- gsub(
            "(.*)(?<=\\\\title\\{)([^\\}]+)(.*)", "\\2", ptext, perl = TRUE
        )
    else if (.tag.exists("bioctitle", text, withBracket = TRUE))
        title <- gsub(
            "(.*)(?<=\\\\bioctitle\\[)(.*\\]\\{)([^\\}]+)(.*)", "\\3",
            ptext, perl = TRUE
        )
    if (.tag.exists("author", text))
        author <- gsub(
            "(.*)(?<=\\\\author\\{)([^\\}]+)(.*)", "\\2", ptext, perl = TRUE
        )
    if (.tag.exists("date", text)) {
        if (.tag.exists("today", text, withOpen = FALSE))
            date <- "`r format(Sys.time(), '%B %d, %Y')`"
        else
            date <- gsub(
                "(.*)(?<=\\\\date\\{)([^\\}]+)(.*)", "\\2", ptext, perl = TRUE
            )
    }

    res <- list(title = title, author = author, date = date)
    lapply(res, function(x) gsub("\\\\", "", trimws(x), fixed = TRUE))
}

.yaml_front_matter <- function(text) {
    template <- c(
        "---",
        "title: \"{{title}}\"",
        "author: \"{{author}}\"",
        "date: \"{{date}}\"",
        "output:",
        "  BiocStyle::html_document",
        "vignette: >",
        "  %\\VignetteIndexEntry{ {{title}} }",
        "  %\\VignetteEngine{knitr::rmarkdown}",
        "  %\\VignetteEncoding{UTF-8}",
        "---"
    )

    datalist <- .identify_front(text)
    utils::capture.output(cat(
        whisker::whisker.render(template, data = datalist)
    ))
}

.remove_rnw_head <- function(text) {
    vigind <- grep("^%\\\\Vignette.*", text)
    if (length(vigind))
        text[-vigind]
    else
        text
}

.remove_BiocStyle_tex <- function(text) {
    latex_ind <- grep("BiocStyle::latex\\(.*", text)
    if (length(latex_ind))
        text <- text[-seq(latex_ind - 1, latex_ind + 1)]
    text
}

.add_remove_front_matter <- function(text) {
    header <- .yaml_front_matter(text)
    body <- .remove_rnw_head(text)
    c(header, body)
}

.remove_meta_head_tags <- function(text) {
    tags <- c(
       "documentclass", "maketitle", "bioctitle",
       "title", "author", "SweaveOpts"
    )
    uses_curly <- c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)
    rm_ind <- Map(
        function(x, y) {
            .start_end_tag_idx(x, text, y)
        },
        x = tags,
        y = uses_curly
    )
    rm_ind <- sort(unlist(rm_ind, use.names = FALSE))
    if (length(rm_ind))
        text[-rm_ind]
    else
        text
}

.start_end_tag_idx <- function(tag, text, curly = TRUE) {
    regtag <- paste0("\\\\", tag)
    start <- grep(regtag, text)
    if (length(start) && curly) {
        end <- start
        while (!grepl("}", text[end], fixed = TRUE)) {
            end <- end + 1
        }
        start:end
    } else {
        start
    }
}

.clean_up_blanks <- function(text) {
    indpos <- which(!nzchar(text))
    diffs <- diff(indpos)
    groups <- split(indpos, cumsum(c(1, diffs != 1)))
    selections <- unlist(lapply(groups, utils::head, 2))
    rinds <- setdiff(indpos, selections)
    if (length(rinds))
        text[-rinds]
    else
        text
}
