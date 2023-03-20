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
#' @export
Rnw2Rmd <- function(from, to, validate = TRUE) {
    if (missing(to))
        to <- gsub(tools::file_ext(from), "Rmd", from)

    new_vig <- readLines(from)
    ## fix escaped underscores
    new_vig <- gsub("\\_", "_", fixed = TRUE, new_vig)
    ## remove stray %%
    new_vig <- gsub("%%", "", fixed = TRUE, new_vig)
    ## replace {} with `` in special R tags
    new_vig <- .remove_tex_chunk(new_vig)
    new_vig <- gsub(
        "(\\\\(Rclass|Rfunction|Rcode|Robject)\\{)([^\\}]+)(\\})",
        "`\\3`",
        new_vig
    )
    ## replace funky sections
    new_vig <- gsub("(\\\\section\\*+\\{)([^\\}]+)(\\})", "## \\2", new_vig)
    ## replace complicated code
    new_vig <- gsub(
        "\\\\Rcode\\{(?<=\\{)(.+?)(?=\\})\\}", "`\\1`", new_vig, perl = TRUE
    )

    ## replace Rpkg with Biocpkg or CRANpkg
    if (!validate)
        new_vig <- gsub(
            "(\\\\Rpackage\\{)([^\\}]+)(\\})", "`r Biocpkg(\"\\2\")`", new_vig
        )
    else
        new_vig <- .validate_replace_tag_pkg(new_vig)

    ## replace \R with R
    new_vig <- gsub("\\\\R(['\" ]+)", "R\\1", new_vig)
    ## write to file
    writeLines(new_vig, con = to)

    if (!requireNamespace("mdsr", quietly = TRUE))
        FUN <- .Rnw2Rmd
    else
        FUN <- mdsr::Rnw2Rmd

    # overwrite with mdsr updates
    FUN(to, to)

    new_vig <- readLines(to)

    new_vig <- .add_remove_front_matter(new_vig)
    new_vig <- .remove_metatags(new_vig)
    new_vig <- .clean_up_blanks(new_vig)
    ## write to file
    writeLines(new_vig, con = to)
}

.validate_replace_tag_pkg <- function(vig_text) {
    ind <-  grep("Rpackage", vig_text)
    text <- vig_text[ind]
    pkgs <- gsub("(.*)(\\\\Rpackage\\{)([^\\}]+)(\\})(.*)", "\\3", text)
    upkgs <- structure(unique(pkgs), .Names = unique(pkgs))
    results <- vapply(upkgs, function(pkg) {
        if (.check_bioc_pkg(pkg))
            "Biocpkg"
        else if (.check_cran_pkg(pkg))
            "CRANpkg"
        else
            stop("\\Rpackage{", pkg, "} is not in CRAN or Bioconductor")
    }, character(1L))
    repd_text <- mapply(
        function(tag, reptext) {
            gsub(
                "(\\\\Rpackage\\{)([^\\}]+)(\\})",
                paste0("`r ", tag, "(\"\\2\")`"),
                reptext
            )
        }, tag = results[pkgs], reptext = text,
        SIMPLIFY = TRUE, USE.NAMES = FALSE
    )
    vig_text[ind] <- repd_text
    vig_text
}

.check_bioc_pkg <- function(pkg) {
    repo <-
        BiocManager:::.repositories_bioc(BiocManager::version())["BioCsoft"]
    db <- utils::available.packages(repos = repo)
    pkg %in% rownames(db)
}

.check_cran_pkg <- function(pkg) {
    repo <- "https://cloud.r-project.org/"
    db <- utils::available.packages(repos = repo)
    pkg %in% db
}

# from mdsr::Rnw2Rmd (archived on CRAN as of 2022-12-22)
.Rnw2Rmd <- function(path, new_path = NULL)  {
    if (is.null(new_path))
        new_path <- gsub(".Rnw", ".Rmd", path)
    x <- readLines(path)
    x <- gsub("(<<)(.*)(>>=)", "```{r \\2}", x)
    x <- gsub("^@", "```", x)
    x <- gsub("(\\\\Sexpr\\{)([^\\}]+)(\\})", "`r \\2`", x)
    x <- gsub("(\\\\chapter\\{)([^\\}]+)(\\})", "# \\2", x)
    x <- gsub("(\\\\section\\{)([^\\}]+)(\\})", "## \\2", x)
    x <- gsub("(\\\\subsection\\{)([^\\}]+)(\\})", "### \\2",
              x)
    x <- gsub("(\\\\subsubsection\\{)([^\\}]+)(\\})", "#### \\2",
              x)
    x <- gsub("(\\\\citep\\{)([^\\}]+)(\\})", "[@\\2]", x)
    x <- gsub("(\\\\cite\\{)([^\\}]+)(\\})", "@\\2", x)
    x <- gsub("(\\\\ref\\{)([^\\}]+)(\\})", "\\\\@ref(\\2)",
              x)
    x <- gsub("(\\\\label\\{)([^\\}]+)(\\})", "{#\\2}", x)
    x <- gsub("(\\\\index\\{)([^\\}]+)(\\})(\\{)([^\\}]+)(\\})\\%",
              "\\\\index{\\2}{\\5}", x)
    x <- gsub("\\\\item", "- ", x)
    x <- gsub("(\\\\emph\\{)([^\\}]+)(\\})", "*\\2*", x)
    x <- gsub("(\\\\textit\\{)([^\\}]+)(\\})", "*\\2*", x)
    x <- gsub("(\\\\textbf\\{)([^\\}]+)(\\})", "**\\2**", x)
    x <- gsub("(\\\\href\\{)([^\\}]+)(\\})(\\{)([^\\}]+)(\\})",
              "[\\5](\\2)", x)
    x <- gsub("(\\\\url\\{)([^\\}]+)(\\})", "(\\2)", x)
    x <- gsub("{\\\\tt ([a-zA-Z0-9. _()=]*)} ", "`\\1` ", x,
              perl = TRUE)
    writeLines(x, new_path)
}

.tag.exists <- function(tag, text) {
    pat <- whisker::whisker.render("\\\\{{word}}\\{", data = c(word = tag))
    any(grepl(pat, x = text))
}

.identify_front <- function(text) {

    date <- "`r format(Sys.time(), '%B %d, %Y')`"
    ptext <- paste0(text, collapse = " ")
    if (.tag.exists("title", text))
        title <- gsub("(.*)(\\\\title\\{)([^\\}]+)(\\})(.*)", "\\3", ptext)
    if (.tag.exists("author", text))
        author <- gsub("(.*)(\\\\author\\{)([^\\}]+)(\\})(.*)", "\\3", ptext)
    if (.tag.exists("date", text)) {
        if (.tag.exists("today", text))
            date <- "`r format(Sys.time(), '%B %d, %Y')`"
        else
            date <- gsub("(.*)(\\\\date\\{)(.*[^\\}]+)(\\})(.*)", "\\3", ptext)
    }

    list(title = title, author = author, date = date)
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
    text[-vigind]
}

.remove_tex_chunk <- function(text) {
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

.remove_metatags <- function(text) {
   tags <- c("documentclass", "begin", "end", "maketitle", "title", "author")
   gexpr <- paste0("^\\\\", tags)
   metaind <- unique(unlist(lapply(gexpr, function(x) { grep(x, text) })))
   text[-metaind]
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
