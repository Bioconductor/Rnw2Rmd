
# Introduction

``` r
library(Rnw2Rmd)
```

The `Rnw2Rmd` package automates parts of the process of converting
Sweave vignettes to R Markdown (or Quarto). It is employed by the
conversion template for Bioconductor packages at
<https://github.com/waldronlab/sweave2rmd_conversiontemplate>.

Output Rmd documents will still have to be tested and corrected
manually, but this should be a lot less work than starting from scratch.
Please report issues at
<https://github.com/Bioconductor/sweave2rmd/issues>.

# Usage

We demonstrate usage on demo `Rnw2Rmd-test.Rnw` vignette included in the
`inst/example` directory of this package:

``` r
fpath <- system.file("example/Rnw2Rmd-test.Rnw", package="Rnw2Rmd")
```

Perform the conversion:

``` r
dir.create(tempdir <- tempfile())
test_rmd <- file.path(tempdir, "Rnw2Rmd-test.Rmd")
Rnw2Rmd(from = fpath, to = test_rmd, validate = TRUE)
```

Note, some validation errors are not actually much of a problem and you
can use `validate=FALSE` to skip validation and still get a useful draft
conversion.

# The product

Compare the source document:

    #>  [1] \\section{First level}                                                       
    #>  [2]                                                                              
    #>  [3] \\subsection{Second level}                                                   
    #>  [4]                                                                              
    #>  [5] \\subsubsection{Thrid level}                                                 
    #>  [6]                                                                              
    #>  [7] Nothing to see here, folks!                                                  
    #>  [8]                                                                              
    #>  [9] The \\R{} programming language                                               
    #> [10]                                                                              
    #> [11] \\R\\ is a programming language.                                             
    #> [12]                                                                              
    #> [13] The name of one programming language is simply \\R.                          
    #> [14]                                                                              
    #> [15] \\Biocpkg{BiocStyle} is a \\Bioconductor{} package.                          
    #> [16]                                                                              
    #> [17] The \\CRANpkg{knitr} is used to create \\textbf{markdown} vignettes.         
    #> [18]                                                                              
    #> [19] Sometimes packages, like \\Githubpkg{AnVILAz}, are only found on Github.     
    #> [20]                                                                              
    #> [21] The \\R{} package \\Rpackage{foo} is not found in any common repository      
    #> [22]                                                                              
    #> [23] \\software{samtools} is pretty \\textit{important} in Bioinformatics...      
    #> [24]                                                                              
    #> [25] \\Robject{mtcars} is a \\Rcode{data.frame}.                                  
    #> [26]                                                                              
    #> [27] \\Rfunction{data.frame} is a function used to create a \\Rcode{data.frame}.  
    #> [28]                                                                              
    #> [29] \\Rfunction{data.frame()} is a function used to create a \\Rcode{data.frame}.
    #> [30]                                                                              
    #> [31] Sometimes inline \\R{} code \\Rcode{x <-                                     
    #> [32] 1 + 1} can span two lines.                                                   
    #> [33]                                                                              
    #> [34] Here's some \\emph{italic} text                                              
    #> [35]                                                                              
    #> [36] Here's an Sexpr that should be evaluated \\Sexpr{1 + 1}                      
    #> [37]                                                                              
    #> [38] \\begin{itemize}                                                             
    #> [39] \\item One item                                                              
    #> [40] \\item Another item                                                          
    #> [41] \\end{itemize}                                                               
    #> [42]                                                                              
    #> [43] \\begin{description}                                                         
    #> [44] \\item[foo] bar                                                              
    #> [45] \\end{description}                                                           
    #> [46]                                                                              
    #> [47] <<>>=                                                                        
    #> [48] 1 + 1                                                                        
    #> [49] @                                                                            
    #> [50]                                                                              
    #> [51] <<named, echo = FALSE>>=                                                     
    #> [52] 1 + 1                                                                        
    #> [53] @

and the generated document:

    #>  [1] ---                                                               
    #>  [2] title: ""                                                         
    #>  [3] author: ""                                                        
    #>  [4] date: "`r format(Sys.time(), '%B %d, %Y')`"                       
    #>  [5] output:                                                           
    #>  [6]   BiocStyle::html_document                                        
    #>  [7] vignette: >                                                       
    #>  [8]   %\\VignetteIndexEntry{  }                                       
    #>  [9]   %\\VignetteEngine{knitr::rmarkdown}                             
    #> [10]   %\\VignetteEncoding{UTF-8}                                      
    #> [11] ---                                                               
    #> [12]                                                                   
    #> [13] # First level                                                     
    #> [14]                                                                   
    #> [15] ## Second level                                                   
    #> [16]                                                                   
    #> [17] ### Thrid level                                                   
    #> [18]                                                                   
    #> [19] Nothing to see here, folks!                                       
    #> [20]                                                                   
    #> [21] The *R* programming language                                      
    #> [22]                                                                   
    #> [23] *R* is a programming language.                                    
    #> [24]                                                                   
    #> [25] The name of one programming language is simply *R*.               
    #> [26]                                                                   
    #> [27] `r Biocpkg(BiocStyle)` is a *Bioconductor* package.               
    #> [28]                                                                   
    #> [29] The `r CRANpkg(knitr)` is used to create **markdown** vignettes.  
    #> [30]                                                                   
    #> [31] Sometimes packages, like `r Githubpkg(AnVILAz)`, are only found on
    #> [32] Github.                                                           
    #> [33]                                                                   
    #> [34] The *R* package `foo` is not found in any common repository       
    #> [35]                                                                   
    #> [36] `samtools` is pretty *important* in Bioinformatics\\...           
    #> [37]                                                                   
    #> [38] `mtcars` is a `data.frame`.                                       
    #> [39]                                                                   
    #> [40] `data.frame` is a function used to create a `data.frame`.         
    #> [41]                                                                   
    #> [42] `data.frame()` is a function used to create a `data.frame`.       
    #> [43]                                                                   
    #> [44] Sometimes inline *R* code `x <-                                   
    #> [45] 1 + 1` can span two lines.                                        
    #> [46]                                                                   
    #> [47] Here's some *italic* text                                         
    #> [48]                                                                   
    #> [49] Here's an Sexpr that should be evaluated `r 1 + 1`                
    #> [50]                                                                   
    #> [51] ```{=latex}                                                       
    #> [52] \\begin{itemize}                                                  
    #> [53] -  One item                                                       
    #> [54] -  Another item                                                   
    #> [55] \\end{itemize}                                                    
    #> [56] ```                                                               
    #> [57] ```{=latex}                                                       
    #> [58] \\begin{description}                                              
    #> [59] - [foo] bar                                                       
    #> [60] \\end{description}                                                
    #> [61] ```                                                               
    #> [62] ```{r}                                                            
    #> [63] 1 + 1                                                             
    #> [64] ```                                                               
    #> [65]                                                                   
    #> [66] ```{r named, echo = FALSE}                                        
    #> [67] 1 + 1                                                             
    #> [68] ```
