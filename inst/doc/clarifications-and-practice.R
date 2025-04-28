## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(refitgaps)

## ----out.width = '99%', echo=FALSE--------------------------------------------
knitr::include_graphics("images/Vi.png")

## -----------------------------------------------------------------------------
sum(MOZ[, 1] != "-")  # numărul de clase prezente în orar

## -----------------------------------------------------------------------------
HG <- have_gaps(MOZ)  # subsetul liniilor pe care există ferestre
stringr::str_extract(HG$ore, "\\*.+\\*") %>%
    paste0(collapse = "") %>% stringr::str_count("-")

## ----eval = FALSE-------------------------------------------------------------
#  prnTime <- function(NL = " ")  # afișează timpul curent
#      cat(strftime(Sys.time(), format = "%H:%M:%S"), NL)
#  prnTime("\n")
#  repeat {
#      ORR <- recast(MOZ, TPL)
#      cat(ORR[[2]], " gaps ")
#      prnTime('\n')
#      if(ORR[[2]] < 10) break
#  }

