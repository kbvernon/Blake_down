
library(glue)
library(here)
library(icon) # github only (ropenscilabs)
library(jsonlite)
library(kableExtra)
library(knitr)
library(RefManageR)
library(tidyverse)

knitr::opts_chunk$set(echo = FALSE, 
                      warning = FALSE, 
                      message = FALSE)

options(knitr.kable.NA = "",
        kableExtra.html.bsTable = TRUE)