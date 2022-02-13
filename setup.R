
### LIBRARIES ###
library(bibtex)
library(dplyr)
library(glue)
library(googlesheets4)
library(here)
library(icons)
library(kableExtra)
library(knitr)
library(RefManageR)
library(tidyr)

### SETTINGS ###
knitr::opts_chunk$set(
  echo = FALSE, 
  warning = FALSE, 
  message = FALSE,
  results = "asis"
)

options(knitr.kable.NA = "")

### HELPER FUNCTIONS ###
fa <- icons::fontawesome # version 5
ai <- icons::academicons

source("cv_lists.R")

### CREDENTIALS ###
load(here("_credentials.RData"))

### BIBLIOGRAPHY ###
bib <- cv_get_bibliography(zotero, collection)
