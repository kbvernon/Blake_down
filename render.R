library(extera)
library(httr2)
library(pagedown)
library(quarto)

collections <- c("article", "manuscript", "presentation", "report", "review")

data_dir <- "generated-content"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}

# download zotero data ---------------------------------------------------
download_zotero_collection <- function(x) {
  collection <- paste0("ZOTERO_", toupper(x))

  zotero_request <- httr2::req_url_path_append(
    httr2::request("https://api.zotero.org"),
    "users",
    Sys.getenv("ZOTERO_ID"),
    "collections",
    Sys.getenv(collection),
    "items"
  )

  zotero_request <- httr2::req_url_query(
    zotero_request,
    key = Sys.getenv("ZOTERO_KEY"),
    itemType = "-note",
    format = "json",
    limit = 99L
  )

  zotero_response <- httr2::req_perform(zotero_request)

  bib <- httr2::resp_body_json(zotero_response, simplifyVector = TRUE)

  bib <- bib[["data"]]

  names(bib) <- tolower(names(bib))

  bib[["author"]] <- sapply(
    bib[["creators"]],
    \(.x) paste(.x[["firstName"]], .x[["lastName"]], collapse = ", ")
  )

  bib[["year"]] <- sub(".*(\\d{4}).*", "\\1", bib[["date"]])

  if ("pages" %in% names(bib)) {
    bib[["pages"]] <- gsub("--", "-", bib[["pages"]])
  }

  # parse sets of "key: value" pairs in the note column
  # turn them into a single named list
  bib[["extra"]] <- lapply(
    strsplit(bib[["extra"]], "\n"),
    function(x) {
      if (all(is.na(x))) {
        return(NA)
      }

      x <- gsub("\\\\", "", x)

      setNames(
        as.list(sub(".*: ", "", x)),
        sub(":.*", "", x)
      )
    }
  )

  bib
}

bibs <- setNames(
  lapply(collections, download_zotero_collection),
  collections
)

# generate qmd-content files ---------------------------------------------
tera <- new_engine("extera-templates/*.html")
tera$autoescape_off()

for (collection in collections) {
  template <- paste0(collection, ".html")

  tera$render(
    template,
    outfile = file.path(data_dir, template),
    id = collection,
    bibliography = bibs[[collection]]
  )
}

# funding ----------------------------------------------------------------
funding <- Sys.getenv("GOOGLE_PAGE_ID") |>
  googlesheets4::read_sheet(sheet = "funding") |>
  subset(result == "Awarded") |>
  transform(year = as.integer(year))

tera$render(
  "funding.html",
  outfile = file.path(data_dir, "funding.html"),
  id = collection,
  bibliography = funding
)

# render cv --------------------------------------------------------------
temp_html <- "_cv.html"

quarto_render(
  "_cv.qmd",
  output_file = temp_html,
  quiet = TRUE
)

chrome_print(
  temp_html,
  output = "cv-vernon.pdf",
  options = list(preferCSSPageSize = TRUE)
)

file.remove(temp_html)

# render website ---------------------------------------------------------
quarto_render()

# cleanup ----------------------------------------------------------------
unlink(data_dir, recursive = TRUE)
