# read zotero collection -------------------------------------------------
get_collection <- function(user, key, collection) {
  tbl <- RefManageR::ReadZotero(
    user = user,
    .params = list(
      key = key,
      collection = collection
    )
  )

  tbl <- as.data.frame(tbl)

  rownames(tbl) <- NULL

  # clean up special characters
  tbl[["author"]] <- gsub(" and ", ", ", tbl[["author"]])
  tbl[["author"]] <- gsub("[{}]", "", tbl[["author"]])
  tbl[["title"]] <- gsub("[{}]", "", tbl[["title"]])

  if ("shorttitle" %in% names(tbl)) {
    tbl[["shorttitle"]] <- gsub("[{}]", "", tbl[["shorttitle"]])
  }


  if ("pages" %in% names(tbl)) {
    tbl[["pages"]] <- gsub("--", "-", tbl[["pages"]])
  }

  # parse sets of "key: value" pairs in the note column
  # turn them into a single named vector
  if ("note" %in% names(tbl)) {
    tbl[["note"]] <- lapply(
      strsplit(tbl[["note"]], "\n"),
      function(x) {
        if (all(is.na(x))) {
          return(NA)
        }

        x <- gsub("\\\\", "", x)

        setNames(sub(".*: ", "", x), sub(":.*", "", x))
      }
    )
  }

  tbl
}

# generate a bibliographic list ------------------------------------------
bib_list <- function(bib, collection) {
  # get block function
  make_block <- switch(collection,
    "article" = article_block,
    "fieldwork" = fieldwork_block,
    "funding" = funding_block,
    "manuscript" = manuscript_block,
    "presentation" = presentation_block,
    "report" = report_block,
    "review" = review_block,
    "synergy" = synergy_block
  )
  # build entries for each year
  year_items <- vector(mode = "list", length = nrow(bib))

  for (i in 1:nrow(bib)) {
    year_items[[i]] <- make_block(bib[i, ])
  }

  # build list item for each year and insert all entries for that year
  unique_years <- sort(unique(bib[["year"]]), decreasing = TRUE)

  bib_years <- setNames(
    vector(mode = "list", length = length(unique_years)),
    unique_years
  )

  for (year in unique_years) {
    i <- which(bib[["year"]] == year)

    bib_years[[year]] <- htmltools::tags$li(
      class = "bib-year",
      id = paste0("bib-year-", year),
      value = year,
      year_items[i]
    )
  }

  # combine all list items in ordered list
  htmltools::tags$ol(
    class = "bib-list",
    id = paste0("bib-", collection),
    bib_years
  )
}

# custom reference blocks ------------------------------------------------
article_block <- function(bib) {
  title <- bib[["title"]]
  authors <- bib[["author"]]
  journal <- bib[["journal"]]

  volume <- sub(" NA", "", paste0(" ", bib[["volume"]]))
  number <- sub(" \\(NA\\):", "", paste0(" (", bib[["number"]], "):"))
  pages <- sub(" NA.", "", paste0(" ", bib[["pages"]]))
  year <- paste0(" (", bib[["year"]], ")")
  doi <- paste0("DOI: ", bib[["doi"]])

  github <- bib[["note"]][[1]]["github"]
  preprint <- bib[["note"]][[1]]["preprint"]
  article <- bib[["url"]]

  block <- htmltools::div(
    class = "bib-block",
    htmltools::div(
      class = "bib-ref",
      htmltools::p(class = "bib-title", title),
      htmltools::p(class = "bib-authors", authors),
      htmltools::p(
        class = "bib-details",
        htmltools::span(class = "bib-journal", journal),
        paste0(volume, number, pages, year),
        htmltools::br(),
        doi
      )
    )
  )

  if (!all(is.na(c(github, preprint, article)))) {
    block <- htmltools::tagAppendChild(
      block,
      child = htmltools::div(
        class = "bib-buttons",
        btn(github, "github"),
        btn(preprint, "preprint"),
        btn(article, "article")
      )
    )
  }

  block
}

funding_block <- function(bib) {
  title <- bib[["title"]]
  authors <- bib[["pi"]]
  fot <- bib[["FOT"]]
  organization <- bib[["organization"]]

  htmltools::div(
    class = "bib-block",
    htmltools::div(
      class = "bib-ref",
      htmltools::p(class = "bib-title", title),
      htmltools::p(class = "bib-authors", authors),
      htmltools::p(
        class = "bib-details",
        paste0(fot, ", ", organization, ".")
      )
    )
  )
}

manuscript_block <- function(bib) {
  title <- bib[["title"]]
  authors <- bib[["author"]]
  journal <- bib[["journal"]]

  github <- bib[["note"]]["github"]
  preprint <- bib[["note"]]["preprint"]
  status <- sub("\\(NA\\)", "", paste0("(", bib[["note"]]["status"], ")"))

  block <- htmltools::div(
    class = "bib-block",
    htmltools::div(
      class = "bib-ref",
      htmltools::p(class = "bib-title", title),
      htmltools::p(class = "bib-authors", authors),
      htmltools::p(
        class = "bib-details",
        htmltools::span(class = "bib-journal", journal),
        status
      )
    )
  )

  if (!all(is.na(c(github, preprint)))) {
    block <- htmltools::tagAppendChild(
      block,
      child = htmltools::div(
        class = "bib-buttons",
        btn(github, "github"),
        btn(preprint, "preprint")
      )
    )
  }

  block
}

presentation_block <- function(bib) {
  title <- bib[["title"]]
  authors <- bib[["author"]]
  type <- bib[["type"]]
  shorttitle <- bib[["shorttitle"]]
  address <- bib[["address"]]

  github <- bib[["note"]][[1]]["github"]
  slides <- bib[["url"]]

  block <- htmltools::div(
    class = "bib-block",
    htmltools::div(
      class = "bib-ref",
      htmltools::p(class = "bib-title", title),
      htmltools::p(class = "bib-authors", authors),
      htmltools::p(
        class = "bib-details",
        paste0(type, " at the ", shorttitle, ", ", address, ".")
      )
    )
  )

  if (!all(is.na(c(github, slides)))) {
    block <- htmltools::tagAppendChild(
      block,
      child = htmltools::div(
        class = "bib-buttons",
        btn(github, "github"),
        btn(slides, "slides")
      )
    )
  }

  block
}

report_block <- function(bib) {
  title <- bib[["title"]]
  authors <- bib[["author"]]
  institution <- bib[["institution"]]
  note <- bib[["note"]]

  htmltools::div(
    class = "bib-block",
    htmltools::div(
      class = "bib-ref",
      htmltools::p(class = "bib-title", title),
      htmltools::p(class = "bib-authors", authors),
      htmltools::p(
        class = "bib-details",
        paste0("Report submitted to the ", institution, ". ", note)
      )
    )
  )
}

review_block <- function(bib) {
  article_block(bib)
}

synergy_block <- function(bib) {
  title <- bib[["title"]]

  if (!is.na(bib[["url"]])) {
    title <- htmltools::a(href = bib[["url"]], title)
  }

  authors <- bib[["author"]]
  description <- bib[["description"]]
  status <- bib[["status"]]

  position <- htmltools::span(
    class = "bib-position",
    bib[["position"]]
  )

  htmltools::div(
    class = "bib-block",
    htmltools::div(
      class = "bib-ref",
      htmltools::p(class = "bib-title", position, title),
      htmltools::p(class = "bib-authors", authors),
      htmltools::p(
        class = "bib-details",
        paste0(description, " Status: ", status)
      )
    )
  )
}

# make bootstrap buttons -------------------------------------------------
btn <- function(href, icon) {
  if (is.null(href) || is.na(href)) {
    return()
  }

  icon_name <- switch(icon,
    "article" = "journal-text",
    "github" = "github",
    "preprint" = "filetype-pdf",
    "slides" = "file-easel"
  )

  span_text <- icon
  substr(span_text, 1, 1) <- toupper(substr(span_text, 1, 1))

  htmltools::tags$a(
    class = "btn btn-outline-dark btn-sm",
    href = href,
    rel = "noopener noreferrer",
    aria_label = icon,
    htmltools::tags$i(class = paste0("bi bi-", icon_name)),
    htmltools::tags$span(span_text)
  )
}
