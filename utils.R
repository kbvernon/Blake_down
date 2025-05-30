# read google collection -------------------------------------------------
get_google_collection <- function(x) {
  bib <- Sys.getenv("GOOGLE_PAGE_ID") |>
    googlesheets4::read_sheet(sheet = x) |>
    subset(result == "Awarded")

  attr(bib, "collection") <- x
  bib
}


# read zotero collection -------------------------------------------------
get_zotero_collection <- function(x) {
  collection <- switch(x,
    "article" = Sys.getenv("ZOTERO_ARTICLE"),
    "manuscript" = Sys.getenv("ZOTERO_MANUSCRIPT"),
    "presentation" = Sys.getenv("ZOTERO_PRESENTATION"),
    "report" = Sys.getenv("ZOTERO_REPORT"),
    "review" = Sys.getenv("ZOTERO_REVIEW")
  )

  zotero_request <- httr2::req_url_path_append(
    httr2::request("https://api.zotero.org"),
    "users",
    Sys.getenv("ZOTERO_ID"),
    "collections",
    collection,
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

  bib <- httr2::resp_body_json(
    zotero_response,
    simplifyVector = TRUE
  )

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
  # turn them into a single named vector
  bib[["extra"]] <- lapply(
    strsplit(bib[["extra"]], "\n"),
    function(x) {
      if (all(is.na(x))) {
        return(NA)
      }

      x <- gsub("\\\\", "", x)

      setNames(sub(".*: ", "", x), sub(":.*", "", x))
    }
  )

  attr(bib, "collection") <- x
  bib
}

# generate a bibliographic list ------------------------------------------
make_bib_list <- function(bib) {
  # get block function
  collection <- attr(bib, "collection")
  make_block <- switch(collection,
    "article" = article_block,
    "funding" = funding_block,
    "manuscript" = manuscript_block,
    "presentation" = presentation_block,
    "report" = report_block,
    "review" = article_block
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

    bib_years[[as.character(year)]] <- htmltools::tags$div(
      class = "bib-year",
      id = paste0("bib-year-", year),
      htmltools::h3(
        class = "bib-year-header",
        year
      ),
      year_items[i]
    )
  }

  # combine all list items in ordered list
  htmltools::tags$div(
    class = "bib-list",
    id = paste0("bib-", collection),
    bib_years
  )
}

# custom reference blocks ------------------------------------------------
article_block <- function(bib) {
  title <- bib[["title"]]
  authors <- bib[["author"]]
  journal <- bib[["publicationtitle"]]

  details <- sprintf(
    "%s (%s): %s (%s)",
    bib[["volume"]],
    bib[["issue"]],
    bib[["pages"]],
    bib[["year"]]
  )

  details <- gsub(" \\(\\)", "", details)
  details <- trimws(details)

  doi <- paste0(" ", bib[["doi"]])

  github <- bib[["extra"]][[1]]["github"]
  preprint <- bib[["extra"]][[1]]["preprint"]
  article <- bib[["url"]]

  block <- htmltools::div(
    class = "bib-block",
    htmltools::div(
      class = "bib-ref",
      htmltools::p(class = "bib-title", title),
      htmltools::p(
        class = "bib-details",
        htmltools::span(class = "bib-journal", journal),
        details
      ),
      htmltools::p(class = "bib-authors", icon_bi_people, authors),
      htmltools::p(
        class = "bib-details",
        id = "doi",
        icon_doi,
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

  details <- sprintf(
    "%s, %s.",
    bib[["fot"]],
    bib[["organization"]]
  )

  htmltools::div(
    class = "bib-block",
    htmltools::div(
      class = "bib-ref",
      htmltools::p(class = "bib-title", title),
      htmltools::p(class = "bib-details", details),
      htmltools::p(class = "bib-authors", icon_bi_people, authors)
    )
  )
}

manuscript_block <- function(bib) {
  title <- bib[["title"]]
  authors <- bib[["author"]]
  journal <- bib[["publicationtitle"]]

  github <- bib[["extra"]][[1]]["github"]
  preprint <- bib[["extra"]][[1]]["preprint"]
  status <- sprintf("(%s)", bib[["extra"]][[1]]["status"])

  block <- htmltools::div(
    class = "bib-block",
    htmltools::div(
      class = "bib-ref",
      htmltools::p(class = "bib-title", title),
      htmltools::p(
        class = "bib-details",
        htmltools::span(class = "bib-journal", journal),
        status
      ),
      htmltools::p(class = "bib-authors", icon_bi_people, authors)
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

  details <- sprintf(
    "%s at the %s, %s.",
    bib[["presentationtype"]],
    bib[["meetingname"]],
    bib[["place"]]
  )

  github <- bib[["extra"]][[1]]["github"]
  slides <- bib[["url"]]

  block <- htmltools::div(
    class = "bib-block",
    htmltools::div(
      class = "bib-ref",
      htmltools::p(class = "bib-title", title),
      htmltools::p(class = "bib-details", details),
      htmltools::p(class = "bib-authors", icon_bi_people, authors)
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

  details <- sprintf(
    "Report submitted to the %s. %s",
    bib[["institution"]],
    bib[["extra"]]
  )

  details <- trimws(details)

  htmltools::div(
    class = "bib-block",
    htmltools::div(
      class = "bib-ref",
      htmltools::p(class = "bib-title", title),
      htmltools::p(class = "bib-details", details),
      htmltools::p(class = "bib-authors", icon_bi_people, authors)
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

# icons ------------------------------------------------------------------

icon_bi_people <- htmltools::tags$svg(
  class = "iconify-icon",
  id = "bi-people",
  xmlns = "http://www.w3.org/2000/svg",
  width = "16",
  height = "16",
  viewBox = "0 0 16 16",
  htmltools::tags$rect(
    width = "16",
    height = "16",
    fill = "none"
  ),
  htmltools::tags$path(
    fill = "currentColor",
    d = "M15 14s1 0 1-1s-1-4-5-4s-5 3-5 4s1 1 1 1zm-7.978-1L7 12.996c.001-.264.167-1.03.76-1.72C8.312 10.629 9.282 10 11 10c1.717 0 2.687.63 3.24 1.276c.593.69.758 1.457.76 1.72l-.008.002l-.014.002zM11 7a2 2 0 1 0 0-4a2 2 0 0 0 0 4m3-2a3 3 0 1 1-6 0a3 3 0 0 1 6 0M6.936 9.28a6 6 0 0 0-1.23-.247A7 7 0 0 0 5 9c-4 0-5 3-5 4q0 1 1 1h4.216A2.24 2.24 0 0 1 5 13c0-1.01.377-2.042 1.09-2.904c.243-.294.526-.569.846-.816M4.92 10A5.5 5.5 0 0 0 4 13H1c0-.26.164-1.03.76-1.724c.545-.636 1.492-1.256 3.16-1.275ZM1.5 5.5a3 3 0 1 1 6 0a3 3 0 0 1-6 0m3-2a2 2 0 1 0 0 4a2 2 0 0 0 0-4"
  )
)

icon_doi <- htmltools::tags$svg(
  class = "iconify-icon",
  id = "custom-doi",
  xmlns = "http://www.w3.org/2000/svg",
  width = "24",
  height = "24",
  viewBox = "0 0 24 24",
  htmltools::tags$rect(
    width = "24",
    height = "24",
    fill = "none"
  ),
  htmltools::tags$path(
    fill = "currentColor",
    d = "M21,4H3v-1h18v1ZM21,20H3v1h18v-1ZM3.2,7.4c-.1.2-.2.5-.2.9v7.1c0,.3,0,.6,0,.8,0,.2.1.3.3.5s.4.2.6.2h2c.4,0,.7,0,.9,0s.5-.2.8-.3.5-.3.7-.6c.3-.3.5-.6.6-1,.2-.4.3-.8.4-1.3s.1-1,.1-1.6c0-1.8-.4-3.1-1.2-3.9-.3-.3-.6-.6-1-.7s-.8-.2-1.4-.2h-2c-.3,0-.6,0-.7.3ZM5.6,8.7c.5,0,.9,0,1.2.2.3.1.6.4.8.9s.3,1.2.3,2.1c0,1.4-.3,2.3-.8,2.9-.1.1-.3.2-.4.3s-.3.1-.5.1-.3,0-.6,0h-1.2v-6.6h1ZM12.7,7.3c-.5.2-.8.6-1.2,1s-.6,1-.7,1.6-.3,1.3-.3,2,0,1.4.2,2c.2.6.4,1.2.7,1.6s.7.8,1.2,1c.5.2,1,.4,1.6.4s1.1-.1,1.6-.4.9-.6,1.2-1,.6-1,.7-1.6.2-1.3.2-2-.1-1.9-.4-2.6c-.3-.7-.7-1.3-1.3-1.7-.6-.4-1.3-.6-2-.6s-1.1.1-1.6.3ZM16.2,13.9c-.2.5-.5.9-.8,1.2s-.7.4-1.1.4-.6,0-.9-.2c-.3-.1-.5-.4-.7-.7-.2-.3-.4-.7-.5-1.1-.1-.4-.2-.9-.2-1.5s0-1,.2-1.5c.1-.4.3-.8.4-1.1s.4-.5.7-.6c.3-.1.5-.2.9-.2s.8.1,1.2.4.6.7.8,1.2.3,1.1.3,1.8,0,1.4-.3,1.9ZM19.7,16.7c.1.2.3.3.6.3s.4,0,.6-.3c.1-.2.2-.5.2-.9v-7.6c0-.4,0-.7-.2-.9-.1-.2-.3-.3-.6-.3s-.4,0-.6.3-.2.5-.2.9v7.6c0,.4,0,.7.2.9Z"
  )
)
