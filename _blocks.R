

#' Create a bootstrap hyperlink button
#'
#' @param x 
#' @param icon 
#'
button <- function(x, icon = ""){
  
  make_button <- function(z, icon){
    
    if(is.na(z)) return("")
    
    q <- switch(
      icon,
      "github"  = "github",
      "pdf"     = "filetype-pdf",
      "slides"  = "file-easel",
      "article" = "journal-text"
    )
    
    img <- htmltools::tags$i(
      "",
      class = paste0("bi bi-", q),
      `aria-label` = icon
    ) |> as.character()
    
    txt <- tools::toTitleCase(icon)
    
    htmltools::a(
      htmltools::HTML(paste(img, txt)),
      href = z,
      class = "btn btn-outline-dark btn-sm",
      target = "_blank",
      rel = "noopener noreferrer",
    ) |> as.character()
    
  }
  
  vapply(x, make_button, character(1), icon = icon, USE.NAMES = FALSE)
  
}

sew_buttons <- function(...){
  
  paste(...) |> 
    htmltools::HTML() |> 
    htmltools::div(class = "buttons") |> 
    as.character()
  
}

#' Wrapper to make div elements
#' 
#' a very, very crude version of htmltools::div()
#'
div <- function(..., class = NULL, sep = NULL) {
  
  open_fence <- paste0("<div class='", class, "'>\n")
  
  content <- if (!is.null(sep)) paste(..., sep = sep) else paste(...)
  
  close_fence <- "\n</div>"
  
  paste0(open_fence, content, close_fence)
  
} 


#' Standard formatting for generic cv blocks (date -- details)
#' 
#' Blocks are html divs wrapped around chunk content 
#' to make styling with css easier.
#'
#' @param x data.frame
#'
make_block <- function(x){
  
  x |> 
    mutate(
      details = div(item, class = "details"),
      date = div(year, class = "date")
    ) |> 
    rowwise() |> 
    mutate(
      block_rows = div(date, details, class = "block-row", sep = "\n")
    ) |>
    group_by(year) |> 
    summarize(
      blocks = div(paste0(block_rows, collapse = "\n"), class = "block-year")
    ) |> 
    arrange(desc(year)) |> 
    pull(blocks) |> 
    paste0(collapse = "\n") |> 
    div(class = "blocks") |> 
    cat()
  
}
