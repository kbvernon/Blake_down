
#' Collect Zotero bibliography with credentials 
#'
#' @param zotero data.frame with credentials for connecting to Zotero
#' @param collection data.frame with collection Zotero IDs
#'
cv_get_bibliography <- function(zotero, collection){
  
  bib_collections <- list()
  
  for(i in 1:nrow(collection)){
    
    bib_col <- RefManageR::ReadZotero(
      user = zotero$id,
      .params = list(key = zotero$key,
                     collection = collection[i, "id"])
    )
    
    bib_col <- as_tibble(bib_col)
    
    bib_col$groups <- collection[i, "collection"]
    
    bib_collections[[i]] <- bib_col
    
  }
  
  bib_collections %>%
    bind_rows() %>%
    mutate(
      author = stringr::str_replace_all(
        author,
        pattern = " and ",
        replacement = ", "
      ),
      author = stringr::str_remove_all(
        author, pattern = "[{}]"
      ),
      title = stringr::str_remove_all(
        title, pattern = "[{}]"
      ),
      title = if_else(
        is.na(url),
        title,
        paste0("[", title, "](", url, ")")
      ),
      shorttitle = stringr::str_remove_all(
        shorttitle, pattern = "[{}]"
      )
    ) %>%
    arrange(desc(year))
  
}


#' Standard formatting for cv lists
#' 
#' A little easier to work with than in the pagedown version
#' as it doesn't require all the breaking, so I can just 
#' make a separate list for each year, then use css to show 
#' the year only for the first item of each year-list 
#'
#' @param x data.frame
#'
make_ordered_list <- function(x){
  
  x %>% 
    group_by(year) %>% 
    summarize(
      block_year = paste0("<li>", item, "</li>", collapse = "\n"),
      blocks = paste("<ol class = 'cv-list' start=", unique(year), ">\n", block_year, "\n</ol>")
    ) %>% 
    arrange(desc(year)) %>% 
    pull(blocks) %>% 
    paste0(collapse = "\n") %>% 
    cat()
  
}
