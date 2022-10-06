
#' Collect Zotero bibliography with credentials 
#'
#' @param zotero data.frame with credentials for connecting to Zotero
#' @param collection data.frame with collection Zotero IDs
#'
bibliography <- function(zotero, collection, group = NULL){
  
  get_bibliography(zotero, collection, group) |> 
    clean_bibliography() |>
    arrange(desc(year))
  
}

get_bibliography <- function(zotero, collection, group = NULL){
  
  if (!is.null(group)) collection <- collection[collection$collection %in% group,]
  
  bib_collections <- list()
  
  for(i in 1:nrow(collection)){
    
    bib_col <- RefManageR::ReadZotero(
      user = zotero$id,
      .params = list(
        key = zotero$key,
        collection = collection[i, "id", drop = TRUE]
      )
    )
    
    bib_col <- as_tibble(bib_col)
    
    bib_col$groups <- collection[i, "collection"]
    
    bib_collections[[i]] <- bib_col
    
  }
  
  bind_rows(bib_collections)
  
}

clean_bibliography <- function(x){
  
  cln <- 
    x |>
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
      shorttitle = stringr::str_remove_all(
        shorttitle, pattern = "[{}]"
      )
    )
  
  # zotero escapes special characters
  if ("note" %in% names(cln)) cln$note <- gsub("\\\\", "", cln$note) 
  
  cln
  
}