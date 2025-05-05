
#' Get the base URL for PanelApp API
#'
#' @param region A character string: "uk" or "aus" to select the data source.
#' @return A base URL string for the selected PanelApp instance.
#' @examples
#' get_panelapp_base_url("uk")
#' get_panelapp_base_url("aus")
get_panelapp_base_url <- function(region = c("uk", "aus")) {
  region <- match.arg(region)
  switch(region,
         "uk" = "https://panelapp.genomicsengland.co.uk/api/v1/panels/",
         "aus" = "https://panelapp-aus.org/api/v1/panels/")
}

#' Fetch all gene panels from PanelApp (UK or Australia)
#'
#' @param region A character string: "uk" or "aus" (default is "uk").
#' @return A data frame containing all available gene panels and metadata.
#' @details This function handles API pagination and combines all results into one table.
#' @importFrom httr GET content status_code
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @examples
#' fetch_all_panels("uk")
#' fetch_all_panels("aus")
fetch_all_panels <- function(region = c("uk", "aus")) {
  region <- match.arg(region)
  url <- get_panelapp_base_url(region)

  all_panels <- list()
  current_url <- url

  repeat {
    response <- httr::GET(current_url)
    if (httr::status_code(response) == 200) {
      content <- jsonlite::fromJSON(httr::content(response, "text"), flatten = TRUE)
      all_panels <- append(all_panels, list(content$results))
      if (is.null(content$`next`)) break
      current_url <- content$`next`
    } else {
      stop("Failed to fetch panels: ", httr::status_code(response))
    }
  }

  all_panels <- dplyr::bind_rows(all_panels)
  return(all_panels)
}

#' Fetch gene symbols from a specific gene panel
#'
#' @param panel_id The panel ID (numeric or character) to retrieve genes from.
#' @param region A character string: "uk" or "aus" (default is "uk").
#' @return A character vector of gene symbols in the specified panel.
#' @importFrom httr GET content status_code
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate pull
#' @importFrom purrr map_chr
#' @examples
#' panels <- fetch_all_panels("uk")
#' get_genes_for_panel(panel_id = panels$id[1], region = "uk")
get_genes_for_panel <- function(panel_id, region = c("uk", "aus")) {
  region <- match.arg(region)
  base_url <- sub("/panels/$", "", get_panelapp_base_url(region))  # Trim the /panels/ path
  url <- paste0(base_url, "/panels/", panel_id, "/")

  response <- httr::GET(url)
  if (httr::status_code(response) == 200) {
    details <- jsonlite::fromJSON(httr::content(response, "text"), flatten = TRUE)
    genes <- details$genes %>%
      dplyr::mutate(
        gene_symbol = purrr::map_chr(gene_data.gene_symbol, ~paste(.x, collapse = "; "), .default = NA_character_)
      ) %>%
      dplyr::pull(gene_symbol)
    return(genes)
  } else {
    stop("Failed to fetch genes: ", httr::status_code(response))
  }
}
