#' get unicode and full names of national park units
#' @examples
#' nps_code = get_nps_code()
#' @export
get_nps_code = function(){
  tibble::as_tibble(jsonlite::fromJSON("https://irma.nps.gov/NPSpecies/Lookup/GetAllUnits"))
}

#' get code for taxa groups
#' @examples
#' taxa_code = get_taxa_code()
#' @export
#'
get_taxa_code = function(){
  jsonlite::fromJSON("https://irma.nps.gov/NPSpecies/Lookup/GetAllTaxonCategories")
}

#' get species list for one park from https://irma.nps.gov/NPSpecies/Search/SpeciesList
#' @param parkcode four digits code for parks from `rnps::get_nps_code()$UnitCode`.
#' @param categcode taxa groups code from `rnps::get_taxa_code()$Id`.
#' 
#'  | Id | Name |
#'  | -- | ---- |
#'  | 1  | Mammals |
#'  | 2  | Birds |
#'  | 3  | Fish |
#'  | 4  | Reptiles |
#'  | 5  | Amphibians |
#'  | 6  | Spiders/Scorpions |
#'  | 7  | Crab/Lobsters/Shrimp |
#'  | 8  | Insects |
#'  | 9  | Slugs/Snails |
#'  | 10 | Other Non-vertebrates |
#'  | 11 | Vascular Plants |
#'  | 12 | Non-vascular Plants |
#'  | 13 | Fungi |
#'  | 14 | Protozoa |
#'  | 15 | Bacteria |
#'  | 16 | Chromista |
#'  | 17 | Archaea |
#'  
#' @param searchlevel the level of search: 1: checklist; 2: full list; 3: full list with details. Default is 3.
#' @export
#' @md
#' @examples
#' get_one_park("ABLI", 11)
get_one_park = function(parkcode = "ACAD", categcode = 11, searchlevel = 3){
  cat(parkcode, " \t")

  if(searchlevel == 1){
    url = "https://irma.nps.gov/NPSpecies/Search/GetSpeciesListChecklistResults"
  }
  if(searchlevel == 2){
    url = "https://irma.nps.gov/NPSpecies/Search/GetSpeciesListFullListResults"
  }
  if(searchlevel == 3){
    url = "https://irma.nps.gov/NPSpecies/Search/GetSpeciesListFullListWithDetailsResults"
  }

  output = RCurl::postForm(uri = url,
                    .params = list(UnitCode = parkcode,
                                   CategoryCode = categcode),
                    .opts = RCurl::curlOptions(referer = "https://irma.nps.gov/NPSpecies/Search/SpeciesList")
  )

  output = as.data.frame(jsonlite::fromJSON(output)$Results, stringsAsFactors = FALSE)

  if(nrow(output) == 0) {
    warning("No results. Please make sure the park code ", parkcode, " is correct.")
    return()
  }

  tibble::as_tibble(output)
}
