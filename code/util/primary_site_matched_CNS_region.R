#' Adds CNS_regions to histology file
#' @param histology file with atleast CNS_region and primary_site columns
#' @param CNS_match_json json format file to get the CNS_region based on primary_site values
#'
#' @return Histology file with matched CNS_region
#' @example 
#' v18_hist <- read_tsv(file.path(pbta_data_dir, "pbta-histologies-base.tsv"),
#' # NAs are being read as logical so specifying as character here
#' col_types = readr::cols(molecular_subtype = readr::col_character(),
#' short_histology = readr::col_character(),
#' integrated_diagnosis = readr::col_character(),
#' broad_histology = readr::col_character(),
#' Notes = readr::col_character()))
#' 
#' get_CNS_region(histology=v18_hist,
#' CNS_match_json="input/CNS_primary_site_match.json") 
#'
#'


get_CNS_region<-function(histology, CNS_match_json){
  
  # CNS_region ~ primary_site matches
  input_cns_matches <- jsonlite::fromJSON(CNS_match_json)
  
  # add CNS_Region if there is a direct match of terms from 
  # primary_site to CNS_region
  histology <- histology %>%
    mutate("CNS_region"= case_when(grepl(";",histology$primary_site) ~ "Mixed",
                                   grepl(paste(input_cns_matches$Hemispheric, collapse = "|"),histology$primary_site) ==TRUE ~ "Hemispheric",
                                   grepl(paste(input_cns_matches$Midline, collapse = "|"),histology$primary_site) ==TRUE ~ "Midline",
                                   grepl(paste(input_cns_matches$Spine, collapse = "|"),histology$primary_site) ==TRUE ~ "Spine",
                                   grepl(paste(input_cns_matches$Ventricles, collapse = "|"),histology$primary_site) ==TRUE ~ "Ventricles",
                                   grepl(paste(input_cns_matches$`Posterior fossa`, collapse = "|"),histology$primary_site) ==TRUE ~ "Posterior fossa",
                                   grepl(paste(input_cns_matches$`Optic pathway`, collapse = "|"),histology$primary_site) ==TRUE ~ "Optic pathway",
                                   grepl(paste(input_cns_matches$Suprasellar, collapse = "|"),histology$primary_site) ==TRUE ~ "Suprasellar",
                                   grepl(paste(input_cns_matches$Other, collapse = "|"),histology$primary_site) ==TRUE ~ "Other"
    ))
  
  
  # if mixed check if all values separated by ";" fit in 1 CNS_region
  cns_region_check <- function(primary_site,input_cns_matches){
    primary_site_split <- unlist(lapply(primary_site, function(x) strsplit(x,";")))
    # Only checking the following because they have multiple primary_site matches
    if (all(primary_site_split %in% input_cns_matches[["Hemispheric"]])){
      return("Hemispheric")
    }else if (all(primary_site_split %in% input_cns_matches[["Midline"]])){
      return("Midline")
    }else if (all(primary_site_split %in% input_cns_matches[["Spine"]])){
      return("Spine")
    }else if (all(primary_site_split %in% input_cns_matches[["Ventricles"]])){
      return("Ventricles")
    }else if (all(primary_site_split %in% input_cns_matches[["Posterior fossa"]])){
      return("Posterior fossa")
    }else if (all(primary_site_split %in% input_cns_matches[["Optic pathway"]])){
      return("Optic pathway")
    }else if (all(primary_site_split %in% input_cns_matches[["Suprasellar"]])){
      return("Suprasellar")
    }else if (all(primary_site_split %in% input_cns_matches[["Other"]])){
      return("Other")
    }else{
      return("Mixed")
    }
    
  }
  
  
  # If CNS_region=="Mixed" check if all values separated by ";"
  # belong to the same CNS_region if not keep as "Mixed"
  histology[which(histology$CNS_region=="Mixed"),"CNS_region"] <-unlist(
    lapply(
      # only check for Mixed primay_sites
      histology[which(histology$CNS_region=="Mixed"),"primary_site"] %>% pull(primary_site),
      function(x) 
        # cns_region_check with Mixed primary_site and input CNS_region matches
        cns_region_check(x,input_cns_matches)
    )
  )
  
  return(histology)
}
