#' Checks row overlap between 2 histologies given a column name to compare between the 2 dataframes
#' @param new_hist A dataframe of OpenPBTA histology format of a new release 
#' @param old_hist A dataframe of OpenPBTA histology format of a previous release
#'
#' @return Check status of row overlap in new and old 
#' 
#'

check_rows <- function(new_hist,old_hist,column_name="Kids_First_Biospecimen_ID",getcount=FALSE){
  
  old_hist <- old_hist %>%
    as.data.frame()
  new_hist <- new_hist %>%
    as.data.frame()
  
  if (getcount){
    count<- new_hist %>%
      group_by(!!as.name(column_name)) %>%
      tally() %>%
      arrange(desc(n)) 
    
    return(count)
    
  }else{
  
  dim_new_old_check <- dim(old_hist) == dim(new_hist)
  
    ids_in_new_not_in_old <- setdiff(sort(new_hist[,column_name]), 
                                     sort(old_hist[,column_name]))
    ids_in_old_not_in_new <- setdiff(sort(old_hist[,column_name]),
                                     sort(new_hist[,column_name]))
    
    if (length(ids_in_new_not_in_old)>0){  
    # Check 1a: ids overlap in new and old ?
    added<-paste("ids in new and not in old in:",column_name, toString(ids_in_new_not_in_old))
    change_text <-list(added=added)
    } else if(length(ids_in_old_not_in_new)>0){
      removed<-paste("ids in old and not in new in:",column_name, toString(ids_in_old_not_in_new))
      change_text <-list(removed=removed)
    } else if (length(ids_in_new_not_in_old)>0 & length(ids_in_old_not_in_new)>0 ){
      added<-paste("ids in new and not in old in:",column_name, toString(ids_in_new_not_in_old))
      removed<-paste("ids in old and not in new in:",column_name, toString(ids_in_old_not_in_new))
      change_text <-list(removed=removed,added=added)
  } else{
    change_text<-paste("ids overlaps in new and old")
  }
    return(change_text)
  }
}

#' Checks column names overlap between 2 histologies 
#' @param new_hist A dataframe of OpenPBTA histology format of a new release 
#' @param old_hist A dataframe of OpenPBTA histology format of a previous release
#'
#' @return Check status of colum names that don't overlap in new and old 
#' 
#'

check_cols <- function(new_hist,old_hist){
  
  old_hist <- old_hist %>%
    as.data.frame()
  new_hist <- new_hist %>%
    as.data.frame()
  
  dim_new_old_check <- dim(old_hist) == dim(new_hist)
  if (!dim_new_old_check[2]){
  col_in_new_not_in_old <- setdiff(names(new_hist), names(old_hist))
  col_in_old_not_in_new <- setdiff(names(old_hist), names(new_hist))
  
  print(paste("Columns in new that are not in old:", col_in_new_not_in_old))
  print(paste("Columns in old that are not in new:", col_in_old_not_in_new))
} else{
  print(paste("Columns overlap in new and old",emo::ji("white_check_mark")))
}

}


#'  Checks value counts overlap between 2 histologies given a column name to compare between the 2 dataframes
#'  
#' @param new_hist A dataframe of OpenPBTA histology format of a new release 
#' @param old_hist A dataframe of OpenPBTA histology format of a previous release
#' @param column_name Column to compare in the 2 versions of histology
#' @param output_dir Output folder to save a dataframe of mismatches
#' @param length_check_only Are the levels in given columns same
#'
#' @return Check status for value counts per give column name
#' 
#'


check_values <- function(new_hist, old_hist,column_name,output_dir=NULL, level_check_only=FALSE){
  
  old_hist <- old_hist %>%
    as.data.frame()
  new_hist <- new_hist %>%
    as.data.frame()
  
df_changes_new <- dplyr::setdiff(new_hist[,c("Kids_First_Biospecimen_ID",column_name)],old_hist[,c("Kids_First_Biospecimen_ID",column_name)])

df_changes_old <- dplyr::setdiff(old_hist[,c("Kids_First_Biospecimen_ID",column_name)],new_hist[,c("Kids_First_Biospecimen_ID",column_name)])

change_log <-full_join(df_changes_new,df_changes_old,by="Kids_First_Biospecimen_ID",suffix=c("_latest","_previous")) %>% dplyr::filter(Kids_First_Biospecimen_ID %in% old_hist$Kids_First_Biospecimen_ID)

if(level_check_only){
  diff_values_new <- setdiff(sort(unique(df_changes_new[,column_name])),sort((unique(df_changes_old[,column_name]))))
  return(diff_values_new)
}

if(nrow(change_log)>0){
  change_log <- change_log %>%
    # if change in latest release is NA mark as critical changes
    # since we would never expect a value to be converted to NA 
    mutate(typeof_change = case_when( is.na(select(.,ends_with("_latest")))  ~ "CRITICAL CHANGE",
                                TRUE ~ "REVIEW NEEDED"
           )
    )
  
  if (!is.null(output_dir))
  {
    write_tsv(change_log,
              file.path(output_dir,paste0("changes_in_",column_name,".tsv")))
  } else{
    return(change_log)
  }
} else{
  eval(parse(text=paste0('change_log<-data.frame(',column_name,'=NA,"typeof_change"=NA)')))
}
}





