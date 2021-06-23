#' Checks row overlap between 2 histologies given a column name to compare between the 2 dataframes
#' @param new_hist A dataframe of OpenPBTA histology format of a new release 
#' @param old_hist A dataframe of OpenPBTA histology format of a previous release
#'
#' @return Check status of row overlap in new and old 
#' 
#'

check_rows <- function(new_hist,old_hist,column_name="Kids_First_Biospecimen_ID"){
  
  old_hist <- old_hist %>%
    as.data.frame()
  new_hist <- new_hist %>%
    as.data.frame()
  dim_new_old_check <- dim(old_hist) == dim(new_hist)
  
  if (!dim_new_old_check[1]){
    ids_in_new_not_in_old <- setdiff(new_hist[,column_name], old_hist[,column_name])
    ids_in_old_not_in_new <- setdiff(old_hist[,column_name], new_hist[,column_name])
    
    # Check 1a: ids overlap in new and old ?
    print(paste("ids in new and not in old in:",column_name, toString(ids_in_new_not_in_old)))
    print(paste("ids in old and not in new in:",column_name, toString(ids_in_old_not_in_new)))
  } else{
    print(paste("ids overlaps in new and old" ,emo::ji("white_check_mark")))
    
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
#'#'
#' @return Check status for value counts per give column name
#' 
#'


check_values <- function(new_hist, old_hist,column_name){
  
  old_hist <- old_hist %>%
    as.data.frame()
  new_hist <- new_hist %>%
    as.data.frame()
  
  # if lengths of values is same check for count
if ( length(unique(old_hist[,column_name])) ==  length(unique(new_hist[,column_name]))){  
count_check <- table(old_hist[,column_name]) == table(new_hist[,column_name])

if (any(count_check==FALSE)){
  print(paste0(names(count_check[count_check ==FALSE])," counts changed"))
  # print("Counts changed because of change in id in new")
  # print(df_changes_new)
  # print("Counts changed because of change in id in old")
  # print(df_changes_old)
}
} else {
  diff_values_old <- setdiff(unique(old_hist[,column_name]),(unique(new_hist[,column_name])))
  diff_values_new <- setdiff(unique(new_hist[,column_name]),(unique(old_hist[,column_name])))
  
  print (paste("Different values found in new histology",toString(diff_values_new)))
  
}

df_changes_new <- dplyr::setdiff(new_hist[,c("Kids_First_Biospecimen_ID",column_name)],old_hist[,c("Kids_First_Biospecimen_ID",column_name)])

df_changes_old <- dplyr::setdiff(old_hist[,c("Kids_First_Biospecimen_ID",column_name)],new_hist[,c("Kids_First_Biospecimen_ID",column_name)])


  
value_in_new_not_in_old <- all(new_hist[,column_name] %in% 
                               old_hist[,column_name])
value_in_old_not_in_new <- all(old_hist[,column_name] %in% 
                               new_hist[,column_name])


if (!value_in_old_not_in_new | !all(df_changes_new$Kids_First_Biospecimen_ID %in% df_changes_old$Kids_First_Biospecimen_ID)) {
 
  print(paste("Levels differ in ", column_name,"because change in",toString(df_changes_new$Kids_First_Biospecimen_ID)))
}else if ( !value_in_new_not_in_old  | !all(df_changes_old$Kids_First_Biospecimen_ID %in% df_changes_new$Kids_First_Biospecimen_ID)) {
  print(paste("Levels differ in ", column_name,"because of change in",toString(df_changes_old$Kids_First_Biospecimen_ID)))
} else{
  print(paste("Levels overlap in new and old",emo::ji("white_check_mark")))
}


}


