
#' @param v vector can convert
#' @export

###############################  chung cho cac ham ############################
removeBlankCol <- function(df){
  df[,which(unlist(lapply(df, function(x) sum(nchar(as.vector(x)))>0)))]
}
###############################  getBusinessReport ########################################
#Loai bo dau ',' giua cac chu so va convert ve dang numeric
convertNumber <- function(v) {
  str_replace_all(v,",","") %>% as.numeric()
}


############################# getPrice VNDIRECT ########################################
myexport <- function(...) {
  arg.list <- list(...)
  names <- all.names(match.call())[-1]
  for (i in seq_along(names)) assign(names[i],arg.list[[i]],.GlobalEnv)
}


dateChar <- function(dateTime){
  dateTime %<>%  as.Date() %>% format(.,format = "%d/%m/%Y")
  return(dateTime)
}

############################## getPrice CP 68 ########################################
#ham convertDate
convertDate <- function(dt){
  paste0(str_sub(dt,7,10),"-",str_sub(dt,4,5),"-",str_sub(dt,1,2))
}

#ham thay the dau comma
subComma <- function(v) {
  str_replace_all(v,",","")
}

#chuyen percent dang text sang numeric
convertPercent <- function(v){
  str_replace_all(v,"%","") %>% as.numeric()*0.01
}
