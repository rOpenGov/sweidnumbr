
#' @title
#' oin_group
#' 
#' @description
#' Calculates the organization group from the organization number for one oin.
#' 
#' @param one_oin Character elemen with oin.
#' 
#' @references 
#' http://www.skatteverket.se/download/18.70ac421612e2a997f85800040284/1302507382017/70909.pdf
#' 
#' @return
#' Character categegory of organisational group.

oin_group_element <- function(one_oin){
  switch(substr(one_oin, 1, 1),
         "1" = "Dödsbo",
         "2" = "Stat, landsting, kommuner, församlingar",
         "3" = "Utländska företag som bedriver näringsverksamhet eller äger fastigheter i Sverige",
         "5" = "Aktiebolag",
         "6" = "Enkelt bolag",
         "7" = "Ekonomiska föreningar",
         "8" = "Ideella föreningar och stiftelser",
         "9" = "Handelsbolag, kommanditbolag och enkla bolag",
         as.character(NA))
}