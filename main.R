library(tercen)
library(dplyr)
 
nBins = function(x){
  x = x[is.finite(x)]
  binSize.x = 2 * IQR(x) / (length(x) ** (1/3))
  nBin.x = (max(x) - min(x)) / binSize.x;
  if (!is.finite(nBin.x)){
    nBin.x = 1
  } else {
    nBin.x = round(nBin.x)
  }
  return(nBin.x)
}

calcBinIds = function(x){
  x = x[is.finite(x)]
  nBin.x  = nBins(x)
  x =  nBin.x  * (x - min(x)) / (max(x) -  min(x))
  x[is.finite(x)] = floor(x[is.finite(x)])
  x[!is.finite(x)] = -2147483648
  return(as.integer(x))
}

ctx = tercenCtx()

if (ctx$hasNumericXAxis || ctx$isPairwise) {
  (binIds = ctx %>% 
     select(.ci, .ri, .x, .y) %>%
     mutate(.rids=row_number()) %>%
     group_by(.ci, .ri) %>%
     do(tibble(bin.x = calcBinIds(.$.x), bin.y = calcBinIds(.$.y), .rids=.$.rids))) %>% 
    group_by(.ci, .ri, bin.x, bin.y) %>%
    summarise(density = length(bin.y)) %>%
    ungroup() %>%
    mutate(density=((density - min(density)) / (max(density) - min(density))) )  %>% 
    left_join(binIds, by= c('.ci', '.ri', 'bin.x', 'bin.y'))  %>% 
    select(density, .rids) %>%
    arrange(.rids) %>%
    select(density) %>%
    ctx$addNamespace() %>%
    ctx$save()
} else {
  (binIds = ctx %>% 
     select(.ci, .ri, .y) %>%
     mutate(.rids=row_number()) %>%
     group_by(.ci, .ri) %>%
     do(tibble(bin.y = calcBinIds(.$.y), .rids=.$.rids))) %>% 
    group_by(.ci, .ri, bin.y) %>%
    summarise(density = length(bin.y)) %>%
    ungroup() %>%
    mutate(density=((density - min(density)) / (max(density) - min(density))) )  %>% 
    left_join(binIds, by= c('.ci', '.ri', 'bin.y'))  %>% 
    select(density, .rids) %>%
    arrange(.rids) %>%
    select(density) %>%
    ctx$addNamespace() %>%
    ctx$save()
}





