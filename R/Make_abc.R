make_abc <- function(data, time, scope) {


  recid_abc <- data %>% 
    dplyr::filter(Year == time) %>%
    dplyr::group_by_("Year", scope) %>%
    dplyr::summarize(vol = sum(TMS_Volume), val = sum(TMS_Value)) %>%
    dplyr::arrange(desc(val)) %>%
    
    mutate(total = sum(val), share = val/total) %>%
    # accumulated sum and percentage
    mutate(acc = cumsum(val), acc_share = acc/total)
  
  top_a = recid_abc %>% 
    filter(acc_share <= .8) %>%
    mutate(pabc = "A")
  top_b = recid_abc %>% 
    filter(acc_share > .8, acc_share <= .95) %>%
    mutate(pabc = "B")
  top_c = recid_abc %>% 
    filter(acc_share > .95) %>%
    mutate(pabc = "C")
  
  recid_abc2 <- bind_rows(top_a, top_b, top_c)

  return(recid_abc2)
  
}