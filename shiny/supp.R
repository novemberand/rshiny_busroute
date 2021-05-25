library(tidyverse)
library(magrittr)


daramg_station %>% names
route1 %>% names
route2 %>% names


daramg_station$노선번호 <- plyr::revalue(daramg_station$노선번호,
                                    replace=c("8221" = "8221 : 장안2동 주민센터-답십리역",
                                              "8441" = "8441 : 은곡마을 - 수서역",
                                              "8552" = "8552 : 신림복지관 - 신림역",
                                              "8761" = "8761 : 신촌로터리 - 국회의사당",
                                              "8771" = "8771 : 구산중 - 녹번역",
                                              "8551" ="8551 : 봉천역 - 노량진역",
                                              "8331" = "8331 : 마천사거리 - 잠실역"))

daramg_station$board <- plyr::revalue(daramg_station$board,
                                      replace = c("승차" = "s", "하차" = "h"))

route1 %<>% 
  mutate(노선번호 = "신규노선 제안 1 : 은평구") %>% 
  gather(board, mean, c(mean_s, mean_h)) %>% 
  select(노선번호, 역명, board, X좌표, Y좌표, mean) %>% 
  separate(board, c(NA, "board"), sep="_")
  
route2 %<>% 
  mutate(노선번호 = "신규노선 제안 2 : 서초구") %>% 
  gather(board, mean, c(mean_s, mean_h)) %>% 
  select(노선번호, 역명, board, X좌표, Y좌표, mean) %>% 
  separate(board, c(NA, "board"), sep="_")

new_daramg <- rbind(daramg_station, route1, route2)
write.csv(new_daramg, file = "new_daramg.csv", row.names = FALSE)
