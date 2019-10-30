library(dplyr)
all_code=read.csv("all_code.csv", header=TRUE)

# student to instructor including codes
fsi_code <- all_code %>% filter(is_reply==1)  %>%
  select(vert1_id,vert2_id, SKI, MKI, DKI) %>% mutate(si_codes = SKI*1+MKI*2+DKI*3)%>%
  group_by(vert1_id)%>% summarise(si_codes=sum(si_codes))