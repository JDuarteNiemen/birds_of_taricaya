#TIME SERIES OF CAPTURES
#packages
library(highcharter)



birds_time <- all_birds %>%
  group_by(year, month, day)%>%
  filter(station %in% c("NEFA", "TARI", "NEF2","HOBO", "RIO2", "AMAZ"))%>%
  summarise(num_obs=n())

birds_time <- birds_time %>%
  mutate(date = make_date(year, month, day))




hchart(birds_time, "line", hcaes(x = date,
                                 y = num_obs))%>%
  hc_annotations()


birds_time %>%
  ggplot(aes(x=date,
         y=num_obs))+
  geom_line(colour="red")+
  geom_smooth(colour="blue",
              se=FALSE)+
  theme_classic()
 