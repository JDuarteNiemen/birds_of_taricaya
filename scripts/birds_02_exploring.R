#Exploration ----

head(all_birds)

glimpse(all_birds)

all_birds %>%
  distinct(station)


#Find out how many observations made for each station
stations <- all_birds %>%
  group_by(station) %>%
  summarise(n=n())%>%
  arrange(desc(n))

#Find how many observations made for different species
bird_species <- all_birds %>%
  group_by(species) %>%
  summarise(n=n()) %>%
  arrange(desc(n))#P.fasciicauda is most abundant.

#Exploring trends in p.fasciicauda and r.carbo ----
birds_fascicarbo <- all_birds %>%
  filter(species %in% c("Pipra fasciicauda", "Ramphocelus carbo"))%>%
  filter(wing < 170)%>%
  filter(beak < 50) %>%
  filter(weight < 250)




#Data viz ----
birds_fascicarbo %>%
  ggplot(aes(x=species,
             y=beak,
             colour=species))+
  geom_boxplot(width=0.2)+
  geom_jitter(alpha=0.7)+
  theme_classic()# nice demonstration of niche separation between two most abundant birds


all_birds %>%
  filter(station %in% c("NEFA", "TARI", "NEF2","HOBO", "RIO2", "AMAZ"))%>%
  filter(species %in% c("Pipra fasciicauda", "Ramphocelus carbo"))%>%
  ggplot(aes(x=weight,
             y=beak,
             colour=station))+
  geom_jitter()+
  theme_classic()
