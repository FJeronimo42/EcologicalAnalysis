####################################
### XVI SIMPOSIO DO PPG BOTANICA ###
######## MINICURSO TIDYVERSE #######
####### FERNANDO F JERONIMO ########
####################################



#### set and check work directory ####
setwd("C:/Users/Fernando Jeronimo/Desktop/CursoTidyverse/Data") #set
getwd() # check



##### install and load packages ####
if (!require("tidyverse")) install.packages("tidyverse") # install
library("tidyverse") # load



##### useful functions ####
update.packages()

citation()

help(lm)

example(lm) 

demo(lm)

args(lm)

ls()

exemplo <- c(1, 5, 10)
rm(exemplo)



##### load and check data frames ####

# pokemon species data
spec.data <- read_csv("pokemon_species_data.csv") # load

str(spec.data) # check

glimpse(spec.data)

View(spec.data) # visualize

# locations land use data
land.data <- read_csv("pokemon_landuse_data.csv") # load

str(land.data) # check

View(land.data) # visualize



#### dplyr functions ####
# filter()
filter(spec.data, Region == "Johto") # filter one category

filter(spec.data, Region != "Johto") # filter different category

filter(spec.data, Location %in% c("Violet", "Viridian")) # filter more than one category

filter(spec.data, Location == "Saffron" & Speed > 75) # filter x AND y conditions

filter(spec.data, Location == "Saffron" | Speed > 75) # filter x OR y conditions

filter(spec.data, Region == "Johto" & Species %in% c("Bulbassaur", "Charmander")) # combined filter

filter(spec.data, is.na(Ovos)) # positive na filter

filter(spec.data, !is.na(Ovos)) # negative na filter

filter(spec.data, Attack > 75) # conditional filter

filter(spec.data, Speed <= 30) # conditional filter

filter(spec.data, between(Health, 50, 60)) # interval select

filter(spec.data, Region == "Johto" & Species == "Squirtle" & Attack > 50) # combined filter



# select()
select(spec.data, Location, Species, Type) # positive select columns 

select(spec.data, -c(Health, Attack, Defense, Speed)) # negative select columns

select(spec.data, Species, Type, Location, ID_Sex) # sort columns with select

select(spec.data, starts_with("S")) # conditional select 

select(spec.data, ends_with("n")) # conditional select 

select(spec.data, contains("ei")) # conditional select

select(spec.data, c(Species, Type), contains("ei")) # combined conditional select



# slice
slice(spec.data, 5) # slice an specific row

slice(spec.data, 40:50) # slice an specific row interval

slice(spec.data, -(1:10)) # negative slice an specific row interval

slice(spec.data, -1, -5, -10, -100, -150) # negative slice a set of rows

slice_sample(spec.data, n = 5, replace = F) # slice a sample



# mutate () ~ transmute()
mutate(spec.data, AttSpd = (Attack + Speed) / 2) # create a formula-based new variable 

mutate(spec.data, AttSpd = (Attack + Speed) / 2, .after = Health) # choice new variable position (or ".before")

mutate(spec.data, 
        AttSpd = (Attack + Speed) / 2, 
        DffSpd = (Defense + Speed) / 2) # create multiple formula-based new variables

mutate_if(spec.data, is.character, as.factor) # conditional changes ("_at", "_all", "_each") 

mutate(spec.data, Type = recode(Type, Agua = "Water",
                                      Fogo = "Fire",
                                      Planta = "Plant")) # factors recoding


transmute(spec.data, AttSpd = (Attack + Speed) / 2) # create a new transmuted vector

transmute(spec.data,
          ID_Sex,
          AttSpd = (Attack + Speed) / 2, 
          DffSpd = (Defense + Speed) / 2)

# rename()
rename(spec.data, "Tipo" = Type) # rename one column

rename(spec.data, "Eggs" = Ovos,
                  "Height" = Height_cm,
                  "Weight" = Weight_kg) # rename multiple columns



# summarise()
summarise(spec.data, mean(Attack, na.rm = T)) # summarize one specific variable

summarise_at(spec.data, c("Attack", "Defense"), mean, na.rm=T) # summarize more than one variable

summarise_if(is.numeric, mean, na.rm = T) # sconditional summarizing 

# tally()
spec.data %>% 
  group_by(Species) %>% 
  tally() #  count observations by group



# _join() - left, right, inner, full, semi, anti, nest....
comp.data <- transmute(spec.data, 
                       ID_Sex,
                       AttSpd = (Attack + Speed) / 2, 
                       DffSpd = (Defense + Speed) / 2) # creating a new table with new variables
comp.data

full_join(spec.data, comp.data, by="ID_Sex") # joining tables

inner_join(spec.data, comp.data) # joining tables



# distinct()
distinct(spec.data, Species, .keep_all = F) # remove duplicated values 



#### tidyr functions ####
# unite() 
unite(spec.data, Region, Location, col = "Localization", sep = "_") # unite two columns



# separate()
separate(spec.data, ID_Sex, into = c("ID", "Sex"), sep = "_") # separate one column



# separate_row()
separate_rows(spec.data, ID_Sex, sep = "_") # separate into rows



# pivot_wider()
pivot_wider(land.data, names_from = c(Radii, Class), values_from = Cover) # expanding two columns into several



# pivot_longer()
land.pivot <- pivot_wider(land.data, names_from = c(Radii, Class), values_from = Cover)

pivot_longer(land.pivot, cols = 3:11, names_to = "Metric", values_to = "Value") # collapsing several columns into two




# missing values NAs
# drop_na 
drop_na(spec.data, Attack) # Drop rows containing NA’s




# fill
fill(spec.data, Attack) # Fill in NA’s in columns using the next or previous value



# replace_na 
replace_na(spec.data, list(Attack = 1000)) # Specify a value to replace NA in selected columns



#### pipe operator ####
# exemplo I
spor.data <- spec.data %>%
  separate(ID_Sex, into = c("ID", "Sex"), sep = "_") %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(Type = recode(Type, Agua = "Water",
                                Fogo = "Fire",
                                Planta = "Plant")) %>%
  mutate(AttSpd = (Attack + Speed) / 2, 
         DffSpd = (Defense + Speed) / 2) %>%
  rename("Eggs" = Ovos,
         "Height" = Height_cm,
         "Weight" = Weight_kg)

# exemplo II
metr.data <- land.data %>% 
  pivot_wider(names_from = c(Radii, Class), values_from = Cover)


poke.data <- inner_join(spor.data, metr.data) %>% 
  mutate_if(is.character, as.factor)

View(poke.data)

glimpse(poke.data)

str(poke.data)

write_csv(poke.data, "poke.data.csv") # save object as file


# exemplo III
spec.data %>%
  separate(ID_Sex, into = c("ID", "Sex"), sep = "_") %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(Type = recode(Type, Agua = "Water",
                       Fogo = "Fire",
                       Planta = "Plant")) %>%
  mutate(AttSpd = (Attack + Speed) / 2, 
         DffSpd = (Defense + Speed) / 2) %>%
  rename("Eggs" = Ovos,
         "Height" = Height_cm,
         "Weight" = Weight_kg) %>% 
  group_by(Species, Region) %>%
  summarise_if(is.numeric, mean, na.rm = T)



#### ggplot functions ####
# ggplot components
ggplot(data = spec.data, mapping = aes(x = Attack, y = Defense)) # cartesian component 

ggplot(data = spec.data, mapping = aes(x = Attack, y = Defense))+
  geom_point() # geometric component

ggplot(data = spec.data, mapping = aes(x = Attack, y = Defense))+
  geom_point()+
  theme_classic() # theme component

ggplot(data = spec.data, mapping = aes(x = Attack, y = Defense))+
  geom_point()+
  theme_classic()+
  annotate("text", x=80, y=80, label="r2 = 0.5, p = 0.05") # annotate component

ggplot(data = spec.data, mapping = aes(x = Attack, y = Defense))+
  geom_point()+
  theme_classic()+
  annotate("text", x=80, y=80, label="r2 = 0.5, p = 0.05")+
  labs(title = "Atk x Def", x = "Pokemon Attack") # labs component

ggplot(data = spec.data, mapping = aes(x = Attack, y = Defense))+
  geom_point()+
  theme_classic()+
  annotate("text", x=80, y=80, label="r2 = 0.5, p = 0.05")+
  labs(title = "Atk x Def", x = "Pokemon Attack")+
  scale_y_continuous(breaks = c(40, 60, 80)) # scales component
  
ggplot(data = spec.data, mapping = aes(x = Attack, y = Defense))+
  geom_point()+
  theme_classic()+
  annotate("text", x=80, y=80, label="r2 = 0.5, p = 0.05")+
  labs(title = "Atk x Def", x = "Pokemon Attack")+
  scale_y_continuous(breaks = c(40, 60, 80)) +
  facet_wrap(~Species) # wrap component



# ggplot useful verbs
# size
ggplot(data = spec.data, mapping = aes(x = Attack, y = Defense, size = Speed))+
  geom_point()+
  theme(title = element_text(size=20),
        axis.title=element_text(size=12))+
  annotate("text", x=80, y=80, label="r2 = 0.5, p = 0.05", size = 5)+
  labs(title = "Atk x Def", x = "Pokemon Attack")



# color & fill
ggplot(data = spec.data, mapping = aes(x = Attack, y = Defense, color = Species))+
  geom_point(size = 3)+
  theme(title = element_text(size=10),
        axis.title=element_text(size=10))+
  annotate("text", x=80, y=80, label="r2 = 0.5, p = 0.05")+
  labs(title = "Atk x Def", x = "Pokemon Attack")



# shape
ggplot(data = spec.data, mapping = aes(x = Attack, y = Defense, shape = Region))+
  geom_point(size = 3, color = "darkred")+
  theme(title = element_text(size=10),
        axis.title=element_text(size=10))+
  annotate("text", x=80, y=80, label="r2 = 0.5, p = 0.05")+
  labs(title = "Atk x Def", x = "Pokemon Attack")



# alpha
ggplot(data = spec.data, mapping = aes(x = Attack, y = Defense, shape = Region))+
  geom_point(size = 3, color = "darkred", alpha=0.75)+
  theme(title = element_text(size=10),
        axis.title=element_text(size=10))+
  annotate("text", x=80, y=80, label="r2 = 0.5, p = 0.05")+
  labs(title = "Atk x Def", x = "Pokemon Attack")



# ggplot theme verbs
ggplot(data = spec.data, mapping = aes(x = Attack, y = Defense, shape = Region))+
  geom_point(size = 3, color = "darkred", alpha=0.75)+
  theme(plot.title = element_text (size = 12, face = "bold", family = "Helvetica"),
        axis.title = element_text (size = 10, face = "bold", color = "darkred"),
        axis.text = element_text (size = 10, face = "italic", color = "darkblue"),
        legend.title = element_text (size = 10, face = "bold", color = "darkmagenta"),
        legend.text = element_text (size = 10, face = "bold", color = "darkgreen"),
        legend.position = "bottom",
        strip.text=element_text(size=12, face="bold", color = "darkcyan"))+
  facet_wrap(~Species)+
  labs(title = "Atk x Def")
        


#### usual plots ####
# scatter plot 
ggplot(data = poke.data, mapping = aes(x = Attack, y = Defense, shape = Region, 
                                       color = Species, size = Eggs))+
    geom_point(alpha = 0.75)+
    # geom_smooth(method = "lm", alpha = 0.1)+
    theme_classic()+
  theme(plot.title = element_blank(),
        axis.title = element_text (size = 12, face = "bold", color = "#000000"),
        axis.text = element_text (size = 10, color = "#000000"),
        legend.title = element_text (size = 12, face = "bold", color = "#000000"),
        legend.text = element_text (size = 10, color = "#000000"),
        legend.position = "bottom",
        strip.text=element_text(size=12, face="italic", color = "#000000"))+
        scale_color_brewer(palette = "Spectral")+
  # facet_wrap(~Region)+
  labs(x = "Attack", y = "Defense")



# boxplot
ggplot(data = poke.data, mapping = aes(x = Health, y = Species, fill = Type))+
  geom_boxplot(width = 0.5)+
  #geom_jitter(size=1, color="black")+
  theme_classic()+
  theme(plot.title = element_blank(),
        axis.title = element_text (size = 12, face = "bold", color = "#000000"),
        axis.text = element_text (size = 10, color = "#000000"),
        legend.title = element_text (size = 12, face = "bold", color = "#000000"),
        legend.text = element_text (size = 10, color = "#000000"),
        legend.position = "bottom",
        strip.text=element_text(size=12, face="italic", color = "#000000"))+
  scale_fill_manual(values=c("#3366CC", "#990000", "#006666"))+
  #scale_x_discrete(labels = c("Johto", "Kanto", "Sinnoh"))+
  #scale_y_continuous(breaks = c(40, 60, 80))+
  facet_wrap(~Region)+
  labs(x = "Attack", y = "Defense")+
  stat_summary(fun=mean, color="#F2EDD7", position=position_dodge(0.5),
               geom="point", shape=18, size=3, show.legend = FALSE)



# violin plot
ggplot(data = poke.data, mapping = aes(x = Region, y = Speed, fill = Region))+
  geom_violin()+
  geom_jitter(size = 1, width = 0.1,  color = "black")+
  #geom_boxplot(width = 0.1)+
  theme_classic()+
  theme(plot.title = element_blank(),
        axis.title = element_text (size = 12, face = "bold", color = "#000000"),
        axis.text = element_text (size = 10, color = "#000000"),
        legend.title = element_text (size = 12, face = "bold", color = "#000000"),
        legend.text = element_text (size = 10, color = "#000000"),
        legend.position = "bottom",
        strip.text=element_text(size=12, face="italic", color = "#000000"))+
  scale_fill_manual(values=c("#FC345C", "#49BEB7", "#6C3483"))+
  #scale_x_discrete(labels = c("Johto", "Kanto", "Sinnoh"))+
  #scale_y_continuous(breaks = c(40, 60, 80))+
  # facet_wrap(~Region)+
  labs(x = "Region", y = "Defense")+
  stat_summary(fun=mean, color="#F2EDD7", position=position_dodge(0.5),
               geom="point", shape=18, size=3, show.legend = FALSE)



# density plot
ggplot(data = poke.data %>% filter(Region == "Johto") %>% na.omit() 
         , aes(x=Attack, fill=Species, 
                                                      color=Species))+
  geom_density(alpha=0.5)+
  # , adjust=1/0.75)+
  geom_vline(aes(xintercept=mean(Attack)),
             linetype="dashed")+
  facet_wrap(~Type)+
  theme_bw()+
  theme(legend.position="bottom",
        axis.text=element_text(size=8), 
        axis.title=element_text(size=12, face="bold"),
        strip.text=element_text(size=12, face="bold"),
        legend.title=element_text(size=8),
        legend.text=element_text(size=8, face="italic"))+
  labs(title="", x="Attack", y="Density")+
  scale_fill_brewer(palette = "Spectral")+
  scale_color_brewer(palette = "Spectral")



# histogram
ggplot(data = poke.data %>% filter(Region == "Johto") %>% na.omit() 
       , aes(x=Speed, fill=Species, 
             color=Species))+
  geom_histogram(alpha=0.5, binwidth=3,)+
  geom_vline(aes(xintercept=mean(Attack)),
             linetype="dashed")+
  # facet_wrap(~Type)+
  theme_bw()+
  theme(legend.position="bottom",
        axis.text=element_text(size=8), 
        axis.title=element_text(size=12, face="bold"),
        strip.text=element_text(size=12, face="bold"),
        legend.title=element_text(size=8),
        legend.text=element_text(size=8, face="italic"))+
  labs(title="", x="Attack", y="Density")+
  scale_fill_brewer(palette = "Spectral")+
  scale_color_brewer(palette = "Spectral")



# heatmap
ggplot(data = poke.data, aes(x = Species, y = Region, fill = Health))+
  geom_tile()+
  theme_bw()+
  theme(legend.position="bottom",
        axis.text=element_text(size=8), 
        axis.title=element_text(size=12, face="bold"),
        strip.text=element_text(size=12, face="bold"),
        legend.title=element_text(size=8),
        legend.text=element_text(size=8, face="italic"))+
  labs(title="", x="Attack", y="Density")+
  scale_fill_continuous(type = "viridis")



# saving plots
png(filename="FIG.1", width=16, height=9, units="cm", res=600)
plot(FIG1)
dev.off()



#### challenges #### 
View(spec.data)












# 1
squi.data <- spec.data %>%
  mutate(AttSpd = (Attack + Speed) / 2) %>% 
  filter(Species == "Squirtle" & AttSpd > 50)

squi.data

# 2
plan.data <- spec.data %>%
  mutate(Type = recode(Type, Agua = "Water",
                       Fogo = "Fire",
                       Planta = "Plant")) %>%
  filter(Type == "Plant" & Location == "Violet")
  
plan.data

# 3
feme.data <- spec.data %>%
  separate(ID_Sex, into = c("ID", "Sex"), sep = "_") %>%
  mutate(Type = recode(Type, Agua = "Water",
                       Fogo = "Fire",
                       Planta = "Plant")) %>%
  filter(Sex == "Female" & Type == "Fire" | Health >= 50)
  
View(feme.data)

# 4
wate.data <- spec.data %>%
  mutate(Type = recode(Type, Agua = "Water",
                       Fogo = "Fire",
                       Planta = "Plant")) %>%
  filter(Type == "Water") %>% 
  group_by(Species, Region) %>%
  summarise_if(is.numeric, mean, na.rm = T)
  
wate.data  
  
# 5
FIG.POK.1 <- ggplot(data=poke.data, mapping = aes(x = reorder(Species, -AttSpd, na.rm = T), 
                                                  y = AttSpd, fill = Sex))+
  geom_violin()+
  geom_boxplot(width = 0.1, position=position_dodge(0.9))+
  theme_classic()+
  theme(plot.title = element_blank(),
        axis.title = element_text (size = 12, face = "bold", color = "#000000"),
        axis.text = element_text (size = 10, color = "#000000"),
        legend.title = element_text (size = 12, face = "bold", color = "#000000"),
        legend.text = element_text (size = 10, color = "#000000"),
        legend.position = "bottom",
        strip.text=element_text(size=12, face="italic", color = "#000000"))+
  scale_fill_manual(values=c("#FC345C", "#49BEB7"))+
  labs(x = "Species", y = "Attack Speed")+
  stat_summary(fun=mean, color="#F2EDD7", position=position_dodge(0.9),
               geom="point", shape=18, size=3, show.legend = FALSE)
FIG.POK.1 



FIG.POK.2 <- ggplot(data=poke.data %>%
                      mutate(Species = factor(Species)) %>% 
                      mutate(Species = fct_relevel(Species, c("Charmander", 
                                                        "Bulbassaur",
                                                        "Squirtle",
                                                        "Cyndaquil",
                                                        "Chikorita",
                                                        "Totodile",
                                                        "Torchic",
                                                        "Treecko",
                                                        "Mudkip"))) %>%
                      arrange(Species),
                      mapping = aes(x = Attack, y = Health,
                                                  shape = Region, color = Sex))+
  geom_smooth(method = "lm", aes(group = Species), color = "black")+
  geom_point(size = 3, alpha = 0.75)+
  facet_wrap(~Species)+
  theme_classic()+
  theme(plot.title = element_blank(),
        axis.title = element_text (size = 12, face = "bold", color = "#000000"),
        axis.text = element_text (size = 10, color = "#000000"),
        legend.title = element_text (size = 12, face = "bold", color = "#000000"),
        legend.text = element_text (size = 10, color = "#000000"),
        legend.position = "bottom",
        strip.text=element_text(size=12, face="italic", color = "#000000"))+
  scale_fill_manual(values=c("#FC345C", "#49BEB7"))+
  labs(x = "Attack", y = "Health")

FIG.POK.2