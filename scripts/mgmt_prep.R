###mgmt test prep data for modelling
##updated from mgmt_test 10.01.22

#clean up 
rm(list = ls())

#create not in
#`` what you need for filtering special column names

'%ni%' <- Negate('%in%')

#pabbm functions

source("./scripts/background/functions_PABBM.R")

#met colour combos
source("./scripts/background/mets.R")

###load packages

library("tidyverse"); theme_set(theme_classic())
library("rlang") #for using {{}} to integrate dplyr with ggplot 
library("lubridate")
library("janitor")
#library("bayestestR") #masks hdi from tidybayes, #for posterior playing
library("brms")
library("gridExtra")
library("grid") #for adding title to grid.dplyr::arrange
library("rstan") #for plotting posteriors
#library("coda") #for function HPDinterval #masks traceplot for rstan
library("scales") #for ggplot scale fixing
library("bayesplot") #for bayes ggploting
library("cowplot")
library("tidybayes") #bayes diagnostics and some summary stuff
library("MetBrewer")
library("broom")

##STEP 1: IMPORT DATA

#village levels given excluding E4 & E10 (not enough data) and C8 (potentially erroneous)

village_levels <- paste0(c(rep("C",10),rep("E",10)), 
                         rep(seq(1:10),2))

#excluding E4 & E10 (not enough data) and C8 (potentially erroneous)
#but all bad data already removed in AJE cleaning, so no need

village_levels <- village_levels[village_levels %ni% c("C8","E4","E10")]

#village 
village_meta <- readRDS("./inputs/meta/village_meta.rds") %>% 
  filter(village %in% village_levels) %>% 
  dplyr::mutate_if(is.character,funs(factor(.))) %>% 
  dplyr::mutate(village = factor(village, levels = village_levels),
                mgmt = factor(mgmt, levels = c("Not proposed", "Before project",
                                               "Implemented", "Unimplemented"))
  ) %>% 
  arrange(village)

village_meta$post_med <- dmy(village_meta$post_med)

#species
species_meta <- readRDS("./inputs/meta/species_meta_traits.rds") %>% 
  dplyr::mutate_if(is.character,funs(factor(.)))

#bushmeat
bushmeat <- readRDS("./inputs/mgmt/bushmeat_transects.rds") %>% 
  dplyr::mutate_if(is.character,funs(factor(.))) %>% 
  filter(village %in% village_levels) %>% 
  dplyr::mutate(village = factor(village, levels = village_levels))

#hunter follows
huntmeat <- readRDS("./inputs/mgmt/huntmeat.rds")
gun_all <- readRDS("./inputs/mgmt/bayes_hunts_inputs_gun.rds")
trap_all <- readRDS("./inputs/mgmt/bayes_hunts_inputs_trap.rds")

#cut hunt longer than 48 hours (only 8 hunts, with 70 hunted animals in them)

longhunts <- gun_all %>% 
  filter(gps_hours > 48) %>% 
  pull(hunt) %>% 
  droplevels()

huntmeat  <- huntmeat %>% 
  filter(hunt %ni% longhunts)

gun_all  <- gun_all %>% 
  filter(hunt %ni% longhunts)

trap_all <- trap_all %>% 
  filter(hunt %ni% longhunts)

#offtake
offtake <- readRDS("./inputs/mgmt/offtake_transects.rds") %>% 
  dplyr::mutate_if(is.character,funs(factor(.))) %>% 
  filter(village %in% village_levels) %>% 
  dplyr::mutate(village = factor(village, levels = village_levels))

##village sales
village_sales <- readRDS("./inputs/mgmt/village_summary.rds") %>% 
  dplyr::mutate_if(is.character,funs(factor(.))) %>% 
  filter(village %in% village_levels) %>% 
  dplyr::mutate(village = factor(village, levels = village_levels)) %>% 
  dplyr::select(village,p_sale = `% Sale`, mean_xaf = `Mean price (FCFA)`) %>% 
  dplyr::mutate(p_sale = p_sale/100) %>% 
  arrange(village)

#effort
effort <- readRDS("./inputs/mgmt/effort_transects.rds") %>% 
  dplyr::mutate_if(is.character,funs(factor(.))) %>% 
  filter(village %in% village_levels) %>% 
  dplyr::mutate(village = factor(village, levels = village_levels)) %>% 
  dplyr::mutate(empty_prop = homes_empty / homes_total)

offtake %>% 
  filter(villageday %ni% effort$villageday)

##prop empty by day, and visited of present households
#rethink about min and max
homes_presence <- effort %>% 
  group_by(villageday) %>% 
  dplyr::summarize(empty_prop = min(empty_prop),
                   visited_prop = max(homes_prop))

##size of villages, proportion empty

effperiod <- left_join(effort,
village_meta %>% 
  dplyr::select(village,type,gestion_s14, mgmt, post_med)
) %>% 
  dplyr::mutate(period = factor(if_else(date >= post_med,
                                        "post",
                                        "pre"), levels = c("pre", "post"))
                )

effperiod %>% 
  group_by(village) %>% 
  summarize(homes_total = median(homes_total),
            empty_prop = mean(empty_prop))

effperiod %>% 
  group_by(village,period) %>% 
  summarize(homes_total = median(homes_total),
            empty_prop = mean(empty_prop)) %>% 
  pivot_wider(names_from = "period", 
              values_from = c("homes_total", "empty_prop"))

#STEP 2: PREPARE DATA: SUMMARIZE OFFTAKE & EFFORT BEFORE AND AFTER RULES

#add 0s to offtake for the days without any

#sum by species

offtake_species_sums <- offtake %>%
  group_by(villageday, species) %>%
  dplyr::summarise(total = n()) 

#create tibble of each day of effort by village

make_offjam <- as_tibble(unique(effort$villageday))
colnames(make_offjam) <- "villageday"

#make a GJAM-ready matrix of effort days x species sums

offtake_wide <- spread(offtake_species_sums, key = species, value = total, fill = 0)

offjam <- left_join(make_offjam, offtake_wide, by = c("villageday")) %>%
  replace(is.na(.), 0) 

offjam_matrix <- column_to_rownames(offjam, "villageday")

###create a tibble of total offtake with 0s
#for now, just # animals... later can do by species or biomass

offtake_total <- make_offjam %>% 
  dplyr::mutate(bushmeat = offjam %>% 
                  dplyr::select(-villageday) %>% 
                  rowSums())

offtake_total$village <- as.factor(sub('\\_.*', '', offtake_total$villageday))
offtake_total$date <- ymd(sub('.*\\_', '',  offtake_total$villageday))

offtake_total <- offtake_total %>% 
  dplyr::select(villageday,village,date,bushmeat)

##join proportion of empty homes

identical(offtake_total$villageday, homes_presence$villageday)

offtake_total <- left_join(offtake_total, homes_presence)

##offtake of 18 hunted threatened species

redlist <- species_meta %>% 
  filter (IUCN %in% c("NT", "VU", "EN", "CR")
          & species %in% bushmeat$species 
  ) %>% 
  pull(species) %>% 
  droplevels()

offtake_threat <- make_offjam %>% 
  dplyr::mutate(threatened = offjam %>% 
                  dplyr::select(redlist) %>% 
                  rowSums())

offtake_total <- left_join(offtake_total, offtake_threat)

#add money

income <- bushmeat %>% 
  group_by(villageday) %>% 
  dplyr::summarize(xaf = sum(price, na.rm = TRUE))

income <- left_join(make_offjam, income) %>%
  replace(is.na(.), 0) 

offtake_total <- left_join(offtake_total, income) %>% 
  dplyr::select(villageday,village,date,bushmeat,threatened,xaf,
         empty_prop,visited_prop)

#froese et al. 2022 capture rate 53%, sale rate 66%, mean xaf 8000
#x2 check mean xaf only when sold (it is)

bushmeat %>%
  filter(number == 1 & state == "E") %>% 
  dplyr::summarize(xaf = mean(price, na.rm = TRUE))

bushmeat %>% 
  filter(number == 1 & state == "E" & use == "V") %>% 
  dplyr::summarize(xaf = mean(price, na.rm = TRUE))

#backup
oftb <- offtake_total

#estimate daily true values using AJE

capture_rate <- 0.528
p_threatened <- round(sum(offtake_total$threatened)/sum(offtake_total$bushmeat),3)

E4_sales <- left_join(
  bushmeat %>% 
    filter(village == "E4") %>% 
    summarise(p_sale = round(mean(use == "V", na.rm = TRUE),2),
              village = unique(village)),
  bushmeat %>% 
    filter(village == "E4") %>% 
    filter(state == "E" & use == "V" |
             state == "ED" & use == "V" |
             state == "EF" & use == "V") %>%
    summarise(mean_xaf = round(mean(price, na.rm = TRUE)/500,0)*500,
              village = unique(village))
)

village_xaf <- bind_rows(village_sales,E4_sales) %>% 
  arrange(village)

#reset if needed
#offtake_total <- oftb

offtake_total <- left_join(offtake_total,village_xaf) %>% 
  dplyr::mutate(bushmeat_e = round((bushmeat/capture_rate),0),
                threatened_e = threatened + round((bushmeat_e - bushmeat)*p_threatened,0),
                xaf_e = xaf + round((bushmeat_e - bushmeat)*p_sale,0)*mean_xaf
  ) %>% 
  select(villageday:bushmeat,bushmeat_e,
         threatened,threatened_e,
         xaf,xaf_e,
         empty_prop,visited_prop)

#make sure daily correlations right
#far less so for estimated earnings

plot(offtake_total$bushmeat_e ~ offtake_total$bushmeat, pch = 19)
cor(offtake_total$bushmeat_e,offtake_total$bushmeat)

plot(offtake_total$xaf_e ~ offtake_total$xaf, pch = 19)
cor(offtake_total$xaf_e,offtake_total$xaf)

#xaf_e and xaf diff correlations to bushmeat (higher when estimated, makes sense)
plot(offtake_total$xaf ~ offtake_total$bushmeat, pch = 19)
cor(offtake_total$xaf,offtake_total$bushmeat)

plot(offtake_total$xaf_e ~ offtake_total$bushmeat_e, pch = 19)
cor(offtake_total$xaf_e,offtake_total$bushmeat_e)

##threatened species correlate to hunted, though not perfectly
#again, higher when estimated

plot(offtake_total$threatened ~ offtake_total$bushmeat, pch = 19)
cor(offtake_total$threatened,offtake_total$bushmeat)

plot(offtake_total$threatened_e ~ offtake_total$bushmeat_e, pch = 19)
cor(offtake_total$threatened_e,offtake_total$bushmeat_e)

offtake_period <- left_join(
  offtake_total,
  village_meta %>% 
    dplyr::select(village,type,gestion_s14, mgmt, post_med)
) %>% 
  dplyr::mutate(period = factor(if_else(date >= post_med,
                                        "post",
                                        "pre"), levels = c("pre", "post"))) %>% 
  dplyr::rename(animals = bushmeat, animals_e = bushmeat_e)

##

#CONFORMITY

##individual hunts
#first summarize known conformity

#guntrap hunts have same inputs, good
bind_rows(
gun_all %>% filter(guntrap == "both"),
trap_all %>% filter(guntrap == "both")
) %>% 
  arrange(hunt) %>% 
  View()

#but different offtake
bind_rows(
  gun_all %>% filter(guntrap == "both"),
  trap_all %>% filter(guntrap == "both")
) %>% 
  arrange(hunt) %>% 
  dplyr::select(hunt,guntrap,offtake,IUCN,
                kg,money) %>% 
  View()

#902 unique hunts (correct)
fins <- bind_rows(
  gun_all,
  trap_all %>% filter(guntrap == "trap")
) %>% 
  dplyr::select(-c(offtake:asold)) %>% 
  arrange(hunt) 

length(unique(fins$hunt))

#add offtake for gun, trap, and both
gun_off <- gun_all %>% 
  dplyr::select(hunt,guntrap,
                animals_gun = offtake, 
                threatened_gun = IUCN,
                kg_gun = kg,
                xaf_gun = money)

trap_off <- trap_all %>% 
  dplyr::select(hunt,guntrap,
                animals_trap = offtake, 
                threatened_trap = IUCN,
                kg_trap = kg,
                xaf_trap = money)

all_off <- bind_rows(
full_join(gun_off %>% filter(guntrap == "both"),
          trap_off %>% filter(guntrap == "both")) %>% 
  mutate(animals = animals_gun + animals_trap, 
         threatened = threatened_gun + threatened_trap,
         kg = kg_gun + kg_trap,
         xaf = xaf_gun + xaf_trap),
left_join(gun_off %>% filter(guntrap == "gun"), trap_off) %>% 
  mutate(animals = animals_gun, 
         threatened = threatened_gun,
         kg = kg_gun,
         xaf = xaf_gun),
left_join(trap_off %>% filter(guntrap == "trap"), gun_off) %>% 
  mutate(animals = animals_trap, 
         threatened = threatened_trap,
         kg = kg_trap,
         xaf = xaf_trap)
) %>% 
  arrange(hunt) 
  
follows_full <- left_join(fins,all_off)

#check that all is well (it is)
follows_full %>% 
  dplyr::select(c(hunt,guntrap,animals_gun:xaf)) %>% 
  print(n = 22)

follows_full %>% 
  dplyr::select(c(hunt,guntrap,animals_gun:xaf)) %>% 
  filter(guntrap == "both") %>% 
  print(n = 22)

follows_full %>% 
  pull(animals) %>% 
  sum()

1333-70

gun_all %>% 
  filter(hunt %ni% longhunts)

#keep what you want
follows <- follows_full %>% 
  filter(village %ni% c("E4", "E10")) %>% 
  dplyr::select(hunt:guntrap,start_date,month,
                ammo_brought = total_ammo_brought, 
                ammo_used = total_ammo_used,
                traps = traps_checked, 
                side_road,
                animals_gun:xaf) %>% 
  droplevels()

levels(follows$village)

##E8 conformity

follows <- left_join(
  follows,
  village_meta %>% 
    dplyr::select(village,post_med)
) %>% 
  dplyr::mutate(period = factor(if_else(start_date >= post_med,
                                        "post",
                                        "pre"), levels = c("pre", "post")))

followsE8 <- follows %>% filter(village == "E8") 
followsE6 <- follows %>% filter(village == "E6") 

known_conf_E8 <- followsE8 %>% 
  group_by(period) %>% 
  dplyr::summarize(n_hunts = n(),
                   n_north = sum(side_road == "N", na.rm = TRUE),
                   p_north = round(mean(side_road == "N", na.rm = TRUE),2)*100,
                   n_animals3 = sum(animals > 3, na.rm = TRUE),
                   p_animals3 = round(mean(animals > 3, na.rm = TRUE),2)*100,
  )

#how often were > 3 animals PER species hunted?
#post, never (most was 3 CBL)
#pre 1 at confirmed, E8_H01_s5_p2 (4 CBL)
#2 to investigate, E8_H04_s7_p1 and E8_H04_s8_p1

animals3E8 <- followsE8 %>% 
  dplyr::filter(village == "E8" & animals > 3)

animals3E8 

#only 1 time (E8_H01_s5_p2, 4 CBL)
huntmeat %>% 
  filter(hunt %in% animals3E8$hunt) %>% 
  dplyr::select(hunt,date_hunted,species,state,number) %>% 
  group_by(hunt,species) %>% 
  summarize(n = sum(number)) %>% 
  arrange(desc(n))

followsE8 %>% 
  filter(hunt == "E8_H01_s5_p2") %>% 
  pull(period)

#add the one %>% pre-mgmt with > 3 of one species
known_conf_E8 <- known_conf_E8 %>% 
  mutate(n_species3 = c(1,0),
         p_species3 = c(round(1/55,2)*100,0),
  ) 

#add traps 

ntrapsE8 <- followsE8 %>% 
  filter(traphunt == "yes") %>% 
  group_by(hunter,period) %>% 
  summarize(ntraps = max(traps))

trap50E8 <- ntrapsE8 %>% 
  group_by(period) %>% 
  dplyr::summarize(n_trappers = n(),
                   n_trap50 = sum(ntraps > 50, na.rm = TRUE),
                   p_trap50 = round(mean(ntraps > 50, na.rm = TRUE),2)*100,
  )

known_conf_E8 <- left_join(known_conf_E8,trap50E8)  

#E6
known_conf_E6 <- followsE6 %>% 
  group_by(period) %>% 
  dplyr::summarize(n_hunts = n(),
                   n_ammo6 = sum(ammo_brought > 6, na.rm = TRUE),
                   p_ammo6 = round(mean(ammo_brought > 6, na.rm = TRUE),2)*100,
                   n_animals5 = sum(animals > 5, na.rm = TRUE),
                   p_animals5 = round(mean(animals > 5, na.rm = TRUE),2)*100,
  )

#giving 5th? 8 hunts with at least 5
animals5E6 <-followsE6 %>% 
  filter(animals >= 5)

#5 of these sold more than 4
sold5E6 <- huntmeat %>% 
  filter(hunt %in% animals5E6$hunt) %>% 
  dplyr::select(hunt,date_hunted,species,state,number,use) %>% 
  group_by(hunt) %>% 
  summarize(n_sold = sum(use == "V")) %>% 
  filter(n_sold > 4) %>% 
  ungroup()

#all these pre, but difficult rule to assess "norm" for before regardless
followsE6 %>% 
  filter(hunt %in% sold5E6$hunt) %>% 
  group_by(period) %>% 
  summarize(n_hunts = n())

known_conf_E6

#hunts per week

min(followsE6$start_date) #WED june 12 2019
max(followsE6$start_date) #THURS march 5th 2020

#lubridate and manual approach to get dates

fmon <- floor_date(min(followsE6$start_date), unit = "week", week_start = 1)
fmon <- ymd("2019-06-10") #first monday

lsun <- ceiling_date(max(followsE6$start_date), unit = "week")
lsun <- ymd("2020-03-08") #last sunday

lmon <- ceiling_date(max(followsE6$start_date), unit = "week", week_start = 1)
lmon <- ymd("2020-03-09") #last monday

mgmt_debutE6 <- ymd("2019-10-07")

#ymd("2019-06-09") %in% 

w1E6seq <- seq(
floor_date(min(followsE6$start_date), unit = "week", week_start = 1),
ceiling_date(min(followsE6$start_date), unit = "week"),
by = "days"
)

w1E6int <-interval(min(w1E6seq),max(w1E6seq))

w1E6seq

#39 unique weeks 
length(seq(fmon,lsun, by = "week"))
length(seq(fmon,lsun, by = "day"))/7
as.numeric(lmon - fmon) / 7

tmptx <- 0:38

E6weeks <- tibble(uweek = 1:39,
                  wdeb = min(w1E6seq), wfin = max(w1E6seq)
                  )

seqweek_list <- list()

for (i in seq_along(tmptx)) {
  seqweek_list[[i]] <- w1E6seq+(7*tmptx[i])
  E6weeks$wdeb[i] <- min(seqweek_list[[i]])
  E6weeks$wfin[i] <- max(seqweek_list[[i]])
}

E6weeks <- E6weeks %>% 
  mutate(wint = interval(wdeb,wfin))

max(followsE6$start_date) %within% E6weeks$wint 

##keep just your dates 
hwlongE6 <- followsE6 %>% 
  mutate(wkday = wday(start_date, label=TRUE)) %>% 
  dplyr::select(hunt:guntrap,period,
                start_date,month,wkday) %>% 
  arrange(hunter,session,partie) %>% 
  mutate(uweek = NA)

for (i in seq_along(1:nrow(hwlongE6))) {
  
  hwlongE6$uweek[i] <- which(
    hwlongE6$start_date[i] %within% E6weeks$wint == TRUE
  ) 
  
}

hwlongE6 <- left_join(hwlongE6, E6weeks) %>% 
  dplyr::select(-c(wdeb,wfin))

hwlongE6 %>% print(n = Inf)

hunts_weekE6 <- hwlongE6 %>%
  group_by(hunter,period,uweek) %>%
  dplyr::summarize(n_hunts = n())

hunts_weekE6 %>% arrange(desc(n_hunts))

hwE6 <- hunts_weekE6 %>%
  group_by(period) %>%
  dplyr::summarize(n_hunter_weeks = n(),
                   n_hw2 = sum(n_hunts > 2, na.rm = TRUE),
                   p_hw2 = round(mean(n_hunts > 2, na.rm = TRUE),2)*100,
  )

#add it
known_conf_E6 <- left_join(known_conf_E6, hwE6)

#add traps

ntrapsE6 <- followsE6 %>% 
  filter(traphunt == "yes") %>% 
  group_by(hunter,period) %>% 
  summarize(ntraps = max(traps))

trap150E6 <- ntrapsE6 %>% 
  group_by(period) %>% 
  dplyr::summarize(n_trappers = n(),
                   n_trap150 = sum(ntraps > 150, na.rm = TRUE),
                   p_trap150 = round(mean(ntraps > 150, na.rm = TRUE),2)*100,
  )

known_conf_E6 <- left_join(known_conf_E6,trap150E6)  

#to resume..
#E8 traps > 50 went from 15 to 9% (1 to 2 trappers)
#north was respected w/ gps (as expected)
#> 3 same species was unheard of (1 hunt) even before mgmt
#E6 hunts with >6 ammo went from 22% to 0, >5 animals from 5% to 0%
#tracked hunters pre mgmt very rarely recorded >2 hunts per week (just twice)
#1 trapper with excessive traps (>200) diminished them
known_conf_E8
known_conf_E6

known_conf <- list(known_conf_E8,known_conf_E6)
names(known_conf) <- c("kcE8","kcE6")

##E6 predict decreases in offtake

#5 gun hunters doing self-follows
nhuntsE6 <- followsE6 %>% 
  group_by(hunter,guntrap) %>% 
  summarize(n_hunt = n()) %>% 
  pivot_wider(names_from = guntrap, 
              values_from = n_hunt) %>% 
  mutate(total = rowSums(across(both:gun), na.rm = T))

ghuntersE6 <- (nhuntsE6 %>% pull(hunter))[2:6] %>% droplevels()

gwhuntsE6 <- hunts_weekE6 %>% 
  filter(hunter %in% ghuntersE6 & period == "pre") %>% 
  group_by(hunter) %>% 
  summarize(mean_nwhunts = mean(n_hunts))

mean(gwhuntsE6$mean_nwhunts)
quantile(gwhuntsE6$mean_nwhunts)

#so 52 weeks in a year 5 gun hunters
#from that estimated 390 gun hunts/yr

5*1.5*52

#ammo

followsE6ammo <- followsE6 %>% 
  mutate(ammo_ruled = case_when(ammo_brought > 6 ~ 6,
                                ammo_brought < 6 ~ ammo_brought,
                                TRUE ~ ammo_brought)
         ) %>% 
  dplyr::select(hunt:ammo_brought,ammo_ruled,everything())

#33 less cartridges in input overall
sum(
followsE6ammo$ammo_brought -
followsE6ammo$ammo_ruled
)

fE6ammo <- followsE6ammo %>% 
  filter(hunter %in% ghuntersE6 & 
           guntrap != "trap" &
           ammo_used > 0
         ) %>% 
  dplyr::select(hunt,hunter,period,
                ammo_brought,ammo_used,
                animals_gun) %>% 
  mutate(wasted_ammo = ammo_used - animals_gun,
         shooting_percentage = animals_gun/ammo_used)

#-% are b/c machete kills or NA on how killed
huntmeat %>% 
  filter(hunt %in% 
(fE6ammo  %>% 
  filter(wasted_ammo < 0) %>% 
  pull(hunt))
) %>% 
  print(n = Inf)

fE6ammo <- fE6ammo %>% 
  mutate(wasted_ammo = case_when(wasted_ammo < 0 ~ 0,
                                wasted_ammo  >= 0 ~ wasted_ammo,
                                       TRUE ~ wasted_ammo),
         shooting_percentage = case_when(shooting_percentage  > 1 ~ 1,
                                         shooting_percentage   <= 1 ~ shooting_percentage, 
                                TRUE ~ shooting_percentage)
         )

fE6ammo %>% 
  arrange(desc(ammo_brought)) %>% 
  print(n = Inf)

quantile(fE6ammo$wasted_ammo)
mean(fE6ammo$wasted_ammo)

quantile(fE6ammo$shooting_percentage)
mean(fE6ammo$shooting_percentage)

#excess animals?
#mean 1.5 per hunt
known_conf$kcE6

mean(followsE6 %>% 
  filter(period == "pre") %>% 
  filter(animals > 5) %>% 
  pull(animals) - 5)

#1.25 animals extra 4% of hunts
#only around 20 animals (neglible)

390*.04*1.25

#remember 5*1.5*52 = ~390 hunts a year

5*1.5*52

#~2.45 animals per hunt
followsE6 %>% 
  group_by(period) %>% 
  filter(hunter %in% ghuntersE6) %>% 
  dplyr::summarize(ma = mean(animals),
                   mag = mean(animals_gun, na.rm = TRUE),
                   mat = mean(animals_trap, na.rm = TRUE)
  )

#this only gives 950-1000 animals/year
5*1.5*52*2.5

opE6 <- offtake_period %>% 
  filter(village == "E6") %>% 
  #estimated local rate E6 is 0.623
  dplyr::mutate(animals_el = round((animals/0.623),0)) %>% 
  group_by(period) %>% 
  summarize(ma = mean(animals),
            mae = mean(animals_e),
            mael = mean(animals_el), 
  )

#which is less even than the OBSERVED ~1300
#far less than estimated 2000-2500
opE6
opE6an <- round(opE6*365,0) %>% mutate(period = c("pre", "post"))
opE6an

#note that reduction of ~2.5-3/day is ~900-1000/year
#so capture rate used not super important for estimating difference
opE6[2,3:4]-opE6[1,3:4]
opE6an[2,3:4]-opE6an[1,3:4]

#so either there are more hunters
#more hunts per hunter
#or more animals per hunt

#explore via transect data
#heuristic use of date-time combo as "hunt"
#animals per hunt

#check when not entire
bushmeat %>% 
  filter(village == "E6") %>% 
  filter(date < mgmt_debutE6) %>% 
  group_by(state) %>% 
  dplyr::summarize(n = n())

bushmeat %>% 
  filter(village == "E6" & state != "E") %>% 
  filter(date < mgmt_debutE6) %>% 
  print(n = Inf)

dtbmE6 <- bushmeat %>% 
  #filter(village == "E6") %>% 
  filter(village == "E6" & state == "E") %>% #change minor w/ this
  filter(date < mgmt_debutE6) %>% 
  group_by(date,time) %>% 
  summarize(n_animals_rough = n()) 

#similar >5 animals to follows!(~6 vs ~4)
#makes sense to be slightly higher (e.g., 2 hunters offtake same house on transects)
dtbmE6 %>% filter(n_animals_rough > 5) %>% nrow() /
  nrow(dtbmE6)

length(dtbmE6$n_animals_rough)
length(dtbmE6$n_animals_rough)

#~2 (slightly lower than 2.45, as expected)
mean(dtbmE6$n_animals_rough)
mean(dtbmE6$n_animals_rough[2:176])

#hunts in time period?

preE6dates <- effort %>% 
  filter(village == "E6" & date < mgmt_debutE6) %>% 
  pull(date) %>% 
  unique()

preE6bmdates <- dtbmE6 %>% 
  pull(date) %>% 
  unique()

#123 sampling days before mgmt
#feb-end september (8 months)
#3 weeks 5 days per week

length(preE6dates)
8*3*5

#90 of those sampling days w/ bushmeat
length(preE6bmdates)

#~1.4 hunts per day
nrow(dtbmE6) / length(preE6dates)

#i.e., around 10 per week
#e.g., 10 hunters 1 hunts/week or 5 hunters 2 hunts/week
#6 or 7/1.5 a week

1.4*7

#this number tracks to OBSERVED bushmeat
1.4*7*52*2.5

opE6
round(opE6*365,0)

#so assume 0.623 capture rate is more... 
#...due to missing entire hunts than animals
#(^makes sense also given similar rates of >5 animals)

#~16 hunts a week lands at bottom of 2000-2500 range
#~19 hunts a week near top
10/0.623
10/0.528

16*52*2.5
19*52*2.5

#some possible combos of hunters/ hunters per week
10*1.5
8*2
5*3

10*2
7*3
6*3

#say 5 hunters doing 1.5 a week
#but 3 do 3 hunts...that would be 17
#or 4 did 4 hunts...taht would be around 20
(5*1.5) + (3*3)
(5*1.5) + (4*3)

#850-1000 hunts a year
((5*1.5) + (3*3))*52
((5*1.5) + (4*3))*52

##still only 40 or 50 less animals a year due to limit (still neglible)
850*.04*1.25
1000*.04*1.25

#if capped you would reduce 100-200 hunts a year
#which would be 400-800 animals a year (20-30% decrease, 15-20% less than observe)
((5*1.5) + (3*2))*52
((5*1.5) + (4*2))*52

((5*1.5) + (3*3))*52*2.5-
  ((5*1.5) + (3*2))*52*2.5

((5*1.5) + (4*3))*52*2.5-
  ((5*1.5) + (3*2))*52*2.5
  
#if coupled with 1 animal per hunt decrease (as modelled)
#1000-1400 animals decrease (50-60% decrease, ~5-15% more than modelled)

((5*1.5) + (3*3))*52*2.5-
  ((5*1.5) + (3*2))*52*1.5

((5*1.5) + (4*3))*52*2.5-
  ((5*1.5) + (3*2))*52*1.5

#oh lastly 
#E6 hunter said km walked had decreased

colnames(follows_full)

kmv_prepost <- function (vc) {
  
    vc_tmp <- follows_full %>% 
      filter(village == {{vc}})
    
    vc2_tmp <- follows %>% 
      filter(village == {{vc}})
  
    left_join(vc_tmp %>% 
      dplyr::select(hunt:guntrap,gps,
                    start_fin_diff_km,ratio_walk,
                    mean_km_village,max_km_village,animals),
    vc2_tmp %>% dplyr::select(hunt,period)
  ) %>% 
    group_by(hunter,period) %>% 
    summarize(mekmv = mean(mean_km_village, na.rm = TRUE),
              makmv = mean(max_km_village, na.rm = TRUE),
    ) %>% 
    pivot_wider(names_from = period, 
                values_from = c(mekmv,makmv)) %>% 
    mutate(mekmv_diff = mekmv_post - mekmv_pre,
           makmv_diff = makmv_post - makmv_pre,
    ) %>% 
    dplyr::select(hunter,mekmv_pre, mekmv_post, mekmv_diff,
                  makmv_pre, makmv_post, makmv_diff)
  
}

kmvE6 <- kmv_prepost("E6")

mean(kmvE6$makmv_diff)
mean(kmvE6$mekmv_diff)

fvl <- unique(follows$village)

kmv_list <- list()

kmv_comp <- tibble(village = fvl, mekmv = NA, makmv = NA)

for (i in seq_along(fvl)) {
  kmv_tmp <- kmv_prepost(as.character(fvl[[i]]))
  kmv_comp$mekmv[[i]] <- mean(kmv_tmp$mekmv_diff, na.rm = TRUE)
  kmv_comp$makmv[[i]]<- mean(kmv_tmp$makmv_diff, na.rm = TRUE)
}

#it had indeed, though this is a trend across villages
#decreased effort, or season bringing ease?
kmv_comp

#similar story in transects, though less consistent
kmv_comp_trans <- left_join(bushmeat,
offtake_period %>% dplyr::select(village,date,period),
by = c("village","date")
) %>% 
  group_by(village,period) %>% 
  dplyr::summarize(mean_km = mean(km, na.rm = TRUE)) %>% 
  pivot_wider(names_from = period, 
              values_from = mean_km) %>% 
  mutate(diff = post - pre)

kmv_comp_trans

#E8 and E7 said saw more animals (E6 too?)
#in terms of hunted/km walked, diff is neglible across villages

makm_prepost <- function (vc) {
  
  vc_tmp <- follows_full %>% 
    filter(village == {{vc}}) %>% 
    filter(ratio_walk > 1)
  
  vc2_tmp <- follows %>% 
    filter(village == {{vc}})
  
  left_join(vc_tmp %>% 
              dplyr::select(hunt:guntrap,
                            total_km_walked,animals),
            vc2_tmp %>% dplyr::select(hunt,period)
  ) %>% 
    group_by(hunter,period) %>% 
    mutate(animals_km = animals/total_km_walked) %>% 
    summarize(makm = mean(animals_km, na.rm = TRUE)) %>% 
    pivot_wider(names_from = period, 
                values_from = makm) %>% 
    mutate(makm_diff = post - pre) %>% 
    dplyr::select(hunter,pre, post, makm_diff)
  
}

makm_prepost("E8")

makm_comp <- tibble(village = fvl, makm = NA)

for (i in seq_along(fvl)) {
  makm_tmp <- makm_prepost(as.character(fvl[[i]]))
  makm_comp$makm[[i]] <- mean(makm_tmp$makm_diff, na.rm = TRUE)
}

makm_comp 

#E8 species composition changing (weight per animal?)
#remains the same

followsE8 %>% 
  mutate(kg_animal = kg/animals) %>% 
  group_by(period) %>% 
  summarize(kg_animal = mean(kg_animal, na.rm = TRUE))

followsE6 <- follows %>% filter(village == "E6")
gunfE6 <- followsE6 %>% filter(guntrap != "trap")

##TEST EXPECTED CHANGE IN E6 
#given reducing ammo to <= 6 and animals to <= 5
#and reducing n hunts via limiting hunters to 2 per week
#animals decreases (as seen when modelled with other villages)

gunfE6 %>% 
  group_by(period) %>% 
  summarize(ammo_brought = mean(ammo_brought, na.rm = TRUE),
            animals = mean(animals, na.rm = TRUE)
            )

#not unique in this (E5 aussi), but high relative to most villages 
#E3 also ~1 less ammo, and way less km 
#rest of villages general decline in km (e.g., E7, E1 quite high)
#so villages w/ decreased offtake seem to be driven by less ammo (not less km_walked)

left_join(follows_full, 
          follows %>% dplyr::select(hunt,period,ammo_brought)) %>%  
  filter(guntrap != "trap" & village %ni% c("E4", "E10") ) %>% 
  dplyr::select(village,period,max_km_village,ammo_brought) %>% 
  group_by(village,period) %>% 
  summarize(ammo_brought = mean(ammo_brought, na.rm = TRUE),
            max_km_village = mean(max_km_village, na.rm = TRUE),
  ) %>% 
  pivot_wider(names_from = period,
              values_from = c(ammo_brought, max_km_village)) %>% 
  mutate(ammo_brought_diff = ammo_brought_post - ammo_brought_pre,
         max_km_village_diff = max_km_village_post - max_km_village_pre,
         both_diff = ammo_brought_diff + max_km_village_diff
  ) %>% 
  arrange(both_diff)

##save for input to fit

saveRDS(offtake_period, "./inputs/mgmt/derived/offtake_period.rds")
saveRDS(follows, "./inputs/mgmt/derived/follows.rds")
saveRDS(follows_full, "./inputs/mgmt/derived/follows_full.rds")
saveRDS(known_conf, "./inputs/mgmt/derived/known_conf.rds")


