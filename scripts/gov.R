#savana gouvernance data

##### clean up

rm(list = ls())

#create not in
#`` what you need for filtering special column names

'%ni%' <- Negate('%in%')

library("dplyr")
library("tidyverse")
library("lubridate")
library("readr")
library("likert")
library("ggplot2")
library("gridExtra")
library("grid") 
library("cowplot")
library("MetBrewer")
library("ggh4x") #facet wrapping

#contrasting colors
met_morg <- function (x) {
  set.seed(92)
  morg <- c()
  for (i in 1:4) {
    morg[i] <- met.brewer("Morgenstern", 6)[i]
  }
  return(morg[x])
}

#read data
#figure out shit later for special characters
ev <- readRDS("./inputs/gov/enquete_village.rds")

eq <- readRDS("./inputs/gov/enquete_questions_garder.rds") %>% 
  mutate_if(is.character,funs(factor(.)))

#gestion villages

gv <- factor(c("E6", "E7", "E8"), levels = c("E6", "E7", "E8"))

#chasseurs par village

ch <- readRDS("./inputs/gov/code_chasseurs.rds") 

#enquete savana
ch_es <- ch %>% 
  filter(enquete_savana == "oui") 

#autosuivi
ch_as <- ch %>% 
  filter(autosuivi == "oui")

ch %>% 
  group_by(village) %>% 
  summarize(n= n())

ch_as %>% 
  group_by(village) %>% 
  summarize(n= n())

ch_es %>% 
  group_by(village) %>% 
  summarize(n= n())

#for 3 villages avec gestion
#enquete avec et sans autosuivi
#pi = project implicated, npi = not..

ch_pi <- ch_es %>% 
  filter(autosuivi == "oui") %>% 
  pull(code_chasseur)

ch_npi <- ch_es %>% 
  filter(autosuivi == "non") %>% 
  pull(code_chasseur)

ch_pit <- ch_as %>% 
  pull(code_chasseur)

#summarize
#pi project involved, npi not project involved, pins pi but not surveyed
n_chi_summ <- left_join(
    left_join(
  ch_es %>% 
    filter(autosuivi == "oui") %>% 
    group_by(village) %>% 
    summarize(n_pi= n()),
  ch_es %>% 
    filter(autosuivi == "non") %>% 
    group_by(village) %>% 
    summarize(n_npi= n())
  ) %>% 
    filter(village %in% gv),
  ch_as %>% 
    filter(village %in% gv) %>% 
    group_by(village) %>% 
    summarize(n_pins= n())
)

#play with summaries

#n questions
nq <- eq %>% 
  group_by(objectif,attribut) %>% 
  summarize(n_questions = n())

nq %>% print(n = Inf)

unique(eq$attribut)

#n questions by category

eq %>% filter(type == "personelles")

#preambule
length(c(1:6))

#gov qual
length(c(7:56))

#conformite
length(c(57:61))

#fermeture
length(c(62:64))


#n 'well-understood' questions
ngq <- eq %>% 
  filter(garder == "oui") %>% 
  group_by(objectif,attribut) %>% 
  summarize(n_questions = n())

ngq %>% print(n = Inf)

#how many poorly understood?

eq %>% 
  filter(type == "binaire") %>% 
  group_by(garder) %>% 
  summarize(n_questions = n())

#keep key questions 

dir_q <- as.character(eq %>% 
  filter(type == "binaire" & attribut == "direction" & garder == "oui") %>% 
  pull(numero) %>% 
  droplevels()
)

par_q <- as.character(eq %>% 
  filter(type == "binaire" & attribut == "participation" & garder == "oui") %>% 
  pull(numero) %>% 
  droplevels()
)

kq <- eq %>% 
  filter(numero %in% c(dir_q,par_q,"58", "33", "62", "56")) %>% 
  pull(numero) %>% 
  droplevels()

levels(kq) <- c(dir_q,par_q,"58", "33", "62", "56")
kq

#explore some questions

ev %>%
  filter(village %in% gv) %>% #conformity
  filter(numero %in% c("57", "57a", "57b",
                       "58", "59", "60", "60a", "60b", 
                       "60c", "60d", "60e", "60f", "60g",
                       "61", "61a")) %>% 
  View()
 
ev %>% 
  filter(village %in% gv
         & numero %in% c("9", "9b", #zone delimitee
                         "33", "33a", #changements en foret
                         "58", "59", #conform et n chasseurs
                         "62", "62a", "63", "64", #succces
                         "56", "56b" #state support
                         ) 
         ) %>% 
  View()

ev %>% 
  filter(village %in% gv
         & numero %in% c("9", "9b" #zone delimitee
                         )) %>% 
  View()

#9 was understood?
q9 <- ev %>% 
  filter(village %in% gv & numero %in% c("9", "9b"))

#21 said delimited, 4 not (2 E6, 2 E7), 7 didn't know (in E7)
q9 %>% 
  filter(numero == "9") %>% 
  group_by(reponse) %>% 
  summarize(n = n())

q9 %>% filter(reponse == "non")

q9 %>% filter(numero == "9" & is.na(reponse))

q9_ch_oui <- q9 %>% 
  filter(numero == "9" & reponse == "oui") %>% 
  pull(chasseur)

comment_delimitee <- tibble(reponse = q9 %>% 
  filter(chasseur %in% q9_ch_oui & numero == "9b") %>% 
  pull(reponse)
)

comment_delimitee %>% print(n = Inf)

#quality associated with oui/non response for each q
ouinon <- eq %>% 
  filter(numero %in% kq) %>% 
  select(numero,oui,non)

#binary responses
br <- left_join(ev %>% filter(numero %in% kq),
          eq %>% select(numero,objectif,attribut,oui,non)
)

tmpbr_all <- ev %>% 
  filter(village %in% gv & numero %in% kq) %>% 
  select(village:reponse)

tmpbr_chpi <- tmpbr_all %>% 
  filter(chasseur %in% ch_pi)

tmpbr_chnpi <- tmpbr_all %>% 
  filter(chasseur %in% ch_npi)


get_gqprops <- function (tmpbr) {
  
  br <- bind_rows(
  left_join(
  tmpbr %>% filter(reponse == "oui"),
  ouinon %>% select(numero, "qual" = oui)
  ),
  left_join(
    tmpbr %>% filter(reponse == "non"),
    ouinon %>% select(numero, "qual" = non)
  ),
  tmpbr %>% filter(is.na(reponse)) %>% 
    mutate(qual = "uq") #unknown quality
  ) %>%
    mutate(numero = factor(numero, levels = levels(kq))) %>% 
    arrange(village,chasseur,numero) %>% 
    mutate_if(is.character,funs(factor(.)))
  
  cats <- c("DIRECTION", "PARTICIPATION",
            "CONFORMITY", "CHANGES", "SUCCESS", "SUPPORT")
  
  br <- br %>% 
    mutate(categorie = case_when(numero %in% dir_q ~ "DIRECTION",
                           numero %in% par_q ~ "PARTICIPATION",
                           numero == "58" ~ "CONFORMITY",
                           numero == "33" ~ "CHANGES",
                           numero == "62" ~ "SUCCESS",
                           numero == "56" ~ "SUPPORT",
                           )) %>% 
    mutate(categorie = factor(categorie, levels = cats))
  
  nqual <- br %>% 
    group_by(village,categorie,qual) %>% 
    summarize(n = n()) %>% 
    mutate(nq = replace_na(case_when(categorie == "DIRECTION" ~ length(dir_q),
                                     categorie == "PARTICIPATION" ~ length(par_q)),1)) %>% 
    mutate(n_adj = n*(5/nq))
    
  total_tmp <- nqual %>%
    group_by(village,categorie) %>%
    summarize(n_total = sum(n),
              n_adj_total = sum(n_adj))
  
  gov_prop <- left_join(nqual,total_tmp) %>% 
    mutate(qual = factor(replace_na(qual, "uq"),
                         levels = c("hq", "lq", "uq"))
           ) %>% 
    mutate(prop = n/n_total,
           prop_adj = n_adj/n_adj_total)
  
  return(gov_prop)
}

gov_prop_all <- get_gqprops(tmpbr_all)
gov_prop_chpi <- get_gqprops(tmpbr_chpi)
gov_prop_chnpi <- get_gqprops(tmpbr_chnpi)

n_chi_summ %>% 
  mutate(per = n_pi/n_pins)

#gov_prop_chpi %>% 
  

plot_gq_per <- function(df) {

    ggplot(df,aes(x="", y=prop, fill=qual))+
    scale_fill_manual(values=c(met_morg(4), met_morg(1), "grey")) +
    geom_bar(stat="identity", width = 1,colour = "black", show.legend = F) +
    coord_polar("y", start=0) +
    facet_grid(categorie ~ village, switch = "both") +
    #facet_wrap(~village, nrow = 1) +
    labs(x = "", y = "Hunters' perceptions (by village)") +
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),
          strip.background = element_blank(),
          strip.text.x = element_text(size=11),
          strip.text.y = element_text(size = 9, face = "bold"),
          legend.position = "none",
          legend.title = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          #axis.title.x = element_blank(),
          axis.title.x= element_text(size = 12),
          axis.title.y= element_text(size = 12),
          plot.title = element_blank()
    )  
  
}

plot_gq_per(gov_prop_all)
plot_gq_per(gov_prop_chpi)
plot_gq_per(gov_prop_chnpi)

###split it up

gov_prop_both <- bind_rows(
gov_prop_chpi %>% mutate(chi = "Project"),
gov_prop_chnpi %>% mutate(chi = "Other")
) %>% 
  mutate(chi = factor(chi, levels = c("Project", "Other"))) %>% 
  arrange(village)

rep_this <- gov_prop_both %>% 
  group_by(village,chi) %>% 
  summarize(n = n()) %>% 
  pull(n)

n_chi_summ

ev %>%
  filter(village %in% gv & numero == "59") %>% 
  mutate(reponse = as.numeric(reponse)) %>% 
  group_by(village) %>% 
  summarize(min_per_ch = min(reponse, na.rm = TRUE),
            max_per_ch = max(reponse, na.rm = TRUE),
            )

chi_nvec <- c(rep("Project (2)", rep_this[1]),
  rep("Other (5)", rep_this[2]),
  rep("Project (11)", rep_this[3]),
  rep("Other (9)", rep_this[4]),
  rep("Project (3)", rep_this[5]),
  rep("Other (3)", rep_this[6])
  )

gov_prop_both$chi_n <- chi_nvec

gov_prop_both <- gov_prop_both %>% 
  mutate(chi_n = factor(chi_n,
         levels = c("Project (2)","Other (5)",
                    "Project (11)","Other (9)",
                    "Project (3)","Other (3)")),
         Perception = recode_factor(qual, hq = "High", 
                                 lq = "Low",
                                 uq = "Unsure"),
         village = recode_factor(village, E6 = "E6: ~10 (6 project)", 
                                    E7 = "E7: ~30 (19 project)",
                                    E8 = "E8: ~10 (7 project)")
         )


plot_per <- ggplot(gov_prop_both,aes(x="", y=prop, fill=Perception))+
  scale_fill_manual(values=c(met_morg(4), met_morg(1), "grey")) +
  geom_bar(stat="identity", width = 1,colour = "black") +
  coord_polar("y", start=0) +
  ggh4x::facet_nested(categorie ~ village + chi_n,
                      #nest_line = element_line(linetype = 1),
                      switch = "both") +
  #facet_wrap(~chi, nrow = 1) +
  labs(x = "",
       y = "Project vs. other hunters surveyed / total by village") +
  # labs(x = "Components of governance and management\n", 
  #      y = "\nProject and other hunters (surveyed/total)\n\nPerceived total hunters (by village)") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        strip.background = element_blank(),
        strip.text.x = element_text(size=11),
        #ggh4x.facet.nestline = element_line(colour = "black"),
        #strip.text.x = element_blank(),
        strip.text.y = element_text(size = 9, face = "bold"),
        legend.justification = "top",
        #legend.title = element_blank(),
        legend.title = element_text(size=10, face = "bold"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        #axis.title.x = element_blank(),
        axis.title.x= element_text(size = 12, face = "bold"),
        axis.title.y= element_text(size = 12),
        plot.title = element_blank()
  )

plot_per

dev.off()

shrinkby <- 0

cairo_pdf(file = "./outputs/figs/fs1.pdf", width = 8, height =  8)

ggdraw() +
  draw_plot(plot_per, x=0, y=0, width = 1, height = 1) +
  draw_line(x = c(0.07,0.863, 0.863,0.07,0.07),
            y = c(0.1499,0.1499,0.945,0.945,0.1499),
            size = 0.5)+
  draw_line(x = c(0.3325, 0.3325), y = c(0.1499, 0.945), size = 0.5) +
  draw_line(x = c(0.6003, 0.6003), y = c(0.1499, 0.945), size = 0.5) +
  draw_line(x = c(0.07+0.01+shrinkby,0.3325-0.01-shrinkby), y = c(0.117,0.117)) +
  draw_line(x = c(0.3325+0.01+shrinkby,0.6003-0.01-shrinkby), y = c(0.117,0.117)) +
  draw_line(x = c(0.6003+0.01+shrinkby,0.863-0.01-shrinkby), y = c(0.117,0.117))

dev.off()

##alternative not splitting by project involvement

gov_prop_both
gov_prop_all 

gov_prop_all <- gov_prop_all %>% 
  mutate(Perception = recode_factor(qual, hq = "High", 
                                    lq = "Low",
                                    uq = "Unsure"),
         village = recode_factor(village, E6 = "E6: 7 (of ~10)", 
                                 E7 = "E7: 20 (of ~30)",
                                 E8 = "E8: 6 (of ~10)")
  )


plot_per_all <- ggplot(gov_prop_all,aes(x="", y=prop, fill=Perception))+
  scale_fill_manual(values=c(met_morg(4), met_morg(1), "grey")) +
  geom_bar(stat="identity", width = 1,colour = "black", show.legend = T) +
  coord_polar("y", start=0) +
  facet_grid(categorie ~ village, switch = "both") +
  labs(x = "",
       y = "Village: hunters surveyed (of total)") +
  # labs(x = "Components of governance and management\n", 
  #      y = "\nProject and other hunters (surveyed/total)\n\nPerceived total hunters (by village)") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        strip.background = element_blank(),
        strip.text.x = element_text(size=12),
        #ggh4x.facet.nestline = element_line(colour = "black"),
        #strip.text.x = element_blank(),
        strip.text.y = element_text(size = 12, face = "bold"),
        legend.justification = "top",
        #legend.title = element_blank(),
        legend.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=12),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        #axis.title.x = element_blank(),
        axis.title.x= element_text(size = 12, face = "bold"),
        axis.title.y= element_text(size = 12),
        plot.title = element_blank()
  )

plot_per_all

dev.off()

shrinkby <- .002

cairo_pdf(file = "./outputs/figs/f3.pdf", width = 6, height =  9)

ggdraw() +
  draw_plot(plot_per_all, x=0, y=0, width = 1, height = 1) +
  draw_line(x = c(0.104,0.79, 0.79,0.104,0.104),
            y = c(0.069,0.069,0.989,0.989,0.069),
            size = 0.5)+
  draw_line(x = c(0.104 + (0.79-0.104)*(1/3)-shrinkby, 0.104 + (0.79-0.104)*(1/3)-shrinkby), y = c(0.989, 0.069), size = 0.5)+
  draw_line(x = c(0.104 + (0.79-0.104)*(2/3)+shrinkby, 0.104 + (0.79-0.104)*(2/3)+shrinkby), y = c(0.989, 0.069), size = 0.5) 
  # draw_line(x = c(0.6003, 0.6003), y = c(0.1499, 0.945), size = 0.5)
  # draw_line(x = c(0.07+0.01+shrinkby,0.3325-0.01-shrinkby), y = c(0.117,0.117)) +
  # draw_line(x = c(0.3325+0.01+shrinkby,0.6003-0.01-shrinkby), y = c(0.117,0.117)) +
  # draw_line(x = c(0.6003+0.01+shrinkby,0.863-0.01-shrinkby), y = c(0.117,0.117))

dev.off()

