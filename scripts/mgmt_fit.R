###mgmt test fit models, check em, make fig
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
library("gridGraphics") #for ggdraw base plots

##read input data to model with
village_meta <- readRDS("./inputs/meta/village_meta.rds")
huntmeat <- readRDS("./inputs/mgmt/huntmeat.rds")
offtake_period <- readRDS("./inputs/mgmt/derived/offtake_period.rds")
follows <- readRDS("./inputs/mgmt/derived/follows.rds")
follows_full <- readRDS("./inputs/mgmt/derived/follows_full.rds")
known_conf <- readRDS("./inputs/mgmt/derived/known_conf.rds")


#village levels

village_levels <- paste0(c(rep("C",10),rep("E",10)), 
                         rep(seq(1:10),2))

#excluding E4 & E10 (not enough data) and C8 (potentially erroneous)
#but all bad data already removed in AJE cleaning, so no need

village_levels <- village_levels[village_levels %ni% c("C8","E4","E10")]

##TEST EXPECTED CHANGE IN E6 
#given reducing ammo to <= 6 and animals to <= 5
#and reducing n hunts via limiting hunters to 2 per week

gunfE6 <- follows  %>% 
  filter(village == "E6" & guntrap != "trap") %>% 
  filter(period == "pre") %>% 
  dplyr::select(ammo_brought,animals_gun) %>% 
  mutate(ammo_brought_CS = stdCS_pred(ammo_brought)) %>% 
  dplyr::select(ammo_brought,ammo_brought_CS,animals_gun)

form_E6ammo <- bf(animals_gun ~ ammo_brought_CS) + poisson()
form_E6ammo_gp <- bf(animals_gun ~ ammo_brought_CS) + negbinomial()
form_E6ammo_ncs <- bf(animals_gun ~ ammo_brought) + poisson()

get_prior(form_E6ammo, data = gunfE6)
get_prior(form_E6ammo_gp, data = gunfE6)

prior_E6ammo <- c(prior(normal(0,0.5), class = b), 
                  prior(normal(0,5), class = "Intercept")
)


#variance higher than expected
mean(gunfE6$animals_gun)
var(gunfE6$animals_gun)

hist(gunfE6$animals_gun)
hist(rgamma(100,shape = 1, rate = 0.1))

#zeros around x2 as expected (~20% versus ~10%)
gunfE6 %>% filter(animals_gun == 0) %>% nrow() /
  gunfE6 %>% nrow()

table(rpois(1000, mean(gunfE6$animals_gun)) == 0)[[2]]/1000

prior_E6ammo_gp <- c(prior(normal(0,0.5), class = b), 
                     prior(normal(0,5), class = "Intercept"),
                     prior(gamma(1,0.1), class = shape)
)

E6ammo <- brm(form_E6ammo,
              prior = prior_E6ammo,
              data = gunfE6,
              chains = 4, iter = 3000, warmup = 1500,
              control = list(adapt_delta = 0.92),
              cores = getOption("mc.cores", 2),
              file = "./outputs/mgmt_fits/E6ammo")

E6ammo_gp <- brm(form_E6ammo_gp,
                 prior = prior_E6ammo_gp,
                 data = gunfE6,
                 chains = 4, iter = 3000, warmup = 1500,
                 control = list(adapt_delta = 0.92),
                 cores = getOption("mc.cores", 2),
                 file = "./outputs/mgmt_fits/E6ammo_gp")

E6ammo_ncs <- brm(form_E6ammo_ncs,
                  prior = prior_E6ammo,
                  data = gunfE6,
                  chains = 4, iter = 3000, warmup = 1500,
                  control = list(adapt_delta = 0.92),
                  cores = getOption("mc.cores", 2),
                  file = "./outputs/mgmt_fits/E6ammo_ncs")

E6ammo <- readRDS("./outputs/mgmt_fits/E6ammo.rds")
E6ammo_gp <- readRDS("./outputs/mgmt_fits/E6ammo_gp.rds")
E6ammo_ncs <- readRDS("./outputs/mgmt_fits/E6ammo_ncs.rds")

#Poisson versus GP gives identical inference
E6ammo
E6ammo_gp 
E6ammo_ncs

pp_check(E6ammo, ndraws = 100)
pp_check(E6ammo_gp, ndraws = 100)
pp_check(E6ammo_ncs, ndraws = 100)

pp_stat <- function (tf,st) {
  
  prop_zero <- function(x) mean(x == 0)
  
  ppc_stat(y = tf$data[1] %>% pull(1),
           posterior_predict(tf)[1:1000,],
           stat = st)
  
}

#captures mean well
pp_stat(E6ammo, "mean")
pp_stat(E6ammo_gp, "mean")
pp_stat(E6ammo_ncs, "mean")
mean(gunfE6$animals_gun)

#posterior predict rapidly
c_eff <- conditional_effects(E6ammo_ncs, prob = 0.92,
                             #line colour doesn't work
                             line_args = c(colour = "black")
                             )
ugly_plot <- plot(c_eff, plot = FALSE)[[1]] + 
scale_y_continuous(name = "Number of animals gun-hunted\n", 
                   limits = c(0,10), breaks = seq(0,10),
                   labels = seq(0,10)) +
scale_x_continuous(name = "\nNumber of ammunition brought", 
                     limits = c(1,11), breaks = seq(1,11),
                     labels = seq(1,11)) +
geom_vline(xintercept=6, linetype="dashed", color = "black") +
geom_vline(xintercept=5, linetype="dashed", color = "black")
ugly_plot
plot(jitter(gunfE6$animals_gun) ~ gunfE6$ammo_brought, pch =19 )

#900-1000 hunts a year
#700-800 if reduced
lprenh <- round(((5*1.5) + (3*3))*52,-2)
hprenh <- round(((5*1.5) + (4*3))*52,-2)

lpostnh <- round(((5*1.5) + (3*2))*52,-2)
hpostnh <-round(((5*1.5) + (4*2))*52,-2)

lprenh
hprenh
lpostnh
hpostnh

inhunts_preammo <- gunfE6 %>% dplyr::select(-ammo_brought_CS)

inhunts_postammo <- inhunts_preammo %>% 
  mutate(ammo = "post",
         ammo_brought = case_when(ammo_brought > 6 ~ 6,
                                  ammo_brought < 6 ~ ammo_brought,
                                  TRUE ~ ammo_brought))

#~0.5 a bullet less mean (~ half of observed)
mean(inhunts_preammo$ammo_brought)
mean(inhunts_postammo$ammo_brought)

#% animals > 5 only predicted ~3% of the time (vs. 5% obs)
tibble(animals = c(posterior_epred(E6ammo_ncs, 
                                   newdata = inhunts_preammo))
      ) %>% 
  summarize(pa5 = mean(animals > 5, na.rm = TRUE)*100)

E6predposts <- function (po_am,pr_nh,po_nh,lab) {
  
  mftmp <- E6ammo_ncs
  
  pre_tmp <- inhunts_preammo
  
  post_tmp <- po_am
  
  #pps and contrasts
  pre_pp_tmp <- posterior_epred(mftmp, newdata = pre_tmp)

  ifelse (po_nh == inhunts_postammo,
    post_pp_tmp <- pmin(posterior_epred(mftmp, newdata = post_tmp),5.4),
    post_pp_tmp <- posterior_epred(mftmp, newdata = post_tmp)
    )
  
  chl <- tibble(
    pre = c(pre_pp_tmp),
    post = c(post_pp_tmp)
    )
  
  #ph per hunt
  #vw village-wide
  #vwpd vw per day
  pre_abs <- tibble(
    prediction = "pre",
    nhunt = pr_nh,
    ph = mean(chl$pre),
    vw = ph*nhunt,
    vwpd = vw/365,
  )
  
  post_abs <- tibble(
    prediction = "post",
    nhunt = po_nh,
    ph = mean(chl$post),
    vw = ph*nhunt,
    vwpd = vw/365,
    )
  
  diff <- tibble(
    prediction = "difference",
    nhunt = post_abs$nhunt - pre_abs$nhunt,
    ph = post_abs$ph - pre_abs$ph,
    vw = post_abs$vw - pre_abs$vw,
    vwpd = post_abs$vwpd - pre_abs$vwpd,
    )
  
  chm <- bind_rows(pre_abs, post_abs,diff) %>% 
    mutate(scenario = lab) %>% 
    dplyr::select(scenario,everything())
    
  return(chm)
}

lprenh
hprenh
lpostnh
hpostnh

#test it works
E6predposts(inhunts_preammo,lprenh,lprenh, "No rules (low hunts)")
E6predposts(inhunts_preammo,hprenh,hprenh, "No rules (high hunts)")
E6predposts(inhunts_preammo,lprenh,hprenh, "No rules (low vs. high hunts)")

#unprecise prediction of scenarios

E6rulepreds <- bind_rows(
E6predposts(inhunts_postammo,lprenh,lprenh, "ammo/hunt ≤ 6, animals/hunt ≤ 5 (low n hunts)"),
E6predposts(inhunts_postammo,hprenh,hprenh, "ammo/hunt ≤ 6, animals/hunt ≤ 5 (high n hunts)"),

E6predposts(inhunts_preammo,lprenh,lpostnh, "hunts/week ≤ 2 (low n hunts)"),
E6predposts(inhunts_preammo,hprenh,hpostnh, "hunts/week ≤ 2 (high n hunts)"),

E6predposts(inhunts_postammo,lprenh,lpostnh, "ammo/hunt ≤ 6, animals/hunt ≤ 5, hunts/week ≤ 2 (low n hunts)"),
E6predposts(inhunts_postammo,hprenh,hpostnh, "ammo/hunt ≤ 6, animals/hunt ≤ 5, hunts/week ≤ 2 (high n hunts)")
)

E6rulepreds

#2589 villages (PNAT V0) disons ~400 gibiers de moins par an
#and 1/10 of villages (E6) of 1/3 of villages (E6-E8)
#100-300k annual reduction

(0.1)*2500*400
(0.3)*2500*400

rop10 <- (0.1)*2500*400
rop30 <- (0.3)*2500*400

#% national?
#average village-wide offtake
(29831/19)
avwo <- 1600

gabo <- avwo*2500

2000-2500

(1500-2500)/2500 #40% observed decline
(1800-2500)/2500 #28% predicted from rules
(2100-2500)/2500 #16% conservative estimate

#3-8% decline
((gabo - rop10) - gabo) / gabo * 100
((gabo - rop30) - gabo) / gabo * 100

#hunting catchments from AJE excluding E1 and E5
#E1 likely didn't show full catchment, E5 including hunts for ceremonies
#hunting mean catchment area
hmca <- (19+62+64+80+91+103+117)/7
hmca 

#gabon surface area
#https://data.worldbank.org/indicator/AG.SRF.TOTL.K2
gsa <- 267670

(0.1)*2500*hmca
(0.3)*2500*hmca

((0.1)*2500*hmca/gsa)*100
((0.3)*2500*hmca/gsa)*100

#E8 ZDcons 

known_conf$kcE8

prehuntsE8 <- follows %>% 
  filter(village == "E8" & period == "pre") %>% 
  pull(hunt) %>% 
  droplevels()
  
huntmeat %>% 
  filter(hunt %in% prehuntsE8) %>% 
  group_by(side_road) %>% 
  summarize(n = n())

32/(32+46) #41%
46/(32+46) #59%

E8tmp <- readRDS("./inputs/mgmt/eyo_species_village.rds") %>% 
  filter(village == "E8") %>% 
  print(n = Inf)

mduiker <- c("CPE", "CBA", "CMN", "CFN","CVB")

TOU_NS <- E8tmp$eyo %>% sum()
CBL_NS <- E8tmp %>% filter(species == "CBL") %>% pull(eyo)
CMN_NS <- E8tmp %>% filter(species %in% mduiker) %>% pull(eyo) %>% sum()

TOU_NS
TOU_NS*0.41

CBL_NS
CMN_NS

recoveryE8 <- tibble(species = c("CBL", "CMN"),
                     #life history (wilkie and poulsen 2021)
                     r = c(1.63,1.54),
                     k_wp = c(3.43,7.33),
                     #not hunted and hunted densities (obrien 2019)
                     den_nhu = c(30,14),
                     den_hu = c(14,2),
                     #home range (yasuoka 2015)
                     km2 = c(2.5/100, 60/100),
                     north_ranges = 13/km2,
                     #n spared
                     nsp = c(CBL_NS,CMN_NS)*0.41,
                     #2020-2022 growth
                     yr1 = nsp*r,
                     yr2 = yr1*r, #+ yr1,
                     yr3 = yr2*r, #+ yr1,
                     off_km2 = nsp/13,
                     yr3_km2 = yr3/13
                     )  %>% 
  mutate(across(7:13, round, 0)) %>% 
  print()


eyoE8 <- tibble(TOU = 883, CBL = 218, CMN = 104)

###FIGURE 1-1 (TOP LEFT, OBSERVED PRE-MGMT OFFTAKE)

#implementation re: offtake

imp_off <- offtake_period %>% 
  filter(period == "pre") %>% 
  group_by(village) %>% 
  dplyr::summarize(animals = mean(animals_e),
                   xaf = round(mean(xaf_e)/500,0)*500
  ) %>% 
  dplyr::mutate(village = factor(village, 
                                 levels = village_levels)) %>% 
  arrange(village)

imp_off <- left_join(imp_off,
                     offtake_period %>% 
                       dplyr::select(village,mgmt) %>% 
                       group_by(village) %>% 
                       dplyr::summarize(mgmt = first(mgmt))
) %>% 
  dplyr::mutate(dummy = 1)

imp_np <- imp_off %>% filter(mgmt == "Not proposed") 
imp_bp <- imp_off %>% filter(mgmt == "Before project")
imp_im <- imp_off %>% filter(mgmt == "Implemented")
imp_ui <- imp_off %>% filter(mgmt == "Unimplemented")
#imp_uiE4E10 <- imp_off_wE4E10 %>% filter(village %in% c("E4", "E10") )

##plotting raw data

pre_mgmt_plot <- function() {
  #pre_mgmt_plot_E4E10 <- function() { 
  
  mar_tmp <- par()$mar #get the current margins (bltr)
  mar_tmp[3] <- mar_tmp[3] + 2 
  mar_tmp[2] <- mar_tmp[2] + 2 #second number is line count
  par(mar = mar_tmp, 
      mgp = c(2.1,0.3,0), #title, tick, tick label closeness
      xpd = TRUE) #set the new margins
  plot(NULL,
       xlim = c(0.95,1.05),
       cex.lab = 1, cex.axis=1,
       ylim = c(1,8.5), 
       #ylim = c(1,10.5), #if including E4 and E10 
       yaxt = "n",
       xaxt = "n",
       xlab = "",
       ylab = paste("Pre-management\nanimals hunted (per day, village-wide)")
  )
  box(lwd=1.1)
  axis(2, at = 1:8, las = 1, tck=-0.008)
  #axis(2, at = 1:10) #if including E4 and E10
  legend("topleft", inset = c(0, -0.22),
         title = (as.expression(bquote(bold("Hunting management")))),
         #title.adj = -0.02, ##if including E4 and E10
         legend=c("Not proposed (8)", "Before project (1)",
                  "Implemented (3)", "Unimplemented (5)"
                  #,"Unimplemented, data post-mgmt (2)" #E4 and E10
         ),
         pt.bg=c("gray", met_jodi(9,10),
                       met_jodi(6,10), met_jodi(1,10)
                       #,met_jodi(2,10) #E4 and E10
         ),
         #col=rep("black", 3),
         pt.cex = 1.6,
         bty = 'n', #border
         pch =21, cex=0.77 #0.8 for 4x7, 0.9 pour 5x7
  )
  ###THIS WEIRD REPEAT SECTION OF PHANTOM POINTS IS TO GET JITTER 
  ##ON GRAY NOT TO OVERLAP (CEMENTING BRUTE FORCE)
  x <- 1
  repeat {
    print(x)
    x = x+1
    points(imp_np$animals ~ jitter(imp_np$dummy),
           pch = 21, bg = "gray", cex = 0, col = "black")
    if (x > 13){ #13th time the charm, datsyuk
      break
    }
  }
  ##CAN NOW PLOT ACTUAL POINTS
  points(imp_np$animals ~ jitter(imp_np$dummy),
         pch = 21, bg = "gray", cex = 2, col = "black")
  points(imp_bp$animals ~ jitter(imp_bp$dummy),
         pch = 21, bg = met_jodi(9,10), cex = 2, col = "black")
  points(imp_im$animals ~ jitter(imp_im$dummy),
         pch = 21, bg = met_jodi(6,10), cex = 2, col = "black")
  points(imp_ui$animals ~ jitter(imp_ui$dummy),
         pch = 21, bg = met_jodi(1,10), cex = 2, col = "black")
  #if including E4 and E10
  # points(imp_uiE4E10$animals ~ jitter(imp_uiE4E10$dummy),
  #        pch = 21, bg = met_jodi(2,10), cex = 2, col = "black")
  text(x=c(0.994,1.022, 1.011, 0.995, 1.005, 0.991, 0.994, 1.026), 
       y=c(8.6,6.8, 7.9, 7, 5.7, 2.18, 3.97, 2.91), 
       labels=c("C1", "C2", "E7", "E6", "E5", "E9", "C3", "E8"), 
       cex=0.77)
}

ggdraw(pre_mgmt_plot)

###FIGURE 1-2 (TOP RIGHT, MGMT ~ OFFTAKE)

#center and scale predictor
mgmtCS <- bind_rows(
  imp_im %>% mutate(mgmt = 1),
  imp_ui %>% mutate(mgmt = 0) 
  #,imp_uiE4E10 %>% mutate(mgmt = 0) #if adding E4 and E10
) %>% 
  mutate(animals_CS = stdCS_pred(animals),
         xaf_CS = stdCS_pred(xaf)) %>% 
  arrange(village) %>% 
  select(-dummy)

form_mgmtoff <- bf(mgmt ~ animals_CS) + bernoulli()

get_prior(form_mgmtoff, data = mgmtCS)

prior_mgmtoff <- c(prior(normal(0,0.5), class = b), 
                   prior(normal(0,5), class = "Intercept")
)

mgmtoff <- brm(form_mgmtoff,
               prior = prior_mgmtoff,
               data = mgmtCS,
               chains = 4, iter = 3000, warmup = 1500,
               control = list(adapt_delta = 0.92),
               cores = getOption("mc.cores", 2),
               file = "./outputs/mgmt_fits/mgmtoff")

mgmtoff <- readRDS("./outputs/mgmt_fits/mgmtoff.rds")

prior_summary(mgmtoff)

#graphical checks

bp_quick(mgmtoff)

#summary
mgmtoff

#probability of mgmt with 0 CS daily hunted animals (intercept)
exp(fixef(mgmtoff)[1,1]) / (1 + exp(fixef(mgmtoff)[1,1]) )

#probabilities with 1 CS increase in daily hunted animals (~2.5 animals)
#~10% higher
exp(fixef(mgmtoff)[1,1] + fixef(mgmtoff)[2,1]) / 
  (1 + exp(fixef(mgmtoff)[1,1] + fixef(mgmtoff)[2,1]) )

##posterior prediction

mgmt_pp <- posterior_epred(mgmtoff)

mgmt_ppl <- as_tibble(mgmt_pp) %>% 
  mutate(iter = 1:dim(mgmt_pp)[1]) %>% 
  pivot_longer(-iter) %>%
  mutate(animals = rep(mgmtCS %>% pull(animals), 
                       dim(mgmt_pp)[1])) %>% 
  dplyr::select(iter,animals,p_mgmt = value)

im_raw <- imp_im %>% 
  mutate(p_mgmt = 1)

ui_raw <- imp_ui %>% 
  mutate(p_mgmt = 0)

#plot it
#https://mjskay.github.io/ggdist/reference/stat_lineribbon.html
mgmtoff_plot <- mgmt_ppl  %>%
  filter(iter %in% 1:100) %>%
  ggplot(aes(x = animals, y = p_mgmt)) +
  labs(#x = "\nAnimals hunted per day (village-wide, pre-management)",
    x = "\nPre-management animals hunted (per day, village-wide)",
    #y = "Predicted probability of a village implementing hunting management\n",
    y = "Predicted probability of management\n",
    title = "") +
  stat_lineribbon(.width = c(0.5,0.92))+
  geom_point(data = im_raw, bg = met_jodi(6,10), 
             shape = 21, size = 4.4)+
  annotate("text", x = 7.5, y = 0.97, label = "E7", size = 3.3)+
  annotate("text", x = 6.6, y = 0.97, label = "E6", size = 3.3)+
  annotate("text", x = 2.85, y = 0.97, label = "E8", size = 3.3)+
  geom_point(data = ui_raw, bg = met_jodi(1,10), 
             shape = 21, size = 4.4)+
  annotate("text", x = 5.63, y = 0.03, label = "E5", size = 3.3)+
  annotate("text", x = 2.11, y = 0.03, label = "E9", size = 3.3)+
  # scale_x_continuous(limits = c(1,8),
  #                    breaks = seq(1,8, by = 1)
  # ) +
  scale_y_continuous(limits = c(-0.01,1.01),
                     breaks = seq(0.0,1.0, by = 0.2),
                     position = "right"
  ) +
  ##IF WITH E4 AND E10
  # geom_point(data = uiE4E10_raw, bg = met_jodi(2,10), 
  #            shape = 21, size = 4.4)+
  # scale_x_continuous(limits = c(1,10.5),
  #                    breaks = seq(1,10, by = 1)
  # ) +
  theme(legend.position = c(0.09,0.97),
        legend.title = element_text(size=9),
        legend.text = element_text(size=9),
        axis.line = element_blank(),
        axis.ticks = element_line(colour = "black"),
        axis.text=element_text(size=11, colour = "black"),
        legend.justification = "top",
        axis.title.x = element_text(size = 12),
        axis.title.y= element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=0.8),
        plot.margin = unit(c(0.4, 2, 0.4, 0.2), "cm")
  ) +
  scale_fill_manual(name = (as.expression(bquote(bold("Uncertainty\ninterval")))),
    values = c("gray88", "gray77"))

mgmtoff_plot 

###FIGURE 1-3 (BOTTOM, OFFTAKE ~ MGMT)

#village-wide
form_offmgmt_v_p <- bf(animals_e ~ period | village) + poisson() 
form_offmgmt_v_zigp <- bf(animals_e ~ period | village,
                        zi ~ period | village
                        ) + zero_inflated_negbinomial()

get_prior(form_offmgmt_v_p, data = offtake_period)
get_prior(form_offmgmt_v_zigp, data = offtake_period)

prior_offmgmt_v_p <- c(
  prior(exponential(2), class = sd), 
  prior(normal(0,5), class = Intercept), 
  prior(lkj(1), class = cor)
)

#self-followed hunts
form_offmgmt_h_p <- bf(animals ~ (1 | hunter) + (period | village)) + poisson() 
form_offmgmt_h_p_g <- bf(animals_gun ~ (1 | hunter) + (period | village)) + poisson() 
form_offmgmt_h_p_t <- bf(animals_trap ~ (1 | hunter) + (period | village)) + poisson() 


form_offmgmt_h_zigp <- bf(animals ~ (1 | hunter) + (period | village),
                        zi ~ (1 | hunter) + (period | village)) + zero_inflated_negbinomial()

get_prior(form_offmgmt_h_p, data = follows)
get_prior(form_offmgmt_h_zigp, data = follows)

prior_offmgmt_h_p <- prior_offmgmt_v_p 

#fit
offmgmt_v_p <- brm(form_offmgmt_v_p,
                   prior = prior_offmgmt_v_p,
                   data = offtake_period,
                   chains = 4, iter = 3000, warmup = 1500,
                   control = list(adapt_delta = 0.92),
                   cores = getOption("mc.cores", 2),
                   file = "./outputs/mgmt_fits/offmgmt_v_p"
                   )

offmgmt_v_zigp <- brm(form_offmgmt_v_zigp,
                   #prior = prior_offmgmt_v_p,
                   data = offtake_period,
                   chains = 4, iter = 3000, warmup = 1500,
                   control = list(adapt_delta = 0.92),
                   cores = getOption("mc.cores", 2),
                   file = "./outputs/mgmt_fits/offmgmt_v_zigp"
                   )

offmgmt_h_p <- brm(form_offmgmt_h_p,
                   prior = prior_offmgmt_h_p,
                   data = follows,
                   chains = 4, iter = 3000, warmup = 1500,
                   control = list(adapt_delta = 0.92),
                   cores = getOption("mc.cores", 2),
                   file = "./outputs/mgmt_fits/offmgmt_h_p"
                   )

offmgmt_h_p_g <- brm(form_offmgmt_h_p_g,
                   prior = prior_offmgmt_h_p,
                   data = follows,
                   chains = 4, iter = 3000, warmup = 1500,
                   control = list(adapt_delta = 0.92),
                   cores = getOption("mc.cores", 2),
                   file = "./outputs/mgmt_fits/offmgmt_h_p_g"
                   )

offmgmt_h_p_t <- brm(form_offmgmt_h_p_t,
                   prior = prior_offmgmt_h_p,
                   data = follows,
                   chains = 4, iter = 3000, warmup = 1500,
                   control = list(adapt_delta = 0.92),
                   cores = getOption("mc.cores", 2),
                   file = "./outputs/mgmt_fits/offmgmt_h_p_t"
                   )

offmgmt_h_zigp <- brm(form_offmgmt_h_zigp,
                    #prior = prior_offmgmt_h_p,
                    data = follows,
                    chains = 4, iter = 3000, warmup = 1500,
                    control = list(adapt_delta = 0.92),
                    cores = getOption("mc.cores", 2),
                    file = "./outputs/mgmt_fits/offmgmt_h_zigp"
                    )

#check

offmgmt_v_p <- readRDS("./outputs/mgmt_fits/offmgmt_v_p.rds")
offmgmt_v_zigp <- readRDS("./outputs/mgmt_fits/offmgmt_v_zigp.rds")
offmgmt_h_p <- readRDS("./outputs/mgmt_fits/offmgmt_h_p.rds")
offmgmt_h_zigp <- readRDS("./outputs/mgmt_fits/offmgmt_h_zigp.rds")

#summaries of inputs across villages
offtake_period %>% 
  filter(village %in% c(imp_im$village,imp_ui$village)&
           period == "pre") %>% 
  group_by(village) %>% 
  summarize(n_days = n()) %>% 
  arrange(n_days)

offtake_period %>% 
  group_by(village) %>% 
  summarize(n_days = n()) %>% 
  arrange(n_days)

follows %>% 
  group_by(village) %>% 
  summarize(n_hunts = n()) %>% 
  arrange(n_hunts)

fl <- list(offmgmt_v_p,offmgmt_v_zigp,offmgmt_h_p,offmgmt_h_zigp)
ft <- c("Poisson (village-wide)", "Zero-inflated gamma-Poisson (village_wide)",
                 "Poisson (hunt)", "Zero-inflated gamma-Poisson (hunt)")

#ZIGP much better captures response of a given hunt
grid.arrange(
  pp_check(fl[[1]], ndraws = 100) + ggtitle(ft[1]),
  pp_check(fl[[2]], ndraws = 100) + ggtitle(ft[2]),
  pp_check(fl[[3]], ndraws = 100) + ggtitle(ft[3]),
  pp_check(fl[[4]], ndraws = 100) + ggtitle(ft[4])
)

#gun versus trap versus both
grid.arrange(
  pp_check(offmgmt_h_p_g, ndraws = 100) + ggtitle("Gun"),
  pp_check(offmgmt_h_p_t, ndraws = 100) + ggtitle("Trap"),
  pp_check(offmgmt_h_p, ndraws = 100) + ggtitle("Both"),
  nrow = 1
)

pp_stat <- function (tf,st) {
  
  prop_zero <- function(x) mean(x == 0)

  ppc_stat(y = tf$data[1] %>% pull(1),
           posterior_predict(tf)[1:1000,],
           stat = st)
  
}

pp_statg <- function (tf,st) {
  
  prop_zero <- function(x) mean(x == 0)
  
  ppc_stat_grouped(y = tf$data[1] %>% pull(1),
                   posterior_predict(tf)[1:1000,],
                   stat = st,
                   group = tf$data %>% pull(village)
                   )
}

#but poisson and zigp capture same mean
grid.arrange(
  pp_stat(fl[[1]], "mean") + ggtitle(ft[1]),
  pp_stat(fl[[2]], "mean") + ggtitle(ft[2]),
  pp_stat(fl[[3]], "mean") + ggtitle(ft[3]),
  pp_stat(fl[[4]], "mean") + ggtitle(ft[4])
)

grid.arrange(
  pp_statg(fl[[1]], "mean") + ggtitle(ft[1]),
  pp_statg(fl[[2]], "mean") + ggtitle(ft[2]),
  pp_statg(fl[[3]], "mean") + ggtitle(ft[3]),
  pp_statg(fl[[4]], "mean") + ggtitle(ft[4])
)

#gun versus trap versus both
grid.arrange(
  pp_stat(offmgmt_h_p_g, "mean") + ggtitle("Gun"),
  pp_stat(offmgmt_h_p_t, "mean") + ggtitle("Trap"),
  pp_stat(offmgmt_h_p, "mean") + ggtitle("Both"),
  nrow = 1
)

#village wide fully misses zeros (by around 0.3)
#hunt by around 0.04
grid.arrange(
  pp_stat(fl[[1]], "prop_zero") + ggtitle(ft[1]),
  pp_stat(fl[[2]], "prop_zero") + ggtitle(ft[2]),
  pp_stat(fl[[3]], "prop_zero") + ggtitle(ft[3]),
  pp_stat(fl[[4]], "prop_zero") + ggtitle(ft[4])
)

grid.arrange(
  pp_statg(fl[[1]], "prop_zero") + ggtitle(ft[1]),
  pp_statg(fl[[2]], "prop_zero") + ggtitle(ft[2]),
  pp_statg(fl[[3]], "prop_zero") + ggtitle(ft[3]),
  pp_statg(fl[[4]], "prop_zero") + ggtitle(ft[4])
)

###posterior predict

vposts <- function (mf) {

  compv_list <- list()
  for (i in seq_along(village_levels)) {
  
  mftmp <- mf
  
  pre_vtmp <- tibble(village = village_levels[i],
                     period = "pre") %>% 
    slice(rep(1:n(), each = 365))
  
  post_vtmp <- pre_vtmp %>% mutate(period = "post")
  
  #pps and contrasts
  pre_pp_tmp <- posterior_epred(mftmp, newdata = pre_vtmp)
  post_pp_tmp <- posterior_epred(mftmp, newdata = post_vtmp)
  
  chl <- tibble(
    pre = c(pre_pp_tmp),
    post = c(post_pp_tmp),
    cnt_diff = post - pre,
    per_diff = (post - pre)/pre*100 #%>% round(0)
  )
  
  chw <- tibble(mean_cnt = mean(chl$cnt_diff),
                lui_cnt = quantile(chl$cnt_diff,.04),
                uui_cnt = quantile(chl$cnt_diff,.96),
                width_cnt = abs(uui_cnt -lui_cnt),
                mean_per = mean(chl$per_diff),
                lui_per = quantile(chl$per_diff,.04),
                uui_per = quantile(chl$per_diff,.96),
                width_per = abs(uui_per -lui_per),
  ) #%>% 
    # mutate(across(contains("cnt"), ~ round(.x,1)),
    #        across(contains("per"), ~ round(.x,0))
    # )
  
  compv_list[[i]] <- chw
  
  }
  compv <- bind_rows(compv_list) %>% 
    mutate(village = village_levels) %>% 
    dplyr::select(village,everything()) %>% 
    arrange(mean_cnt)
  
  return(compv)
}

#ZIGP has ~x2 uncertainty
comp_v_p <- vposts(offmgmt_v_p)
comp_v_zigp <- vposts(offmgmt_v_zigp)

quantile(comp_v_p$width_cnt)
quantile(comp_v_zigp$width_cnt)

quantile(comp_v_p$width_per)
quantile(comp_v_zigp$width_per)

#ZIGP has lower mean change
quantile(comp_v_p$mean_cnt)
quantile(comp_v_zigp$mean_cnt)

quantile(comp_v_p$mean_per)
quantile(comp_v_zigp$mean_per)

#which better maps to observed?
##Poission is far closer

comp_obs <- offtake_period %>% 
  group_by(village,period) %>% 
  summarize(animals = mean(animals_e)) %>% 
  pivot_wider(names_from = period, values_from = animals) %>% 
  mutate(mean_cnt = post - pre,
         mean_per = (post - pre)/pre*100)

com_tab <- left_join(
  left_join(
    comp_obs %>% 
        dplyr::select(village, 
                obs_cnt = mean_cnt,
                obs_per = mean_per),
    comp_v_p %>% 
      dplyr::select(village, 
                p_cnt = mean_cnt,
                p_per = mean_per)
  ),
    comp_v_zigp %>% 
      dplyr::select(village, 
                zigp_cnt = mean_cnt,
                zigp_per = mean_per)
) %>% 
  arrange(obs_cnt)

com_tab

###hunt-level posteriors

fvl <- levels(follows %>% 
                pull(village) %>% 
                droplevels())

#min n hunts pre and post is ~20
follows %>% 
  group_by(village,period) %>% 
  summarize(nhunt = n())

#only use hunters that hunted both pre and post to predict

prehunters <- follows %>% 
  filter(period == "pre") %>% 
  pull(hunter) %>% 
  unique() %>%
  droplevels()

posthunters <- follows %>% 
  filter(period == "post") %>% 
  pull(hunter) %>% 
  unique() %>%
  droplevels()

hunters_prepost <- follows %>% 
  filter(hunter %in% prehunters &
           hunter %in%  posthunters) %>% 
  group_by(village,hunter,period) %>% 
  dplyr::summarise(n_hunts = n()) %>%
  pivot_wider(names_from = period,
              values_from = n_hunts)

#predict only with hunters that had at least 2 hunts pre and post
hunters_prepost2 <- hunters_prepost %>% 
  filter(pre > 1 & post > 1) %>% 
  droplevels()

length(unique(follows$hunter))
nrow(hunters_prepost2) 

#gun hunters and trappers
hunters_prepost2_guntrap <- follows %>% 
  filter(hunter %in% hunters_prepost2$hunter) %>% 
  group_by(village,hunter,guntrap) %>% 
  dplyr::summarize(n_hunt = n()) %>% 
  pivot_wider(names_from = guntrap, 
              values_from = n_hunt) %>% 
  mutate(total = rowSums(across(trap:both), na.rm = T)) %>% 
  replace(is.na(.), 0) 

thunters_tmp <- hunters_prepost2_guntrap %>% 
  filter(trap > 0 | both > 0) %>% 
  pull(hunter) %>% 
  droplevels()

ghunters_tmp <- hunters_prepost2_guntrap %>% 
  filter(gun > 0 | both > 0) %>% 
  pull(hunter) %>% 
  droplevels()

thunters_prepost2 <- hunters_prepost %>% 
  filter(hunter %in% thunters_tmp & 
           pre > 1 & post > 1) %>% 
  droplevels()

ghunters_prepost2 <- hunters_prepost %>% 
  filter(hunter %in% ghunters_tmp & 
           pre > 1 & post > 1) %>% 
  droplevels()

#number of hunters by kind with > 1 hunt pre and post by village
#lowest for all and gun hunters is 5, lowest for trappers is 3...
#...(though often, eg., E9_H13 a `trapper` may have been so because
#he recorded a single trap but aside from that only gun-hunted
bind_rows(
  hunters_prepost2 %>% mutate(hukind = "all"),
  ghunters_prepost2 %>% mutate(hukind = "gun"),
  thunters_prepost2 %>% mutate(hukind = "trap"),
) %>% 
  group_by(village,hukind) %>% 
  summarize(n_hunter = n()) %>% 
  pivot_wider(names_from = hukind,
              values_from = n_hunter)

hposts <- function (mf,hin) {
  
    comph_list <- list()
    for (i in seq_along(fvl)) {
      
      mftmp <- mf
      
      vtmp <- fvl[i]
      
      htmp <- (hin %>% 
        filter(village == vtmp) %>% 
        pull(hunter) %>% 
        rep(125))[1:365]
      
      pre_vtmp <- tibble(village = rep(vtmp, 365),
                         hunter = htmp,
                         period = "pre")
      
      post_vtmp <- pre_vtmp %>% mutate(period = "post")
      
      #pps and contrasts
      pre_pp_tmp <- posterior_epred(mftmp, newdata = pre_vtmp)
      post_pp_tmp <- posterior_epred(mftmp, newdata = post_vtmp)
      
      chl <- tibble(
        pre = c(pre_pp_tmp),
        post = c(post_pp_tmp),
        cnt_diff = post - pre,
        per_diff = (post - pre)/pre*100 #%>% round(0)
      )
      
      chw <- tibble(mean_cnt = mean(chl$cnt_diff),
                    lui_cnt = quantile(chl$cnt_diff,.04),
                    uui_cnt = quantile(chl$cnt_diff,.96),
                    width_cnt = abs(uui_cnt -lui_cnt),
                    mean_per = mean(chl$per_diff),
                    lui_per = quantile(chl$per_diff,.04),
                    uui_per = quantile(chl$per_diff,.96),
                    width_per = abs(uui_per -lui_per),
      ) #%>% 
      #   mutate(across(contains("cnt"), ~ round(.x,1)),
      #          across(contains("per"), ~ round(.x,0))
      #   )
      
      comph_list[[i]] <- chw
    }
    compv <- bind_rows(comph_list) %>% 
      mutate(village = fvl) %>% 
      dplyr::select(village,everything()) %>% 
      arrange(mean_cnt)
    
    return(compv) 
}

##poisson versus zigp
comp_h_p <- hposts(offmgmt_h_p, hunters_prepost2)
comp_h_zigp <- hposts(offmgmt_h_zigp, hunters_prepost2)

#zigp more uncertain
quantile(comp_h_p$width_cnt)
quantile(comp_h_zigp$width_cnt)

quantile(comp_h_p$width_per)
quantile(comp_h_zigp$width_per)

#very similar mean change
quantile(comp_h_p$mean_cnt)
quantile(comp_h_zigp$mean_cnt)

quantile(comp_h_p$mean_per)
quantile(comp_h_zigp$mean_per)

comp_h_p
comp_h_zigp

#gun versus trap change
comp_h_p_g <- hposts(offmgmt_h_p_g, ghunters_prepost2)
comp_h_p_t <- hposts(offmgmt_h_p_t, thunters_prepost2)

#most change is gun
quantile(comp_h_p$mean_cnt)
quantile(comp_h_p_g$mean_cnt)
quantile(comp_h_p_t$mean_cnt)

#no change in trapping (aside from minor decrease E9)
comp_h_p
comp_h_p_g
comp_h_p_t

#so we retain the Poissin distribution models
offmgmt_v_p
offmgmt_h_p

#we've seen they predict well, last check of chains and rhats

mcmc_trace(offmgmt_v_p)
mcmc_trace(offmgmt_h_p)

mcmc_rhat(rhat(offmgmt_v_p)) + yaxis_text(hjust = 1)
mcmc_rhat(rhat(offmgmt_h_p)) + yaxis_text(hjust = 1)

#plot the final panel of the figure

change2plot <- bind_rows(comp_v_p %>%
                           mutate(`Animals hunted (92% UI)` = "Per day, village-wide"),
                         comp_h_p %>%
                           mutate(`Animals hunted (92% UI)` = "Per (recorded) hunt"),
) %>% 
  mutate(village = factor(village, levels = comp_v_p$village),
         `Animals hunted (92% UI)` = factor(`Animals hunted (92% UI)`,
                                            levels = c("Per day, village-wide", 
                                                       "Per (recorded) hunt"))
  ) 

change2plot <- left_join(change2plot,
                         village_meta %>% 
                           dplyr::select(village,mgmt)
) %>% 
  dplyr::select(village,`Hunting management` = mgmt,
                `Animals hunted (92% UI)`, everything())

min(change2plot$lui_cnt)
max(change2plot$uui_cnt)

min(change2plot$lui_per)
max(change2plot$uui_per)

mgmt_cols <- c(`Not proposed` = "gray",
               `Before project` = met_jodi(9,10),
               Implemented = met_jodi(6,10),
               Unimplemented = met_jodi(1,10)
)

mgmt_lt <- c(`Per day, village-wide` = "solid",
             `Per (recorded) hunt` = "dashed"
)

mgmt_cnt_plot <- ggplot(change2plot, aes(x=village, y=mean_cnt, 
                                         linetype = `Animals hunted (92% UI)`, 
                                         colour = `Hunting management`, 
                                         fill = `Hunting management`)) +
  geom_hline(yintercept=0, linetype="dotted", color = "black") +
  geom_errorbar(aes(ymin=lui_cnt, ymax=uui_cnt),
                width= 0, position=position_dodge(.6))+
  geom_point(position=position_dodge(.6),
             shape = 21,  size = 3, colour = "black", show.legend = FALSE) +
  scale_fill_manual(values = c(mgmt_cols)) +
  scale_colour_manual(values = c(mgmt_cols)) +
  scale_linetype_manual(values = c(mgmt_lt)) +
  labs(x = "\nVillage", 
       #fill = (as.expression(bquote(bold("Hunting management")))),
       linetype = (as.expression(bquote(bold("Animals hunted (92% UI)"))))
  )+
  guides(color = "none" 
         #,fill = guide_legend(order = 1)
  )+
  scale_y_continuous(name = "Post-management\nestimated change\n", 
                     limits = c(-3.6,1.4),
                     #breaks = seq(-0.6,0.6, by = 0.3),
                     #labels = seq(-0.6,0.6, by = 0.3)
  ) +
  theme(#legend.position = c(0.09,0.97),
    legend.title = element_text(size=10),
    legend.text = element_text(size=10),
    axis.line = element_blank(),
    axis.ticks = element_line(colour = "black"),
    axis.text=element_text(size=11, colour = "black"),
    legend.justification = "top",
    axis.title.x = element_text(size = 12),
    axis.title.y= element_text(size = 12),
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(colour = "black", fill=NA, size=0.8),
    plot.margin = unit(c(0.4, 2, 0.4, 0.2), "cm"))

mgmt_cnt_plot

#final plot
dev.off()

cairo_pdf(file = "./outputs/figs/f1.pdf", 
          width = 10, height =  10) 

ggdraw() +
  draw_plot(pre_mgmt_plot,
            x = 0.017, 
            y = 0.3 -0.025,
            width = 0.33, height = 0.7) +
  draw_plot(mgmtoff_plot,
            x = 0.33 + 0.017,
            y = 0.3,
            width = 0.71, #0.667
            height = 0.7) +
  draw_plot(mgmt_cnt_plot,
            x = 0.055, width = 1,
            y = 0, height = 0.3)

dev.off()

