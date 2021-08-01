# investigate joint characteristics of variables

################################################################################
## setup 

rm(list=ls()); gc()
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(tidyr)
library(GGally)
library(gtsummary)
library(gt)



################################################################################
### load data

df <- read.csv(file.path("csvs", "training.csv"))

df <- df %>% 
  mutate(fss = fssmental + fsssensory + fsscommun + fssmotor + fssfeeding + fssresp)




################################################################################
### clustering y/n variables

ynvars <- grep("yn$", names(df))
?dist
d <- dist(df[,ynvars], "manhattan")

?hclust
hc <- hclust(d)
plot(hc)
cut1 <- cutree(hc, h=7.5)
cut2 <- cutree(hc, h=4.5)
cut3 <- cutree(hc, h=0.5)

df$c1 <- cut1
df$c2 <- cut2
df$c3 <- cut3

df %>% 
  mutate(mortality = 1*(hospdisposition=="Mortality")) %>% 
  select(c(c1, c2, c3, ynvars, mortality)) %>%
  group_by(c1) %>% add_count() %>% summarize_all(mean) %>% round(2) %>% View()

df %>% 
  mutate(mortality = 1*(hospdisposition=="Mortality")) %>% 
  select(c(c1, c2, c3, ynvars, mortality)) %>% 
  group_by(c1, c2) %>% add_count() %>% summarize_all(mean) %>% round(2) %>% View()

df %>% 
  mutate(mortality = 1*(hospdisposition=="Mortality")) %>% 
  select(c(c1, c2, c3, ynvars, mortality)) %>% 
  group_by(c1, c2, c3) %>% add_count() %>% summarize_all(mean) %>% round(2) %>% View()


################################################################################
### alternative representation for time based variables?

admit_vars <- grep("admitto*", names(df), value=T)

dft <- df %>% 
  mutate(mort = hospdisposition == "Mortality") %>%
  select(c(admit_vars, studyid, age, female, fss, mort)) %>%
  pivot_longer(admit_vars, names_to="var", values_to="days") %>%  
  filter(!is.na(days)) %>% 
  mutate(label = stringr::str_replace(var, "admitto", ""),
         label_nonum = stringr::str_replace(label, "[1-4]", ""),
         start_end = case_when(grepl("start", label_nonum) ~ "start",
                                grepl("end", label_nonum) ~ "end"),
         admit_dis = case_when(grepl("admit", label_nonum) ~ "admit",
                               grepl("dc", label_nonum) ~ "discharge"),
         cat = stringr::str_replace_all(label_nonum, c("start"="", "end"="", "admit"="", "dc"="")),
         cat = case_when(cat=="int" | cat=="ext" | cat=="trach" ~ "int/ext/trach",
                         cat=="tpn" | cat=="gast" | cat=="entnut" ~ "nutri",
                         cat=="crani" | cat=="edhevac" | cat == "lmbdrain" | cat == "sdhevac" ~ "head",
                         cat=="ct" | cat=="icp" ~ "diag",
                         TRUE ~ cat),
         ypos = case_when(cat == "cath" ~ 0.5,
                          cat == "diag" ~ 1.0,
                          cat == "nutri" ~ 1.5,
                          cat == "int/ext/trach" ~ -0.5,
                          cat == "head" ~ -1.0,
                          cat == "icu" ~ -1.5,
                          cat == "fss" ~ -2.0)
         ) %>%
  mutate(label = as.factor(label),
         label_nonum = as.factor(label_nonum), 
         cat = as.factor(cat)) %>% 
  #mutate(days = case_when(days >= 365 ~ days-365, TRUE ~ days*1)) %>% 
  mutate()

df %>% ggplot(aes(log(fss))) + geom_histogram()

ids <- df[order(-df$fss),"studyid"]
js <- seq(1, length(ids), 5)
for(i in seq_along(js)){
  print(i)
  if(i == 1){next}
  dft2 <- dft %>% filter(studyid %in% ids[js[i-1]:(js[i]-1)])
  df2 <- df %>% filter(studyid %in% ids[js[i-1]:(js[i]-1)])
  dft2 %>% 
    arrange(fss) %>% 
    ggplot(aes(days, ypos , color=cat, label=label)) +
    geom_hline(yintercept=0, size=1.2) + 
    geom_segment(aes(yend=0, xend=days), size=1.2) + 
    geom_label(angle=0, hjust=0.5, label.size=0) +
    geom_point(aes(y=0), size=5.0) + 
    geom_text(data=df2, aes(x=Inf, y=Inf, color=NA, label=paste0(ifelse(female==1, "female", "male"),
                                                                 ", age:", round(age/365, 1), "  ",
                                                                 "\nfss:", fss, "  "), hjust=1, vjust=1)) +
    scale_y_continuous(breaks=c()) +
    scale_x_continuous(breaks=seq(0, max(dft2$days), 10)) + 
    labs(y="") + 
    facet_grid(factor(studyid, levels=unique(studyid[order(-fss)]))~.)
  ggsave(file.path(getwd(), "other_output", "eda2", "timelines", paste0("timeline_", i-1, ".png")), height=14, width=16, units="in", dpi=300)
}

dft %>% 
  arrange(fss) %>% 
  #filter(studyid < 120) %>%
  filter(fss >= 30) %>%
  ggplot(aes(days, ypos , color=cat, label=label)) +
  geom_hline(yintercept=0, size=1.2) + 
  geom_segment(aes(yend=0, xend=days), size=1.2) + 
  geom_label(angle=0, hjust=0.5, label.size=0) +
  geom_point(aes(y=0), size=5.0) + 
  geom_text(data=df %>% 
              #filter(studyid < 120)
              filter(fss >=30)
            , aes(x=Inf, y=Inf, color=NA, 
                                                   label=paste0(ifelse(female==1, "female", "male"), 
                                                                ", age:", round(age/365, 1),
                                                                "\nfss:", fss), hjust=1, vjust=1)) + 
  scale_y_continuous(breaks=c()) +
  labs(y="") + 
  facet_grid(factor(studyid, levels=unique(studyid[order(-fss)]))~.)

################################################################################
### 

df %>% ggplot(aes(age, fss, color=as.factor(female))) + geom_point()
df %>% ggplot(aes(admittofss, fss, color=age)) + geom_point() + 
  scale_color_viridis_c()



################################################################################
### feature engineering

admit_vars <- grep("admitto*", names(df), value=T)

scales <- list()
scales[["age"]] <- c(mean(df$age), sd(df$age))
names(df)

get_scale_fn <- function(scales){
  scale_var <- function(x, varname){
    s <- scales[[varname]]
    (x - s[1])/s[2]
  }
}
scale_fn <- get_scale_fn(scales)

# responses and demographics
df_resp <- df %>% 
  mutate(fss = fssmental + fsssensory + fsscommun + fssmotor + fssfeeding + fssresp,
         mort = (hospdisposition == "Mortality")) %>% 
  select(studyid, fss, mort, female, age)

# catheters time, count, type
df_cath <- df %>% 
  select(studyid, contains("cath")) %>% 
  mutate(cathdt1 = admittocathend1 - admittocathstart1,
         cathdt2 = admittocathend2 - admittocathstart2,
         cathdt3 = admittocathend3 - admittocathstart3,
         cathdt4 = admittocathend4 - admittocathstart4,
         cath_n = 4 - is.na(cathdt1) - is.na(cathdt2) - is.na(cathdt3) - is.na(cathdt4)) %>%
  select(-starts_with("admittocath")) %>%
  pivot_longer(starts_with("cathdt"), names_to="cathdt_name", values_to="cathdt") %>% 
  filter(!is.na(cathdt)) %>%
  pivot_longer(starts_with("cathtype"), names_to="cathtype_name", values_to="cathtype") %>%
  mutate(cathdt_num = as.integer(substr(cathdt_name, 7, 7)),
         cathtype_num = as.integer(substr(cathtype_name, 9, 9))) %>% 
  filter(cathdt_num == cathtype_num) %>% 
  select(studyid, cath_n, cathtype, cathdt) %>%
  mutate(cathtype2 = case_when(cathtype == "Central venous catheter" ~ "cath_dt_cv",
                               cathtype == "Arterial catheter" ~ "cath_dt_art",
                               cathtype == "Peripherally inserted central catheter (PICC)" ~ "cath_dt_picc"
                               )) %>%
  group_by(studyid, cath_n, cathtype2) %>% summarize(cathdt = sum(cathdt)) %>% ungroup() %>%
  filter(cathdt > 0) %>%
  pivot_wider(id_cols=c(studyid, cath_n), names_from=cathtype2, values_from=cathdt, values_fill=0) %>%
  mutate(cath_dt_tot = cath_dt_cv + cath_dt_art + cath_dt_picc)
# todo: time from cath type to fss? time from last cath to fss?

# icu time, count
df_icu <- df %>%
  select(studyid, starts_with("admittoicu")) %>%
  mutate(icudt1 = admittoicudc1,
         icudt2 = admittoicudc2 - admittoicuadmit2,
         icudt3 = admittoicudc3 - admittoicuadmit3) %>% 
  select(-starts_with("admittoicu")) %>%
  pivot_longer(starts_with("icudt"), names_to="icudt_name", values_to="icu_dt") %>%
  filter(!is.na(icu_dt), icu_dt > 1) %>%  
  group_by(studyid) %>% 
  summarize(icu_dt_tot = sum(icu_dt), icu_n = n()) %>% ungroup()
  
# intubation time, count
df_int <- df %>%
  select(studyid, "admittoint", "admittoext") %>%
  mutate(intub_dt = admittoext-admittoint) %>% 
  filter(intub_dt > 0) %>%
  select(studyid, intub_dt)

# pupils
df_pup <- df %>%
  select(studyid, puplrcticu) %>%
  mutate(pup_name = case_when(puplrcticu == "Both Reactive" ~ "pup_reactive",
                              puplrcticu == "Both Fixed" ~ "pup_fixed",
                              puplrcticu == "Unknown" ~ "pup_unknown",
                              puplrcticu == "" ~ "pup_unknown",
                              is.na(puplrcticu) ~ "pup_unknown",
                              puplrcticu == "One Fixed" ~ "pup_one_fixed")) %>%
  mutate(val = 1) %>%
  pivot_wider(id_cols=studyid, names_from=pup_name, values_from=val, values_fill=0) %>% 
  mutate(pup_n_reactive = ifelse(pup_unknown==1, NA, 2*pup_reactive + pup_one_fixed)) %>%
  select(-pup_fixed)

# icp monitor time, count
df_icp <- df %>%
  select(studyid, contains("icp")) %>% 
  mutate(icpdt1 = admittoicpend1 - admittoicpstart1,
         icpdt2 = admittoicpend2 - admittoicpstart2,
         icpdt3 = admittoicpend3 - admittoicpstart3,
         icp_n = 3 - is.na(icpdt1) - is.na(icpdt2) - is.na(icpdt3)) %>% 
  select(-starts_with("admittoicp")) %>% filter(icp_n > 0) %>% 
  pivot_longer(starts_with("icpdt"), names_to="icpdt_name", values_to="icpdt") %>%
  filter(!is.na(icpdt)) %>% 
  pivot_longer(starts_with("icptype"), names_to="icptype_name", values_to="icptype") %>% 
  mutate(icpdt_num = as.integer(substr(icpdt_name, 6, 6)),
         icptype_num = as.integer(substr(icptype_name, 8, 8))) %>%
  filter(icpdt_num == icptype_num) %>%
  select(studyid, icp_n, icptype, icpdt) %>%
  mutate(icptype2 = case_when(icptype == "Intraparenchymal (Camino or bolt)" ~ "icp_dt_intp",
                              icptype == "Ventriculostomy (External Ventricular Drain or EVD)" ~ "icp_dt_vent",
                              icptype == "Other" ~ "icp_dt_other",
                              icptype == "" ~ "icp_dt_other")) %>% 
  group_by(studyid, icp_n, icptype2) %>% summarize(icpdt = sum(icpdt)) %>% ungroup() %>% 
  filter(icpdt > 0) %>%
  pivot_wider(id_cols=c(studyid, icp_n), names_from=icptype2, values_from=icpdt, values_fill=0) %>% 
  mutate(icp_dt_tot = icp_dt_intp + icp_dt_vent + icp_dt_other)

# GCS variables
df_gcs <- df %>% select(studyid, contains("gcs")) %>% select(-contains("yn"))

# yes/no variables

df_yn <- df %>% select(studyid, contains("yn"))

# ct scan
df_ct <- df %>% 
  select(studyid, starts_with("ct"))

# time relative to fss
# df_tfss <- 
#df %>% 
#  mutate(across(starts_with("admitto"), ~ admittofss - .x)) %>% 
#  rename_with(~paste0(stringr::str_replace(.x, "admitto", ""), "tofss"), admit_vars) %>%
#  select(ends_with("tofss"))

# time to fss
df_tfss <- df %>% 
  select(studyid, admittofss)


df2 <- df_resp %>% 
  left_join(df_cath) %>%
  left_join(df_icu) %>% 
  left_join(df_int) %>% 
  left_join(df_pup) %>%
  left_join(df_icp) %>%
  left_join(df_gcs) %>%
  left_join(df_yn) %>% 
  left_join(df_tfss)

## impute values
X <- df2 %>% select(-c(studyid, mort, fss))

## scale values
X_mean <- apply(X, 2, function(x) mean(x, na.rm=T))
X_sd <- apply(X, 2, function(x) sd(x, na.rm=T))
X2 <- sapply(seq_len(ncol(X)), function(i){x2 <- (X[,i] - X_mean[i])/X_sd[i]; x2[is.na(x2)] <- 0; x2})
colnames(X2) <- names(X)

Ym <- df2$mort
Yf <- df2$fss

c2 <- !grepl("fss", colnames(X2))
nnam <- !is.na(Ym)
rfm <- randomForest::randomForest(X2[nnam,c2], as.factor(Ym[nnam]), ntree=1000, classwt = c(mean(Ym), 1-mean(Ym)))
rfm

nnaf <- !is.na(Yf)
rff <- randomForest::randomForest(X2[nnaf,], Yf[nnaf], ntree=2000, mtry=ncol(X2)/2, sampsize=ceiling(0.6*sum(nnaf)))





  


