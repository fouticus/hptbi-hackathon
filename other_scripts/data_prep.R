# load some functions without loading the whole namespace
`%>%`   <- magrittr::`%>%`
select  <- dplyr::select
mutate  <- dplyr::mutate
filter  <- dplyr::filter
arrange <- dplyr::arrange
case_when <- dplyr::case_when
group_by  <- dplyr::group_by
summarize <- dplyr::summarize
ungroup   <- dplyr::ungroup
left_join <- dplyr::left_join
pivot_longer <- tidyr::pivot_longer
pivot_wider  <- tidyr::pivot_wider
add_case     <- tibble::add_case


# data frame
# mode: either "fss" or "mort"
data_prep <- function(df, mode){
  studyids <- df %>% select(studyid) %>% mutate(idorder = 1:nrow(df))
  admit_vars <- grep("admitto*", names(df), value=T)
  
  # demographics
  df_demo <- df %>% select(studyid, female, age)
  
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
    group_by(studyid, cath_n, cathtype2) %>% 
    summarize(cathdt = sum(cathdt)) %>% 
    ungroup() %>%
    filter(cathdt > 0) %>%
    add_case(studyid = -999, cathtype2 = "cath_dt_cv") %>%
    add_case(studyid = -999, cathtype2 = "cath_dt_art") %>%
    add_case(studyid = -999, cathtype2 = "cath_dt_picc") %>%
    tidyr::pivot_wider(id_cols=c(studyid, cath_n), names_from=cathtype2, values_from=cathdt, names_sort=T, values_fill=0) %>%
    dplyr::mutate(cath_dt_tot = cath_dt_cv + cath_dt_art + cath_dt_picc) %>%
    dplyr::filter(studyid != -999)
  # todo: time from cath type to fss? time from last cath to fss?
  
  # icu time, count
  df_icu <- df %>%
    dplyr::select(studyid, starts_with("admittoicu")) %>%
    dplyr::mutate(icudt1 = admittoicudc1,
           icudt2 = admittoicudc2 - admittoicuadmit2,
           icudt3 = admittoicudc3 - admittoicuadmit3) %>% 
    dplyr::select(-starts_with("admittoicu")) %>%
    tidyr::pivot_longer(starts_with("icudt"), names_to="icudt_name", values_to="icu_dt") %>%
    dplyr::filter(!is.na(icu_dt), icu_dt > 1) %>%  
    dplyr::group_by(studyid) %>% 
    dplyr::summarize(icu_dt_tot = sum(icu_dt), icu_n = dplyr::n()) %>% dplyr::ungroup()
    
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
    add_case(studyid = -999, pup_name = "pup_reactive") %>%
    add_case(studyid = -999, pup_name = "pup_fixed") %>%
    add_case(studyid = -999, pup_name = "pup_unknown") %>%
    add_case(studyid = -999, pup_name = "pup_one_fixed") %>%
    mutate(val = 1) %>%
    pivot_wider(id_cols=studyid, names_from=pup_name, values_from=val, values_fill=0) %>% 
    mutate(pup_n_reactive = ifelse(pup_unknown==1, NA, 2*pup_reactive + pup_one_fixed)) %>%
    select(-pup_fixed) %>%
    filter(studyid != -999)
  
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
    add_case(studyid=-999, icptype2 = "icp_dt_intp") %>%
    add_case(studyid=-999, icptype2 = "icp_dt_vent") %>%
    add_case(studyid=-999, icptype2 = "icp_dt_other") %>%
    group_by(studyid, icp_n, icptype2) %>% summarize(icpdt = sum(icpdt)) %>% ungroup() %>% 
    filter(icpdt > 0) %>%
    pivot_wider(id_cols=c(studyid, icp_n), names_from=icptype2, values_from=icpdt, values_fill=0) %>% 
    mutate(icp_dt_tot = icp_dt_intp + icp_dt_vent + icp_dt_other) %>%
    filter(studyid != 999)
  
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
  if(mode=="fss"){
    df_tfss <- df %>% select(studyid, admittofss)
  }
  
  
  df2 <- df_demo %>% 
    left_join(studyids) %>%
    left_join(df_cath) %>%
    left_join(df_icu) %>% 
    left_join(df_int) %>% 
    left_join(df_pup) %>%
    left_join(df_icp) %>%
    left_join(df_gcs) %>%
    left_join(df_yn)
  
  if(mode=="fss"){
    df2 <- df2 %>% left_join(df_tfss)
  }
  df2 <- df2 %>% 
    arrange(idorder) %>% 
    select(-c(studyid, idorder))
  
  list(X=df2)
}
