# set folder
setwd("/Users/joekidson/Library/Mobile Documents/com~apple~CloudDocs/Economics Masters/dissertation/Services")

# load libraries
library(penppml)
library(tidyverse)
library(OECD)
library(readxl)
library(sjmisc)

# get services data
name <- "BATIS_EBOPS2010"
#browse_metadata(name)
info <- get_data_structure(name)
sectors <- info$SERVICE$id
importers <- info$LOCATION$id
exporters <- info$PARTNER$id

# sample
df_services_bilat <- get_dataset(name, filter = list(c("AUS"), c("BEL"), c("D"), sectors, c("BV")))#, start_time = 2018, end_time = 2018)

# actual
# y <- 2005
# df_services_bilat <- get_dataset(name, filter = list(importers, exporters, c("D"), sectors, c("BV")), start_time = 2005, end_time = 2005)
# write.csv(df_services_bilat, paste0("data/bilateral_services_bysectors_", y, ".csv"))


# years <- seq(2007, 2019, 1)
# for (y in years) {
#   print(y)
#   write.csv(get_dataset(name, 
#                                          filter = list(importers, exporters, c("D"), sectors, c("BV")), 
#                                          start_time = y, end_time = y
#                              ),
#   paste0("data/bilateral_services_bysectors_", y, ".csv"))
# }

# y <- 2005
# df_bilat <- read.csv(paste0("data/bilateral_services_bysectors_", y, ".csv")) %>% select(!X)
# for (y in years) {
#   print(y)
#   df_bilat <- df_bilat %>% rbind(read.csv(paste0("data/bilateral_services_bysectors_", y, ".csv")) %>% select(!X))
# }
# write.csv(df_bilat, "data/bilateral_services_by_sectors_2005_2019.csv")
df_bilat <- read.csv("data/bilateral_services_by_sectors_2005_2019.csv") %>% select(!X)

# load FTA provisions - FTA in place, covers services?, provisions
df_services_ftas <- read_excel("data/chapter-4.-services.xlsx", sheet = "Transpose")

# code services options

df_services_fta <- df_services_ftas %>%
  select(!c(dr_qual_to, dr_sinwin_nat, exc_oth)) %>%
  mutate(othdip_mon = ifelse(othdip_mon == "NA", "0", othdip_mon))

df_services_ftas_dummies <- df_services_ftas %>%
  mutate(dr_objec_nat = ifelse(id == 194, "NA", dr_objec_nat),
         mov_prov_cov = ifelse(mov_prov == 0, "0", mov_prov_cov),
         mov_prov_emp = ifelse(mov_prov == 0, "0", mov_prov_emp),
         othdip_monr = ifelse(othdip_mon == 0, "0", othdip_monr)) %>%
  mutate_at(vars(-one_of(c('PTAcode', 'id', 'year_inforce'))), funs(recode(., NR = "NA"))) %>%
  to_dummy(dis_ma, dispute, dr, dr_inf_nat, dr_licdec_nat, dr_mutrec_nat, dr_objec_nat, dr_status_nat,
           mov_prov_cov, mov_prov_emp, othdip_add, othdip_epr, othdip_grad, othdip_lcr, othdip_lpr, othdip_monr,
           othdip_new, othdip_oth, othdip_sen, othdip_ttr, roo_m3, roo_m4, s_exc_air, s_exc_airt, s_exc_gov, s_exc_oth,
           s_lib_app, s_pol_gov, s_pol_jobs, s_pol_oth, s_pol_subs, struc, struc_chapt1, struc_hier, trans_comm_nat, trans_nat,
           suffix = "label") %>%
  mutate(othdip_monr_A = ifelse(`othdip_monr_A,B`, 1, othdip_monr_A),
         othdip_monr_B = ifelse(`othdip_monr_A,B`, 1, othdip_monr_B),
         roo_m3_A = ifelse(`roo_m3_A,B,C`, 1, roo_m3_A),
         roo_m3_B = ifelse(`roo_m3_A,B,C`, 1, roo_m3_B),
         roo_m3_C = ifelse(`roo_m3_A,B,C`, 1, roo_m3_C),
         trans_comm_nat_A = ifelse(`trans_comm_nat_A,C`, 1, trans_comm_nat_A),
         trans_comm_nat_C = ifelse(`trans_comm_nat_A,C`, 1, trans_comm_nat_C))%>%
  select(!ends_with("NA") & !c(dr_inf_nat_C, dr_status_nat_C, `othdip_monr_A,B`, roo_m3_G, s_lib_app_C, `trans_comm_nat_A,C`,
                               trans_comm_nat_D, trans_nat_C))

df_services_ftas_clean <- df_services_ftas %>%
  select(PTAcode:Party2, dis_mfn, dis_nt, dr_inf, dr_licdec, dr_mutrec, dr_objec, dr_qual, dr_qual_t, dr_sinwin, dr_status, exc, 
         exc_pru_fins, exc_sec, s_lib_rat, s_lib_stand, safe_bop, safeg, safeg_reneg, struc_chapt, trans, trans_app,
         trans_comm) %>%
  cbind(df_services_ftas_dummies)

# merge datasets to establish FTA application
df_fta <- trade %>% left_join(df_services_ftas_clean %>% select(!c(PTAcode, PTAname, Party1, Party2)), by = "id")
df_fta <- df_fta %>%
  select(!c(time, export)) %>%
  filter(id != 0 & !is.na(year_inforce)) %>%
  distinct(exp, imp, id, .keep_all = TRUE)

# join onto services data
df_bilat <- df_bilat %>%
  filter(SERVICE == "S") %>%
  select(!c(EXPRESSION, MEASURE, TIME_FORMAT, SERVICE)) %>%
  rename(imp = LOCATION, exp = PARTNER, time = obsTime, service_trade = obsValue)

# obtain fta rows
df_all <- df_bilat %>% 
  left_join(df_fta, by = c("imp", "exp")) %>%
  filter(time >= year_inforce)

df_merged <- df_bilat %>% 
  left_join(df_all, by = c("imp", "exp", "time", "service_trade")) %>% 
  relocate(year_inforce, .after = time)

# look at multiple countries problem
df_merged %>% group_by(imp, exp, time) %>% summarise(n=n()) %>% filter(n != 1)
View(df_merged %>% filter(imp == "AUS", exp == "MYS"))

# ok so take the most recent FTA 
df_final <- df_merged %>% 
  arrange(desc(year_inforce)) %>%
  distinct(imp, exp, time, .keep_all = TRUE) %>%
  arrange(imp, exp, time)

df_final %>% group_by(imp, exp, time) %>% summarise(n=n()) %>% filter(n != 1)

# clean up nas
df_final <- df_final %>%
  replace(is.na(.), 0)

# save data and go to regressions.R
write.csv(df_final, "data/regression_data_2005_2016_allservices_servicesFTAs_cleaned.csv")

# checks
nrow(df_merged %>% distinct(id))
