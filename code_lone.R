
library(labelled)
library(forcats)
library(dplyr)
#
library(broom)
#
library(purrr)

source("luddite/FUN_varview.R")

l_data <- haven::read_sav("ISSN_13countries_25ormore.sav")


l_data <- drop_unused_value_labels(l_data)
# varview(l_data, vju = FALSE)

#### l_data$INDEX_LONELY |> hist()

## 13 countries ## OK
## l_data$country |> to_factor() |> fct_count(sort = TRUE) |> View()
# table(l_data$Istok_dummy, to_factor(l_data$country))


# l_data$RELIGIJA_REC

# xtabs(~ as_factor(country) + as_factor(RELIGIJA_REC), data = l_data) |> 
#   proportions(margin = 1) |> 
#   round(2)*100


###

# xtabs(~ to_factor(SEX) + MALE_DUMMY, data = l_data, addNA = TRUE)
# xtabs(~ AGE + dob5_rec, data = l_data)
# l_data$v34 |> table()
# l_data$v35 |> table()
# cor(l_data$v34, l_data$v35, use = "complete.obs")

##

# l_data$v35 |> table()

##

# l_data |> 
#   mutate(dobREK = case_when(
#     AGE < 35 ~ "25-34",
#     AGE >= 35 & AGE < 45 ~ "35-44",
#     AGE >= 45 & AGE < 55 ~ "45-54",
#     AGE >= 55 & AGE < 65 ~ "55-64",
#     AGE >= 65 & AGE < 75 ~ "65-74",
#     TRUE ~ "75+"
#   ))|> pull(dobREK) |> as.factor() |> fct_relevel("45-54") |> 
#   table()


model_data <- tibble(country = to_factor(l_data$country),
                     
                     INDEX_LONELY = l_data$INDEX_LONELY,
                     
                     spol = l_data$SEX |> to_factor() |> fct_relevel("Female"),
                     
                     AGE_continuous = l_data$AGE,
                     
                     dob_rec = l_data |> 
                       mutate(dob_rec = case_when(
                         dob1_rec == 1 ~ "25-34",
                         dob2_rec == 1 ~ "35-44",
                         dob4_rec == 1 ~ "55-64",
                         dob5_rec == 1 ~ "65+",
                         TRUE ~ "aaaREF45-54"
                       )) |> pull(dob_rec) |> as.factor() |> fct_relevel("aaaREF45-54"),
                     
                     dobREK = l_data |> 
                       mutate(dobREK = case_when(
                         AGE < 35 ~ "25-34",
                         AGE >= 35 & AGE < 45 ~ "35-44",
                         AGE >= 45 & AGE < 55 ~ "45-54",
                         AGE >= 55 & AGE < 65 ~ "55-64",
                         AGE >= 65 & AGE < 75 ~ "65-74",
                         TRUE ~ "75+"
                       ))|> pull(dobREK) |> as.factor() |> fct_relevel("45-54"),
                     
                     educ = l_data$DEGREE,
                     
                     # participation = l_data |> 
                     #   mutate(x = case_when(
                     #     Participation_culture_dummy == 1 ~ "culture",
                     #     Participation_politics_dummy == 1 ~ "politics",
                     #     Participation_voluntary_dummy == 1 ~ "voluntary",
                     #     TRUE ~ "aaaREF"
                     #   )) |> pull(x) |> as.factor(),
                     
                     Participation_culture = l_data$Participation_culture_dummy,
                     Participation_politics = l_data$Participation_politics_dummy,
                     Participation_voluntary = l_data$Participation_voluntary_dummy,
                     
                     go_out = l_data$Going_out_rec,
                     
                     in_contact = l_data$v46,
                     
                     KINTIES_daily = l_data$KINTIES_daily,
                     
                     CloseFriend_daily = l_data$CloseFriend_daily,
                     
                     conflict = l_data$INDEX.NEG,
                     
                     living_alone = l_data$Living_alone_dummy,
                     
                     blocs = l_data |> 
                       mutate(x = case_when(
                         Sjever_dummy == 1 ~ "nordic",
                         Istok_dummy == 1 ~ "eastern",
                         TRUE ~ "continental"
                       )) |> pull(x) |> as.factor(),
                     
                     # relig = l_data |> 
                     #   mutate(x = case_when(
                     #     No_religion_dummy == 1 ~ "no_religion",
                     #     Protestant_dumm == 1 ~ "protestant",
                     #     Other_religion_dummy == 1 ~ "other_relig",
                     #     TRUE ~ "aaaREF"
                     #   )) |> pull(x) |> as.factor(),
                     
                     # Take_Advantage_rec = l_data$Take_Advantage_rec,
                     
                     Take_Advantage = l_data$v34,
                     
                     # Can_be_trusted_rec = l_data$Can_be_trusted_rec,
                     
                     Can_be_trusted = l_data$v35,
                     
                     settlement_size = l_data |> 
                       mutate(x = case_when(
                         Big_city_dummy == 1 ~ "big city",
                         Village_dummy == 1 ~ "village",
                         TRUE ~ "town"
                       )) |> pull(x) |> as.factor() |> fct_relevel("town"),
                     
                     ends_meet = l_data |> 
                       mutate(x = case_when(
                         Make_end_meet_rec == 1 ~ "difficult",
                         Make_end_meet_rec == 3 ~ "easy",
                         Make_end_meet_rec == 2 ~ "aaaREF"
                       )) |> pull(x) |> as.factor(),
                     
                     # relig_attend = l_data$ATTEND|>
                     #   to_factor() |>
                     #   fct_rev() |>
                     #   to_labelled(),
                     
                     relig_aff = l_data$RELIGIJA_REC |> 
                       to_factor() |> 
                       fct_relevel("Katolici"),
                     
                     work_status = l_data |> 
                       mutate(x = MAINSTAT |> to_factor() |> fct_lump_prop(.03)) |> pull()
                     
                     ### unemployed = l_data$ ### samo 4%
                     
)



# table(l_data$Going_out_rec, l_data$v44)
# table(model_data$blocs, model_data$country)
# l_data$ATTEND 
# xtabs(~country + blocs, data = model_data)


# GINI coefficient ### ----------------------------------------------------

macros <- readxl::read_xlsx("ilc_di12_gini_et_al.xlsx") |> 
  select(country, gini, gdp_ppp)

model_data_fin <- left_join(model_data, macros)




# model -------------------------------------------------------------------


# m1 <- lm(
#   INDEX_LONELY ~ spol + dob_rec + educ,
#   data = model_data_fin)
# 
# glance(m1)  
# 
# tidy(m1) ## |> pull("p.value") |> round(3) # |> writexl::write_xlsx("lm1.xlsx")
###

m1 <- lm(
  INDEX_LONELY ~ spol + dob_rec + educ +
    Participation_culture + Participation_politics + Participation_voluntary + go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone +
    blocs + relig_attend + Take_Advantage + Can_be_trusted +
    settlement_size + ends_meet + work_status,
  data = model_data_fin)


library(lm.beta)

# m1.beta <- lm.beta::lm.beta(m1)
# m1.beta$standardized.coefficients |>
#   as_tibble(rownames = "vars")
  # writexl::write_xlsx("betas_full.xlsx")


m1.0 <- lm(
  INDEX_LONELY ~ spol + dob_rec + educ, #  +
    # Participation_culture + Participation_politics + Participation_voluntary + go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone +
    # blocs + relig_attend + Take_Advantage + Can_be_trusted +
    # settlement_size + ends_meet + work_status,
  data = model_data_fin)

m1.1 <- lm(
  INDEX_LONELY ~ spol + dob_rec + educ +
    Participation_culture + Participation_politics + Participation_voluntary + go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone, #  +
    # blocs + relig_attend + Take_Advantage + Can_be_trusted +
    # settlement_size + ends_meet + work_status,
  data = model_data_fin)

m1.2 <- lm(
  INDEX_LONELY ~ spol + dob_rec + educ +
    Participation_culture + Participation_politics + Participation_voluntary + go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone +
    blocs + relig_attend + Take_Advantage + Can_be_trusted,
    # settlement_size + ends_meet + work_status,
  data = model_data_fin)

m1.full <- lm(
  INDEX_LONELY ~ spol + dob_rec + educ +
    Participation_culture + Participation_politics + Participation_voluntary + go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone +
    blocs + relig_attend + Take_Advantage + Can_be_trusted +
    settlement_size + ends_meet + work_status,
  data = model_data_fin)

# glance(m1)
list(m1.0, m1.1, m1.2, m1.full) |> 
  # lapply(tidy) |>
  lapply(lm.beta) |> 
  map("standardized.coefficients") |> 
  map(as_tibble, rownames = "vars") |> 
  writexl::write_xlsx("m1_seq_betas.xlsx")

list(m1.0, m1.1, m1.2, m1.full) |> 
  map_dfr(glance) |> 
  writexl::write_xlsx("full_models_glance.xlsx")

###
model_data_fin_split <- split(model_data_fin, model_data_fin$blocs)
###



m1.0.east <- lm(
  INDEX_LONELY ~ spol + dob_rec + educ, #  +
  # Participation_culture + Participation_politics + Participation_voluntary + go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone +
  # blocs + relig_attend + Take_Advantage + Can_be_trusted +
  # settlement_size + ends_meet + work_status,
  data = model_data_fin_split$eastern)

m1.1.east <- lm(
  INDEX_LONELY ~ spol + dob_rec + educ +
    Participation_culture + Participation_politics + Participation_voluntary + go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone, #  +
  # blocs + relig_attend + Take_Advantage + Can_be_trusted +
  # settlement_size + ends_meet + work_status,
  data = model_data_fin_split$eastern)

m1.2.east <- lm(
  INDEX_LONELY ~ spol + dob_rec + educ +
    Participation_culture + Participation_politics + Participation_voluntary + go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone +
    relig_attend + Take_Advantage + Can_be_trusted,
  # settlement_size + ends_meet + work_status,
  data = model_data_fin_split$eastern)

m1.full.east <- lm(
  INDEX_LONELY ~ spol + dob_rec + educ +
    Participation_culture + Participation_politics + Participation_voluntary + go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone +
    relig_attend + Take_Advantage + Can_be_trusted +
    settlement_size + ends_meet + work_status,
  data = model_data_fin_split$eastern)

list(m1.0.east, m1.1.east, m1.2.east, m1.full.east) |> 
  # lapply(tidy) |>
  lapply(lm.beta) |> 
  map("standardized.coefficients") |> 
  map(as_tibble, rownames = "vars") |> 
  writexl::write_xlsx("m.east_seq_betas.xlsx")


model_data_fin$blocs |> table()


list(m1.full.cont, m1.full.east, m1.full.nord) |> 
  lapply(glance) |> 
  writexl::write_xlsx("m1_blocs_glance.xlsx")

list(m1.0.nord, m1.1.nord, m1.2.nord, m1.full.nord) |> 
  map_dfr(glance) |> 
  writexl::write_xlsx("m1_nord_glance.xlsx")

list(m1.0.cont, m1.1.cont, m1.2.cont, m1.full.cont) |> 
  map_dfr(tidy, .id = "model_level") |> 
  writexl::write_xlsx("m1_cont_ALL.xlsx")

list(m1.0.east, m1.1.east, m1.2.east, m1.full.east) |> 
  map_dfr(tidy, .id = "model_level") |> 
  writexl::write_xlsx("m1_east_ALL.xlsx")

list(m1.0.nord, m1.1.nord, m1.2.nord, m1.full.nord) |> 
  map_dfr(tidy, .id = "model_level") |> 
  writexl::write_xlsx("m1_nord_ALL.xlsx")


modelz <- map(model_data_fin_split,
    ~ lm(
  INDEX_LONELY ~ spol + dob_rec + educ +
    Participation_culture + Participation_politics + Participation_voluntary + go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone +
    relig_attend + Take_Advantage_rec + Can_be_trusted_rec +
    settlement_size + ends_meet + work_status,
  data = model_data_fin_split$continental))




map_dfr(modelz, tidy, .id = "bloc")

list(bl_cont, bl_east, bl_nord) |> 
  lapply(tidy) |> 
  writexl::write_xlsx("models_blocs.xlsx")

list(bl_cont, bl_east, bl_nord) |> 
  lapply(glance) |> 
  bind_rows()
  writexl::write_xlsx("models_blocs_glance.xlsx")
  
  # lapply(tidy) |> 
  # writexl::write_xlsx("models_countries.xlsx")
  
  map_dfr(glance, .id = "country") |> 
  writexl::write_xlsx("models_countriesLONG.xlsx")


library(lme4)

mult1 <- lmer(INDEX_LONELY ~ spol + dob_rec + educ +
                go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone +
                relig_attend + work_status + (1 | country),
     data = model_data_fin)

mult2 <- lmer(INDEX_LONELY ~ spol + dob_rec + educ +
                go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone +
                relig_attend + work_status + (gdp_ppp | country),
              data = model_data_fin)

model_data_fin$

###

library(arm)

arm::display(mult2)
arm::

# model_data_fin |> 
#   group_by(blocs) |> 
#   summarise(mean(INDEX_LONELY, na.rm = TRUE))

# /METHOD=ENTER MALE_DUMMY dob1_rec dob2_rec dob4_rec dob5_rec DEGREE
# /METHOD=ENTER Participation_culture_dummy Participation_politics_dummy 
# Participation_voluntary_dummy v44 v46 KINTIES_daily CloseFriend_daily INDEX.NEG Living_alone_dummy
# /METHOD=ENTER Sjever_dummy Istok_dummy No_religion_dummy Protestant_dumm Other_religion_dummy 
# Take_Advantage_rec Can_be_trusted_rec
# /METHOD=ENTER Big_city_dummy Village_dummy Ends_meet_difficult Ends_meet_easy Unemployed_dummy
