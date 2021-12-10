library(dplyr)
library(readr)
library(purrr)


# REGRESIJSKE TABLICE -----------------------------------------------------

tbl2 <- read.delim2("tbl2.txt")

tbl2_nos <- tbl2 |> 
  as_tibble() |> 
  mutate_at(vars(value1:value4), parse_double) |> 
  rename(term = vars_short)

tbl2_sigs <- readxl::read_xlsx("sigs_full.xlsx")

tbl2_fullinfo <- cbind(
  tbl2_nos,
  select(tbl2_sigs, -term)
)

tbl2_fullinfo |> 
  
  mutate(value1.sig = case_when(
    p.value1 < .001 ~ paste0(value1, "***"),
    p.value1 < .01 ~ paste0(value1, "**"),
    p.value1 < .05 ~ paste0(value1, "*"),
    TRUE ~ as.character(value1)
  
    )) |> 
  
  mutate(value2.sig = case_when(
    p.value2 < .001 ~ paste0(value2, "***"),
    p.value2 < .01 ~ paste0(value2, "**"),
    p.value2 < .05 ~ paste0(value2, "*"),
    TRUE ~ as.character(value2)
    
  )) |> 
  
  mutate(value3.sig = case_when(
    p.value3 < .001 ~ paste0(value3, "***"),
    p.value3 < .01 ~ paste0(value3, "**"),
    p.value3 < .05 ~ paste0(value3, "*"),
    TRUE ~ as.character(value3)
    
    )) |> 
  
  mutate(value4.sig = case_when(
    p.value4 < .001 ~ paste0(value4, "***"),
    p.value4 < .01 ~ paste0(value4, "**"),
    p.value4 < .05 ~ paste0(value4, "*"),
    TRUE ~ as.character(value4)
    
  )) |> 
  
  select(term, ends_with("sig"))
  
  # writexl::write_xlsx("m1_to_word.xlsx")

east <- readxl::read_xlsx("east_BETA_SIG.xlsx")

east |> 
  
  mutate_if(is.double, round, 3) |> 
  
  mutate(beta1.sig = case_when(
    sig1 < .001 ~ paste0(beta1, "***"),
    sig1 < .01 ~ paste0(beta1, "**"),
    sig1 < .05 ~ paste0(beta1, "*"),
    TRUE ~ as.character(beta1)
    
  )) |> 
  
  mutate(beta2.sig = case_when(
    sig2 < .001 ~ paste0(beta2, "***"),
    sig2 < .01 ~ paste0(beta2, "**"),
    sig2 < .05 ~ paste0(beta2, "*"),
    TRUE ~ as.character(beta2)
    
  )) |> 
  
  mutate(beta3.sig = case_when(
    sig3 < .001 ~ paste0(beta3, "***"),
    sig3 < .01 ~ paste0(beta3, "**"),
    sig3 < .05 ~ paste0(beta3, "*"),
    TRUE ~ as.character(beta3)
    
  )) |> 
  
  mutate(beta4.sig = case_when(
    sig4 < .001 ~ paste0(beta4, "***"),
    sig4 < .01 ~ paste0(beta4, "**"),
    sig4 < .05 ~ paste0(beta4, "*"),
    TRUE ~ as.character(beta4)
    
  )) |> 

select(vars, ends_with("sig")) |> 
  
  writexl::write_xlsx("east_to_word.xlsx")

###


# GRAF --------------------------------------------------------------------

library(ggplot2)

model_data_fin |> 
  ggplot(aes(x = country ,y = INDEX_LONELY)) +
  geom_boxplot() +
  facet_wrap(~ blocs, scales = "free_x")


model_data_fin$country
mean()

se_mean <- function(x) sd(x, na.rm = TRUE)/sqrt(length(x))


samplemean <- mean(model_data_fin$INDEX_LONELY, na.rm = TRUE)
sample_semean <- se_mean(model_data_fin$INDEX_LONELY)

#

model_data_fin |> 
  group_by(blocs) |> 
  summarise(bla = mean(INDEX_LONELY, na.rm = TRUE),
            bbla = se_mean(INDEX_LONELY)) |> 
  writexl::write_xlsx("OUT/blocs.xlsx")

#

model_data_fin |> 
  group_by(country, blocs) |> 
  summarise(bla = mean(INDEX_LONELY, na.rm = TRUE),
            bbla = se_mean(INDEX_LONELY)) |> 
  group_by(blocs) |> 
  arrange(bla, .by_group = TRUE) |> 
  writexl::write_xlsx("OUT/countries.xlsx")

data_gg <- readxl::read_xlsx("data/countries_blocs_mean.xlsx")

data_gg$country <- fct_inorder(data_gg$country) |> fct_rev()
data_gg$blocs <- fct_relevel(data_gg$blocs, "sample")

data_gg |> 
  ggplot(aes(x = country, y = mean)) +
  geom_linerange(aes(ymin = se_low, ymax = se_high)) +
  # geom_point(aes(size = size), shape = 1) +
  geom_point(aes(shape = shape, size = size+1.5)) +
  scale_shape_identity() +
  scale_size_identity() +
  # scale_shape(solid = FALSE) +
  coord_flip() + # ylim = c(3.5,6.5)) +
  
  # facet_wrap(~ blocs, ncol = 1, scales = "free_x") +
  theme_minimal() +
  labs(y = "mean +/- 1.96*s.e.", x = "")

data_gg |> writexl::write_xlsx("fig1_data.xlsx")

# tbl struktura po blokovima ---------------------------------------------

t1 <- xtabs(~ spol + blocs, data = model_data_fin) |>
  proportions(margin = 2) |> 
  round(3)*100

#

model_data_fin$educ_rec <- model_data_fin$educ |> 
  as_factor() |> 
  fct_collapse(primary = c("No formal education", "Primary school (elementary education)"),
               secondary = c("Lower secondary (secondary completed that does not allow ent", "Upper secondary (programs that allows entry to university)", "Post secondary, non-tertiary (other upper secondary programs"),
               other_level = "tertiary")

#

t2 <- xtabs(~ educ_rec + blocs, data = model_data_fin) |>
  proportions(margin = 2) |> 
  round(3)*100

#

t3 <- xtabs(~ settlement_size + blocs, data = model_data_fin) |>
  proportions(margin = 2) |> 
  round(3)*100

t4 <- xtabs(~ work_status + blocs, data = model_data_fin) |>
  proportions(margin = 2) |> 
  round(3)*100

age <- bind_rows(
  model_data_fin |> 
    # group_by(blocs) |> 
    summarise(age_mean = mean(AGE_continuous, na.rm = TRUE),
              age_sd = sd(AGE_continuous, na.rm = TRUE)) |> 
    tibble::add_column(blocs = "SAMPLE", .before = 1) ,
  
  model_data_fin |> 
    group_by(blocs) |> 
    summarise(age_mean = mean(AGE_continuous, na.rm = TRUE),
              age_sd = sd(AGE_continuous, na.rm = TRUE))
)



list(t1, t2, t3, t4, age) |>
  lapply(as.data.frame) |> 
  writexl::write_xlsx("Ts.xlsx")



table(model_data_fin$educ_rec, model_data_fin$educ)

# Education	
# Primary or less	3.3
# Secondary	64.3
# Tertiary 	32.4



