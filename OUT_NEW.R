# modelz to automate #

modelZ_full <- function(data) {
  
  m1.0 <- lm(INDEX_LONELY ~ spol + dob_rec + educ, #  +
             # Participation_culture + Participation_politics + Participation_voluntary + go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone +
             # blocs + relig_attend + Take_Advantage + Can_be_trusted +
             # settlement_size + ends_meet + work_status,
             data = data)
  
  m1.1 <- lm(
    INDEX_LONELY ~ spol + dob_rec + educ +
      Participation_culture + Participation_politics + Participation_voluntary + go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone,
    #  +
    # blocs + relig_attend + Take_Advantage + Can_be_trusted +
    # settlement_size + ends_meet + work_status,
    data = data
  )
  
  m1.2 <- lm(
    INDEX_LONELY ~ spol + dob_rec + educ +
      Participation_culture + Participation_politics + Participation_voluntary + go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone +
      blocs + relig_aff + Take_Advantage + Can_be_trusted,
    # settlement_size + ends_meet + work_status,
    data = data
  )
  
  m1.full <- lm(
    INDEX_LONELY ~ spol + dob_rec + educ +
      Participation_culture + Participation_politics + Participation_voluntary + go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone +
      blocs + relig_aff + Take_Advantage + Can_be_trusted +
      settlement_size + ends_meet + work_status,
    data = data
  )
  
  list(m1.0, m1.1, m1.2, m1.full)
  
}


modelZ_blocks2 <- function(data) {
  
  m1.0 <- lm(INDEX_LONELY ~ spol + dob_rec + educ, #  +
             # Participation_culture + Participation_politics + Participation_voluntary + go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone +
             # blocs + relig_attend + Take_Advantage + Can_be_trusted +
             # settlement_size + ends_meet + work_status,
             data = data)
  
  m1.1 <- lm(
    INDEX_LONELY ~ spol + dob_rec + educ +
      Participation_culture + Participation_politics + Participation_voluntary + go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone,
    #  +
    # blocs + relig_attend + Take_Advantage + Can_be_trusted +
    # settlement_size + ends_meet + work_status,
    data = data
  )
  
  m1.2 <- lm(
    INDEX_LONELY ~ spol + dob_rec + educ +
      Participation_culture + Participation_politics + Participation_voluntary + go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone +
      relig_aff + Take_Advantage + Can_be_trusted,
    # settlement_size + ends_meet + work_status,
    data = data
  )
  
  m1.full <- lm(
    INDEX_LONELY ~ spol + dob_rec + educ +
      Participation_culture + Participation_politics + Participation_voluntary + go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone +
      relig_aff + Take_Advantage + Can_be_trusted +
      settlement_size + ends_meet + work_status,
    data = data
  )
  
  list(m1.0, m1.1, m1.2, m1.full)
  
}

model_block_NORD <- function(data) {
  
  m1.0 <- lm(INDEX_LONELY ~ spol + dob_rec + educ, #  +
             # Participation_culture + Participation_politics + Participation_voluntary + go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone +
             # blocs + relig_attend + Take_Advantage + Can_be_trusted +
             # settlement_size + ends_meet + work_status,
             data = data)
  
  m1.1 <- lm(
    INDEX_LONELY ~ spol + dob_rec + educ +
      Participation_culture + Participation_politics + Participation_voluntary + go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone,
    #  +
    # blocs + relig_attend + Take_Advantage + Can_be_trusted +
    # settlement_size + ends_meet + work_status,
    data = data
  )
  
  m1.2 <- lm(
    INDEX_LONELY ~ spol + dob_rec + educ +
      Participation_culture + Participation_politics + Participation_voluntary + go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone +
      fct_relevel(relig_aff, "Protestanti") + Take_Advantage + Can_be_trusted,
    # settlement_size + ends_meet + work_status,
    data = data
  )
  
  m1.full <- lm(
    INDEX_LONELY ~ spol + dob_rec + educ +
      Participation_culture + Participation_politics + Participation_voluntary + go_out + in_contact + KINTIES_daily + CloseFriend_daily + conflict + living_alone +
      fct_relevel(relig_aff, "Protestanti") + Take_Advantage + Can_be_trusted +
      settlement_size + ends_meet + work_status,
    data = data
  )
  
  list(m1.0, m1.1, m1.2, m1.full)
  
}

modelZ_full(model_data_fin) |> 
  
  map_dfr(glance) |> 
  writexl::write_xlsx("delteR.xlsx")


modelZ_blocks(model_data_fin_split$continental) |> 
  
  lapply(lm.beta) |> 
  
  map_dfr(tidy, .id = "model") |>
  
  filter(term != "(Intercept)") |> 
  
  select(model, term, std_estimate, p.value) |> 
  
  mutate(std_estimate = round(std_estimate, 3)) |> 
  
  mutate(beta1.sig = case_when(
    p.value < .001 ~ paste0(std_estimate, "***"),
    p.value < .01 ~ paste0(std_estimate, "**"),
    p.value < .05 ~ paste0(std_estimate, "*"),
    TRUE ~ as.character(std_estimate)))
  
  # writexl::write_xlsx("OUT/model_NORD_FINAL.xlsx")

model_block_NORD(model_data_fin_split$nordic) |> 
  
  lapply(lm.beta) |> 
  
  map_dfr(tidy, .id = "model") |>
  
  filter(term != "(Intercept)") |> 
  
  select(model, term, std_estimate, p.value) |> 
  
  mutate(std_estimate = round(std_estimate, 3)) |> 
  
  mutate(beta1.sig = case_when(
    p.value < .001 ~ paste0(std_estimate, "***"),
    p.value < .01 ~ paste0(std_estimate, "**"),
    p.value < .05 ~ paste0(std_estimate, "*"),
    TRUE ~ as.character(std_estimate))) |> 
  
  # writexl::write_xlsx("OUT/model_NORD_FINAL_noVIF.xlsx")
  



list(
  modelZ_blocks(model_data_fin_split$continental) |>
    map_dfr(glance),
  modelZ_blocks(model_data_fin_split$eastern) |>
    map_dfr(glance),
  modelZ_blocks(model_data_fin_split$nordic) |>
    map_dfr(glance)
) |> writexl::write_xlsx("OUT/models_blocks_GLANCE.xlsx")
  




N_cont = 5022
N_east = 5211
N_nord = 3985

sum(N_cont, N_east, N_nord)

### collinearity


