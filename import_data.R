

library(here)
library(tidyverse)
library(readxl)
library(gt)

Experiment_1N <- read_excel("input/Experiment 1N.xlsx", sheet = "Experiment 1N")

Experiment_1V <- read_excel("input/Experiment 1V.xlsx", sheet = "Experiment1V")

Experiment_2N <- read_excel("input/Experiment 2nv.xlsx", sheet = "Experiment 2N") %>% 
                  dplyr::select(WD:Height)

Experiment_2V <- read_excel("input/Experiment 2nv.xlsx", sheet = "Experiment 2V") %>% 
                  dplyr::select(WD:Height)


lista_exp <- list(Experiment_1N, Experiment_1V, Experiment_2N, Experiment_2V)
names(lista_exp) <- c("1N","1V","2N","2V")


lista_exp <- lista_exp %>% map(~ .x %>% dplyr::select(-c(Area, BX, BY, Width, Height)))

lista_exp$`1N` <- lista_exp$`1N` %>% dplyr::rename(Conc = `Conc(µm)`)
lista_exp$`1V` <- lista_exp$`1V` %>% dplyr::rename(Conc = `Conc(µm)`)
lista_exp$`2N` <- lista_exp$`2N` %>% dplyr::rename(Cells = `Cell no(mil)`)
lista_exp$`2V` <- lista_exp$`2V` %>% dplyr::rename(Cells = `Cell no(mil)`)

exp1 <- lista_exp$`1N` %>% mutate(exp = "N") %>% 
  bind_rows(
    lista_exp$`1V` %>% mutate(exp = "V")
  ) %>% 
  select(Conc, everything())

exp2 <- lista_exp$`2N` %>% mutate(exp = "N") %>% 
  bind_rows(
    lista_exp$`2V` %>% mutate(exp = "V")
  ) %>% 
  select(Cells, everything()) %>% 
  mutate_at(vars(Cells), ~ ifelse(. == "10 (fara Fuorescenta)", "10FF",.)) %>% 
  mutate_at(vars(Cells), ~ ifelse(. == "10 (fara Fluorescenta)", "10FF",.)) 

rm(lista_exp, Experiment_1N, Experiment_1V, Experiment_2N, Experiment_2V)

##### EXP1

# aici dovedim ca WD si Magnification nu sunt relevante pentru 1N
# Conc si Intensity sunt semnificative pentru 1N

lm1<- lm(Mean ~ WD + Conc + Intensity,
   data = exp1 %>% filter(exp == "N"))

lm1 %>% broom::tidy() %>% 
  slice(-1, -5) %>% select(-std.error) %>% 
  arrange(p.value) %>% 
  gt() %>%
  fmt_number(columns = c(2:4), decimals = 3) %>% 
  tab_header(
    title = "The preliminary model",
    subtitle = md("dependent variable: **luminosity**")
  ) %>% 
  tab_footnote(
    footnote = md(" **WD** (the working distance) is **non-significant**"),
    locations = cells_body(
      columns = vars(p.value),
      rows = 3)
  )
  
  
  
# TO-DO : da facut frumos modelu linear







#####

exp2 <- exp2 %>% filter(!Cells %in% c("0")) %>% 
  mutate_at(vars(Cells), ~ ifelse(. == "10FF", "0", .)) %>% 
  mutate(Cells = as.integer(Cells))

exp2N <- exp2 %>% filter(exp == "N") %>% select(-exp)
exp2V <- exp2 %>% filter(exp == "V") %>% select(-exp)







