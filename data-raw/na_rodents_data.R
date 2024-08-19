library("xlsx")
na_rodents_possitive <- readxl::read_excel("C:/Users/norma/Downloads/NAmRodents-Positives-July7-2022.xlsx")
na_rodents_negative <- readxl::read_excel("C:/Users/norma/Downloads/NAmRodents-Negatives-NotPrimaryData-July7-2022.xlsx")
library(tidyverse)
lacs

na_rodents_possitive <- na_rodents_possitive |>
  select(DOI, Title, Abstract) |>
  mutate(DOI = str_remove(DOI, "\\n")) |>
  rename(doi = DOI) |>
  rename(title = Title) |>
  rename(abstract = Abstract) |>
  mutate(class = "possitive")

na_rodents_negative <- na_rodents_negative |>
  select(DOI, Title, Abstract) |>
  rename(doi = DOI) |>
  mutate(doi = str_remove(doi, "\\r\\n")) |>
  rename(title = Title) |>
  rename(abstract = Abstract) |>
  mutate(class = "unknown")
na_rodents <- bind_rows(na_rodents_possitive, na_rodents_negative)

na_rodents <- na_rodents |> sample_frac(1)
write_csv(na_rodents, "C:/Users/norma/OneDrive - Arizona State University/lacs/data-raw/na_rodents.csv")

