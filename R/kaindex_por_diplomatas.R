# Dados de diplomatas

tabelas <- rvest::read_html(here::here("data/classificao_postos.html"), encoding = "UTF-8") %>% 
  rvest::html_table()

tabela_embaixadas <- tabelas[[1]] %>% 
  mutate(no_pais = NA_character_) %>% 
  janitor::clean_names() %>% 
  select(posto, grupo, chefe, mc, c_sec, no_pais)

tabela_consulados <- tabelas[[2]] %>% 
  mutate(no_pais = NA_character_)

# write_excel_csv(tabela_embaixadas, 
#                 here::here("data/diplomatas_embaixadas.csv"))
# 
# write_excel_csv(tabela_consulados, 
#                 here::here("data/diplomatas_consulados.csv"))

diplomatas_embaixadas <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1QCmhKlnxdTAs9gj0-vMLwkAQKposPwgZrI_LpItnIBg/edit#gid=1852273486") %>% 
  janitor::clean_names() %>% 
  mutate(total = chefe + mc + c_sec) %>% 
  select(posto, total, no_pais, grupo)

diplomatas_consulados <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1iB-a1vBMk9L_bFB-HNPBVFTuzN0tl9qK9yJ-AMQttwM/edit#gid=440147432") %>% 
  janitor::clean_names() %>% 
  mutate(total = chefe + diplomatas) %>% 
  select(posto, total, no_pais, grupo)

total_diplomatas <- diplomatas_consulados %>% 
  dplyr::bind_rows(diplomatas_embaixadas) %>% 
  group_by(no_pais) %>% 
  summarise(total = sum(total))

# Kaindex

kaindex <- readr::read_rds(here::here("data/kaindex.rds"))

dados_kaindex_diplomatas <- kaindex %>% 
  mutate(no_pais = case_when(str_detect(no_pais, "Bósnia") ~ "Bósnia e Herzegovina",
                             str_detect(no_pais, "Kuweit") ~ "Kuwait",
                             str_detect(no_pais, "Malavi") ~ "Malawi",
                             str_detect(no_pais, "Holanda") ~ "Holanda",
                             str_detect(no_pais, "Taiwan") ~ "Taiwan",
                             TRUE ~ no_pais)) %>% 
  left_join(total_diplomatas) 
  
dados_kaindex_diplomatas %>% 
  filter(no_pais != "Brasil") %>% 
  drop_na() %>% 
  slice_max(icitp, n = 20) %>%   
  ggplot() +
  # geom_point(aes(total, value, color = no_pais), show.legend = F)) +
  geom_text(aes(total, icitp, label = no_pais, size = total, color = icitp), show.legend = F) +
  labs(x = "Total de Diplomatas",
       y = "ICITP",
       title = "Diplomatas por Índice de Investimentos e Cooperação Tecnológica",
       caption = "Fonte: MRE e MEcon") +
  scale_y_continuous(labels = scales::label_number_si())

  View()
