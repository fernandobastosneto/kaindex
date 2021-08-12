library(tidyverse)
library(readxl)

busca_fuzzy<-function(x,y){
  
  `%>%` <- magrittr::`%>%`  
  
  
  x1 <- x %>% 
    stringi::stri_trans_general("latin-ascii") %>% 
    stringi::stri_trans_tolower() %>% 
    stringi::stri_trim_both() %>% 
    stringi::stri_replace_all_regex("\\s+","_")
  
  y1 <- y %>% 
    stringi::stri_trans_general("latin-ascii") %>% 
    stringi::stri_trans_tolower() %>% 
    stringi::stri_trim_both() %>% 
    stringi::stri_replace_all_regex("\\s+","_")
  
  purrr::map(x1, ~{
    
    a <- stringdist::stringdist(.x,y1)
    
    b <- which.min(a)
    
    d <- y[b]
    
  }) %>% 
    unlist()
  
}

# Comércio -----

exp_participacao <- comerciobr::sh1_df %>% 
  filter(co_ano == max(co_ano)-1) %>% 
  filter(path == "EXP") %>% 
  group_by(no_pais) %>% 
  summarise(exportacoes = sum(value)) %>% 
  ungroup() %>% 
  mutate(total = sum(exportacoes)) %>% 
  mutate(porcentagem_exportacoes = exportacoes/total) %>% 
  select(-c(total))

imp_participacao <- comerciobr::sh1_df %>% 
  filter(co_ano == max(co_ano)-1) %>% 
  filter(path == "IMP") %>% 
  group_by(no_pais) %>% 
  summarise(importacoes = sum(value)) %>% 
  ungroup() %>% 
  mutate(total = sum(importacoes)) %>% 
  mutate(porcentagem_importacoes = importacoes/total) %>% 
  select(-c(total))

corrente_participacao <- comerciobr::sh1_df %>% 
  filter(co_ano == max(co_ano)-1) %>% 
  group_by(path, no_pais) %>% 
  summarise(value = sum(value)) %>% 
  pivot_wider(names_from = path, values_from = value) %>% 
  mutate(corrente = EXP + IMP,
         saldo = EXP - IMP) %>% 
  select(no_pais, corrente) %>% 
  drop_na() %>% 
  mutate(total = sum(corrente)) %>% 
  mutate(porcentagem_corrente = corrente/total) %>% 
  select(-c(total))

# Tecnologia -----

## Patentes ------

patentes <- read_excel(here::here("data/deposito_patentes_tipo_pi_por_pais.xls"), skip = 7) %>% 
  rename(País = 1) %>% 
  select(-c(2)) %>% 
  pivot_longer(cols = 2:last_col(), names_to = "ano", values_to = "patentes") %>% 
  drop_na() %>% 
  filter(ano == max(ano)) %>% 
  filter(País != "Brasil") %>% 
  drop_na() %>% 
  group_by(País) %>% 
  summarise(patentes = sum(patentes)) %>% 
  ungroup() %>% 
  mutate(total = sum(patentes)) %>% 
  mutate(porcentagem_patentes = round((patentes/total), 2)) %>% 
  arrange(desc(porcentagem_patentes)) %>% 
  # padronização dos nomes abaixo
  mutate(nome_corrigido = case_when(País == busca_fuzzy(País, comerciomundo::dic_comtrade_mdic$no_pais) ~ País,
                                    TRUE ~ "A CORRIGIR")) %>% 
  rename(no_pais = nome_corrigido, pais_patente = País) %>% 
  mutate(no_pais = case_when(pais_patente == "Holanda" | str_detect(pais_patente, "Países Baixos")~ "Países Baixos (Holanda)",
                             str_detect(pais_patente, "Coréia \\(Sul\\)") ~ "Coreia do Sul",
                             str_detect(pais_patente, "Taiwan") ~ "Taiwan (Formosa)",
                             str_detect(pais_patente, "Russ") ~ "Rússia",
                             str_detect(pais_patente, "Ilhas Caiman") ~ "Cayman, Ilhas",
                             str_detect(pais_patente, "Singapura") ~ "Cingapura",
                             str_detect(pais_patente, "Tcheca") ~ "Tcheca, República",
                             str_detect(pais_patente, "Virgens") ~ "Virgens, Ilhas (Britânicas)",
                             str_detect(pais_patente, "Coréia \\(Norte\\)") ~ "Coreia do Norte",
                             str_detect(pais_patente, "Hong-Kong") ~ "Hong Kong",
                             str_detect(pais_patente, "Eslovenia") ~ "Eslovênia",
                             str_detect(pais_patente, "Emirados Arabes") ~ "Emirados Árabes Unidos",
                             str_detect(pais_patente, "Bósnia") ~ "Bósnia-Herzegovina",
                             str_detect(pais_patente, "Irã") ~ "Irã",
                             str_detect(pais_patente, "Kuwait") ~ "Coveite (Kuweit)",
                             TRUE ~ pais_patente
  )) %>% 
  drop_na() %>% 
  select(no_pais, patentes, porcentagem_patentes) %>% 
  group_by(no_pais) %>% 
  summarise(patentes = sum(patentes),
            porcentagem_patentes = sum(porcentagem_patentes))


## OMPI ----



## Bolsas no Exterior ------

### CAPES -------

capes <- vroom::vroom(here::here("data/br-capes-bolsas-programas-mobilidade-internacional-2017a2019-2021-03-01.csv"),
                      locale = locale(encoding = "ISO-8859-1")) %>% 
  filter(str_detect(NM_NIVEL, "DOUTORA")) %>% 
  count(NM_PAIS_IES_ESTUDO) %>% 
  mutate(NM_PAIS_IES_ESTUDO = stringr::str_to_title(NM_PAIS_IES_ESTUDO)) %>% 
  arrange(desc(n)) %>% 
  filter(NM_PAIS_IES_ESTUDO != "Brasil") %>% 
  rename(pais = NM_PAIS_IES_ESTUDO, capes = n) %>% 
  # mutate(capes = as.integer(capes)) %>% 
  mutate(total = sum(capes)) %>% 
  mutate(porcentagem_capes = capes/total) %>% 
  select(-c(total)) %>% 
  mutate(no_pais = case_when(pais == "Holanda" | str_detect(pais, "Países Baixos")~ "Países Baixos (Holanda)",
                             str_detect(pais, "Taiwan") ~ "Taiwan (Formosa)",
                             str_detect(pais, "Russ") ~ "Rússia",
                             str_detect(pais, "Tcheca") ~ "Tcheca, República",
                             str_detect(pais, "Coreia") ~ "Coreia do Sul",
                             str_detect(pais, "África Do Sul") ~ "África do Sul",
                             str_detect(pais, "Polonia") ~ "Polônia",
                             str_detect(pais, "Timor-Leste") ~ "Timor Leste",
                             str_detect(pais, "Beni") ~ "Benin",
                             TRUE ~ pais)) %>% 
  drop_na() %>% 
  select(no_pais, capes, porcentagem_capes)

### CNPQ ------

cnpq_files <- fs::dir_ls(here::here("data"), regexp = "bolsas_cnpq")

cnpq <- vroom::vroom(cnpq_files,
          locale = locale(encoding = "ISO-8859-1")) %>% 
  janitor::clean_names() %>% 
  select(pais_destino, linha_de_fomento) %>% 
  filter(str_detect(linha_de_fomento, "Doutorado")) %>%
  count(pais_destino) %>% 
  arrange(desc(n)) %>% 
  filter(str_detect(pais_destino, "Brasil", negate = T)) %>% 
  mutate(pais_destino = str_remove(pais_destino, "\\w{3} - ")) %>% 
  rename(cnpq = n) %>% 
  mutate(total = sum(cnpq)) %>% 
  mutate(porcentagem_cnpq = cnpq/total) %>% 
  select(-c(total)) %>% 
  mutate(nome_corrigido = case_when(str_detect(pais_destino, "Inglaterra") |
                                      str_detect(pais_destino, "Escócia") |
                                      str_detect(pais_destino, "Irlanda do Norte") |
                                      str_detect(pais_destino, "Gales") ~ "Reino Unido",
                                    pais_destino == "Holanda" ~ "Países Baixos (Holanda)",
                                    str_detect(pais_destino, "Suiça") ~ "Suíça",
                                    str_detect(pais_destino, "Austria") ~ "Áustria",
                                    str_detect(pais_destino, "Tcheca") ~ "Tcheca, República",
                                    TRUE ~ pais_destino)) %>% 
  rename(no_pais = nome_corrigido) %>% 
  select(no_pais, cnpq, porcentagem_cnpq) %>% 
  group_by(no_pais) %>% 
  summarise(cnpq = sum(cnpq),
            porcentagem_cnpq = sum(porcentagem_cnpq))

bolsas <- capes %>% 
  left_join(cnpq) %>% 
  mutate(bolsas = capes + cnpq) %>% 
  replace_na(list(bolsas = 0)) %>% 
  # mutate(capes = replace_na(0)
  mutate(total = sum(bolsas)) %>% 
  mutate(porcentagem_bolsas = bolsas/total) %>% 
  select(no_pais, bolsas, porcentagem_bolsas)

# Investimentos -----

## Investimentos Banco Central ----

controlador_final_investimentos <- investimentos::bc_idp_pais_controlador %>% 
  filter(discriminacao != "Total") %>% 
  filter(ano == max(ano)) %>% 
  drop_na() %>% 
  mutate(total = sum(value)) %>% 
  mutate(porcentagem_inv_controladorfinal = value/total) %>% 
  mutate(nome_corrigido = case_when(discriminacao == busca_fuzzy(discriminacao, comerciomundo::dic_comtrade_mdic$no_pais) ~ discriminacao,
                                    TRUE ~ "A CORRIGIR")) %>% 
  mutate(no_pais = case_when(discriminacao == "Holanda" | str_detect(discriminacao, "Países Baixos")~ "Países Baixos (Holanda)",
                             str_detect(discriminacao, "Coréia do Sul") ~ "Coreia do Sul",
                             str_detect(discriminacao, "Taiwan") ~ "Taiwan (Formosa)",
                             str_detect(discriminacao, "Ilhas Caiman") ~ "Cayman, Ilhas",
                             str_detect(discriminacao, "Virgens") ~ "Virgens, Ilhas (Britânicas)",
                             str_detect(discriminacao, "Bahrein") ~ "Barein",
                             TRUE ~ discriminacao)) %>% 
  rename(investimentos_controladorfinal = value) %>%
  select(investimentos_controladorfinal, porcentagem_inv_controladorfinal, no_pais)

## Investimentos UNCTAD ----

unctad <- read_csv(here::here("data/unctad_fdi_flows_stock_2020.csv")) %>% 
  janitor::clean_names() %>% 
  filter(direction_label == "Outward") %>% 
  filter(mode_label == "Stock") %>% 
  filter(year == max(year)) %>% 
  filter(economy_label != "World") %>% 
  filter(str_length(economy) == 3) %>% 
  # Correção do nome
  mutate(nome_corrigido = case_when(economy_label == busca_fuzzy(economy_label, comerciomundo::dic_comtrade_mdic$no_pais_ing) ~ economy_label,
                                    TRUE ~ busca_fuzzy(economy_label, comerciomundo::dic_comtrade_mdic$no_pais_ing))) %>% 
  mutate(no_pais_ing = case_when(str_detect(economy_label, "Bolivia") ~ "Bolivia",
                                    str_detect(economy_label, "British Vir") ~ "Virgin Islands (UK)",
                                    str_detect(economy_label, "Verde") ~ "Cape Verde",
                                    str_detect(economy_label, "Taiwan") ~ "Taiwan",
                                    str_detect(economy_label, "Congo, Dem.") ~ "Democratic Republic of the Congo",
                                    str_detect(economy_label, "Czechia") ~ "Czech Republic",
                                    str_detect(economy_label, "Dominica") ~ "Dominica Island",
                                    str_detect(economy_label, "France") ~ "France",
                                    str_detect(economy_label, "Palestine") ~ "Palestine",
                                    str_detect(economy_label, "China, Hong Kong SAR") ~ "Hong Kong",
                                    str_detect(economy_label, "Iran") ~ "Iran",
                                    str_detect(economy_label, "Côte d'Ivoire") ~ "Cote D'Ivore",
                                    str_detect(economy_label, "Korea, Republic of") ~ "South Korea",
                                    str_detect(economy_label, "Macao") ~ "China",
                                    str_detect(economy_label, "Russian Federation") ~ "Russia",
                                    str_detect(economy_label, "Viet Nam") ~ "Vietnam",
                                    str_detect(economy_label, "Switzerland") ~ "Switzerland",
                                    str_detect(economy_label, "Syrian Arab Republic") ~ "Syria",
                                    str_detect(economy_label, "United States of America") ~ "United States",
                                    str_detect(economy_label, "Venezuela") ~ "Venezuela",
                                    str_detect(economy_label, "Bahrain") ~ "Bahrain",
                                 TRUE ~ nome_corrigido)) %>% 
  left_join(comerciomundo::dic_comtrade_mdic) %>% 
  select(no_pais, us_dollars_at_current_prices_in_millions, percentage_of_total_world) %>% 
  group_by(no_pais) %>% 
  summarise(us_dollars_at_current_prices_in_millions = sum(us_dollars_at_current_prices_in_millions),
            percentage_of_total_world = sum(percentage_of_total_world))


# Financiamento -----

emprestimos <- read_excel(here::here("data/EmprestimosDiretosLongoPrazoPassivop.xls"), skip = 4) %>% 
  drop_na() %>% 
  # janitor::clean_names() %>% 
  filter(Discriminação != "Total") %>% 
  mutate(across(.cols = 2:dplyr::last_col(), as.numeric)) %>% 
  pivot_longer(cols = 2:dplyr::last_col(), names_to = "ano", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  filter(ano == max(ano)) %>% 
  arrange(desc(value)) %>% 
  mutate(total = sum(value)) %>% 
  mutate(porcentagem_financiamento = value/total) %>% 
  rename(pais = Discriminação, financiamento = value) %>% 
  select(pais, financiamento, porcentagem_financiamento) %>% 
  mutate(nome_corrigido = case_when(pais == busca_fuzzy(pais, comerciomundo::dic_comtrade_mdic$no_pais) ~ pais,
                                  TRUE ~ "A CORRIGIR")) %>% 
  mutate(no_pais = case_when(pais == "Holanda" | str_detect(pais, "Países Baixos")~ "Países Baixos (Holanda)",
                             str_detect(pais, "Coréia do Sul") ~ "Coreia do Sul",
                             str_detect(pais, "Ilhas Cayman") ~ "Cayman, Ilhas",
                             str_detect(pais, "Virgens Brit") ~ "Virgens, Ilhas (Britânicas)",
                             str_detect(pais, "Demais país") ~ NA_character_,
                             TRUE ~ pais)) %>%
  drop_na() %>% 
  select(no_pais, financiamento, porcentagem_financiamento)

# Base ----

df <- exp_participacao %>% 
  left_join(imp_participacao) %>% 
  left_join(corrente_participacao) %>% 
  left_join(patentes) %>% 
  left_join(controlador_final_investimentos) %>% 
  left_join(emprestimos) %>% 
  left_join(unctad) %>% 
  left_join(bolsas)


# df %>%
#   count(no_pais) %>%
#   arrange(desc(n))

df <- df %>% 
  mutate(across(everything(), ~ replace_na(.x, 0))) %>% 
  mutate(indice = porcentagem_exportacoes + porcentagem_importacoes + 
           porcentagem_patentes + 
           porcentagem_inv_controladorfinal + 
           porcentagem_financiamento + 
           # percentage_of_total_world + 
           porcentagem_bolsas) %>% 
  select(no_pais, indice, starts_with("porcentagem")) %>% 
  arrange(desc(indice)) 