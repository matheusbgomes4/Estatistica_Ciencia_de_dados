dados <- read.csv("MICRODADOS_ENEM_2022.csv", sep = ";", stringsAsFactors = FALSE)



dados <- dados |> 
  select("SG_UF_PROVA", "NU_NOTA_CN", "NU_NOTA_CH",
         "NU_NOTA_LC", "NU_NOTA_MT", "NU_NOTA_REDACAO")

dados[dados == ""] <- NA

dados[,2:5] <- lapply(dados[,2:5], as.numeric)

library(sf)


estados <- geobr::read_state(year = 2020)
plot(estados$geom)

ggplot() +
  geom_sf(data = estados, aes(geometry = geom)) +
  theme_classic() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank())



dados_prop <- dados |> 
  filter(!is.na(NU_NOTA_REDACAO)) |> 
  mutate(Redacao = ifelse(NU_NOTA_REDACAO > 800,
                          "Maior que 800",
                          "Menor ou igual a 800")) |>
  group_by(SG_UF_PROVA, Redacao) |>
  count() |>
  ungroup() |>
  group_by(SG_UF_PROVA) |>
  mutate(prop = n/sum(n)) |>
  filter(Redacao == "Maior que 800")

dados_unidos <-
  dplyr:: left_join(dados_prop, estados,
                    by = c("SG_UF_PROVA" = "abbrev_state"))


ggplot(data = dados_unidos) +
  geom_sf(aes(geometry = geom, fill = prop),
          color = "grey80") +
  geom_sf_label(aes(geometry = geom, label = SG_UF_PROVA),
                size = 3.0) +
  scale_fill_continuous(low = "#56B1F7", high = "#132B43",
                        na.value = "grey90",
                        labels = scales::percent_format()) +
  labs(fill = "Proporção de notas\nde redação acima\nde 800",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank())