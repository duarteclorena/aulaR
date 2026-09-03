# Atividade FCM - aula 4
# Gráfico da média de cada atributo para cada tipo de pokémon

library (tidyverse)
library(ggplot2)

dados <- read.csv("input/Pokemon_full.csv")
head(dados)
str(dados)

# Dados que vou utilizar

dados_est <- dados %>%
  select(
    type,
    hp,
    attack,
    defense,
    sp.atk,
    sp.def,
    speed
  )

# Dados que vou calcular a média por tipo

dados_media <- dados_est %>%
  group_by(type) %>%
  summarise(
    hp = mean(hp, na.rm = TRUE),
    attack = mean(attack, na.rm = TRUE),
    defense = mean(defense, na.rm = TRUE),
    sp.atk = mean(sp.atk, na.rm = TRUE),
    sp.def = mean(sp.def, na.rm = TRUE),
    speed = mean(speed, na.rm = TRUE)
  )

dados_media

# Transformar colunas em uma variável

dados_long <- dados_media %>%
  pivot_longer(
    cols = c(hp, attack, defense, sp.atk, sp.def, speed),
    names_to = "est",
    values_to = "media"
  )

# Testando gráficos

# Gráfico feio
ggplot(dados_long, aes(x = est, y = media, group = type, color = type)) +
  geom_line() +
  geom_point()

# Gráfico feio 2

ggplot(
  dados_long,
  aes(
    x = est,
    y = media,
    group = type,
    color = type
  )
) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 3) +
  labs(
    title = "Perfil médio dos Pokémon por tipo",
    subtitle = "Comparação dos principais atributos",
    x = "Atributo",
    y = "Valor médio",
    color = "Tipo"
  ) +
  theme_minimal()


# Gráfico bonitinho

ggplot(
  dados_long,
  aes(
    x = est,
    y = media,
    group = type,
    color = type
  )
) +
  
  geom_line(
    linewidth = 1.2,
    alpha = 0.75
  ) +
  
  geom_point(
    size = 3
  ) +
  
  scale_color_viridis_d(
    option = "viridis"
  ) +
  
  scale_x_discrete(
    labels = c(
      "hp" = "HP",
      "attack" = "Attack",
      "defense" = "Defense",
      "sp.atk" = "Sp.Attack",
      "sp.def" = "Sp.Defense",
      "speed" = "Speed"
    )
  ) +
  
  labs(
    title = "Perfil médio dos Pokémon por tipo",
    subtitle = "Média dos atributos de batalha",
    x = "Atributo",
    y = "Valor médio",
    color = "Tipo"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      size = 20,
      face = "bold"
    ),
    
    plot.subtitle = element_text(
      size = 13,
      face = "italic"
    ),
    
    axis.title = element_text(
      size = 14,
      face = "bold"
    ),
    
    axis.text = element_text(
      size = 12
    ),
    
    legend.title = element_text(
      size = 13,
      face = "bold"
    ),
    
    legend.text = element_text(
      size = 10
    )
  )

ggsave(
  "outputs/grafico_linhas.png",
  width = 10,
  height = 7,
  dpi = 300
)


# Fazendo um heat map que o Gemini me ajudou

ggplot(
  dados_long,
  aes(
    x = est,
    y = type,
    fill = media
  )
) +
  
  geom_tile(
    color = "white",
    linewidth = 0.5
  ) +
  
  geom_text(
    aes(label = round(media, 1)),
    size = 3.5
  ) +
  
  scale_x_discrete(
    labels = c(
      "hp" = "HP",
      "attack" = "Attack",
      "defense" = "Defense",
      "sp.atk" = "Sp. Attack",
      "sp.def" = "Sp. Defense",
      "speed" = "Speed"
    )
  ) +
  
  scale_fill_viridis_c(
    option = "viridis"
  ) +
  
  labs(
    title = "Perfil médio dos Pokémon por tipo",
    subtitle = "Média dos atributos",
    x = "Atributo",
    y = "Tipo",
    fill = "Valor médio"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      size = 20,
      face = "bold"
    ),
    
    plot.subtitle = element_text(
      size = 13,
      face = "italic"
    ),
    
    axis.title = element_text(
      size = 14,
      face = "bold"
    ),
    
    axis.text.x = element_text(
      size = 11,
      face = "bold"
    ),
    
    axis.text.y = element_text(
      size = 10
    ),
    
    legend.title = element_text(
      size = 12,
      face = "bold"
    ),
    
    legend.text = element_text(
      size = 10
    )
  )


ggsave(
  "outputs/heatmap_pokemon.png",
  width = 10,
  height = 8,
  dpi = 300
)
