### Mapa das Ocorrencias de Dengue (2008-2012)

# Fonte: http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sinannet/cnv/dengueSP.def

## Data Cleaning

# Pacotes necessarios
library(dplyr)
library(tidyr)
library(stringr)

# Funcao para abertura e limpeza das tabelas anuais
Open.Dengue <- function (linha) {
  
  #Abertura do arquivo
  nome <- as.character(linha[1])
  dengue <- read.csv(nome, sep = ';', skip = 3, stringsAsFactors = F, na.strings
                     = '-', header = T, blank.lines.skip = TRUE)
  dengue_tbl <- dengue %>% as.tbl() %>% slice(1:526)
  
  # Data Cleaning
  
  # tidy
  dengue_t <- dengue_tbl %>% 
    select(-Total) %>% 
    gather(key = Regiao, value = Casos, -Município.de.notificação, na.rm = T)
  names(dengue_t) <- c('Municipio', 'Regiao', 'Casos')
  
  # Data Cleaning
  dengue_clean <- dengue_t
  
  # Municipio
  dengue_clean$Municipio <- str_replace_all(dengue_clean$Municipio, '[:digit:]', '')
  
  # Regiao
  dengue_clean$Regiao <- str_replace_all(dengue_clean$Regiao, '[:digit:]|[X.]', ' ') %>% str_trim()
  
  # Ano
  ano <- as.numeric(linha[2])
  dengue_clean$Ano <- ano
  return(dengue_clean)
}

# Vetor com os arquivos a serem gerados 
dengue_arquivos <- list.files(pattern = ".csv")
dengue_arquivos

# Vetor com os anos a serem usados na funcao
dengue_anos <- extract_numeric(dengue_arquivos) 

# Lista com ambos
dengue_lista <- data.frame(df=dengue_arquivos, ano=dengue_anos, stringsAsFactors = F)

# Lista com dataframes
#teste <- Open.Dengue(dengue_lista[1, ])
dengue_dfs_lista <- apply(dengue_lista, 1, Open.Dengue)

# Juncao dos dfs
dengue_df <- do.call(rbind, dengue_dfs_lista)
glimpse(dengue_df)


## EDA

# Pacotes
library(ggplot2)
library(RColorBrewer)

# Plot 1 do avanco dos casos totais ao longo dos anos <- size tá ruim
dengue_df %>% 
  group_by(Ano) %>%
  summarise(Total_ano = sum(Casos)) %>% 
  ggplot(aes(x = Ano, y = Total_ano)) +
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Série de Casos Totais Anuais", 
       subtitle="Soma dos Casos Ocorridos no Estado por Ano", 
       caption="Source: Ministério da Saúde", 
       y='Total') +  # title and caption
  theme_bw() #+
 # theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
 #       panel.grid.minor = element_blank())

# Df com as regioes e a sua quantidade de casos
dengue_df_regioes <- dengue_df %>%  
  group_by(Regiao, Ano) %>% 
  summarise('Total' = sum(Casos))

# Df para construcao do plot comparativo de quanto as regioes estao em relacao a media e consecutivo plot 2
dengue_df_m <- dengue_df_regioes %>% 
  group_by(Regiao) %>% 
  summarise(Media = mean(Total)) %>%
  mutate(Media_s = scale(Media)) %>% 
  mutate(Situacao = ifelse(Media_s > 0, 'Acima', 'Abaixo'))
  
dengue_df_m %>% 
  arrange(Media) %>% 
  mutate(Regiao = factor(Regiao, levels = Regiao,ordered = TRUE)) %>% 
  ggplot(aes(x=Regiao, y=Media_s, label=Media_s)) + 
  geom_bar(stat='identity', aes(fill=Situacao), width=.5)  +
  scale_fill_manual(name="Situação em relação à Média", 
                    labels = c("Abaixo", "Acima"), 
                    values = c("Abaixo"="#FF0000", "Acima" = "#00ba38")) + 
  labs(subtitle="Quais são as regiões mais críticas dos últimos anos", 
       title= "Regiões em Relação à Média Histórica") + 
  coord_flip() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Df com as regioes mais críticas e plot 3 consecutivo
regioes_maisCasos <- dengue_df_regioes %>% 
  group_by(Regiao) %>% 
  summarise(Media = mean(Total)) %>% 
  arrange(desc(Media)) %>% 
  head(n = 7) 

regioes_maisCasos %>%
  mutate(Regiao = factor(Regiao, levels = Regiao,ordered = TRUE)) %>% 
  ggplot(aes(x = Regiao, y = Media, fill = Regiao)) +
  geom_bar(stat = 'identity') +
  labs(subtitle="Quais são as regiões mais críticas dos últimos anos", 
       title= "Regiões em Relação à Média Histórica") +
  scale_fill_brewer(direction = -1) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()) 

# Variacao dos casos do dengue nas 6 maiores regioes e seu plot 4 
dengue_df_regioes %>%
  inner_join(regioes_maisCasos) %>% 
  ggplot(aes(x = Ano, y = Total, group = Regiao, color = Regiao)) +
  geom_line() +
  geom_point() +
  labs(title="Casos das Regiões mais Críticas", 
       subtitle="Variação do número ao longo do período analisado",
       color=NULL) +
  theme_bw()

## Montagem do Dashboard interativo do Estado no ano mais recente

# Carregando pacotes necessários
library(ggmap)
library(leaflet)

# df para geracao do dashboard
dengue_df_dashB <- dengue_df_regioes %>% 
  filter(Ano == max(unique(dengue_df_regioes$Ano)))

# Gerando os mapas de cada cidade de SP
longlat <- geocode(dengue_df_dashB$Regiao) %>% 
  mutate(loc = unique(dengue_df_regioes$Regiao)) 

# Criacao do df com ano mais recente e geolocalizacoes obtidas
dengue_df_dashB  %>% 
  left_join(longlat, by = c("Regiao" = "loc")) %>% 
  mutate(LatLon = paste(lat, lon, sep = ":")) -> criacao_mapa

# Formatando a saída e gerando um movo dataframe chamado long_formapping
num_de_vezes_repetir <- criacao_mapa$Total
criacao_mapa <- criacao_mapa[rep(seq_len(nrow(criacao_mapa)),
                                  num_de_vezes_repetir),]

# Gerando o mapa com o dataframe
leaflet(criacao_mapa_limpo) %>% 
  addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions())