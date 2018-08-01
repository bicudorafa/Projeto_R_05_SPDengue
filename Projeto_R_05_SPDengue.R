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
# Variacao dos casos do dengue por regiao
 
  group_by(Regiao)
