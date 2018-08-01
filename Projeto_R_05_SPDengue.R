### Dengue

# Fonte: http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sinannet/cnv/dengueSP.def
library(dplyr)

dengue2008 <- read.csv('dengue_sp_2007.csv', sep = ';', skip = 3, stringsAsFactors = F, na.strings
= '-', header = T, blank.lines.skip = TRUE)
dengue2008_tbl <- dengue2008 %>% as.tbl() %>% slice(1:526)
head(dengue2008)
glimpse(dengue2008)

# Data Cleaning
library(tidyr)
library(stringr)

# tidy
dengue2008_t <- dengue2008_tbl %>% 
  select(-Total) %>% 
  gather(key = Regiao, value = Casos, -Município.de.notificação, na.rm = T)
names(dengue2008_t) <- c('Municipio', 'Regiao', 'Casos')

# Data Cleaning
dengue2008_clean <- dengue2008_t

# Municipio
dengue2008_clean$Municipio <- str_replace_all(dengue2008_t$Municipio, '[:digit:]', '')

# Regiao
dengue2008_clean$Regiao <- str_replace_all(dengue2008_clean$Regiao, '[:digit:]|[X.]', ' ')
