# ----------------------------- Descarga de datos ------------------------------
# Descarga de datos del IRCL-IMF a través de un API. Actualizado 01/10/2020


# install.packages(c('imfr', 'IMFData', 'httr', 'jsonlite', 'dplyr')
library(imfr)
library(IMFData)
library(httr)
library(jsonlite)
library(dplyr)

# Codelist para IRFCL
IRFCL_IDS <- imf_codelist("IRFCL")

# Países e indicadores
CODE_COUNTRY <- imf_codes(codelist="CL_AREA_IRFCL")
CODE_INDICATOR <- imf_codes(codelist="CL_INDICATOR_IRFCL")

INDICATOR <- c("RAF_USD", "RAFA_USD", "RAFAFX_USD", "RAFAFXS_USD", "RAFAFXSI_USD", 
               "RAFAFXCD_USD", "RAFAFXCDN_USD", "RAFAFXCDBI_USD", "RAFAFXCDBIA_USD", 
               "RAFAFXCDBO_USD", "RAFAFXCDBOA_USD", "RAFAIMF_USD", "RAFASDR_USD", 
               "RAFAGOLD_USD", "RAFAGOLDV_OZT", "RAFAGOLDVGB_OZT", "RAFAGOLDVUG_OZT", 
               "RAFAO_USD", "RAFAOF_USD", "RAFAOL_USD", "RAOFA_USD", "RAOFAS_USD", 
               "RAOFAD_USD", "RAOFAL_USD","RAOFAF_USD", "RAOFAG_USD", "RAOFAO_USD")


# Indicadores para todos los países 
TOTAL <- data.frame()
df <- data.frame()

for (i in 1:length(INDICATOR)){
  df <- imf_data(database_id = 'IRFCL', indicator = INDICATOR[i], country = 'all', 
                 freq = 'M', start = 2008, end = current_year())
  df <- df[df[,3] != 0, ]
  df <- df[!duplicated(data.frame(df[,1],df[,2])), ]
  if(i == 1){
    TOTAL <- df
  }else{
    TOTAL <- merge(x = TOTAL, y = df, all.x = TRUE)
  }
  assign(INDICATOR[i],df)
}

# Arreglo de datos fallidos
TOTAL[(which(TOTAL$iso2c == 'EE' & TOTAL$year_month == '2011-01')), 'RAF_USD'] <- 247.65
TOTAL <- TOTAL[!TOTAL$iso2c == 'BE',]
df_1 <- data.frame()
for (i in 1:length(INDICATOR)){
  df_1 <- imf_data(database_id = 'IRFCL', indicator = INDICATOR[i], country = 'BE', 
                   freq = 'M', start = 2008, end = current_year())
  if(i == 1){
    df_1 <- df_1[df_1[,3] > 1000, ]
  }else{
    df_1 <- df_1[df_1[,3] != 0, ]
    df_1 <- df_1[!duplicated(data.frame(df_1[,1],df_1[,2])), ]
  }
  if(i == 1){
    BE <- df_1
  }else{
    BE <- merge(x = BE, y = df_1, all.x = TRUE)
  }
}

TOTAL <- rbind(TOTAL,BE)


# Países con todos los indicadores
AUX <- RAF_USD$iso2c
COUNTRIES <- intersect(CODE_COUNTRY$codes, AUX)
LIST_C <- list()

for (i in 1:length(COUNTRIES)){
  df <- TOTAL[TOTAL$iso2c == COUNTRIES[i],]
  if(i == 56){
    assign(gsub(" ", "", paste (COUNTRIES[i],'M')),df)
  }else{
    assign(COUNTRIES[i],df)
  }
}

# Indicadores paa todos lo paises en valores porcentuales
INDICATOR_P <- TOTAL[3:29]/TOTAL$RAF_USD
TOTAL_P <- data.frame(TOTAL[,1:2], INDICATOR_P)

# Limpieza de CODE_COUNTRY y CODE_INDICATOR
df_1 <- data.frame(x = CODE_INDICATOR$codes, description = CODE_INDICATOR$description)
df_2 <- data.frame(x = CODE_COUNTRY$codes, description = CODE_COUNTRY$description)

Code_I <- merge(INDICATOR, df_1, all.x = TRUE)
Code_C <- merge(COUNTRIES, df_2, all.x = TRUE)


save(RAF_USD, RAFA_USD, RAFAFX_USD, RAFAFXS_USD, RAFAFXSI_USD, RAFAFXCD_USD, 
     RAFAFXCDN_USD, RAFAFXCDBI_USD, RAFAFXCDBIA_USD, RAFAFXCDBO_USD, RAFAFXCDBOA_USD, 
     RAFAIMF_USD, RAFASDR_USD, RAFAGOLD_USD, RAFAGOLDV_OZT, RAFAGOLDVGB_OZT, 
     RAFAGOLDVUG_OZT, RAFAO_USD, RAFAOF_USD, RAFAOL_USD, RAOFA_USD, RAOFAS_USD, 
     RAOFAD_USD, RAOFAL_USD,RAOFAF_USD, RAOFAG_USD, RAOFAO_USD, 
     file = "Indicadores.Rda")

save(AL, AR, AM, AU, AT, BY, BE, BO, BR, BG, CA, CL, HK, CN, CO, CR, HR, CY, CZ, 
     DK, DO, EC, EG, SV, EE, FI, FR, GE, DE, GR, GT, HN, HU, IS, IN, ID, IE, IL, 
     IT, JM, JP, JO, KZ, KR, KG, LV, LT, LU, MY, MT, MU, MX, MD, MN, MA, NAM, NL, 
     NZ, NI, MK, NO, PY, PE, PH, PL, PT, RO, RU, SA, RS, SC, SG, SK, SI, ZA, ES, 
     LK, SE, CH, TH, TN, TR, UA, GB,
     US, UY, PS, file = "Paises.Rda")

save(TOTAL, TOTAL_P, file = "Total.Rda")
save(Code_C, Code_I, file = "Codigos.Rda")
save(COUNTRIES, INDICATOR, file = "Etiquetas.Rda")
