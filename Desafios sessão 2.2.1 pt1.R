

#Importando bibliotecas
library(dslabs)
library(dplyr)
library(lubridate)
library(caret)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

head(dat)

#############################################################################
#Q1 - Identificando a proporção de mulheres em aulas online e presencial
#############################################################################

#Criando DF para registrar a proporção de mulheres que tiveram aula presencial
df_inclass <- subset(dat,type=='inclass')  #Filtrando aulas presenciais
proporcao_presencial_mulher <- mean(df_inclass$sex == "Female")
head(proporcao_inclass_mulher)

#Criando DF para registrar a proporção de mulheres que tiveram aula online
df_online <- subset(dat,type=="online")  #Filtrando aulas online
proporcao_online_mulher <- mean(df_online$sex == "Female")
head(proporcao_online_mulher)

#############################################################################
#Q2 - Faça a previsão do sexo com base do modelo de aula presencial ou remoto
#############################################################################

#Definindo prevalencia presencial por sexo
sexo_prevalente_presencial <- ifelse(proporcao_presencial_mulher > 0.5,"Female","Male")
head(sexo_prevalente_presencial)

#Definindo a prevalencia online por sexo
sexo_prevalente_online <- ifelse(proporcao_online_mulher > 0.5,"Female","Male")
head(sexo_prevalente_online)

#Criando a Previsão de sexo com base no modelo de aula
previsao_sexo <- ifelse(dat$type=="inclass",sexo_prevalente_presencial,sexo_prevalente_online)
head(previsao_sexo) 

#Comparando previsão com o dataframe original / Linha a linha e criando a média, valores TRUE valem 1 e FALSE vale 0
precisao <- mean(previsao_sexo == dat$sex)

# Mostrar a precisão
cat("Precisão da previsão de sexo baseada no tipo de classe: ", precisao, "\n")

#############################################################################
#Q3 Apresente os resultados em uma matriz de confusão
#############################################################################

table(previsao_sex, dat$sex)

#############################################################################
#Q4 Identifique a sensibilidade da predição usando o comando sensitivity()
#############################################################################

#Transformando DFs em Factor para agrupar dados categoricos
y_previsao <- factor(previsao_sexo, levels = c("Female","Male"))
y_dat <- factor(dat$sex, levels = c("Female","Male"))

#Identificando o % de sensibilidade - Identificação de verdadeiros positivos
sensitivity(y_previsao,y_dat)

#############################################################################
#Q5 Identifique a Especificidade da predição usando o comando specificity()
#############################################################################

#identificando o % de especificidade - % correto de verdadeiros negativos
specificity(y_previsao,y_dat)

#############################################################################
#Q6 Qual a prevalencia % de mulheres no dataset "dat"
#############################################################################

#Calculando prevalencia
mean(dat$sex=="Female")

#Prevalência é a quantidade de casos de uma condição em um grupo de pessoas durante um certo tempo. 
#Por exemplo, se 10 em cada 100 pessoas têm um resfriado, a prevalência do resfriado é de 10%.









