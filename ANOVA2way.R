library(readxl)
library(dplyr)
library(tidyverse)
dados <- read_xlsx("helicopter.xlsx")

#Transformando variaveis em fatores
dados$papel <- factor(dados$papel, levels = c("Seda", "Sulfite", "Canson"))
for (i in 1:nrow(dados)){
  if (dados$clips[i] == "Não"){
    dados$clips[i] <- "Sem clipe"
  } else {
    dados$clips[i] <- "Com clipe"
  }
}
dados$clips <- as.factor(dados$clips)

#Variavel resposta: media dos tempos
dados$tempo <- rowMeans(dados %>% select(terreo, segundo_andar))


#Analise descritiva
descritiva <- function(variavel){
  medidas <- summary(variavel)
  standard.deviation <- sd(variavel)
  
  return(c(medidas, SD = standard.deviation))}


medidas.resumo <- dados %>% select(c("papel", "clips", "tempo")) %>% group_by(papel, clips) %>% 
  summarise(n = n(), Media = mean(tempo), standard.deviation = sd(tempo), 
            cv = (standard.deviation/Media)*100, Minimo = min(tempo), Maximo = max(tempo))

library(knitr)
kable(medidas.resumo, col.names = c("papel", "clips", "n", "Media", "standard.deviation", "cv", "Minimo", "Maximo"),
      digits = 2, caption = "Tabela Teste") 

#Os helicpteros de seda , independente da presenca de clipes, tiveram os tempos mais variados em relacao aos helicopteros de outros papeis:
dados %>% ggplot(aes(x = papel, y = tempo, fill = papel)) + 
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~ clips) +
  labs(x = "Tipo de papel", y = "Tempo médio de queda (em segundos)") +
  theme_bw()

#Gráfico de dispersão:
dados %>% ggplot(aes(x = papel, y = tempo, color = papel)) + 
  geom_point(show.legend = FALSE) +
  facet_wrap(~ clips) +
  labs(x = "Tipo de papel", y = "Tempo médio de queda (em segundos)") +
  theme_bw()

# Gráfico de interação:
medias <- dados %>% group_by(clips, papel) %>% summarise(media_tempo = mean(tempo))

dados %>% ggplot(aes(x = papel, y = tempo)) +
  geom_point() +
  geom_line(data = medias, aes(x = papel, y = media_tempo, group = clips, color = clips)) +
  labs(x = "Tipo de papel", y = "Tempo médio de queda (em segundos)", color = "") +
  theme_bw()


# ANOVA
modelo.anova <- lm(tempo ~ papel*clips, dat = dados)
fit<-anova(modelo.anova)

# Diagnóstico do modelo
#qqplot - normalidade
dados$residuos <- residuals(modelo.anova)
dados %>% ggplot(aes(sample = residuos)) + 
  stat_qq(color = "blue") + 
  stat_qq_line(color = "red") + 
  theme_bw()

dados$res_padronizados <- dados$residuos/sqrt(fit$`Mean Sq`[4])
dados %>% ggplot(aes(sample = res_padronizados)) + 
  stat_qq(color = "blue") + 
  stat_qq_line(color = "red") + 
  theme_bw()

#residuos padronizados versus niveis fator A
dados %>% select(c(papel, res_padronizados)) %>% ggplot(aes(x = papel, y = res_padronizados, color = papel)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Tipo de papel", y = "Resíduos padronizados") +
  theme_bw()

#residuos padronizados versus niveis fator B
dados %>% select(c(clips, res_padronizados)) %>% ggplot(aes(x = clips, y = res_padronizados, color = clips)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Presença/ Ausência de clipe", y = "Resíduos padronizados") +
  theme_bw()

# TRANSFORMAÇÃO log
#qqplot - normalidade
dados$tempo_log <-log(dados$tempo)

modelo.anova2 <- lm(tempo_log ~ papel*clips, data = dados)
fit2<-anova(modelo.anova2)

dados$residuos.2 <- residuals(modelo.anova2)
dados %>% ggplot(aes(sample = residuos.2)) + 
  stat_qq(color = "blue") + 
  stat_qq_line(color = "red") + 
  labs(x = "Quantis teóricos", y = "Quantis amostrais") +
  theme_bw()


# ANOVA NAO PARAMÉTRICA
# Transformação em postos

aov.rnk <- aov(rank(tempo) ~ papel*clips, data = dados, 
               contrasts = list(papel = 'contr.sum', clips = 'contr.sum'))

summary(aov.rnk, type = 'III')

# ou: (tanto faz)
aov.rnk.2 <- lm(rank(tempo) ~ papel*clips, data = dados, 
                contrasts = list(papel = 'contr.sum', clips = 'contr.sum'))

fit3<-anova(aov.rnk.2)
fit3

# Resíduos
#qqplot - normalidade
dados$res.np <- residuals(aov.rnk.2)
dados %>% ggplot(aes(sample = res.np)) + 
  stat_qq(color = "blue") + 
  stat_qq_line(color = "red") + 
  labs(x = "Quantis teóricos", y = "Quantis amostrais") +
  theme_bw()

dados$res_padronizados_np <- dados$res.np/sqrt(fit3$`Mean Sq`[4])
dados %>% ggplot(aes(sample = res_padronizados_np)) + 
  stat_qq(color = "blue") + 
  stat_qq_line(color = "red") + 
  theme_bw()

#residuos padronizados versus niveis fator A
dados %>% select(c(papel, res_padronizados_np)) %>% ggplot(aes(x = papel, y = res_padronizados_np, color = papel)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Tipo de papel", y = "Resíduos padronizados") +
  theme_bw()

#residuos padronizados versus niveis fator B
dados %>% select(c(clips, res_padronizados_np)) %>% ggplot(aes(x = clips, y = res_padronizados_np, color = clips)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Presença/ Ausência de clipe", y = "Resíduos padronizados") +
  theme_bw()


# POS-TESTE
TukeyHSD(aov.rnk)
plot(TukeyHSD(aov.rnk), las=1, col="red", cex.axis=0.2) 
