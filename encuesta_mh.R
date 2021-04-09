# p.18 (dejar fosas como estan)
# P.23C (no es una medida necesaria + no es una medida oportuna)
# P24 Y P24A (comisiones de investigacion —respuesta negativa)
# La de los simbolos la mas importante, en todo caso. Las otras pueden ayudar a reflejar hasta que punto hay gente en Espana que no quiere que se remueva el pasado, es decir que no está a favor de medidas TJ como el cambio de nombre de las calles.

setwd("~/Google Drive/Academic/Projects/streets_vox")
options(stringsAsFactors = FALSE)
library(tidyverse)
library(muniSpain)

data = read.csv("data/CIS2760/CIS2760.csv") %>%
  select(CCAA, PROV,
    P41, # Left-right self placement
    P38A, # vote remembered
    P18, # Que deberia hacerse con las victimas en fosas comunes?
    P1901, # Ag/Disag: "Durante el franquismo, las victimas de la Guerra Civil tuvieron un reconomiento diferente según el bando al que pertenecieron"
    P1905, # Ag/Disag: "Es mejor olvidarse del pasado porque, si se recumueve, podría volver a repetirse la Guerra Civil"
    P23A, # Qué le parece la LMH 2007?
    P23C, # Cuál de las siguientes frases refleja mejor lo que opina sobre la LMH?
    P24, # Debería de crearse una Comisión de Investigación indep del Gob, para la guerra civil?
    P24A, # Y para la dictadura?
    P2503, # Ag/Disag: "Los símbolos que suponen la exaltaciuon de la Guerra Civil, deben ser retirados de los lugares públicos"
    P2505) %>% # Ag/Disag: "Los símbolos que rinden homenaje a Franco y al franquismo, deben ser retirados de los lugares públicos"
  mutate(provincia = code_to_prov(PROV)) %>%
  rename(left_right = P41, prev_vote = P38A,
    victimas_fosas = P18,
    reconomiento_dif = P1901,
    remover_pasado = P1905,
    opinion_LMH = P23A,
    frases_LMH = P23C,
    com_inv_guerracivil = P24,
    com_inv_dictadura = P24A,
    simbolos_guerra = P2503,
    simbolos_franquismo = P2505) %>%
  mutate(
    reconomiento_dif = factor(recode(reconomiento_dif,
      '1' = "Agree", '2' = "Neutral", '3' = "Disagree", '8' = "Dont Know/NA", '9' = "Dont Know/NA")),
    remover_pasado = factor(recode(remover_pasado,
      '1' = "Agree", '2' = "Neutral", '3' = "Disagree", '8' = "Dont Know/NA", '9' = "Dont Know/NA")),
    simbolos_guerra = factor(recode(simbolos_guerra,
      '1' = "Agree", '2' = "Neutral", '3' = "Disagree", '8' = "Dont Know/NA", '9' = "Dont Know/NA")),
    simbolos_franquismo = factor(recode(simbolos_franquismo,
      '1' = "Agree", '2' = "Neutral", '3' = "Disagree", '8' = "Dont Know/NA", '9' = "Dont Know/NA"))) %>%
  mutate(
    reconomiento_dif = fct_relevel(reconomiento_dif,
      c("Agree", "Neutral", "Disagree", "Dont Know/NA")),
    remover_pasado = fct_relevel(remover_pasado,
      c("Agree", "Neutral", "Disagree", "Dont Know/NA")),
    simbolos_guerra = fct_relevel(simbolos_guerra,
      c("Agree", "Neutral", "Disagree", "Dont Know/NA")),
    simbolos_franquismo = fct_relevel(simbolos_franquismo,
      c("Agree", "Neutral", "Disagree", "Dont Know/NA"))) %>%
  mutate(
    victimas_fosas = factor(recode(victimas_fosas,
      '1' = "Should be identified and moved to a cemetery",
      '2' = "Should only be identified",
      '3' = "Nothing, leave them as they are",
      '4' = "There are no victims in mass graves",
      '8' = "Dont know/NA", '9' = "Dont know/NA")),
    frases_LMH = factor(recode(frases_LMH,
      '1' = "Necessary/Not enough",
      '2' = "Wrong/Not necessary",
      '3' = "Necessary/Not enough",
      '4' = "Wrong/Not necessary",
      '7' = "",
      '8' = "Dont know/NA", '9' = "Dont know/NA")),
    com_inv_guerracivil = factor(recode(com_inv_guerracivil,
      '1' = "Yes", '2' = "No",
      '8' = "Dont know/NA", '9' = "Dont know/NA")),
    com_inv_dictadura = factor(recode(com_inv_dictadura,
      '1' = "Yes", '2' = "No",
      '8' = "Dont know/NA", '9' = "Dont know/NA")))



# Agree/Disagree variables

pdf("data/CIS2760/reconomiento_dif.pdf", width = 8, height = 3.25)
ggplot(data, aes(x = reconomiento_dif)) +
  geom_bar(stat = "count") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 12),
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),,
    # panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank()) +
  labs(title = "''During the Francoist regime, the victims of the civil war were recognized differently\ndepending on the side they belonged to.''", y = "", x = "")
dev.off()

pdf("data/CIS2760/remover_pasado.pdf", width = 8, height = 3)
ggplot(data, aes(x = remover_pasado)) +
  geom_bar(stat = "count") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 12),
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),,
    # panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank()) +
  labs(title = "''It is better to forget the past because, if it is stirred, the civil war could break out again.''", y = "", x = "")
dev.off()

pdf("data/CIS2760/simbolos_guerra.pdf", width = 8, height = 3)
ggplot(data, aes(x = simbolos_guerra)) +
  geom_bar(stat = "count") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 12),
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),,
    # panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank()) +
  labs(title = "''The symbols that praise the civil war should be removed from public spaces''", y = "", x = "")
dev.off()

pdf("data/CIS2760/simbolos_franquismo.pdf", width = 8, height = 3)
ggplot(data, aes(x = simbolos_franquismo)) +
  geom_bar(stat = "count") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 12),
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),,
    # panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank()) +
  labs(title = "''The symbols that praise the Francoist regime should be removed from public spaces''", y = "", x = "")
dev.off()


# Others

pdf("data/CIS2760/victimas_fosas.pdf", width = 8, height = 3)
ggplot(data, aes(x = victimas_fosas)) +
  geom_bar(stat = "count") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 12),
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),,
    # panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank()) +
  coord_flip() +
  labs(title = "''What should be done with the remains\nof the victims that are still in mass graves?''", x = "", y = "")
dev.off()

pdf("data/CIS2760/frases_LMH.pdf", width = 8, height = 3)
ggplot(subset(data, !is.na(frases_LMH) & frases_LMH != ""), aes(x = frases_LMH)) +
  geom_bar(stat = "count") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 12),
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),,
    # panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank()) +
  labs(title = "''What do you think about the 2007 Law of Historical Memory?''", y = "", x = "")
dev.off()

# pdf("data/CIS2760/com_inv_guerracivil.pdf", width = 8, height = 3)
# ggplot(data, aes(x = com_inv_guerracivil)) +
#   geom_bar(stat = "count") +
#   theme_bw() +
#   theme(axis.text.y = element_text(size = 12),
#     panel.background = element_blank(),
#     panel.grid.major.x = element_blank(),,
#     # panel.grid.major.y = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.y = element_blank()) +
#   labs(title = "''''", y = "", x = "")
# dev.off()
#
# pdf("data/CIS2760/com_inv_dictadura.pdf", width = 8, height = 3)
# ggplot(data, aes(x = com_inv_dictadura)) +
#   geom_bar(stat = "count") +
#   theme_bw() +
#   theme(axis.text.y = element_text(size = 12),
#     panel.background = element_blank(),
#     panel.grid.major.x = element_blank(),,
#     # panel.grid.major.y = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     axis.ticks.y = element_blank()) +
#   labs(title = "''''", y = "", x = "")
# dev.off()
