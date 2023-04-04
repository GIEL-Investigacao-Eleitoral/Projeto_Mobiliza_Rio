
library(tidyr)
idades = alunos %>% select(idade)
table(faixas_01$horarios)
table(faixas_02$horarios)
table(faixas_03$horarios)

idades$id = rownames(idades) 
faixas_01$id = rownames(idades) 
faixas_02$id = rownames(idades) 
faixas_03$id = rownames(idades) 

idades = idades %>% left_join(faixas_01,by = join_by(id))
idades = idades %>% left_join(faixas_02,by = join_by(id))
idades = idades %>% left_join(faixas_03,by = join_by(id))

idades = idades %>% select(idade,horarios.x,horarios.y,horarios)
colnames(idades) = c('idade','horarios_ativ_01','horarios_ativ_02','horarios_ativ_03')
