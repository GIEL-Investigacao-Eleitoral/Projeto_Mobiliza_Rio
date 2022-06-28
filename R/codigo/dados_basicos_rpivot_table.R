library(rpivotTable)
library(janitor)
library(readxl)
Mobiliza_Rio <- read_excel("C:/Users/Hp/Desktop/Borba Mobiliza Rio/dados/Perfil alunos Mobiliza Rio.xlsx") %>% clean_names()
names(Mobiliza_Rio)




selecao <- c("bairro",                                                                                                                                                                       
"peso",                                                                                                                                                                         
"altura",                                                                                                                                                                       
"ativo",                                                                                                                                                                        
"escola_tipo",                                                                                                                                                                  
"escola_serie",                                                                                                                                                                 
"escola_turno",                                                                                                                                                                 
"escolaridade",                                                       
"anamnese_tem_alguma_doenca_cronica",                                                                                                                                           
"anamnese_tem_alguma_doenca_cronica_qual",                                                                                                                                      
"anamnese_tem_doenca_ou_lesao",                                                                                                                                                 
"anamnese_tem_doenca_ou_lesao_explique",                                                                                                                                        
"anamnese_tem_protese",                                                                                                                                                         
"anamnese_tem_protese_quais",                                                                                                                                                   
"anamnese_usa_medicamento",                                                                                                                                                     
"anamnese_usa_medicamento_quais",                                                                                                                                               
"anamnese_usa_medicamento_motivo",                                                                                                                                              
"anamnese_possui_alergias",                                                                                                                                                     
"anamnese_possui_alergias_quais",                                                                                                                                               
"anamnese_fez_alguma_cirurgia",                                                                                                                                                 
"anamnese_fez_alguma_cirurgia_quais",                                                                                                                                           
"anamnese_tabagismo",                                                                                                                                                           
"anamnese_etilismo",                                                                                                                                                            
"anamnese_possui_doenca_na_familia",                                                                                                                                            
"anamnese_possui_doenca_na_familia_quais",                                                                                                                                      
"atividade_1",                                                                                                                                                                  
"atividade_2",                                                                                                                                                                  
"atividade_3",                                                                                                                                                                  
"atividade_4",                                                                                                                                                                  
"atividade_5")  

selecao2<-c("algum_medico_ja_disse_que_voce_possui_algum_problema_de_coracao_ou_pressao_arterial_e_que_somente_deveria_realizar_atividade_fisica_supervisionado_por_profissionais_de_saude",
            "voce_sente_dores_no_peito_quando_pratica_atividade_fisica",                                                                                                                    
            "no_ultimo_mes_voce_sentiu_dores_no_peito_ao_praticar_atividade_fisica",                                                                                                        
            "voce_apresenta_algum_desequilibrio_devido_a_tontura_e_ou_perda_momentanea_da_consciencia",                                                                                     
            "voce_possui_algum_problema_osseo_ou_articular_que_pode_ser_afetado_ou_agravado_pela_atividade_fisica",                                                                         
            "voce_toma_atualmente_algum_tipo_de_medicacao_de_uso_continuo",                                                                                                                 
            "voce_realiza_algum_tipo_de_tratamento_medico_para_pressao_arterial_ou_problemas_cardiacos",                                                                                    
            "voce_realiza_algum_tratamento_medico_continuo_que_possa_ser_afetado_ou_prejudicado_com_a_atividade_fisica",                                                                    
            "voce_ja_se_submeteu_a_algum_tipo_de_cirurgia_que_comprometa_de_alguma_forma_a_atividade_fisica",                                                                               
            "sabe_de_alguma_outra_razao_pela_qual_a_atividade_fisica_possa_eventualmente_comprometer_a_sua_saude",                                                                          )

Mobiliza_Rio$escolaridade <-as.numeric(Mobiliza_Rio$escolaridade)
library(dplyr)
Mobiliza_Rio <-Mobiliza_Rio %>% select(selecao)


rpivotTable(Mobiliza_Rio)
