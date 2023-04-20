#script de pruebas
library(RcppArmadillo)
library(IdR)

ictneo2HGM = list( 
  
  testApgar5 = node( Type = "CHANCE", Name = "testApgar5", Values = c("A0_3", "A4_7", "A8_10"), Preds = c("AsfixiaPerinatal"), 
                     Pots = matrix( data = c(
                       0.01,  0.1, 0.89, 
                       0.20,  0.7, 0.1),
                       nrow = 2, ncol = 3, byrow = TRUE, dimnames = NULL)),
  
  FactorRHHijo = node( Type = "CHANCE", Name = "FactorRHHijo", Values = c("NEGATIVE","POSITIVE"), Preds = c("FactorRHMadre"), 
                       Pots = matrix( data = c(
                         0.30, 0.70, 
                         0.09, 0.91),
                         nrow = 2, ncol = 2, byrow = TRUE, dimnames = NULL)), 
  
  FactorRHMadre  = node(Type="CHANCE", Name="FactorRHMadre", Values = c("NEGATIVE","POSITIVE"), Preds = c(), 
                        Pots = matrix( data = c(
                          0.1316, 0.8684),
                          nrow = 1, ncol = 2, byrow = TRUE, dimnames = NULL)), 
  
  testCoombsIsoRHHijo = node(Type="CHANCE", Name="testCoombsIsoRHHijo", Values = c("NEGATIVE","POSITIVE"), 
                             Preds = c("IsoinmunizacionRh"), 
                             Pots = matrix( data = c(
                               0.95, 0.05,
                               0.07, 0.93),
                               nrow=2,ncol=2,byrow=TRUE,dimnames=NULL)), 
  
  pesoNacer = node(Type="CHANCE", Name="pesoNacer", Values = c("P499_999", "P1000_1499", "P2500"), Preds = c(), 
                   Pots = matrix( data = c(
                     0.06, 0.40, 0.54),
                     nrow=1, ncol=3, byrow=TRUE, dimnames=NULL)), 
  
  antecedentesParto = node(Type="CHANCE", Name="antecedentesParto", Values = c("Primeriza","Multipara"), Preds = c(), 
                           Pots = matrix( data = c(
                             0.93, 0.07),
                             nrow=1, ncol=2, byrow=TRUE, dimnames=NULL)), 
  
  partoInstrumental = node(Type="CHANCE", Name="partoInstrumental", Values = c("NoInstrumental","SiInstrumental"), Preds = c(), 
                           Pots = matrix( data = c(
                             0.85, 0.15),
                             nrow=1, ncol=2, byrow=TRUE, dimnames=NULL)), 
  
  edadHijo1 = node(Type="CHANCE", Name="edadHijo1", Values = c("menos_de_12horas", "mas_de_12horas"), Preds = c(), 
                   Pots = matrix( data = c(
                     0.85, 0.15),
                     nrow=1,ncol=2,byrow=TRUE,dimnames=NULL)), 
  
  concentrBilirrubina1 = node(Type="CHANCE", Name="concentrBilirrubina1", Values = c("Normal","Alta"), 
                              Preds = c("concentrHemoglobina1","edadHijo1"), 
                              Pots = matrix( data = c(
                                0.85, 0.15,
                                0.0,  1.0,
                                0.0,  1.0,
                                0.0,  1.0,
                                0.0,  1.0,
                                0.0,  1.0),
                                nrow=6,ncol=2,byrow=TRUE,dimnames=NULL)), 
  
  concentrHemoglobina1 = node(Type="CHANCE", Name="concentrHemoglobina1", Values = c("Baja","Media","Alta"), 
                              Preds = c("IsoinmunizacionGrupo","IsoinmunizacionRh","AsfixiaPerinatal","OtrasPatologias","HemorragiaOculta"), 
                              Pots = matrix( data = ORGate( 3, c( 2, 2, 2, 2, 2), c(0.3, 0.1, 0.05, 0.45, 0.25, 0.2, 0.0, 0.9, 0.1, 0.5)),
                                             nrow=32, ncol=3, byrow=TRUE, dimnames=NULL)), 
  
  edadHijo2  = node(Type="CHANCE", Name="edadHijo2", Values = c("menos_de_24horas", "mas_de_24horas"), Preds = c(), 
                    Pots = matrix( data = c(
                      0.85, 0.15),
                      nrow=1,ncol=2,byrow=TRUE,dimnames=NULL)), 
  
  concentrBilirrubina2 = node(Type="CHANCE", Name="concentrBilirrubina2", Values = c("Normal","Alta"), 
                              Preds = c("concentrHemoglobina2","edadHijo2"), 
                              Pots = matrix( data = c(
                                0.8, 0.2,
                                0.0, 1.0,
                                0.8, 0.2,
                                0.8, 0.2,
                                0.0, 1.0,
                                0.8, 0.2),
                                nrow=6,ncol=2,byrow=TRUE,dimnames=NULL)), 
  
  concentrHemoglobina2 = node(Type="CHANCE", Name="concentrHemoglobina2", Values = c("Baja","Media","Alta"), 
                              Preds = c("IsoinmunizacionGrupo","IsoinmunizacionRh","AsfixiaPerinatal","OtrasPatologias","HemorragiaOculta"), 
                              Pots = matrix( data = ORGate( 3, c( 2, 2, 2, 2, 2), c(0.28, 0.08, 0.05, 0.42, 0.1, 0.18, 0.0, 0.25, 0.08, 0.03)),
                                             nrow=32, ncol=3, byrow=TRUE, dimnames=NULL)), 
  
  IsoinmunizacionGrupo = node(Type="CHANCE", Name="IsoinmunizacionGrupo", Values = c("A","P"), Preds = c(), 
                              Pots = matrix( data = c(
                                0.90, 0.10),
                                nrow=1,ncol=2,byrow=TRUE,dimnames=NULL)), 
  
  IsoinmunizacionRh = node(Type="CHANCE", Name="IsoinmunizacionRh", Values = c("A","P"), 
                           Preds = c("FactorRHMadre","FactorRHHijo","AsfixiaPerinatal"), 
                           Pots = matrix( data = c(
                             0.99, 0.01,
                             0.99, 0.01,
                             0.99, 0.01,
                             0.99, 0.01,
                             0.95, 0.05,
                             0.90, 0.10,
                             0.99, 0.01,
                             0.99, 0.01),
                             nrow=8,ncol=2,byrow=TRUE,dimnames=NULL)), 
  
  AsfixiaPerinatal = node(Type="CHANCE", Name="AsfixiaPerinatal", Values = c("A","P"), Preds = c(), 
                          Pots = matrix( data = c(
                            0.83, 0.17),
                            nrow=1,ncol=2,byrow=TRUE,dimnames=NULL)), 
  
  OtrasPatologias = node(Type="CHANCE", Name="OtrasPatologias", Values = c("A","P"), Preds = c(), 
                         Pots = matrix( data = c(
                           0.96, 0.04),
                           nrow=1,ncol=2,byrow=TRUE,dimnames=NULL)), 
  
  HemorragiaOculta = node(Type="CHANCE", Name="HemorragiaOculta", Values = c("A","P"), Preds = c("partoInstrumental"), 
                          Pots = matrix( data = c(
                            0.95, 0.05,
                            0.85, 0.15),
                            nrow=2,ncol=2,byrow=TRUE,dimnames=NULL)), 
  
  PatologLeveGrave = node(Type="CHANCE", Name="PatologLeveGrave", Values = c("Leve", "Grave", "MuyGrave"), 
                          Preds = c("IsoinmunizacionGrupo", "IsoinmunizacionRh", "AsfixiaPerinatal", "OtrasPatologias", "HemorragiaOculta"), 
                          Pots = matrix( data = ORGate( 3, c( 2, 2, 2, 2, 2), c(0.7, 0.2, 0.2, 0.3, 0.4, 0.1, 0.7, 0.7, 0.6, 0.3)),
                                         nrow=32,ncol=3,byrow=TRUE,dimnames=NULL)), 
  
  CosteEconomico = node(Type="CHANCE", Name="CosteEconomico", Values = c("Nulo", "Bajo", "Medio", "Alto", "MuyAlto"), 
                        Preds = c("Terapia1", "Terapia2"), 
                        Pots = matrix( data = c(
                          1.0,  0.0,  0.0,  0.0,  0.0,
                          1.0,  0.0,  0.0,  0.0,  0.0,
                          1.0,  0.0,  0.0,  0.0,  0.0,
                          1.0,  0.0,  0.0,  0.0,  0.0,
                          1.0,  0.0,  0.0,  0.0,  0.0,
                          1.0,  0.0,  0.0,  0.0,  0.0,
                          1.0,  0.0,  0.0,  0.0,  0.0,
                          1.0,  0.0,  0.0,  0.0,  0.0,
                          
                          1.0,  0.0,  0.0,  0.0,  0.0,
                          0.0,  0.99, 0.01, 0.0,  0.0,
                          0.0,  0.99, 0.01, 0.0,  0.0,
                          0.0,  0.95, 0.05, 0.0,  0.0,
                          0.0,  0.9,  0.1,  0.0,  0.0,
                          0.0,  0.85, 0.15, 0.0,  0.0,
                          0.0,  0.99, 0.01, 0.0,  0.0,
                          0.0,  0.0,  0.0,  0.7,  0.3,
                          
                          1.0,  0.0,  0.0,  0.0,  0.0,
                          0.0,  0.9,  0.1,  0.0,  0.0,
                          0.0,  0.9,  0.1,  0.0,  0.0,
                          0.0,  0.85, 0.15, 0.0,  0.0,
                          0.0,  0.8,  0.2,  0.0,  0.0,
                          0.0,  0.75, 0.25, 0.0,  0.0,
                          0.0,  0.9,  0.1,  0.0,  0.0,
                          0.0,  0.0,  0.0,  0.65, 0.35,
                          
                          1.0,  0.0,  0.0,  0.0,  0.0,
                          0.0,  0.8,  0.2,  0.0,  0.0,
                          0.0,  0.8,  0.2,  0.0,  0.0,
                          0.0,  0.75, 0.25, 0.0,  0.0,
                          0.0,  0.7,  0.3,  0.0,  0.0,
                          0.0,  0.65, 0.35, 0.0,  0.0,
                          0.0,  0.8,  0.2,  0.0,  0.0,
                          0.0,  0.0,  0.0,  0.6,  0.4),
                          nrow=32,ncol=5,byrow=TRUE,dimnames=NULL)), 
  
  CosteSocial = node(Type="CHANCE", Name="CosteSocial", Values = c("Bajo", "Medio", "Alto"), Preds = c(), 
                     Pots = matrix( data = c(
                       0.90, 0.06, 0.04),
                       nrow=1,ncol=3,byrow=TRUE,dimnames=NULL)), 
  
  CosteEmocional = node(Type="CHANCE", Name="CosteEmocional", Values = c("Bajo", "Medio", "Alto"), Preds = c(), 
                        Pots = matrix( data = c(
                          0.03, 0.85, 0.12),
                          nrow=1,ncol=3,byrow=TRUE,dimnames=NULL)), 
  
  RiesgoIngreso = node(Type="CHANCE", Name="RiesgoIngreso", Values = c("Nulo", "Bajo", "Medio", "Alto"), Preds = c(), 
                       Pots = matrix( data = c(
                         0.01, 0.85, 0.10, 0.04),
                         nrow=1,ncol=4,byrow=TRUE,dimnames=NULL)), 
  
  DagnoTerapia = node(Type="CHANCE", Name="DagnoTerapia", Values = c("NULO", "BAJO", "MEDIO", "ALTO", "MUY_ALTO"), 
                      Preds = c("DagnoTerapia1", "DagnoTerapia2"), 
                      Pots = matrix( data = c(
                        0.97, 0.03, 0.0,  0.0,  0.0,
                        0.0,  0.97, 0.03, 0.0,  0.0,
                        0.0,  0.0,  0.97, 0.03, 0.0,
                        0.0,  0.0,  0.0,  0.97, 0.03,
                        0.0,  0.0,  0.0,  0.0,  1.0,
                        
                        0.0,  0.97, 0.03, 0.0,  0.0,
                        0.0,  0.95, 0.05, 0.0,  0.0,
                        0.0,  0.0,  0.97, 0.03, 0.0,
                        0.0,  0.0,  0.0,  0.98, 0.02,
                        0.0,  0.0,  0.0,  0.0,  1.0,
                        
                        0.0,  0.0,  0.98, 0.02, 0.0,
                        0.0,  0.0,  0.97, 0.03, 0.0,
                        0.0,  0.0,  0.95, 0.05, 0.0,
                        0.0,  0.0,  0.0,  0.97, 0.03,
                        0.0,  0.0,  0.0,  0.0,  1.0,
                        
                        0.0,  0.0,  0.0,  0.98, 0.02,
                        0.0,  0.0,  0.0,  0.97, 0.03,
                        0.0,  0.0,  0.0,  0.96, 0.04,
                        0.0,  0.0,  0.0,  0.95, 0.05,
                        0.0,  0.0,  0.0,  0.0,  1.0,
                        
                        0.0,  0.0,  0.0,  0.0,  1.0,
                        0.0,  0.0,  0.0,  0.0,  1.0,
                        0.0,  0.0,  0.0,  0.0,  1.0,
                        0.0,  0.0,  0.0,  0.0,  1.0,
                        0.0,  0.0,  0.0,  0.0,  1.0),
                        nrow=25,ncol=5,byrow=TRUE,dimnames=NULL)), 
  
  DagnoTerapia1 = node(Type="CHANCE", Name="DagnoTerapia1", Values = c("NULO", "BAJO", "MEDIO", "ALTO", "MUY_ALTO"), 
                       Preds = c("Terapia1", "concentrBilirrubina1", "edadHijo1"), 
                       Pots = matrix( data = c(
                         1.0,  0.0,  0.0,  0.0,  0.0,
                         1.0,  0.0,  0.0,  0.0,  0.0,
                         0.0,  0.0,  0.0,  0.0,  1.0,
                         0.0,  0.0,  0.0,  0.0,  1.0,
                         
                         0.0,  0.95, 0.05, 0.0,  0.0,
                         0.0,  1.0,  0.0,  0.0,  0.0,
                         0.96, 0.02, 0.01, 0.01, 0.0,
                         0.98, 0.02, 0.0,  0.0,  0.0,
                         
                         0.0,  0.0,  0.98, 0.02, 0.0,
                         0.0,  0.99, 0.01, 0.0,  0.0,
                         0.96, 0.04, 0.0,  0.0,  0.0,
                         0.98, 0.02, 0.0,  0.0,  0.0,
                         
                         0.0,  0.0,  1.0,  0.0,  0.0,
                         0.0,  0.0,  1.0,  0.0,  0.0,
                         0.98, 0.02, 0.0,  0.0,  0.0,
                         0.98, 0.02, 0.0,  0.0,  0.0),
                         nrow=16,ncol=5,byrow=TRUE,dimnames=NULL)), 
  
  DagnoTerapia2 = node(Type="CHANCE", Name="DagnoTerapia2", Values = c("NULO", "BAJO", "MEDIO", "ALTO", "MUY_ALTO"), 
                       Preds = c("Terapia2", "concentrBilirrubina2", "edadHijo2"), 
                       Pots = matrix( data = c(
                         1.0,  0.0,  0.0,  0.0,  0.0,
                         1.0,  0.0,  0.0,  0.0,  0.0,
                         1.0,  0.0,  0.0,  0.0,  0.0,
                         1.0,  0.0,  0.0,  0.0,  0.0,
                         
                         0.99, 0.01, 0.0,  0.0,  0.0,
                         0.99, 0.01, 0.0,  0.0,  0.0,
                         0.0,  0.0,  0.0,  0.99, 0.01,
                         0.0,  0.0,  0.02, 0.98, 0.0,
                         
                         0.05, 0.475,0.475,0.0,  0.0,
                         0.03, 0.485,0.485,0.0,  0.0,
                         0.95, 0.05, 0.0,  0.0,  0.0,
                         0.03, 0.97, 0.0,  0.0,  0.0,
                         
                         0.0,  0.3,  0.7,  0.0,  0.0,
                         0.0,  0.05, 0.95, 0.0,  0.0,
                         0.3,  0.7,  0.0,  0.0,  0.0,
                         0.35, 0.65, 0.0,  0.0,  0.0,
                         
                         0.0,  0.2,  0.7,  0.1,  0.0,
                         0.0,  0.2,  0.8,  0.0,  0.0,
                         0.2,  0.8,  0.0,  0.0,  0.0,
                         0.5,  0.5,  0.0,  0.0,  0.0,
                         
                         0.99, 0.01, 0.0,  0.0,  0.0,
                         0.99, 0.01, 0.0,  0.0,  0.0,
                         0.0,  0.0,  0.0,  0.99, 0.01,
                         0.0,  0.0,  0.02, 0.98, 0.0,
                         
                         0.0,  0.0,  0.0,  0.0,  1.0,
                         0.0,  0.0,  0.0,  0.0,  1.0,
                         0.99, 0.01, 0.0,  0.0,  0.0,
                         0.95, 0.05, 0.0,  0.0,  0.0,
                         
                         0.0,  0.0,  0.0,  0.0,  1.0,
                         0.0,  0.0,  0.0,  0.0,  1.0,
                         0.99, 0.01, 0.0,  0.0,  0.0,
                         0.95, 0.05, 0.0,  0.0,  0.0),
                         nrow=32,ncol=5,byrow=TRUE,dimnames=NULL)), 
  
  DagnoHiperBrb = node(Type="CHANCE", Name="DagnoHiperBrb", Values = c("NULO", "BAJO", "MEDIO", "ALTO", "MUY_ALTO", "EXTREMO"), 
                       Preds = c("DagnoHiperBrb1", "DagnoHiperBrb2"), 
                       Pots = matrix( data = c(
                         0.98,0.02,0.0, 0.0, 0.0, 0.0,
                         0.0, 0.97,0.03,0.0, 0.0, 0.0,
                         0.0, 0.0, 0.97,0.03,0.0, 0.0,
                         0.0, 0.0, 0.0, 0.97,0.03,0.0,
                         0.0, 0.0, 0.0, 0.0, 0.97,0.03,
                         0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                         
                         0.0, 0.97,0.03,0.0, 0.0, 0.0,
                         0.0, 0.95,0.05,0.0, 0.0, 0.0,
                         0.0, 0.0, 0.97,0.03,0.0, 0.0,
                         0.0, 0.0, 0.0, 0.98,0.02,0.0,
                         0.0, 0.0, 0.0, 0.0, 0.98,0.02,
                         0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                         
                         0.0, 0.0, 0.98,0.02,0.0, 0.0,
                         0.0, 0.0, 0.97,0.03,0.0, 0.0,
                         0.0, 0.0, 0.95,0.05,0.0, 0.0,
                         0.0, 0.0, 0.0, 0.97,0.03,0.0,
                         0.0, 0.0, 0.0, 0.0, 0.97,0.03,
                         0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                         
                         0.0, 0.0, 0.0, 0.98,0.02,0.0,
                         0.0, 0.0, 0.0, 0.97,0.03,0.0,
                         0.0, 0.0, 0.0, 0.96,0.04,0.0,
                         0.0, 0.0, 0.0, 0.95,0.05,0.0,
                         0.0, 0.0, 0.0, 0.0, 0.98,0.02,
                         0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                         
                         0.0, 0.0, 0.0, 0.0, 0.99,0.01,
                         0.0, 0.0, 0.0, 0.0, 0.98,0.02,
                         0.0, 0.0, 0.0, 0.0, 0.97,0.03,
                         0.0, 0.0, 0.0, 0.0, 0.96,0.04,
                         0.0, 0.0, 0.0, 0.0, 0.95,0.05,
                         0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                         
                         0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                         0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                         0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                         0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                         0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                         0.0, 0.0, 0.0, 0.0,0.0, 1.0),
                         nrow=36,ncol=6,byrow=TRUE,dimnames=NULL)), 
  
  DagnoHiperBrb1 = node(Type="CHANCE", Name="DagnoHiperBrb1", Values = c("NULO", "BAJO", "MEDIO", "ALTO", "MUY_ALTO", "EXTREMO"), 
                        Preds = c("concentrBilirrubina1", "PatologLeveGrave", "edadHijo1"), 
                        Pots = matrix( data = c(
                          0.98,0.02,0.0, 0.0, 0.0, 0.0,
                          0.99,0.01,0.0, 0.0, 0.0, 0.0,
                          0.98,0.02,0.0, 0.0, 0.0, 0.0,
                          0.99,0.01,0.0, 0.0, 0.0, 0.0,
                          0.98,0.02,0.0, 0.0, 0.0, 0.0,
                          0.99,0.01,0.0, 0.0, 0.0, 0.0,
                          
                          0.05,0.1, 0.2, 0.3, 0.35,0.0,
                          0.1, 0.2, 0.3, 0.2, 0.2, 0.0,
                          0.0, 0.05,0.1, 0.2, 0.45,0.2,
                          0.0, 0.1, 0.2, 0.3, 0.25,0.15,
                          0.0, 0.0, 0.05,0.2,0.25, 0.5,
                          0.0, 0.0, 0.1, 0.15,0.3, 0.45),
                          nrow=12,ncol=6,byrow=TRUE,dimnames=NULL)), 
  
  DagnoHiperBrb2 = node(Type="CHANCE", Name="DagnoHiperBrb2", Values = c("NULO", "BAJO", "MEDIO", "ALTO", "MUY_ALTO", "EXTREMO"), 
                        Preds = c("concentrBilirrubina2", "PatologLeveGrave", "edadHijo2"), 
                        Pots = matrix( data = c(
                          0.98,0.02,0.0, 0.0, 0.0, 0.0,
                          0.99,0.01,0.0, 0.0, 0.0, 0.0,
                          0.98,0.02,0.0, 0.0, 0.0, 0.0,
                          0.99,0.01,0.0, 0.0, 0.0, 0.0,
                          0.98,0.02,0.0, 0.0, 0.0, 0.0,
                          0.99,0.01,0.0, 0.0, 0.0, 0.0,
                          
                          0.1, 0.2, 0.3, 0.2, 0.2, 0.0,
                          0.15,0.25,0.35,0.15,0.1, 0.0,
                          0.0, 0.1, 0.2, 0.3, 0.25,0.15,
                          0.0, 0.15,0.25,0.3, 0.2, 0.1,
                          0.0, 0.0, 0.1, 0.15,0.3, 0.45,
                          0.0, 0.0, 0.15,0.2, 0.25,0.4),
                          nrow=12,ncol=6,byrow=TRUE,dimnames=NULL)), 
  
  Terapia1 = node( Type = "DECISION", Name = "Terapia1", Values = c("TerNo", "Fot6", "Fot12", "Fot24"), 
                   Preds = c("edadHijo1", "concentrBilirrubina1", "concentrHemoglobina1", 
                             "testCoombsIsoRHHijo", "testApgar5", "antecedentesParto", "partoInstrumental", 
                             "CosteSocial", "CosteEmocional", "RiesgoIngreso", "pesoNacer"), 
                   Pots = matrix( data = c(1.0), dimnames = list("phase","Terapia1"))), 
  
  Terapia2 = node( Type = "DECISION", Name = "Terapia2", Values = c("TerNo", "ObsAlt", "Obs6", "Fot6", "Fot12", "Fot24", "Fot6ExaFot6", "Fot6ExaFot12"), 
                   Preds = c("edadHijo2", "concentrBilirrubina2", "concentrHemoglobina2", "Terapia1"), 
                   Pots = matrix( data = c(2.0), dimnames = list("phase","Terapia2"))), 
  
  UTILITY = node(Type="UTILITY", Name="UTILITY", Values = c(0.0,1.0), Preds = c("CosteEconomico", "CosteSocial", "CosteEmocional", "RiesgoIngreso", "DagnoTerapia", "DagnoHiperBrb"), 
                 Pots = matrix( data = utility.function( c( 5, 3, 3, 4, 5, 6), c(
                   6.2729, 
                   0.1094, 
                   0.0310, 0.1818,  0.1094, 
                   0.5787, 
                   0.4213, 0.5580, 0.4420)),
                   nrow=5400, ncol=1, byrow=TRUE, dimnames=list( NULL, c("UTILITY"))))
  
)

#Evaluación modelo entero, toma algo de tiempo en ejecutarse ----

cat("Influence Diagram: ictneo2HGM\n", names(ictneo2HGM),"\n")
ictneo2HGM <- influence.diagram(ictneo2HGM)
arcrevalg.eval(ictneo2HGM,DUMP=TRUE)
dump.netG(ictneo2HGM, "ictneo2HGM")

##--------------------------------------------------------------

#Evaluamos parte del modelo, en concreto, se instancia nodos de probabilidad que
#son antencendetes a la decisión de la terapia 1 y volvemos a evaluar el diagrama

ictneo2HGM_inst <- evid.inst(ictneo2HGM, c("edadHijo1","menos_de_12horas"))
ictneo2HGM_inst <- evid.inst(ictneo2HGM_inst, c("concentrBilirrubina1","Normal"))
ictneo2HGM_inst <- evid.inst(ictneo2HGM_inst, c("concentrHemoglobina1","Baja"))

#dump.netR(ictneo2HGM_inst, "ictneo2HGM_inst") ## 

arcrevalg.eval(ictneo2HGM_inst, name="ictneo2HGM_inst", DUMP=TRUE)
dump.netG(ictneo2HGM_inst, "ictneo2HGM_inst") ## siempe despues de arcrevalg.eval

df3 <- read.csv(file="Terapia1.txt",sep=",",header=TRUE)

print(summary(df3))
## max by 3
CU <- dim(df3)[2]
CR <- dim(df3)[2]-1
RR <- length( unique(df3[,CR])) ## R
print(RR)
ri <- (1:dim(df3)[1]) %% RR == 1
ri <- (1:dim(df3)[1])[ri]
print(ri[1:15])
dfx <- data.frame()
for( i in ri ) {
  r. <- i:(i+(RR-1))
  df. <- df3[r.,]
  k <- which( df.$Utility==max( df.$Utility))[1]
  dfx <- rbind( dfx,as.data.frame( df.[k,]))
  cat(i,"<",k,">\n")
}
cat("\n")
print(summary(dfx))  
print(head(dfx,15)) ## estracction de la alternativa optima (max utilidad) de cada bloque
#write.csv(dfx, file = "OptimalKB.csv")
##
CU <- dim(dfx)[2]
CR <- dim(dfx)[2]-1
dfx0 <- dfx[dfx[,CR]==0,]
dfx1 <- dfx[dfx[,CR]==1,]
dfx2 <- dfx[dfx[,CR]==2,]
dfx0 <- dfx0[,-c(CR,CU)]
print(summary(dfx0))
print(cor(dfx0))
dfx1 <- dfx1[,-c(CR,CU)]
print(summary(dfx1))
print(cor(dfx1))
dfx2 <- dfx2[,-c(CR,CU)]
print(summary(dfx2))
print(cor(dfx2))