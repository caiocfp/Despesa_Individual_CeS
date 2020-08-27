
setwd("C:/Users/Admin/Desktop/Projetos/micro_Pof_17_18")
getwd()

#pacotes#
pacotes <- c("dplyr","ggplot2","caTools","reshape2","magrittr","normtest","micEconCES","miscTools")


#importar os dados###############################################################################

DESPESA_INDIVIDUAL <- 
  read.fwf("DESPESA_INDIVIDUAL.txt" 
           , widths = c(2,4,1,9,2,1,2,2,2,7,2,10,2
                        ,2,1,1,1,12,10,1,2,14,14,10)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG"
                           , "COD_UPA", "NUM_DOM", "NUM_UC"
                           , "COD_INFORMANTE", "QUADRO", "SEQ", "V9001"
                           , "V9002", "V8000", "V9010", "V9011", "V9012"
                           , "V4104", "V4105", "DEFLATOR", "V8000_DEFLA"
                           , "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO"
                           , "PESO", "PESO_FINAL", "RENDA_TOTAL")
           , dec="."
  )   



# retirar os valores de despesa iguais à 9999999.99, porque, indica inexistencia #############


DESPESA_INDIVIDUAL0 <- DESPESA_INDIVIDUAL %>%
  subset(V8000 != 9999999.99)

# diminuir a amostra da despesa individual #

amostra0 <- 
  sample.split(DESPESA_INDIVIDUAL0,
               SplitRatio = .30)


DESPESA_INDIVIDUAL0 <- DESPESA_INDIVIDUAL0 %>%
   subset(amostra0 == T)



# estatistica descritiva e grafico da despesa individual#######################################

summary(DESPESA_INDIVIDUAL0$RENDA_TOTAL)

summary(DESPESA_INDIVIDUAL0$V8000)


ggplot(data = DESPESA_INDIVIDUAL0) +
  geom_point(mapping = aes(x = RENDA_TOTAL,
                           y = V8000,
                           color = QUADRO)) +
      scale_colour_gradientn(colours=rainbow(4)) +
           labs(x = "renda total",
                y = "despesa total",
                color = "QUADRO")
   






# funcao demanda ############################################################################



#########cesta de 'vicios'###################################################################

DESPESA_INDIVIDUAL2a <- DESPESA_INDIVIDUAL0 %>% 
                        subset(QUADRO == 21)


DESPESA_INDIVIDUAL2a1 <- DESPESA_INDIVIDUAL2a %>%
                         group_by(RENDA_TOTAL) %>%
                         summarise(valor = sum(V8000))



######## cesta de "restante das despesas individuais" ########################################

DESPESA_INDIVIDUAL2 <- DESPESA_INDIVIDUAL0 %>% 
                        subset(QUADRO != 21)


DESPESA_INDIVIDUAl2b <- DESPESA_INDIVIDUAL2 %>%
                       group_by(RENDA_TOTAL) %>%
                       summarise(valor = sum(V8000))
                                 



######## unindo ambos as cesta e renda ######################################################
                       
cesta <- merge.data.frame(x = DESPESA_INDIVIDUAL2a1,
                          y = DESPESA_INDIVIDUAl2b,
                          by = 'RENDA_TOTAL',
                          all.x = T,
                          all.y = F)    

colnames(cesta) <- c('y',
                     'x1',
                      'x2')
cesta <- cesta %>% 
        subset(y > 0 &
               x1 > 0  &
                x2 > 0 )


#criar a varivel que indica o restante dos bens, toda renda deve ser consumida############## 

cesta <- cesta %>% within.data.frame(x3 <- y - (x1 + x2))
 

x3a <- ifelse(cesta$x3 < 0,
              0,
              cesta$x3)

cesta <- cbind(cesta,
               x3a)
cesta <- cesta[,-4]

colnames(cesta) <- c('y',
                     'x1',
                     'x2',
                     'x3')


##### retirar os zeros #####################################################################


cesta <- cesta %>% 
         subset(x3 != 0)



plot(cesta$x1,
     cesta$x2,
     xlab = 'x1',
     ylab = 'x2')


# com 3 bens Levenberg-Marquardt ########################################################


cesLm2 <- cesEst(yName = 'y',
                 xNames = c('x1','x2','x3'),
                 method = "LM",
                 data = cesta,
                 vrs = T)

summary(cesLm2)


####os resultados apontam que a funcao de demanda eh leontief############################

#ces para tres cestas# 

CeS <- function(g,delta_1,delta,rho_1,rho,nu,x1,x2,x3)
  {g*(
        delta * (delta_1*x1^-rho_1 + (1-delta_1)*x2^-rho_1)^(rho/rho_1)
                  
                  + (1-delta)*x3^-rho 
                                       )^(nu/rho)
                                          }

Ces_Ind <- CeS(3,0.5,0.667,-1,-1,1,cesta$x1,cesta$x2,cesta$x3)


plot(Ces_Ind)

