
setwd("C:/Users/Admin/Desktop/Projetos/micro_Pof_17_18")
getwd()

#pacotes#

library(dplyr)
library(ggplot2)
library(caTools) #divide a amostra#
library(lmtest)
library(reshape2)
library(magrittr)
library(normtest)
library(micEconCES)
library(miscTools)

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


RENDIMENTO_TRABALHO <- 
  read.fwf("RENDIMENTO_TRABALHO.txt" 
           , widths = c(2,4,1,9,2,1,2,2,1,1,7,1,1,1,1,1,1,7,7,7
                        ,7,2,2,3,1,12,10,10,10,10,1,1,14,14,10)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG"
                           , "COD_UPA", "NUM_DOM", "NUM_UC"
                           , "COD_INFORMANTE", "QUADRO", "SUB_QUADRO"
                           , "SEQ", "V9001", "V5302", "V53021", "V5303"
                           , "V5304", "V5305", "V5307", "V8500", "V531112"
                           , "V531122", "V531132", "V9010", "V9011"
                           , "V5314", "V5315", "DEFLATOR", "V8500_DEFLA"
                           , "V531112_DEFLA", "V531122_DEFLA"
                           , "V531132_DEFLA", "COD_IMPUT_VALOR"
                           , "FATOR_ANUALIZACAO", "PESO", "PESO_FINAL"
                           , "RENDA_TOTAL")
           , dec="."
  )


DESPESA_COLETIVA <- 
  read.fwf("DESPESA_COLETIVA.txt"
           , widths = c(2,4,1,9,2,1,2,2,7,2,4,10,2,2,1
                        ,10,1,12,10,10,1,1,2,14,14,10)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG"
                           , "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO"
                           , "SEQ", "V9001", "V9002", "V9005", "V8000"
                           , "V9010", "V9011", "V9012", "V1904"
                           , "V1905", "DEFLATOR", "V8000_DEFLA"
                           , "V1904_DEFLA", "COD_IMPUT_VALOR"
                           , "COD_IMPUT_QUANTIDADE", "FATOR_ANUALIZACAO"
                           , "PESO", "PESO_FINAL", "RENDA_TOTAL")
           , dec="."
  )

#importar o cadastro de produtos##################################################################

cadastro <- read.csv('Cadastro de Produtos.csv',
                     header = T,
                     sep = ";",
                     stringsAsFactors = T)


colnames(cadastro) <- c('QUADRO',
                        'codigo',
                        'produto')


# observacoes preliminares e classes ############################################################ 

sapply(c(DESPESA_COLETIVA,
         DESPESA_INDIVIDUAL,
         RENDIMENTO_TRABALHO), class)



# cestas #######################################################################################

unique(DESPESA_INDIVIDUAL$QUADRO)
unique(DESPESA_COLETIVA$QUADRO)

# o quadro aqui refere-se ao tipo de trabalho #
unique(RENDIMENTO_TRABALHO$QUADRO)


#reducao da amostra de despesa individual######################################################


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
   


# quais produtos sao demandados em codigo  ###################################################

# funcao agregando as despesas por produto ###################################################

Produto <- function(x,y){y %>% 
                       group_by(as.factor(x)) %>%
                       summarize(count=n())
                       } 


# criar um data frame com as compras #########################################################

Compras <- as.data.frame(Produto(DESPESA_INDIVIDUAL0$V9001,
                                 DESPESA_INDIVIDUAL0))

colnames(Compras) <- c('codigo',
                       'numero_despesa')



#nomes dos produtos############################################################################


Compras <- merge.data.frame(x = Compras,
                            y = cadastro,
                            by = 'codigo',
                            all.x = T,
                            all.y = F)                             



#Despesa total por aquisicao individual########################################################

DESPESA_INDIVIDUAL1 <- DESPESA_INDIVIDUAL0 %>%
                       group_by(V9001) %>% 
                       summarise(valor = sum(V8000),
                                 renda = median(RENDA_TOTAL))

colnames(DESPESA_INDIVIDUAL1) <- c('codigo',
                                   'valor',
                                   'renda')


Compras_Indiv <- merge.data.frame(x = Compras,
                            y = DESPESA_INDIVIDUAL1,
                            by = 'codigo',
                            all.x = T,
                            all.y = T)                             



#media das despesas por aquisicao##############################################################

Compras_Indiv <- Compras_Indiv %>%
           within.data.frame(desp_media <- valor / numero_despesa)


names(Compras_Indiv)

# grafico das compras e preco #

ggplot(data = Compras_Indiv) +
  geom_point(mapping = aes(x = numero_despesa,
                           y = desp_media,
                           color = renda)) +
       labs(x = "quantidade",
            y = "custo",
            color = "renda")




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
#### regressao elast #########################################################################

attach(cesta)

Reg.x1 <- lm(log(x1) ~ 
              log(x2) +
               log(y),
              data = cesta)


coeftest(Reg.x1)

plot(Reg.x1$fitted.values,
     Reg.x1$residuals,
     xlab = 'fit',
     ylab = 'residuos')


hist(Reg.x1$residuals,
     main = 'residuos')


Reg.x2 <- lm(log(x2) ~ 
               log(x1) +
               log(y),
             data = cesta)


coeftest(Reg.x2)

plot(Reg.x2$fitted.values,
     Reg.x2$residuals,
     xlab = 'fit',
     ylab = 'residuos')


hist(Reg.x2$residuals,
     main = 'residuos')



# testes de normalidade ######################################################################

jb.norm.test(Reg.x1$residuals,
             nrepl = 2000)

kurtosis.norm.test(Reg.x1$residuals,
                   nrepl = 2000)

skewness.norm.test(Reg.x1$residuals,
                   nrepl=2000)

ajb.norm.test(Reg.x1$residuals,
              nrepl=2000)


jb.norm.test(Reg.x2$residuals,
             nrepl = 2000)

kurtosis.norm.test(Reg.x2$residuals,
                   nrepl = 2000)

skewness.norm.test(Reg.x2$residuals,
                   nrepl=2000)

ajb.norm.test(Reg1.Res,
              nrepl=2000)




     

##### CES B#############################################################################


c_elastic <- function(p, beta, alfa) {exp(beta*log(p)+alfa)}

p <- Compras_Indiv$desp_media

CES <-    c_elastic(p,
                    0.165004,
                    7.624844)




demand <- as.data.frame(cbind(p,
                         CES))


ggplot(data = demand) +
  geom_line(mapping = aes(x = p,
                          y = CES)) +
  labs(x = 'preço',
       y = 'demanda')




########################################################################################

# DESPESA COLETIVA #

## reducao da amostra ##

DESPESA_COLETIVA0 <- DESPESA_COLETIVA %>%
  subset(V8000 != 9999999.99)


amostra1 <- 
  sample.split(DESPESA_COLETIVA0,
               SplitRatio = .30)


DESPESA_COLETIVA0 <- DESPESA_COLETIVA0 %>%
  subset(amostra1 == T)

# grafico da despesa coletiva e renda #

ggplot(data = DESPESA_COLETIVA0) +
  geom_point(mapping = aes(x = RENDA_TOTAL,
                           y = V8000,
                           color = QUADRO)) +
  scale_colour_gradientn(colours=rainbow(4)) +
  labs(x = "RENDA",
       y = "despesa total",
       color = "QUADRO")


# quais produtos sao demandados em codigo #

Compras_COLET <- as.data.frame(Produto(DESPESA_COLETIVA0$V9001,
                                 DESPESA_COLETIVA0))


colnames(Compras_COLET) <- c('codigo',
                             'numero_despesa')


Compras_COLET <- merge.data.frame(x = Compras_COLET,
                                  y = cadastro,
                                  by = 'codigo',
                                  all.x = T,
                                  all.y = F)                             



#Despesa total COLETIVA por aquisicao#

  DESPESA_COLETIVA1 <- DESPESA_COLETIVA0 %>%
  group_by(V9001) %>% 
  summarise(valor = sum(V8000))

colnames(DESPESA_COLETIVA1) <- c('codigo',
                                   'valor')



Compras_COLET2 <- merge.data.frame(x = Compras_COLET,
                                   y = DESPESA_COLETIVA1,
                                   by = 'codigo',
                                   all.x = T,
                                   all.y = T)       



##media das despesas por aquisicao coletiva#

Compras_COLET2 <- Compras_COLET2 %>%
                   within.data.frame(desp_media <- valor / numero_despesa)


# grafico #

ggplot(data = Compras_COLET2) +
  geom_point(mapping = aes(x = numero_despesa,
                           y = desp_media,
                           color = quadro)) +
              labs(x = "quantidade",
                   y = "custo",
                   color = "quadro")


# regressao para a demanda #

attach(Compras_COLET2)

Reg2 <- lm(log(numero_despesa) ~
           log(desp_media),
           data = Compras_COLET2)

coeftest(Reg2)

Reg2.Fit <- Reg2$fitted.values
Reg2.Res <- Reg2$residuals

Reg2.F_R <- as.data.frame(cbind(Reg2.Fit,
                                Reg2.Res,
                                Compras_COLET$produto,
                                Compras_COLET$quadro))


colnames(Reg2.F_R) <- c('fit',
                        'residuo',
                        'produto',
                        'quadro')



ggplot(data = Reg2.F_R) +
  geom_point(mapping = aes(x = fit,
                           y = residuo,
                           color = quadro)) +
  scale_colour_gradientn(colours=rainbow(4)) +
  labs(x = 'fit',
       y = 'residuo',
       color = 'quadro')


### seperar a amostra coletiva apenas pelo quadro 8 ##

Compras_COLET2_8 <- Compras_COLET2 %>%
                    subset(quadro == 8)


### regressao sem intercepto ###

Reg2_8 <- lm(log(numero_despesa) ~
             log(desp_media)
             - 1,
             data = Compras_COLET2_8)


coeftest(Reg2_8)

summary(Reg2_8)

## regressao do quadro 8 ##

coeftest(Reg2)

Reg2_8.Fit <- Reg2_8$fitted.values
Reg2_8.Res <- Reg2_8$residuals

Reg2_8.F_R <- as.data.frame(cbind(Reg2_8.Fit,
                                Reg2_8.Res,
                                Compras_COLET2_8$produto,
                                Compras_COLET2_8$quadro))


colnames(Reg2_8.F_R) <- c('fit',
                        'residuo',
                        'produto',
                        'quadro')



# grafico #

ggplot(data = Reg2_8.F_R ) +
  geom_point(mapping = aes(x = fit,
                           y = residuo,
                           color = produto)) +
  labs(x = "fit",
       y = "residuo",
       color = "produto")


## funcao demanda CES ##


p_C <- Compras_COLET2$desp_media

CES_Col <-    c_elastic(p_C,
                        0.4419,
                        0)


demand_Col <- as.data.frame(cbind(p_C,
                              CES_Col))


ggplot(data = demand_Col) +
   geom_line(mapping = aes(x = p_C,
                           y = CES_Col)) 


