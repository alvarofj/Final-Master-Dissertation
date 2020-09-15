#########################################################################################################
# MASTER THESIS - ÁLVARO FERNÁNDEZ JUNQUERA (MSC IN ECONOMICS)
#########################################################################################################

setwd("C:/Users/alvar/Documents/. UNIVERSIDAD/5º Master Economics/TFM/directorioR")

#********************************************************************************************************
# MATRICES FUNDAMENTALES
#********************************************************************************************************

# Matriz Z
matrizZ = read.table(file = "matrizZ.csv",
                     sep = ";", # separador del CSV
                     dec = ",", # simbolo que indica decimales
                     header = TRUE, # encabezado
                     stringsAsFactors = FALSE,
                     fileEncoding="UTF-8-BOM")

cols.num <- c(1:34)
matrizZ[cols.num] <- sapply(matrizZ[cols.num],as.numeric)
sapply(matrizZ, class)

matrizZ=as.matrix(matrizZ)


# Matriz W
matrizW = read.table(file = "matrizW.csv",
                     sep = ";", # separador del CSV
                     dec = ",", # simbolo que indica decimales
                     header = TRUE, # encabezado
                     stringsAsFactors = FALSE,
                     fileEncoding="UTF-8-BOM")

cols.num <- c(1:34) # 34 son el total de columnas
matrizW[cols.num] <- sapply(matrizW[cols.num],as.numeric)
sapply(matrizW, class) # para comprobar que estÃ¡ bien hecho

matrizW <- as.matrix(matrizW)

# Matriz X
matrizX = read.table(file = "vectorX.csv",
                     sep = ";", # separador del CSV
                     dec = ",", # simbolo que indica decimales
                     header = TRUE, # encabezado
                     stringsAsFactors = FALSE,
                     fileEncoding="UTF-8-BOM")

matrizX <- as.matrix(matrizX)
matrizX <- as.numeric(matrizX)

matrizXparadiv = 1/matrizX
matrizXparadiv <- as.matrix(matrizXparadiv)


# Matriz V
matrizV <- matrix(nrow=4, ncol=34)

for(i in 1:4)
{
  for(j in 1:34)
    matrizV[i,j] = matrizW[i,j] * matrizXparadiv[j,1]
}



#********************************************************************************************************
#### 1ª ecuación: X = SF*                              
#### Matriz S = ((I-A)-PV)^-1
#********************************************************************************************************

#-----------------------------------------------------------------------------
# matriz PV
#-----------------------------------------------------------------------------

### matriz P ----> p(i,l)=c(i,l)/y(l)

matrizC = read.table(file = "matrizC.csv",
                     sep = ";", # separador del CSV
                     dec = ",", # simbolo que indica decimales
                     header = TRUE, # encabezado
                     stringsAsFactors = FALSE)

cols.num <- c(1:4) # 4 son el total de columnas
matrizC[cols.num] <- sapply(matrizC[cols.num],as.numeric)
sapply(matrizC, class) # para comprobar que estÃ¡ bien hecho

matrizC <- as.matrix(matrizC)



matrizY <- read.table(file = "matrizY.csv",
                      sep = ";", # separador del CSV
                      dec = ",", # simbolo que indica decimales
                      header = TRUE, # encabezado
                      stringsAsFactors = FALSE,
                      fileEncoding="UTF-8-BOM")

matrizY$Total <- as.numeric(as.character(matrizY$Total))
matrizY <- as.matrix(matrizY)
matrizYparadiv <- 1/matrizY


matrizP <- matrix(nrow = 34, ncol = 4)

for(i in 1:34)
{
  for(j in 1:4)
    matrizP[i,j] = matrizC[i,j] * matrizYparadiv[j,1]
}




matrizPV <- matrizP %*% matrizV


#------------------------------------------------------------------------------
# matriz I-A: A a i34
#------------------------------------------------------------------------------

matrizA <- matrix(nrow = 34, ncol = 34)

for(i in 1:34)
{
  for(j in 1:34)
    matrizA[i,j] = matrizZ[i,j] * matrizXparadiv[j,1]
}


I34 <- diag(34)
matrizI_A <- I34 - matrizA


#-----------------------------------------------------------------------------
#(I-A) - PV
#-----------------------------------------------------------------------------

matrizI_A_PV <- matrizI_A - matrizPV


#-----------------------------------------------------------------------------
# Finalmente: S = ((I-A)-PV)^1
#-----------------------------------------------------------------------------
library(matlib)

InvI_A_PV <- Inverse(matrizI_A_PV)





#********************************************************************************************************
# ESTÍMULO: INCREMENTO DE OUTPUT (Parte x~)
#********************************************************************************************************
fasterisco <- read.table(file = "fasterisco.csv",
                         sep = ";", # separador del CSV
                         header = TRUE, # encabezado
                         stringsAsFactors = FALSE,
                         fileEncoding="UTF-8-BOM")

cols.num <- c(1:34)
fasterisco[cols.num] <- sapply(fasterisco[cols.num],as.numeric)
sapply(fasterisco, class)

fasterisco=as.matrix(fasterisco)

fasteriscoES <- fasterisco + I34

estimuladooutput <- InvI_A_PV %*% fasteriscoES

estimuladooutput <- t(estimuladooutput)

#library(openxlsx)
#write.xlsx(estimuladooutput, 'estimuladooutput.xlsx')


#********************************************************************************************************
# ESTÍMULO: INCREMENTO DE OUTPUT ////// 2.- (Parte y~)
#********************************************************************************************************

#------------------------------------------------------------------------------
#### 2ª ecuación: Yes = UF*                                                 
#### Matriz U = -(-V)S = VS                                                 
#------------------------------------------------------------------------------

matrizU <- matrizV %*% InvI_A_PV

estimuladooutputY <- matrix(data=0, nrow=34, ncol=4)

####### (Este bucle mejor leerlo de dentro hacia afuera)

for (k in 1:34) # desfijo la primera columna del segundo incriplicador
{
  for(i in 1:4) # para la incriplicación matricial cambiando la fila del primer incriplicador fijando la primera columna del segundo incriplicador
  {
    for(j in 1:34)  # para la incriplicación matricial fijando la fila del primer incriplicador fijando la primera columna del segundo incriplicador
      estimuladooutputY[k,i] <- estimuladooutputY[k,i] + matrizU[i,j]*fasteriscoES[j,k]
  }
}




#********************************************************************************************************
# ESTÍMULO: INCREMENTO DE OUTPUT ////// 3.- Q~ = Unir (Parte x~) con (Parte y~)
#********************************************************************************************************

library(tidyverse)
estimuladooutputX <- as.data.frame(estimuladooutput)
estimuladooutputX <- estimuladooutputX %>% mutate(n=1:34)

estimuladooutputY <- as.data.frame(estimuladooutputY)
estimuladooutputY <- estimuladooutputY %>% mutate(n=1:34)

matrizQvirgulilla <- merge(estimuladooutputX, estimuladooutputY, by="n")
matrizQvirgulilla$n = NULL



#********************************************************************************************************
# ESTÍMULO: INCREMENTO DE OUTPUT ////// 4.- Cálculo del incremento absoluto
#********************************************************************************************************
vectorxi38 <- read.table(file = "vectorxi38.csv",
                         sep = ";", # separador del CSV
                         header = FALSE, # encabezado
                         stringsAsFactors = FALSE)
                         
matrizQvirgulilla <- t(matrizQvirgulilla)

matrizQvirgulilla <- as.matrix(matrizQvirgulilla)
vectorxi38 <- as.matrix(vectorxi38)


#DIFERENCIAS con bucle
incrOUTPUT <- matrix(nrow=38, ncol=34)

for (j in 1:34)
{
  for(i in 1:38)
  incrOUTPUT[i,j] = matrizQvirgulilla[i,j] - vectorxi38[i,1]
}


#SUMATORIO con bucle
incrOUTPUTtotal <- matrix(data=0, nrow=1, ncol=34)

for (j in 1:34)   
{
  for (i in 1:38)
  incrOUTPUTtotal[1,j] = incrOUTPUTtotal[1,j] + incrOUTPUT[i,j]
}

#Presentación
incrOUTPUTtotal = t(incrOUTPUTtotal)
incrOUTPUTtotalx = incrOUTPUTtotal
incrOUTPUTtotal <- as.data.frame(incrOUTPUTtotal)
incrOUTPUTtotal <- incrOUTPUTtotal %>% mutate(t=1:34)

titulosMS <- read.table(file = "titulosMS.csv",
                         sep = ";", # separador del CSV
                         header = TRUE, # encabezado
                         stringsAsFactors = FALSE,
                         fileEncoding="UTF-8-BOM")

incrOUTPUTtotal <- merge(incrOUTPUTtotal, titulosMS, by="t")

incrOUTPUTtotal <- incrOUTPUTtotal %>% arrange (desc(V1)) # de forma descendente

library(openxlsx)
write.xlsx(incrOUTPUTtotal, 'incrOUTPUTtotal.xlsx')



#********************************************************************************************************
# ESTÍMULO: INCREMENTO OUTPUT ////// 5.- Cálculo del incremento relativo
#********************************************************************************************************
matrizX <- as.matrix(matrizX)

incrOUTPUTtotalrx <- matrix(nrow=34, ncol=1)

incrOUTPUTtotalrx <- (incrOUTPUTtotalx / sum(vectorxi38)) * 100

#Presentación
incrOUTPUTtotalr = incrOUTPUTtotalrx
incrOUTPUTtotalr <- as.data.frame(incrOUTPUTtotalr)
incrOUTPUTtotalr <- incrOUTPUTtotalr %>% mutate(t=1:34)

incrOUTPUTtotalr <- merge(incrOUTPUTtotalr, titulosMS, by="t")

incrOUTPUTtotalr <- incrOUTPUTtotalr %>% arrange (desc(V1)) # de forma descendente

library(openxlsx)
write.xlsx(incrOUTPUTtotalr, 'incrOUTPUTtotalr.xlsx')

















#********************************************************************************************************
# 4.1.3. ESTÍMULO: INCREMENTO DESAGREGADO INGRESOS ////// 1.- Cálculo del incremento absoluto
#********************************************************************************************************
incrINCOMES <- incrOUTPUT[35:38,]
incrINCOMES <- t(incrINCOMES)

incrINCOMES <- as.data.frame(incrINCOMES)
library(tidyverse)
incrINCOMES <- incrINCOMES %>% mutate(t=1:34)

### Ranking en favor de h1
h1 <- c("V1", "t")
incrINCOMES1 <- incrINCOMES[h1]
incrINCOMES1 <- incrINCOMES1 %>% arrange (desc(V1))
incrINCOMES1 <- subset(incrINCOMES1, select=c(t,V1))


### Ranking en favor de h2
h2 <- c("V2", "t")
incrINCOMES2 <- incrINCOMES[h2]
incrINCOMES2 <- incrINCOMES2 %>% arrange (desc(V2))
incrINCOMES2 <- subset(incrINCOMES2, select=c(t,V2))


### Ranking en favor de h3
h3 <- c("V3", "t")
incrINCOMES3 <- incrINCOMES[h3]
incrINCOMES3 <- incrINCOMES3 %>% arrange (desc(V3))
incrINCOMES3 <- subset(incrINCOMES3, select=c(t,V3))


### Ranking en favor de h4
h4 <- c("V4", "t")
incrINCOMES4 <- incrINCOMES[h4]
incrINCOMES4 <- incrINCOMES4 %>% arrange (desc(V4))
incrINCOMES4 <- subset(incrINCOMES4, select=c(t,V4))


### Juntar los 4 rankings en una sola matriz
library(dplyr)
incrINCOMESord <- bind_cols(incrINCOMES1,incrINCOMES2,incrINCOMES3,incrINCOMES4)

library(openxlsx)
write.xlsx(incrINCOMESord, 'incrINCOMESord.xlsx')



#********************************************************************************************************
# ESTÍMULO DESAGREGADO: INCREMENTO DESAGREGADO INGRESOS ////// 2.- Cálculo del incremento relativo
#********************************************************************************************************

matrizYt <- t(matrizY)

incrINCOMESr = incrINCOMES
incrINCOMESr$t = NULL

incrINCOMESrh <- matrix(nrow=34, ncol=4)

for(i in 1:34)
{
  for(j in 1:4)
    incrINCOMESrh[i,j] = (incrINCOMESr[i,j] / matrizYt[1,j]) * 100
}


#Presentación
incrINCOMESrh <- as.data.frame(incrINCOMESrh)
library(tidyverse)
incrINCOMESrh <- incrINCOMESrh %>% mutate(t=1:34)

### Ranking en favor de h1
#h1 <- c("V1", "t")
incrINCOMES1rh <- incrINCOMESrh[h1]
incrINCOMES1rh <- incrINCOMES1rh %>% arrange (desc(V1))
incrINCOMES1rh <- subset(incrINCOMES1rh, select=c(t,V1))


### Ranking en favor de h2
#h2 <- c("V2", "t")
incrINCOMES2rh <- incrINCOMESrh[h2]
incrINCOMES2rh <- incrINCOMES2rh %>% arrange (desc(V2))
incrINCOMES2rh <- subset(incrINCOMES2rh, select=c(t,V2))


### Ranking en favor de h3
#h3 <- c("V3", "t")
incrINCOMES3rh <- incrINCOMESrh[h3]
incrINCOMES3rh <- incrINCOMES3rh %>% arrange (desc(V3))
incrINCOMES3rh <- subset(incrINCOMES3rh, select=c(t,V3))


### Ranking en favor de h4
#h4 <- c("V4", "t")
incrINCOMES4rh <- incrINCOMESrh[h4]
incrINCOMES4rh <- incrINCOMES4rh %>% arrange (desc(V4))
incrINCOMES4rh <- subset(incrINCOMES4rh, select=c(t,V4))


### Juntar los 4 rankings en una sola matriz
library(dplyr)
incrINCOMESrhord <- bind_cols(incrINCOMES1rh,incrINCOMES2rh,incrINCOMES3rh,incrINCOMES4rh)

library(openxlsx)
write.xlsx(incrINCOMESrhord, 'incrINCOMESrhord.xlsx')









#********************************************************************************************************
# 4.1.2. INCREMENTS OF AGGREGATED INCOME & AGGREGATED EMPLOYMENT
#********************************************************************************************************

#--------------------------------------------------------------------------------------------
# Aggregated income (agregación de números absolutos)
#--------------------------------------------------------------------------------------------

aggreINCOMES <- incrINCOMES
aggreINCOMES$t = NULL
aggreINCOMES <- t(aggreINCOMES)

aggreINCOMESx <- matrix(data=0, nrow=1, ncol=34)

for (j in 1:34)   
{
  for (i in 1:4)
    aggreINCOMESx[1,j] = aggreINCOMESx[1,j] + aggreINCOMES[i,j]
}

aggreINCOMESx <- t(aggreINCOMESx)

aggreINCOMESx <- as.data.frame(aggreINCOMESx)


# Presentación
library(tidyverse)
aggreINCOMESx <- aggreINCOMESx %>% mutate(t=1:34)
aggreINCOMESx <- merge(aggreINCOMESx, titulosMS, by="t")

aggreINCOMESx <- aggreINCOMESx %>% arrange (desc(V1)) # de forma descendente

library(openxlsx)
write.xlsx(aggreINCOMESx, 'aggreINCOMESx.xlsx')



#--------------------------------------------------------------------------------------------
# Aggregated income (agregación de números relativos)
#--------------------------------------------------------------------------------------------

incrINCOMESt <- incrINCOMES
incrINCOMESt$t <- NULL
incrINCOMESt <- t(incrINCOMESt)

#Sumatorio
incragINCOMESr <- matrix(data=0, nrow=1, ncol=34)

for (j in 1:34)   
{
  for (i in 1:4)
    incragINCOMESr[1,j] = incragINCOMESr[1,j] + incrINCOMESt[i,j]
}

AgInr <- (incragINCOMESr / sum(matrizY)) * 100

AgInr <- t(AgInr)

# Presentación
AgInr <- as.data.frame(AgInr)

library(tidyverse)
AgInr <- AgInr %>% mutate(t=1:34)
AgInr <- merge(AgInr, titulosMS, by="t")

AgInr <- AgInr %>% arrange (desc(V1)) # de forma descendente

library(openxlsx)
write.xlsx(AgInr, 'aggregatedINCOMESrelative.xlsx')



#--------------------------------------------------------------------------------------------
# Aggregated employment (agregación de números relativos)
#--------------------------------------------------------------------------------------------
matrizL <- matrizY[1:3,]
matrizL <- as.matrix(matrizL)

incrEMPLt <- incrINCOMES[,1:3]
incrEMPLt <- t(incrEMPLt)

#Sumatorio
incragEMPLr <- matrix(data=0, nrow=1, ncol=34)

for (j in 1:34)   
{
  for (i in 1:3)
    incragEMPLr[1,j] = incragEMPLr[1,j] + incrEMPLt[i,j]
}

AgEmr <- (incragEMPLr / sum(matrizL)) * 100

AgEmr <- t(AgEmr)

# Presentación
AgEmr <- as.data.frame(AgEmr)

library(tidyverse)
AgEmr <- AgEmr %>% mutate(t=1:34)
AgEmr <- merge(AgEmr, titulosMS, by="t")

AgEmr <- AgEmr %>% arrange (desc(V1)) # de forma descendente

library(openxlsx)
write.xlsx(AgEmr, 'aggregatedEMPLOYMENTrelative.xlsx')





















#********************************************************************************************************
# 4.1.3. ESTÍMULO: INCREMENTO DESAGREGADO EMPLEO ////// 1.- Cálculo del incremento absoluto
#********************************************************************************************************

matrizL <- matrizV[1:3,]

incrEMPLEO <- matrix(nrow=3, ncol=34)

for(i in 1:3)
{
  for(j in 1:34)
    incrEMPLEO[i,j] = matrizL[i,j] * incrOUTPUTtotalx[j,1]
}

#### Presentación
incrEMPLEOp = incrEMPLEO
incrEMPLEOp <- t(incrEMPLEOp)

incrEMPLEOp <- as.data.frame(incrEMPLEOp)
library(tidyverse)
incrEMPLEOp <- incrEMPLEOp %>% mutate(t=1:34)

### Ranking en favor de h1
#h1 <- c("V1", "t")
incrEMPLEOp1 <- incrEMPLEOp[h1]
incrEMPLEOp1 <- incrEMPLEOp1 %>% arrange (desc(V1))
incrEMPLEOp1 <- subset(incrEMPLEOp1, select=c(t,V1))


### Ranking en favor de h2
#h2 <- c("V2", "t")
incrEMPLEOp2 <- incrEMPLEOp[h2]
incrEMPLEOp2 <- incrEMPLEOp2 %>% arrange (desc(V2))
incrEMPLEOp2 <- subset(incrEMPLEOp2, select=c(t,V2))


### Ranking en favor de h3
#h3 <- c("V3", "t")
incrEMPLEOp3 <- incrEMPLEOp[h3]
incrEMPLEOp3 <- incrEMPLEOp3 %>% arrange (desc(V3))
incrEMPLEOp3 <- subset(incrEMPLEOp3, select=c(t,V3))


### Juntar los 3 rankings en una sola matriz
library(dplyr)
incrEMPLEOpord <- bind_cols(incrEMPLEOp1,incrEMPLEOp2,incrEMPLEOp3)

library(openxlsx)
write.xlsx(incrEMPLEOpord, 'incrEMPLEOpord.xlsx')


#********************************************************************************************************
# 4.1.3. ESTÍMULO: INCREMENTO DESAGREGADO EMPLEO ////// 2.- Cálculo del incremento relativo
#********************************************************************************************************
matrizYw <- matrizY[1:3,]
matrizYw <- as.matrix(matrizYw)

incrEMPLEOr <- matrix(nrow=3, ncol=34)

for(i in 1:3)
{
  for(j in 1:34)
    incrEMPLEOr[i,j] = (incrEMPLEO[i,j] / matrizYw[i,1]) * 100
}


#### Presentación
incrEMPLEOrp = incrEMPLEO
incrEMPLEOrp <- t(incrEMPLEOr)

incrEMPLEOrp <- as.data.frame(incrINCOMES)
library(tidyverse)
incrEMPLEOrp <- incrEMPLEOrp %>% mutate(t=1:34)

### Ranking en favor de h1
#h1 <- c("V1", "t")
incrEMPLEOrp1 <- incrEMPLEOrp[h1]
incrEMPLEOrp1 <- incrEMPLEOrp1 %>% arrange (desc(V1))
incrEMPLEOrp1 <- subset(incrEMPLEOrp1, select=c(t,V1))


### Ranking en favor de h2
#h2 <- c("V2", "t")
incrEMPLEOrp2 <- incrEMPLEOrp[h2]
incrEMPLEOrp2 <- incrEMPLEOrp2 %>% arrange (desc(V2))
incrEMPLEOrp2 <- subset(incrEMPLEOrp2, select=c(t,V2))


### Ranking en favor de h3
#h3 <- c("V3", "t")
incrEMPLEOrp3 <- incrEMPLEOrp[h3]
incrEMPLEOrp3 <- incrEMPLEOrp3 %>% arrange (desc(V3))
incrEMPLEOrp3 <- subset(incrEMPLEOrp3, select=c(t,V3))


### Juntar los 3 rankings en una sola matriz
library(dplyr)
incrEMPLEOrpord <- bind_cols(incrEMPLEOrp1,incrEMPLEOrp2,incrEMPLEOrp3)

library(openxlsx)
write.xlsx(incrEMPLEOrpord, 'incrEMPLEOrpord.xlsx')