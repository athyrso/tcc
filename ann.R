
# pacotes utilizadados -----------------------------------------------------

{
   pacotes <- c("plotly", # graficos
                "tidyverse", # ferramentas
                "kableExtra", # tabelas
                "gridExtra", #graficos
                "readr", # importa arquivo
                "readxl", # importa dados
                "rnn",# rede neural
                "ggplot2", # grafico
                "grid", #grafico
                "nnfor",# linscale
                "beepr") # executa som
   
   if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
      instalador <- pacotes[!pacotes %in% installed.packages()]
      for(i in 1:length(instalador)) {
         install.packages(instalador, dependencies = T)
         break()}
      sapply(pacotes, require, character = T) 
   } else {
      sapply(pacotes, require, character = T) 
   }
   
}


# Local de trabalho -------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Carregando os dados -----------------------------------------------------


#write.csv2(df_012, "d:/preliminar/df_012_rnn.csv")

#df_label_teste <- read_delim("D:/preliminar/df_label_teste.csv", 
#                            delim = ";",
#                           escape_double = FALSE,
#                          locale = locale(decimal_mark = ",", 
#                         grouping_mark = "."), na = "null", 
#                        trim_ws = TRUE)

#data <- read_delim("D:/preliminar/df_012.csv", 
#                   delim = ";", 
#                   escape_double = FALSE,
#                   locale = locale(decimal_mark = ",",
#                                  grouping_mark = "."),
#                   trim_ws = TRUE)
#data <- read_excel("D:/preliminar/df_012_rnn.xlsx")
#
#
#data <- read_excel("D:/preliminar/rnn/df_NN.xlsx", 
#                   sheet = "df_NN")

# load("dados01.rdata")


pca_rnn <- read_excel("pca_rnn.xlsx", sheet = "fatores_pca")
head (pca_rnn)

# Definindo e visualizando o intervalo ------------------------------------------------

data_used <- pca_rnn[21:3020,2:9]
head (data_used)
x=data_used[,2:8]
y=data_used[,1]

#?scale
#Yscaled = (y - min(y)) / (max(y) - min(y))
#Xscaled = (x - min(x)) / (max(x) - min(x))

#y <- Yscaled
#x <- Xscaled

x <- as.matrix(x)
y <- as.matrix(y)

X <- matrix(x, nrow = 30)
Y <- matrix(y, nrow = 30)

#train test split
train=1:80
test=81:100


pca_rnn %>% 
   ggplot(aes(x = 1:nrow(pca_rnn), y = p_vapor)) +
   geom_point(color = "dodgerblue4",
              size = 2) +
   labs(x = "Período de coleta",
        y = "Pressão de vapor (Bar)") +
   geom_smooth()+
   theme_bw()-> p

ggplotly(p)


 # modelo de rede utilizado --------------------------------------------------------


   nome_modelo <- "model_rnn_pca.rdata"
   nome_figura <- "~/03 - Data Science/99 - TCC - ESTABILIZACAO_DE_PRESSAO_ REDE_NEURAL/04 - TCC/04 - Scripts/ANN/graficos/model_rnn_pca.png"
   
   set.seed(451)
   aprend <- 0.05
   epochs <- 1000
   arquit <- c(14,7,4)
   decaimento <- 0.999
   
# Treinamento ------------------------------------------------------------------


{
   
   
   model <- rnn::trainr(Y = Y[,train],
                        X = X[,train],
                        learningrate = aprend,
                        hidden_dim = arquit,
                        network_type = "rnn",
                        #seq_to_seq_unsync = T,
                        learningrate_decay = decaimento,
                        #batch_size = 300,
                        numepochs = epochs)
   
}
 beepr::beep(4)


# Calculando predição -----------------------------------------------------
{
   Yp <- predictr(model, X[,test])
   Ytest <- matrix(Y[,test], nrow = 1)
   Ytest <- t(Ytest)
   
   Ypredicted <- matrix(Yp, nrow = 1)
   Ypredicted <- t(Ypredicted)
   
   result_data <- data.frame(Ytest)
   result_data$Ypredicted <- Ypredicted
   result_data$residuo <- Ytest-Ypredicted
   
   #Percentual de variação em uma variável explicada por outra
   #por enquanto: entenda que é um percentual de variação explicada
   
   # rsq <- function(y_actual,y_predict)
   #{
   #  cor(y_actual,y_predict)^2
   #}
   
   corr <-  cor(result_data$Ytest,result_data$Ypredicted)^2
}  

corr

# Avaliação gráfica ----------------------------------------------------------------

{   erro <- as.data.frame(colMeans(model$error)) %>% 
      rename(error = "colMeans(model$error)")
   
   ggplot(erro) + 
      geom_line(aes(x=1:nrow(erro), y=error)) + 
      labs(x="Epochs",
           y="erro", 
           title="Gráfico de erro",
           size = 10)+
      theme_minimal() -> a
   
   ggplot(result_data) + 
      geom_line(aes(x=Ytest, y=Ypredicted)) + 
      labs(x="Teste", y="Predição", 
           title="Correlação previsão",
           subtitle = "Correlação =",
           size = 10) + 
      theme_minimal() -> b
   
   ggplot(result_data) + 
      geom_line(aes(x=1:nrow(result_data), y=residuo)) + 
      labs(x="Indivíduo", y="Resíduo",
           title="Resíduos",
           size = 10) + 
      theme_minimal() -> c
   
   grid.arrange(a,b,c,p)
}

# Salvando o modelo -------------------------------------------------------
{
save(model ,file =  nome_modelo)
ggsave(file = nome_figura,
       plot = ga,
       width = 7,
       height = 4.5,
       dpi = 600)
}


save.image(file = "modelo_ann.Rdata")

