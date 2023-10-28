pacman::p_load("tidyverse","ggplot2","shiny","rsconnect")

rsconnect::setAccountInfo(name='vy20g3-ramon0tavares',
                          token='4377040138C2B03B9BE6AF2DAF7AF1A5',
                          secret='cy3iYOQkgljiJkmCwpjNSGlMKG43PRZJIQiQScyH')
numeroAmostras <- rep(1:94)
embalagensNaoConformes <- c(12,15,8,10,4,7,16,9,14,10,5,6,17,12,22,
                            8,10,5,13,11,20,18,24,15,9,12,7,13,9,6,
                            9,6,12,5,6,4,6,3,7,6,2,4,3,6,5,4,8,5,6,
                            7,5,6,3,5,8,7,5,6,4,5,2,3,4,7,6,5,5,3,7,
                            9,6,10,4,3,5,8,11,9,7,3,5,2,1,4,5,3,7,6,
                            4,4,6,8,5,6)
fracaoNaoConforme <- c(
  .24,.30,.16,.20,.08,.14,.32,.18,.28,.20,.10,.12,.34,.24,.44,
  .16,.20,.10,.26,.22,.40,.36,.48,.30,.18,.24,.14,.26,.18,.12,
  .18,.12,.24,.10,.12,.08,.12,.06,.14,.12,.04,.08,.06,.12,.10,
  .08,.16,.10,.12,.14,.10,.12,.06,.10,.16,.14,.10,.12,.08,.10,
  .04,.06,.08,.14,.12,.10,.10,.06,.14,.18,.12,.20,.08,.06,.10,
  .16,.22,.18,.14,.06,.10,.04,.02,.08,.10,.06,.14,.12,.08,.08,
  .12,.16,.10,.12)

dados <- data.frame(
  numero_amostras = numeroAmostras,
  embalagens_nao_conformes = embalagensNaoConformes,
  fracao_amostral_nao_conforme = fracaoNaoConforme
)
dados

operacoes <- function(dados, n){
  m <- length(dados)
  p <- sum(dados) / (n*m)
  numerador <- p*(1-p)
  lic <- p - 3 * sqrt(numerador/n)
  lsc <- p + 3 * sqrt(numerador/n)
  lista <- list(
    p = p,
    lic = lic,
    lsc = lsc
  )
  return(lista)
}

dados1 <- dados %>%
  slice(1:30)

limites <- operacoes(dados1$embalagens_nao_conformes, 50)  

valoresFiltrados <- dados1 %>%
  filter(fracao_amostral_nao_conforme>limites$lsc)
# Defina os dados e funções que você já possui

## SEGUNDO GRÁFICO
dadosSemPontosFora <- dados1 %>%
  filter(fracao_amostral_nao_conforme <= limites$lsc)
dadosSemPontosFora

novosLimites <- operacoes(dadosSemPontosFora$embalagens_nao_conformes,50)

valoresFiltrados2 <- dados1 %>%
  filter(fracao_amostral_nao_conforme > novosLimites$lsc)

dados2 <- dados %>%
  slice(1:54)
valoresIguais <- dados2 %>%
  filter(fracao_amostral_nao_conforme <= round(limites$lic,2))

dadosVinteQuatro <- dados2 %>%
  slice(31:54)
dadosVinteQuatro
novosLimites24 <- operacoes(dadosVinteQuatro$embalagens_nao_conformes,50)
novosLimites24[2] <- 0
# Define a UI (interface do usuário) do aplicativo Shiny
ui <- fluidPage(
  titlePanel("Gráfico de Controle Interativo"),
  sidebarLayout(
    sidebarPanel(
      splitLayout(
        sliderInput("limite_superior", "L.S.C(1°Gráfico):", min = 0, max = 1, value = limites$lsc),
        sliderInput("limite_inferior", "L.I.C(1°Gráfico):", min = 0, max = 1, value = limites$lic),
        sliderInput("quantidade_eixo_x", "Eixo X (1°Gráfico):", min = 1, max = 30, value = 30),
      ),
      splitLayout(
        sliderInput("novoLimiteSup", "L.S.C(2°Gráfico):", min = 0, max = 1, value = novosLimites$lsc),
        sliderInput("novoLimiteInf", "L.I.C(2°Gráfico):", min = 0, max = 1, value = novosLimites$lic),
        sliderInput("quantidade_eixo_x2", "Eixo X (2°Gráfico):", min = 1, max = 30, value = 30),
      ),
      splitLayout(
        sliderInput("novoLimiteSup3", "L.S.C(3°Gráfico):", min = 0, max = 1, value = novosLimites$lsc),
        sliderInput("novoLimiteInf3", "L.I.C(3°Gráfico):", min = 0, max = 1, value = novosLimites$lic),
        sliderInput("quantidade_eixo_x3", "Eixo X (3°Gráfico):", min = 1, max = 54, value = 54),
      ),
      splitLayout(
        sliderInput("quantidade_eixo_x4", "Eixo X (4°Gráfico):", min = 1, max = 54, value = 54),
      ), 
      splitLayout(
        sliderInput("quantidade_eixo_x5", "Eixo X (5°Gráfico):", min = 1, max = 94, value = 94)
      )
    ),
    mainPanel(
      plotOutput("grafico_interativo"),
      plotOutput("segundo_grafico"),
      plotOutput("terceiro_grafico"),
      plotOutput("quarto_grafico"),
      plotOutput("quinto_grafico")
      # Renderiza o gráfico interativo
    )
  )
)

# Define a função de servidor do aplicativo Shiny
server <- function(input, output) {
  # Renderiza o gráfico interativo
  output$grafico_interativo <- renderPlot({
    dados_filtrados <- dados1 %>%
      filter(fracao_amostral_nao_conforme > input$limite_superior | 
               fracao_amostral_nao_conforme < input$limite_inferior)
    
    dados_visualizacao <- dados1 %>% slice(1:input$quantidade_eixo_x)
    
    ggplot(data = dados_visualizacao, aes(x = numero_amostras, y = fracao_amostral_nao_conforme)) +
      geom_line() +
      geom_point() +
      geom_hline(yintercept = input$limite_superior, color = "red") +
      geom_hline(yintercept = input$limite_inferior, color = "red") +
      geom_hline(yintercept = limites$p, color = "blue") +
      geom_point(data = dados_filtrados, color = "red", shape = 21, size = 3) +
      labs(x = "Número de Amostras",
           y = expression("Fração amostral não conforme " ~ italic(hat(p[i]))),
           title = "Primeiro Gráfico de Controle") +
      scale_x_continuous(breaks = seq(min(dados_visualizacao$numero_amostras), 
                                      max(dados_visualizacao$numero_amostras), 
                                      by = 1), expand = c(0, 0)) +
      annotate("text", x = 0, y = input$limite_superior + 0.01,
               label = paste("Limite Superior =", round(input$limite_superior, 4)),
               hjust = 0, color = "black") +
      annotate("text", x = 0, y = input$limite_inferior - 0.01,
               label = paste("Limite Inferior =", round(input$limite_inferior, 4)),
               hjust = 0, color = "black")
  })
  ##############################################################################
  ##############################################################################
  # Renderiza o segundo gráfico
  output$segundo_grafico <- renderPlot({
    dados_filtrados2 <- dados1 %>%
      filter(fracao_amostral_nao_conforme > input$novoLimiteSup | 
               fracao_amostral_nao_conforme < input$novoLimiteInf)
    
    dados_visualizacao2 <- dados1 %>% slice(1:input$quantidade_eixo_x2)
    
    ggplot(data = dados_visualizacao2, aes(x = numero_amostras, y = fracao_amostral_nao_conforme)) +
      geom_line() +
      geom_point() +
      geom_hline(yintercept = novosLimites$p, color = "blue") +
      geom_hline(yintercept = input$novoLimiteInf, color = "red") +
      geom_hline(yintercept = input$novoLimiteSup, color = "red") +
      geom_point(data = dados_filtrados2, color = "red",
                 shape = 21, size = 3) +
      labs(x = "Número de Amostras",
           y = expression("Fração amostral não conforme " ~ italic(hat(p[i]))),
           title = "Segundo Gráfico de Controle") +
      scale_x_continuous(breaks = seq(min(dados_visualizacao2$numero_amostras),
                                      max(dados_visualizacao2$numero_amostras),
                                      by = 1), expand = c(0, 0)) +
      annotate("text", x = 0, y = input$novoLimiteSup + 0.01,
               label = paste("LSC revisto =", round(input$novoLimiteSup, 4)),
               hjust = 0, color = "black") +
      annotate("text", x = 0, y = novosLimites$p - 0.01,
               label = paste("Linha Central =", round(novosLimites$p, 4)),
               hjust = 0, color = "black") +
      annotate("text", x = 0, y = input$novoLimiteInf + 0.01,
               label = paste("LIC revisto =", round(input$novoLimiteInf, 4)),
               hjust = 0, color = "black") +
      annotate("text", x = 14, y = fracaoNaoConforme[15] + 0.01,
               label = paste("Novo Material =", fracaoNaoConforme[15]),
               hjust = 0, color = "black") +
      annotate("text", x = 22, y = fracaoNaoConforme[23] + 0.01,
               label = paste("Novo Operador =", fracaoNaoConforme[23]),
               hjust = 0, color = "black") +
      annotate("text", x = 19, y = fracaoNaoConforme[21] + 0.01,
               label = paste("Nenhuma Causa Atribuível =", fracaoNaoConforme[21]),
               hjust = 0, color = "black")
  })
  ##############################################################################
  ##############################################################################
  output$terceiro_grafico <- renderPlot({
    dados_filtrados3 <- dados2 %>%
      filter(fracao_amostral_nao_conforme > input$novoLimiteSup3 | 
               fracao_amostral_nao_conforme < input$novoLimiteInf3)
    
    dados_visualizacao3 <- dados2 %>% slice(1:input$quantidade_eixo_x3)
    
    ggplot(data = dados_visualizacao3, aes(x = numero_amostras, y = fracao_amostral_nao_conforme)) +
      geom_line() +
      geom_point() +
      
      geom_hline(yintercept = novosLimites$p, color = "blue")+
      geom_hline(yintercept = input$novoLimiteInf3, color = "red")+
      geom_hline(yintercept = input$novoLimiteSup3, color = "red") +
      
      
      geom_point(data = dados_filtrados3[-2,], color = "red",
                 shape = 21, size = 3) +
      geom_point(data = dados_filtrados3[2,], color = "green",
                 shape = 21, size = 3) +
      geom_point(data = valoresIguais, color = "green",
                 shape = 21, size = 3) +
      
      
      labs(x = "Número de Amostras",
           y = expression("Fração amostral não conforme " ~italic(hat(p[i]))),
           title = "Terceiro Gráfico de Controle")+
      
      
      scale_x_continuous(breaks = seq(min(dados2$numero_amostras+1), 
                                      max(dados2$numero_amostras), 
                                      by = 2), expand = c(0, 0))+
      
      
      annotate("text", x = 0, y = input$novoLimiteSup3 + 0.01,
               label = paste("LSC revisto =", round(input$novoLimiteSup3,4)),
               hjust = 0, color = "black")+
      annotate("text", x = 0, y = novosLimites$p - 0.01,
               label = paste("Linha Central =", round(novosLimites$p,4)),
               hjust = 0, color = "black")+
      annotate("text", x = 0, y = input$novoLimiteInf3 + 0.01,
               label = paste("LIC revisto =",round(input$novoLimiteInf3,4)),
               hjust = 0, color = "black")+
      
      
      annotate("text", x = 14, y = fracaoNaoConforme[15] + 0.01,
               label = paste("Novo Material =",fracaoNaoConforme[15]),
               hjust = 0, color = "black")+
      annotate("text", x = 22, y = fracaoNaoConforme[23] + 0.01,
               label = paste("Novo Operador =",fracaoNaoConforme[23]),
               hjust = 0, color = "black")+
      annotate("text", x = 19, y = fracaoNaoConforme[21] + 0.01,
               label = paste("Nenhuma Causa Atribuível =",fracaoNaoConforme[21]),
               hjust = 0, color = "black")+
      
      annotate("text", x = 39, y = fracaoNaoConforme[41] - 0.01,
               label = paste("Ponto Igual ao Limite =",fracaoNaoConforme[41]),
               hjust = 0, color = "black")+
      
      
      geom_point(x = 33, y = 0.45, shape = 21, size = 3, color = "red") +
      geom_point(x = 33, y = 0.45) +
      annotate("text", x = 33.3, y = 0.45,
               label = "= Pontos não incluídos nos cálculos do limite de controle",
               hjust = 0, color = "black") +
      
      
      geom_point(x = 33, y = 0.42, shape = 21, size = 3, color = "green") +
      geom_point(x = 33, y = 0.42) +
      annotate("text", x = 33.3, y = 0.42,
               label =
                 "= Pontos extremos observados mas considerados nos limites",
               hjust = 0, color = "black") +
      annotate(geom = "curve",
               arrow = arrow(type = "closed",length = unit(2.45, "mm")),
               curvature = 0.4, x = 36, xend = 31, y=0.3, yend = 0.185) +
      annotate("text", x = 36, y = 0.3,
               label = "Ajustes na máquina",
               hjust = 0, color = "black") 
  })
  ##############################################################################
  ##############################################################################
  output$quarto_grafico <- renderPlot({
    dados_visualizacao4 <- dados2 %>% slice(1:input$quantidade_eixo_x4)
    ggplot(data = dados_visualizacao4, aes(x = numero_amostras, y = fracao_amostral_nao_conforme)) +
      geom_line() +
      geom_point() +
      
      geom_segment(aes(x = 0, xend = 30, y = novosLimites$p,
                       yend = novosLimites$p), color = "blue") +
      geom_segment(aes(x = 0, xend = 30, y = novosLimites$lic,
                       yend = novosLimites$lic), color = "red") +
      geom_segment(aes(x = 0, xend = 30, y = novosLimites$lsc,
                       yend = novosLimites$lsc), color = "red") +
      
      
      geom_point(data = valoresFiltrados2[-2,], color = "red",
                 shape = 21, size = 3) +
      geom_point(data = valoresFiltrados2[2,], color = "green",
                 shape = 21, size = 3) +
      
      
      geom_segment(aes(x = 30, xend = 30, y = novosLimites$lsc,
                       yend = novosLimites24$lic), color = "gray",
                   linetype = "solid")+
      
      
      geom_segment(aes(x = 30, xend = 54, y = novosLimites24$lsc,
                       yend = novosLimites24$lsc),color = "red") +
      geom_segment(aes(x = 30, xend = 54, y = novosLimites24$p,
                       yend = novosLimites24$p),color = "blue") +
      geom_segment(aes(x = 30, xend = 54, y = novosLimites24$lic,
                       yend = novosLimites24$lic),color = "red") +
      
      
      labs(x = "Número de Amostras",
           y = expression("Fração amostral não conforme " ~italic(hat(p[i]))),
           title = "Quarto Gráfico de Controle")+
      
      
      scale_x_continuous(breaks = seq(min(dados2$numero_amostras+1), 
                                      max(dados2$numero_amostras), 
                                      by = 2), expand = c(0, 0))+
      
      
      annotate("text", x = 0, y = novosLimites$lsc + 0.01,
               label = paste("LSC revisto =", round(novosLimites$lsc,4)),
               hjust = 0, color = "black")+
      annotate("text", x = 0, y = novosLimites$p - 0.01,
               label = paste("Linha Central =", round(novosLimites$p,4)),
               hjust = 0, color = "black")+
      annotate("text", x = 0, y = novosLimites$lic + 0.01,
               label = paste("LIC revisto =",round(novosLimites$lic,4)),
               hjust = 0, color = "black")+
      
      
      annotate("text", x = 30.2, y = novosLimites24$lsc + 0.01, 
               label = paste("LSC =", round(novosLimites24$lsc,4)),
               hjust = 0, color = "black")+
      # annotate("text", x = 30.2, y = novosLimites24$p - 0.01, 
      #          label = paste("Linha Central =", round(novosLimites24$p,4)),
      #          hjust = 0, color = "black")+
      annotate("text", x = 30.2, y = novosLimites24$lic + 0.01,
               label = paste("LIC =",round(novosLimites24$lic,4)), 
               hjust = 0, color = "black")+
      
      
      annotate("text", x = 14, y = fracaoNaoConforme[15] + 0.01,
               label = paste("Novo Material =",fracaoNaoConforme[15]),
               hjust = 0, color = "black")+
      annotate("text", x = 22, y = fracaoNaoConforme[23] + 0.01,
               label = paste("Novo Operador =",fracaoNaoConforme[23]),
               hjust = 0, color = "black")+
      annotate("text", x = 19, y = fracaoNaoConforme[21] + 0.01, 
               label = paste("Nenhuma Causa Atribuível =",fracaoNaoConforme[21]),
               hjust = 0, color = "black")+
      
      
      geom_point(x = 33, y = 0.45, shape = 21, size = 3, color = "red") +
      geom_point(x = 33, y = 0.45) +
      
      
      annotate("text", x = 33.3, y = 0.451,
               label = "= Pontos não incluídos nos cálculos do limite de controle",
               hjust = 0, color = "black") +
      
      
      geom_point(x = 33, y = 0.42, shape = 21, size = 3, color = "green") +
      geom_point(x = 33, y = 0.42) +
      annotate("text", x = 33.3, y = 0.421,
               label = 
                 "= Pontos extremos observados ainda considerados nos limites",
               hjust = 0, color = "black") +
      annotate(geom = "curve",arrow = arrow(type = "closed",length = unit(2,"mm")),
               curvature = 0.2, x = 38, xend = 30.5, y=0.35, yend = 0.3) +
      
      annotate("text", x = 38, y = 0.35, 
               label = paste(" = Ajuste Na máquina"),
               hjust = 0, color = "black")
  })
  #############################################################################
  #############################################################################
  output$quinto_grafico <- renderPlot({
    dados_visualizacao5 <- dados %>% slice(1:input$quantidade_eixo_x5)
    ggplot(data = dados_visualizacao5, aes(x = numero_amostras, y = fracao_amostral_nao_conforme)) +
      geom_line() +
      geom_point() +
      
      
      geom_segment(aes(x = 0, xend = 30, y = novosLimites$p,
                       yend = novosLimites$p), color = "blue") +
      
      geom_segment(aes(x = 0, xend = 30, y = novosLimites$lic,
                       yend = novosLimites$lic), color = "red") +
      
      geom_segment(aes(x = 0, xend = 30, y = novosLimites$lsc,
                       yend = novosLimites$lsc), color = "red") +
      
      
      
      geom_point(data = valoresFiltrados2[-2,], color = "red",
                 shape = 21, size = 3) +
      geom_point(data = valoresFiltrados2[2,], color = "green",
                 shape = 21, size = 3) +
      
      
      geom_segment(aes(x = 30, xend = 30, y = novosLimites$lsc,
                       yend = novosLimites24$lic), color = "gray",
                   linetype = "solid")+
      
      
      geom_segment(aes(x = 30, xend = 94, y = novosLimites24$lsc,
                       yend = novosLimites24$lsc),color = "red") +
      geom_segment(aes(x = 30, xend = 94, y = novosLimites24$p,
                       yend = novosLimites24$p),color = "blue") +
      geom_segment(aes(x = 30, xend = 94, y = novosLimites24$lic,
                       yend = novosLimites24$lic),color = "red") +
      
      
      labs(x = "Número de Amostras",
           y = expression("Fração amostral não conforme " ~italic(hat(p[i]))),
           title = "Quinto Gráfico de Controle")+
      scale_x_continuous(breaks = seq(min(dados$numero_amostras+1), 
                                      max(dados$numero_amostras), 
                                      by = 2), expand = c(0, 0))+
      
      
      annotate("text", x = 0, y = novosLimites$lsc + 0.01, 
               label = paste("LSC revisto =", round(novosLimites$lsc,4)),
               hjust = 0, color = "black")+
      annotate("text", x = 0, y = novosLimites$p - 0.01,
               label = paste("Linha Central =", round(novosLimites$p,4)),
               hjust = 0, color = "black")+
      annotate("text", x = 0, y = novosLimites$lic + 0.01,
               label = paste("LIC revisto =",round(novosLimites$lic,4)),
               hjust = 0, color = "black")+
      
      
      annotate("text", x = 30.2, y = novosLimites24$lsc + 0.01,
               label = paste("LSC =", round(novosLimites24$lsc,4)), 
               hjust = 0, color = "black")+
      # annotate("text", x = 30.2, y = novosLimites24$p - 0.01,
      #          label = paste("Linha Central =", round(novosLimites24$p,4)),
      #          hjust = 0, color = "black")+
      annotate("text", x = 30.2, y = novosLimites24$lic + 0.01, 
               label = paste("LIC =",round(novosLimites24$lic,4)),
               hjust = 0, color = "black")+
      
      
      annotate("text", x = 13, y = fracaoNaoConforme[15] + 0.01,
               label = paste("Novo Material =",fracaoNaoConforme[15]),
               hjust = 0, color = "black")+
      annotate("text", x = 22, y = fracaoNaoConforme[23] + 0.01,
               label = paste("Novo Operador =",fracaoNaoConforme[23]),
               hjust = 0, color = "black")+
      annotate("text", x = 19, y = fracaoNaoConforme[21] + 0.01,
               label = paste("Nenhuma Causa Atribuível =",fracaoNaoConforme[21]), 
               hjust = 0, color = "black")+
      
      
      geom_point(x = 45, y = 0.45, shape = 21, size = 3, color = "red") +
      geom_point(x = 45, y = 0.45) +
      annotate("text", x = 45.3, y = 0.451,
               label = " = Pontos não incluídos nos cálculos do limite de controle",
               hjust = 0, color = "black") +
      
      
      geom_point(x = 45, y = 0.42, shape = 21, size = 3, color = "green") +
      geom_point(x = 45, y = 0.42) +
      annotate("text", x = 45.3, y = 0.421,
               label = 
                 " = Pontos extremos observados mas considerados nos limites",
               hjust = 0, color = "black") +
      
      annotate(geom = "curve",arrow = arrow(type = "closed",length = unit(2,"mm")),
               curvature = 0.4, x = 38, xend = 33, y=0.2, yend = 0.114) +
      
      annotate("text", x = 38, y = 0.2, 
               label = paste("Linha Central =", round(novosLimites24$p,4)),
               hjust = 0, color = "black") + 
      
      annotate(geom = "curve",arrow = arrow(type = "closed",length = unit(2,"mm")),
               curvature = 0.2, x = 38, xend = 30.5, y=0.35, yend = 0.3) +
      
      annotate("text", x = 38, y = 0.35, 
               label = paste(" = Linha divisória a partir do ajuste"),
               hjust = 0, color = "black")
  })
  
}
shinyApp(ui, server)