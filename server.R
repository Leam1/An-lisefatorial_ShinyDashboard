##########################################################################################
##                                                                                      ##
##                                Análise Fatorial                                      ## 
##                                   Ismael Dias                                        ##
##                                                                                      ##
##                                                                                      ##
##                                                                                      ##
##########################################################################################


## LENDO OS DADOS
dados = read.table("Beer.txt", header = T)

# SUBSTITUINDO OS VALORES VAZIOS PELA MÉDIA GERAL
dados$COST[which(is.na(dados$COST))] = mean(dados$COST, na.rm = T)
dados$SIZE[which(is.na(dados$SIZE))] = mean(dados$SIZE, na.rm = T)
dados$ALCOHOL[which(is.na(dados$ALCOHOL))] = mean(dados$ALCOHOL, na.rm = T)
dados$REPUTAT[which(is.na(dados$REPUTAT))] = mean(dados$REPUTAT, na.rm = T)
dados$COLOR[which(is.na(dados$COLOR))] = mean(dados$COLOR, na.rm = T)
dados$AROMA[which(is.na(dados$AROMA))] = mean(dados$AROMA, na.rm = T)
dados$TASTE[which(is.na(dados$TASTE))] = mean(dados$TASTE, na.rm = T)
dados$SES[which(is.na(dados$SES))] = mean(dados$SES, na.rm = T)
dados$GROUP[which(is.na(dados$GROUP))] = mean(dados$GROUP, na.rm = T)

#REMOVENDO VARIÁVEL NA QUAL EM ANÁLISE PRÉVIA MOSTROU NAO FAZER PARTE DE NENHUM GRUPO
dados = dados[,-c(4)]

server = function(input,output){
  output$basededados_output = renderDataTable({datatable(dados, options = list(columnDefs = list(list(
    targets = 5,
    render = JS(
      "function(data, type, row, meta) {",
      "return type === 'display' && data.length > 6 ?",
      "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
      "}")
  ))), callback = JS('table.page(3).draw(false);'))
  })
  output$dispersao_plot = renderPlot({
    pairs.panels(dados[,-8])
  })
  output$correlacao_plot = renderPlot({
    corrplot(cor(dados), order = "hclust")
  })
  output$boxplot_output = renderPlot({
    boxplot(dados[,-8],col = "blue")
  })
  output$histograma_output = renderPlot({
    plot(dados[,-8],col = "blue")
    lines(lowess(dados))
  })
  output$summary_output = renderPrint({
    summary(dados)
  })
  output$shapiro_output = renderPrint({
    mvShapiro.Test(as.matrix(dados))
  })
  output$bartlett_output = renderPrint({
    n = dim(dados)[1]
    R = cor(dados)
    cortest.bartlett(R,n)
  })
  output$kaiser_output = renderPlot({
    fit<-princomp(dados,cor=TRUE)
    screeplot(fit, col = "blue")
  })
  output$analise_plot = renderPrint({
    R = cor(dados)
    k = sum(eigen(R)$values>=1)
    af = principal(dados, k, rotate = 'varimax')
    L1 = round(af$loadings,2)
    L1
  })
  output$qualidadeajuste_plot = renderPrint({
    R = cor(dados)
    k = sum(eigen(R)$values>=1)
    af = principal(dados, k, rotate = 'varimax')
    fit.off1 = 1-sum((af$residual-diag(diag(af$residual)))^2)/sum((R - diag(rep(1,dim(dados)[2])))^2)
    fit.off1
  })
  output$histogramavariavel_output = renderPlot({
    ggplot(dados, aes(x=dados[,c(input$histograma_input)]))+
      geom_histogram(color="darkblue", fill="lightblue")
      })
}

shinyServer(server)
