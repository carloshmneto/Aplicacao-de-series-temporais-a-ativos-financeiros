# Aplicação de Séries Temporais a Ativos Financeiros

Para esse projeto, foram selecionadas 10 bases de dados de diferentes índices e ações mundiais fornecidas pelo Yahoo para aplicação de técnicas de séries temporais, como treinamento de um modelo do tipo ARMA-GARCH e cálculo de Valor em Risco futuro para cada um dos casos. As bases selecionadas foram as seguintes:

1. Ação da Apple
2. Ação da Microsoft
3. Ação da Petrobras
4. Ação da Tesco
5. Ação da Volkswagen
6. Ação do Banco Santander
7. Índice Bovespa
8. Índice S&P 500
9. Preço do Bitcoin
10. Taxa de câmbio de dólar (USD) para real (BRL)

## Estrutura do projeto
- `trabalho.R` contém o código completo, e pode ser rodado sem requisitos especiais
- na pasta dados, podem ser obtidos os arquivos `.csv` para todas as 10 séries utilizadas

## Etapas do Projeto

1. Importação e ajuste das bases
  a. Exclusão de valores ausentes
  b. Criação de colunas para retorno bruto e log-retorno
2. Exploração dos dados
  a. Visualização das séries
  b. Gráfico de autocorrelação e autocorrelação parcial
  c. Teste de normalidade por assimetria e curtose
3. Ajuste de um modelo ARMA para linearidade pelo auto.arima
  a. Definição de parâmetros relevantes através da comparação de seu coeficiente com seu erro padrão
5. Ajuste de um modelo GARCH para volatilidade em conjunto ao ARMA
  a. Definição de parâmetros relevantes através do p-valor
6. Definição do Valor em Risco a 1% para os próximos 10 dias

## Resultados

Um exemplo de resultado a ser exibido é para a série AAPL (ações da Apple):

![visualizacao](https://drive.google.com/uc?export=view&id=1Av93OjAI0byQ8403RZbgNrZafRGiUs4Z)

Visualização das séries temporais de preço ajustado, retorno bruto e log-retorno

![autocorrelacoes](https://drive.google.com/uc?export=view&id=1nPZveHt-yM9Ast10THjxN95d9sIF25Ey)

Visualização da autocorrelação e autocorrelação parcial

Modelo final treinado: ARMA(2,0)-GARCH(1,1)
Valor em risco para os próximos 10 dias: 0,03%

Para resultados dos outros dados, rodar o código localmente para obter detalhadamente.

## Contato
- [Meu LinkedIn](https://www.linkedin.com/in/carlos-neto-5668b0265/)
- Email: carloshmneto@usp.br
