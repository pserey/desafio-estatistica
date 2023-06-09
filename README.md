# Desafio: Modelo de Regressão Linear para apostas em _Over 2.5 Goals_

Neste repositório estão os códigos em R e em Python (não funcional e apenas constam no repositório por motivos de histório de desenvolvimento), dados e gráficos plotados para o desenvolvimento de um modelo de regressão linear e sua simulação para a aposta em jogos de campeonatos na odd de _Over 2.5 Goals_.

> Mais sobre o desenvolvimento e tomada de escolhas nesta [documentação]().

## Clonando e rodando o código
Para rodar o código, deve-se ter R e seu ambiente instalado na sua máquina assim como as seguintes bibliotecas da linguagem:

- Tidyverse
- dplyr
- ggplot2

Com o ambiente devidamente configurado, é possível clonar o repositório em sua máquina da seguinte maneira:

```
git clone https://github.com/pserey/desafio-estatistica
```

Após clonado, é possível treinar o modelo de regressão com um CSV com no mínimo 40 jogos com resultados (quantidade mínima escolhida para o treinamento). A partir desses 40, qualquer jogo restante será previsto e o modelo retornará os jogos nos quais há evidência estatística de que a quantidade de gols será maior que 2.5.