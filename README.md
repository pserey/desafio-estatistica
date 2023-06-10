# Desafio: Modelo de Regressão Linear para apostas em _Over 2.5 Goals_

Neste repositório estão os códigos em R e em Python (não funcional e apenas constam no repositório por motivos de histório de desenvolvimento), dados e gráficos plotados para o desenvolvimento de um modelo de regressão linear e sua simulação para a aposta em jogos de campeonatos na odd de _Over 2.5 Goals_.

> Mais sobre o desenvolvimento e tomada de escolhas nesta [documentação](https://docs.google.com/document/d/1qS7nrC66aKFhCHv7MkzPIyCFoBDH1oNqFqMpEte9LjE/edit?usp=sharing).

## Clonando e rodando o código
Para rodar o código, deve-se ter R e seu ambiente instalado na sua máquina assim como as seguintes bibliotecas da linguagem:

- Tidyverse
- dplyr
- tidyr
- ggplot2

Com o ambiente devidamente configurado, é possível clonar o repositório em sua máquina da seguinte maneira:

```
git clone https://github.com/pserey/desafio-estatistica
```

Após clonado, é possível executar o modelo da seguinte maneira:

- Montar um CSV com jogos com resultados e jogos sem resultados (jogos a ser previstos). 
- Para rodar o modelo em cima do CSV, é possível usar a ferramenta de linha de comando de R com o nome do arquivo como argumento:
```
Rscript final/modelo_real.R "arquivo.csv"
```
> Dar atenção ao local do arquivo que deve ser relativo ao diretório onde o script está rodando.

- A partir desses dados, o modelo irá tentar prever os jogos sem resultados a partir dos dados calculados dos jogos anteriores e retornará a lista de jogos que o modelo considera como boas apostas.