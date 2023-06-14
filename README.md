# Desafio: Modelo de Regressão Linear para apostas em _Over 2.5 Goals_

Neste repositório estão os códigos em R e em Python (não funcional e apenas constam no repositório por motivos de histório de desenvolvimento), dados e gráficos plotados para o desenvolvimento de um modelo de regressão linear e sua simulação para a aposta em jogos de campeonatos na odd de _Over 2.5 Goals_.

> Mais sobre o desenvolvimento e tomada de escolhas nesta [documentação](https://drive.google.com/file/d/1OG5HLKxJkMVfA45aAqYu-6uSGm6UTxAP/view?usp=sharing).

## Clonando e rodando o código
Para rodar o código, deve-se ter R e seu ambiente instalado na sua máquina assim como as seguintes bibliotecas da linguagem:

- Tidyverse
- dplyr
- tidyr
- ggplot2
- here
- httr

Com o ambiente devidamente configurado, é possível clonar o repositório em sua máquina da seguinte maneira:

```
git clone https://github.com/pserey/desafio-estatistica
```

Após clonado, é possível executar o modelo de duas maneiras: para o uso real de previsões de apostas suas ou para simular o modelo em campeonatos cadastrados no software

### Uso real do modelo (escolha de jogos para apostas)

- Montar um CSV com jogos com resultados e jogos sem resultados (jogos a ser previstos) e colocar esse CSV no diretório `/jogos`

- Para rodar o modelo em cima do CSV, é possível usar a ferramenta de linha de comando de R com o nome do arquivo como argumento:
```
Rscript final/modelo_real.R <arquivo.csv>
```
- Ao rodar o script dessa maneira, aparecerá um menu para que você escolha o campeonato que está prevendo para que o melhor modelo seja aplicado.

```
Rscript final/modelo_real.R <arquivo.csv> <campeonato>
```

> Caso a quantidade de jogos do CSV seja pequena (começo de temporada), será proposto a adição da última temporada na previsão de jogos atuais a custo de acurácia. Para adicionar a última temporada basta incluir `y` como argumento final para o script.

- A partir desses dados, o modelo irá tentar prever os jogos sem resultados a partir dos dados calculados dos jogos anteriores e retornará a lista de jogos que o modelo considera como boas apostas.

### Simulação do modelo

```
Rscript final/leitura.R
```

- Ao rodar esse script, um menu para selecionar uma temporada específica de um campeonato irá aparecer. Para selecionar a temporada, basta rodar novamente o script com a opção como argumento

```
Rscript final/leitura.R <temporada>
```
- Após isso, um menu para a escolha de um país será apresentado. É possível escolher um desses países para simular o modelo na temporada escolhida do principal campeonato do país. Basta adicionar o país como argumento novamente.

```
Rscript final/leitura.R <temporada> <pais>
```
