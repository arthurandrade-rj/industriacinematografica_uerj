---
layout: post
title: "Ajustando automaticamente"
tags: [PowerBI, cinema, inflação, tutorial]
color: blue
author: arthurandrade-rj
---

Esse foi tema de uma das minhas aulas de Business Intelligence com Power BI, ministradas no INFNET em parceria com A Ponte Para PretXs.

Se vocês forem na Wikipedia, poderão ver que dos 100 filmes com as maiores bilheterias, 95 foram lançados após os anos 2000. Mas com um detalhe: esses valores não estão ajustados de acordo com a inflação.

Eu sempre achei isso curioso e resolvi me perguntar: **a indústria cinematográfica mudou tanto assim ou foi o dinheiro que ficou mais forte?**

# Ajustando automaticamente bilheteria de filmes pela inflação

Para começar a responder essa questão, vamos tentar ajustar valores de bilheteria de filmes a partir da inflação. Como os valores estão em dólar americano, vou ajustar pela inflação dos EUA. No final da publicação, vcs poderão encontrar o código em linguagem M, e no próximo post faremos a mesma coisa, mas utilizando o R!

# 1. Web Scrapping e Transformação de Dados
O conjunto de dados primário que utilizaremos é o "movies_metadata.csv" descrito [nesta competição do Kaggle](https://www.kaggle.com/rounakbanik/the-movies-dataset).

Depois que você já baixou e importou o dataset de filmes pro Power BI, nós vamos precisar [deste site](https://www.usinflationcalculator.com/monthly-us-inflation-rates-1913-present/), que nos dará informações sobre a inflação.

**Passo 1:** importar a tabela do site "US Inflation Calculator"


![Passo 1](https://i.imgur.com/LPlEf5Lg.png)

![Passo 1](https://i.imgur.com/qmOsI89.png)

**Passo 2:** Selecionar todas as colunas de Jan a Dec, ir até a guia "Transformar" e transpôr as colunas em linhas

![Passo 3](https://i.imgur.com/xBczvbU.png)

**Passo 3:** Adicionar uma coluna condicional que transforme o texto de mês em valores

![Passo 4](https://i.imgur.com/P3Kxr9p.png)

**Passo 4:** Você precisará transformar a coluna de Ano (Year) e Numero de Mês (n_month) em texto, e a coluna valor em número decimal

![Passo 5](https://i.imgur.com/5Tkebea.png)

**Passo 5:** Vá até a guia "Adicionar coluna" e crie uma coluna personalizada, cujo valor será uma concatenação entre Numero do Mês e Ano

![Passo 6](https://i.imgur.com/s3wVoMu.png)

**Passo 6:** Seu novo campo precisa ser categorizado como data. Para isso, clique na coluna e selecione a opção "Usar Localidade" na lista. No campo "Tipo de Dados" selecione Data e em Localidade selecione Português (Brasil).

![Passo 7](https://i.imgur.com/KSjfrco.png)

**Passo 7:** Na sua tabela "movies_table", transforme o campo "release_date" em Data. Mas atenção! Como esse campo está no formato americano (padrão) ele pode tanto ser transformado diretamente, quanto utilizando a opção "Usar Localidade...", e escolhendo o país EUA.

![Passo 8](https://i.imgur.com/pAlwm3R.png)

**Passo 8:** Você perceberá que alguns valores trarão erro. Por exemplo, é impossível categorizar "N/A" como data. Para isso, vamos tratar da seguinte forma: toda vez que algum valor for considerado erro, substituiremos por "null". Clique no botão direito, escolha a opção "Substituir erros..." e coloque null no campo de valor.

![Passo 9](https://i.imgur.com/1iXHdwh.png)

**Passo 9:** A nossa missão dentro do Power Query terminou. Você já pode clicar em Fechar e Aplicar e passar para a tela do Power BI. Aqui, criaremos uma coluna que nos trará o valor de "release_date" ajustado, ou seja: apenas o primeiro dia do mês, até pq, nós não vemos inflação por dia. Para isso, abra a sua tabela no Power BI, e crie a seguinte coluna calculada:

![Passo 10](https://i.imgur.com/oPjNyU0.png)

# 2. Estabelecendo relações

Nesta segunda etapa, precisaremos acessar o menu "Ferramentas de tabela" e escolher a opção "Gerenciar relações". Estabeleceremos uma relação de "Muitos para um (*:1)" entre as colunas release (movies_table) e Date (inflation_table). Isso significa que para cada muitos registros de data na tabela movies_table teremos apenas um registro de data na tabela inflation_table.

![Passo 11](https://i.imgur.com/YBrJCUg.png)


# 3. Criando as fórmulas
Nessa etapa final, criaremos as fórmulas necessárias para chegar ao nosso resultado.

![Passo 12](https://i.imgur.com/rqUGSt4.png)

**Formula 1: cum_inflation (Inflação Acumulada)**

Essa primeira fórmula é a mais importante. 

Primeiro, criaremos uma variável chamada **min_date**, que me dará a data mínima a ser retornada. Porque criamos essa variável? Porque quando estamos trabalhando com datas, precisamos tomar cuidado com sua hierarquia. Campos de data geralmente se hierarquizam em Ano/Trimestre/Mês/Dia. 

Isso significa que podemos ver a receita dos filmes lançados tanto no ano todo quanto num mês específico. Para fins de simplificação, a inflação de 1990 até 2020 será considerada desde Jan/1990 até o ultimo mês de 2020.

   
    cum_inflation = 
        VAR min_date = IF(ISINSCOPE(inflation_table[Date].[Ano]);
        DATE(MIN(inflation_table[Date].[Ano]);1;1);
        MIN(inflation_table[Date]))
    

    RETURN
        CALCULATE(
        PRODUCTX(inflation_table; 1 + 'inflation_table'[Valor]/100);
        DATESBETWEEN('inflation_table'[Date];min_date;
        CALCULATE(MAX('inflation_table'[Date]);ALL(inflation_table))        
    ))


A função **ISINSCOPE** testa se você está usando uma hierarquia ou não. Se estiver usando uma hierarquia de ano, a variável retornará a menor data de Janeiro. Se não, retornará a própria data. Por exemplo: se estivermos vendo filmes lançados em 01/04/1990 na hierarquia anual, então o mês de base para o cálculo será 01/01/1990. Se não estivermos vendo na hierarquia, o mês base será o próprio 01/04/1990.

Como retorno, usaremos o **CALCULATE** (que filtra e depois calcula) para trazer o **PRODUCTX** dentro da inflation_table, na fórmula padrão de inflação acumulada (1 + valor/100). O filtro será as datas entre **min_date** (que calculamos anteriormente) e a maior data de todas disponível da tabela (que hoje é o dia 01/03/2020). Por isso usamos o artifício **ALL**. Ou seja: estaremos querendo calcular qual a inflação acumulada em Jan/1990 até hoje, em Abr/1990 até hoje, e assim em diante...

**Fórmula 2: adjusted_revenue (Receita Ajustada)**

Depois disso, é só alegria. Basta multiplicar os valores de receita (revenue) pelo valor da inflação que acabamos de calcular!

    
    adjusted_revenue = SUM(movies_table[revenue])*[cum_inflation]
   

# 4. O resultado...
Tcharam!

![Passo 13](https://i.imgur.com/RMId5To.png)

Vemos ali entre o ano de 1940 e 1960 uma mudança significativa entre os valores "originais" e os valores ajustados. Lembrando que entre 1939 e 1945 tivemos a Segunda Guerra Mundial, que impactou fortemente as economias globais e mudou a história do cinema pra sempre (você pode ler mais sobre aqui).

---------------------


E aí? Curtiram? Esse trabalho otimiza bastante tempo, e você nunca mais precisará importar nenhuma tabela, todos os valores estarão atualizados automaticamente.

Isso pode ser muito útil quando você precisar atualizar valores por um determinado índice (por exemplo, IGP-DI).

Duvidas? Sugestões? Críticas? Manda um email pra arthurandrade.rj@hotmail.com, ou então me chama no wpp: +5521981133625.



---------------------

# Código M - Tabela de Inflação
    
