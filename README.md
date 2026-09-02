---
title: "RadarSocial"
output: github_document
---

# 📡 RadarSocial

### Transformando dados do eSocial em informação confiável.

https://img.shields.io/badge/License-MIT-green.svg](LICENSE)
https://img.shields.io/badge/R-%3E%3D4.3-blue](https://www.r-project.org/)
https://img.shields.io/badge/Shiny-Web%20App-lightblue](https://shiny.posit.co/)
https://img.shields.io/badge/Status-Em%20Desenvolvimento-yellow]()
https://img.shields.io/github/last-commit/SEU-USUARIO/RadarSocial](https://github.com/SEU-USUARIO/RadarSocial)

<br>

<p align="center">
  img/logo_radarsocial.png
</p>

<p align="center">
<b>Análise Inteligente de Eventos, Rejeições e Totalizadores do eSocial</b>
</p>

---

# 📖 Sobre o Projeto

O **RadarSocial** é uma aplicação desenvolvida para apoiar organizações públicas e privadas no monitoramento da qualidade das informações transmitidas ao **eSocial**.

A ferramenta permite a análise de eventos inconsistentes, rejeitados e processados pelo ambiente nacional do eSocial, oferecendo uma visão consolidada dos problemas identificados e dos respectivos impactos sobre os totalizadores oficiais.

Além da análise de eventos, o RadarSocial disponibiliza recursos específicos para interpretação e acompanhamento dos retornos totalizadores:

- S-5001
- S-5002
- S-5003
- S-5011
- S-5012
- S-5013

Seu objetivo é transformar informações técnicas e dispersas em conhecimento de fácil compreensão para gestores, equipes de RH, departamentos de pessoal, contabilidade e governança corporativa.

---

# 🎯 Objetivos

- Identificar eventos rejeitados pelo eSocial;
- Detectar inconsistências cadastrais e funcionais;
- Facilitar a análise dos retornos do ambiente nacional;
- Apoiar a correção de erros antes de impactos financeiros;
- Melhorar a qualidade dos dados trabalhistas, previdenciários e tributários;
- Fornecer indicadores de conformidade;
- Apoiar auditorias internas e externas;
- Reduzir retrabalho operacional.

---

# ✨ Funcionalidades

- 📥 Importação de arquivos e relatórios do eSocial;
- 🚨 Monitoramento de eventos rejeitados;
- 🔍 Identificação automática de inconsistências;
- 📊 Dashboard de indicadores;
- 📈 Estatísticas por tipo de evento;
- 📋 Consolidação de rejeições;
- 🧾 Análise detalhada dos retornos totalizadores;
- 🔎 Pesquisa avançada;
- 📑 Exportação de relatórios;
- 📚 Histórico de processamento;
- 📌 Classificação de criticidade dos erros;
- ✅ Acompanhamento das correções realizadas.

---

# 📊 Totalizadores Monitorados

O RadarSocial oferece recursos específicos para análise dos seguintes totalizadores:

| Evento | Descrição |
|----------|------------|
| S-5001 | Informações das contribuições sociais por trabalhador |
| S-5002 | Imposto de Renda Retido na Fonte |
| S-5003 | FGTS por trabalhador |
| S-5011 | Consolidação das contribuições sociais |
| S-5012 | Imposto de Renda consolidado |
| S-5013 | FGTS consolidado |

---

# 👥 Público-Alvo

A solução foi concebida para utilização por:

## Setor Público

- Tribunais;
- Ministérios;
- Autarquias;
- Fundações;
- Prefeituras;
- Governos Estaduais;
- Empresas Públicas;
- Sociedades de Economia Mista.

## Setor Privado

- Empresas de médio e grande porte;
- Escritórios de contabilidade;
- Consultorias trabalhistas;
- Empresas de terceirização;
- Organizações com grande volume de vínculos.

---

# ✅ Benefícios

- Aumento da conformidade perante o eSocial;
- Melhoria da qualidade dos dados;
- Redução de rejeições;
- Menor risco de passivos trabalhistas e previdenciários;
- Maior segurança na gestão da folha de pagamento;
- Apoio à tomada de decisão;
- Fortalecimento da governança de dados.

---

# 🛠 Tecnologias Utilizadas

```r
R
Shiny
shinydashboard
dplyr
DT
plotly
ggplot2
lubridate
stringr
readr
purrr
tidyr
```

---

# 📂 Estrutura do Projeto

```text
RadarSocial/
│
├── app/
│   ├── ui.R
│   ├── server.R
│   └── global.R
│
├── data/
├── docs/
├── img/
│   └── logo_radarsocial.png
│
├── relatorios/
├── scripts/
├── tests/
│
├── README.Rmd
├── README.md
├── LICENSE
└── .gitignore
```

---

# 🚀 Instalação

## Clonar o repositório

```bash
git clone https://github.com/SEU-USUARIO/RadarSocial.git
```

## Instalar dependências

```r
install.packages(
  c(
    "shiny",
    "shinydashboard",
    "dplyr",
    "DT",
    "plotly",
    "ggplot2",
    "lubridate",
    "stringr",
    "readr",
    "purrr",
    "tidyr"
  )
)
```

---

# ▶️ Execução

```r
shiny::runApp()
```

---

# 🏛 Governança e Conformidade

O RadarSocial foi concebido para fortalecer os processos de controle, auditoria e governança relacionados às obrigações trabalhistas, previdenciárias e tributárias transmitidas ao eSocial.

A solução contribui para:

- Integridade dos dados;
- Transparência institucional;
- Conformidade regulatória;
- Eficiência operacional;
- Governança da informação;
- Prevenção de inconsistências e passivos.

---

# 👨‍💻 Desenvolvedores

## Kleberson Carlos Pinto

**Técnico Judiciário - Programação de Sistemas**  
Tribunal de Justiça do Estado de Sergipe (TJSE)

## Edison Carvalho

**Técnico Judiciário - Programação de Sistemas**  
Tribunal de Justiça do Estado de Sergipe (TJSE)

---

# 🏛 Instituição

**Tribunal de Justiça do Estado de Sergipe (TJSE)**

---

# 📜 Licença

## Licença MIT

Copyright (c) 2026 Kleberson Carlos Pinto e Edison Carvalho

É concedida permissão, gratuitamente, a qualquer pessoa que obtenha uma cópia deste software e dos arquivos de documentação associados ("Software"), para utilizar o Software sem restrição, incluindo, sem limitação, os direitos de usar, copiar, modificar, mesclar, publicar, distribuir, sublicenciar e/ou vender cópias do Software, e permitir que as pessoas a quem o Software seja fornecido façam o mesmo, sujeito às seguintes condições:

O aviso de copyright acima e esta permissão deverão ser incluídos em todas as cópias ou partes substanciais do Software.

O SOFTWARE É FORNECIDO "NO ESTADO EM QUE SE ENCONTRA", SEM GARANTIA DE QUALQUER NATUREZA, EXPRESSA OU IMPLÍCITA, INCLUINDO, MAS NÃO SE LIMITANDO ÀS GARANTIAS DE COMERCIALIZAÇÃO, ADEQUAÇÃO A UM DETERMINADO PROPÓSITO E NÃO VIOLAÇÃO. EM NENHUMA HIPÓTESE OS AUTORES OU DETENTORES DOS DIREITOS AUTORAIS SERÃO RESPONSÁVEIS POR QUALQUER RECLAMAÇÃO, DANO OU OUTRA RESPONSABILIDADE, SEJA EM AÇÃO CONTRATUAL, ILÍCITO CIVIL OU OUTRA FORMA, DECORRENTE DE, OU EM CONEXÃO COM O SOFTWARE OU O USO OU OUTRAS NEGOCIAÇÕES NO SOFTWARE.

---

# 📚 Justificativa da Licença MIT

O RadarSocial adota a Licença MIT por entender que soluções voltadas à melhoria da qualidade das informações transmitidas ao eSocial devem incentivar a colaboração, a transparência e o compartilhamento de conhecimento entre organizações públicas e privadas.

A utilização dessa licença permite que órgãos governamentais, empresas e instituições adaptem e ampliem a ferramenta conforme suas necessidades específicas, preservando o reconhecimento dos autores e fomentando a evolução contínua da solução.

A escolha da Licença MIT está alinhada aos princípios de:

- Cooperação institucional;
- Compartilhamento de conhecimento;
- Transparência tecnológica;
- Inovação colaborativa;
- Governança de dados;
- Modernização da Administração Pública.

---

# 🤝 Contribuições

Sugestões, correções e melhorias podem ser registradas por meio da seção **Issues** deste repositório.

Contribuições são bem-vindas.

---

# 📌 Slogan

> **RadarSocial**
>
> *Transformando dados do eSocial em informação confiável.*

---

<p align="center">
Desenvolvido no Tribunal de Justiça do Estado de Sergipe (TJSE).
</p>
