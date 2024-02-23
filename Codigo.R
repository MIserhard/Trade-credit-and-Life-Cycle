

#Local dos arquivos
setwd("C:/Users/iserh/OneDrive/Área de Trabalho/Outros/Mestrado/Dissertação/Trabalho/Dados")

#Pacotes necessários para as análises.
library(tidyverse)
library(DescTools)
library(ggcorrplot)
library(lfe)
library(stargazer)
library(car)

#Importação dos dados
df_DadosEco <- read_csv("Dados_Economatica.csv")
df_SetorEco <- read_csv("Economatica_Screening.csv", locale = locale(encoding = "latin1"))
df_DadosFS <- read_csv("Factset_screening_long.csv")
df_IdadeFS <- read_csv("Factset_idade.csv")


#Organização dos dados
#Manipular a base de dados da Factset (df_DadosFS) para criar uma variável year e para que as variáveis de interesse (RandD e Analysts) fiquem nas colunas.
df_DadosFS <- df_DadosFS %>% 
  pivot_longer(-c(FS_code:Variable), names_to = "year", values_to = "Value") %>%
  pivot_wider(names_from = "Variable", values_from = "Value") %>% 
  mutate(year = as.numeric(year))

#Manipular a base de dados da Factset contendo idade das firmas (df_IdadeFS) para criar uma variável year e para que a variável de interesse (firm_age) fique na coluna.
df_IdadeFS <- df_IdadeFS %>% 
  pivot_longer(-c(FS_code:Foundation), names_to = "year", values_to = "firm_age") %>%
  mutate(year = as.numeric(year))

#Tratamento de R&D
#Substituir todos os valores NA na variável RandD por zero. No Brasil, muitas empresas que não investem em pesquisa e desenvolvimento acabam por não informar que gastam zero. Isso gera valores faltantes nas bases de dados.

df_DadosFS <- df_DadosFS %>% 
  mutate_at(vars(RandD), ~ replace(., is.na(.), 0))

#Juntando as bases
#Juntar todas as bases de dados em uma só.

df_Dados <- df_DadosEco %>%
  left_join(df_SetorEco, by = "ticker") %>% 
  left_join(df_DadosFS, by = c("ticker", "year")) %>% 
  left_join(df_IdadeFS, by = c("FS_code", "year"))


#Criação de variáveis
#Criar as variáveis de interesse para a pesquisa usando o comando mutate.

df_Dados <- df_Dados %>% 
  mutate(cv_int = if_else(fco < 0 & fci < 0 & fcf > 0, 1, 0),
         cv_cre = if_else(fco > 0 & fci < 0 & fcf > 0, 1, 0),
         cv_mat = if_else(fco > 0 & fci < 0 & fcf < 0, 1, 0),
         cv_dec = if_else(fco < 0 & fci > 0, 1, 0),
         cv_con = if_else(cv_int == 0 & cv_cre == 0 & cv_mat == 0 & cv_dec == 0, 1, 0),
         forn_rl = if_else(receita > 0, fornec / receita, NA_real_),
         forn_cpv = if_else(cpv > 0, fornec / cpv, NA_real_),
         roe = if_else(pl > 0, ll / pl, NA_real_),
         logMktCap = if_else(vm > 0, log(vm), NA_real_),
         mtb = if_else(pl > 0, vm / pl, NA_real_),
         alav = if_else(at > 0, (endivCP + endivLP) / at, NA_real_),
         altman = if_else(at > 0, (1.2 * ((ac - pc) / at) + 1.4 * (reservaLucr / at) + 3.3 * (ebit / at) + 0.6 * (vm / (at - pl)) + (receita / at)), NA_real_),
         size = if_else(at > 0, log(at), NA_real_),
         RD_at = if_else(at > 0, RandD / at, NA_real_),
         cobJuros = if_else(despFin > 0, ebit / despFin, NA_real_))

#Filtrando observações
#Filtrar empresas com base em critérios de interesse.

df_Dados <- df_Dados %>%
  filter(at > 1000,
         pl > 1000,
         receita > 1000)


#Setores com base nos dígitos SIC
#Cria uma variável setor que são os primeiros dois dígitos no SIC number. Ela vai ficar gravada como texto, apesar de aparecer como valores numéricos. Ela pode ser usada no lugar da variável NAICS. Como pode ser visto em seguida, a quantidade de setores diferentes com base nos dois primeiros dígitos do SIC code é bem maior que a quantidade de setores NAICS no nível 1.

df_Dados <- df_Dados %>% 
  mutate(SIC_Code = na_if(SIC_Code, "@NA")) %>% 
  mutate(SIC_2dig = as.character(substr(SIC_Code, 1, 2)))

length(table(df_Dados$naics1))

length(table(df_Dados$SIC_2dig))



#Estatísticas descritivas
#Winsorização
#O código a seguir realiza as seguintes operações:
#1. Seleciona apenas as variáveis que vão ser úteis para as análises.
#2. Elimina as observações com dados faltantes
#3. Winsoriza as variáveis
#4. Calcula as estatísticas descritivas para as variáveis Winsorizadas.

df_DadosW <- df_Dados %>%
  select(ticker, year, naics1, SIC_2dig, analysts, firm_age, cv_int:cobJuros) %>%
  filter(complete.cases(.)) %>%
  mutate(forn_rl = Winsorize(forn_rl, probs = c(0.01, 0.99), na.rm = TRUE),
         forn_cpv = Winsorize(forn_cpv, probs = c(0.01, 0.99), na.rm = TRUE),
         roe = Winsorize(roe, probs = c(0.01, 0.99), na.rm = TRUE),
         logMktCap = Winsorize(logMktCap, probs = c(0.01, 0.99), na.rm = TRUE),
         mtb = Winsorize(mtb, probs = c(0.01, 0.99), na.rm = TRUE),
         alav = Winsorize(alav, probs = c(0.01, 0.99), na.rm = TRUE),
         altman = Winsorize(altman, probs = c(0.01, 0.99), na.rm = TRUE),
         size = Winsorize(size, probs = c(0.01, 0.99), na.rm = TRUE),
         RD_at = Winsorize(RD_at, probs = c(0.01, 0.99), na.rm = TRUE),
         cobJuros = Winsorize(cobJuros, probs = c(0.01, 0.99), na.rm = TRUE))

df_DadosW %>%
  pivot_longer(-c(ticker:SIC_2dig), names_to = "Var", values_to = "Valor") %>%
  group_by(Var) %>%
  summarise(Obs = sum(!is.na(Valor)),
            Mean = round(mean(Valor, na.rm = TRUE), 3),
            SD = round(sd(Valor, na.rm = TRUE), 3),
            Min = round(min(Valor, na.rm = TRUE), 3),
            Q1 = round(quantile(Valor, probs = 0.25, na.rm = TRUE), 3),
            Median = round(median(Valor, na.rm = TRUE), 3),
            Q3 = round(quantile(Valor, probs = 0.75, na.rm = TRUE), 3),
            Max = round(max(Valor, na.rm = TRUE), 3))


# Correlações
# Matriz de correlação das variáveis

df_Correl <- df_DadosW %>%
  select(cv_int:cobJuros)
mat_Correl <- round(cor(df_Correl, use = "complete.obs"), 2)
mat_Correl


# Visualização gráfica:
  
  ggcorrplot(mat_Correl, hc.order = TRUE, type = "lower",
             outline.col = "white", ggtheme = theme_gray,
             lab = TRUE, colors = c("#6D9EC1", "white", "#E46726")) +
  ylab(NULL) + xlab(NULL) +
  theme_minimal()

  
  
  
# Regressões
# Estimações com logmktCAP
 reg_FE_1 <- felm(forn_cpv ~ cv_int + cv_cre + cv_mat + cv_dec + roe + alav
                   + cobJuros + altman + logMktCap + mtb + RD_at
                   + firm_age + analysts | ticker + year | 0 | SIC_2dig, 
                   data = df_DadosW)
 reg_FE_2 <- felm(forn_cpv ~ cv_int + cv_cre + cv_mat + cv_dec + roe + alav
                   + cobJuros + altman + logMktCap + mtb + RD_at
                   + firm_age + analysts | ticker + factor(SIC_2dig):factor(year) 
                   | 0 | SIC_2dig, data = df_DadosW)
 reg_FE_3 <- felm(forn_rl ~ cv_int + cv_cre + cv_mat + cv_dec + roe + alav
                   + cobJuros + altman + logMktCap + mtb + RD_at
                   + firm_age + analysts | ticker + year | 0 | SIC_2dig, 
                   data = df_DadosW)
 reg_FE_4 <- felm(forn_rl ~ cv_int + cv_cre + cv_mat + cv_dec + roe + alav
                   + cobJuros + altman + logMktCap + mtb + RD_at
                   + firm_age + analysts | ticker + factor(SIC_2dig):factor(year)
                   | 0 | SIC_2dig, data = df_DadosW)
  
 
 
 
#Stargazer table
stargazer(reg_FE_1, reg_FE_2, reg_FE_3, reg_FE_4,
           omit.stat = c("f", "ser"),
           add.lines = list(c("Firm FE", "Yes", "Yes", "Yes", "Yes"),
                            c("Year FE", "Yes", "No", "Yes", "No"),
                            c("Firm x Year FE", "No", "Yes", "No", "Yes"),
                            c("Industry clustered SE", "Yes", "Yes", "Yes", "Yes")),
           type = "text")

 
 
 
#Testes de diferença entre os coeficientes - 
#Regressão 1
 
 linearHypothesis(reg_FE_1, c("cv_int - cv_cre = 0"))
 linearHypothesis(reg_FE_1, c("cv_int - cv_mat = 0"))
 linearHypothesis(reg_FE_1, c("cv_int - cv_dec = 0"))
 linearHypothesis(reg_FE_1, c("cv_cre - cv_mat = 0"))
 linearHypothesis(reg_FE_1, c("cv_cre - cv_dec = 0"))
 linearHypothesis(reg_FE_1, c("cv_mat - cv_dec = 0"))
 
#Regressão 2
 linearHypothesis(reg_FE_2, c("cv_int - cv_cre = 0"))
 linearHypothesis(reg_FE_2, c("cv_int - cv_mat = 0"))
 linearHypothesis(reg_FE_2, c("cv_int - cv_dec = 0"))
 linearHypothesis(reg_FE_2, c("cv_cre - cv_mat = 0"))
 linearHypothesis(reg_FE_2, c("cv_cre - cv_dec = 0"))
 linearHypothesis(reg_FE_2, c("cv_mat - cv_dec = 0"))
 
#Regressão 3
 linearHypothesis(reg_FE_3, c("cv_int - cv_cre = 0"))
 linearHypothesis(reg_FE_3, c("cv_int - cv_mat = 0"))
 linearHypothesis(reg_FE_3, c("cv_int - cv_dec = 0"))
 linearHypothesis(reg_FE_3, c("cv_cre - cv_mat = 0"))
 linearHypothesis(reg_FE_3, c("cv_cre - cv_dec = 0"))
 linearHypothesis(reg_FE_3, c("cv_mat - cv_dec = 0"))
 
#Regressão 4
 linearHypothesis(reg_FE_4, c("cv_int - cv_cre = 0"))
 linearHypothesis(reg_FE_4, c("cv_int - cv_mat = 0"))
 linearHypothesis(reg_FE_4, c("cv_int - cv_dec = 0"))
 linearHypothesis(reg_FE_4, c("cv_cre - cv_mat = 0"))
 linearHypothesis(reg_FE_4, c("cv_cre - cv_dec = 0"))
 linearHypothesis(reg_FE_4, c("cv_mat - cv_dec = 0"))
 
 
#ALTMAN - CODIGO NOVO
 install.packages("lmtest")
 library(lmtest)
 
 
 #Segmentar em zona os dados do ALTMAN
 df_Distress <- df_DadosW %>% filter(altman <= 1.8)
 df_Safe <- df_DadosW %>% filter(altman > 2.99)

 
 
 # Regressão para cada zona
 #cpv
 reg_Distress_cpv <- felm(forn_cpv ~ cv_int + cv_cre + cv_mat + cv_dec + roe + alav
      + cobJuros + altman + logMktCap + mtb + RD_at
      + firm_age + analysts | ticker + year | 0 | SIC_2dig, 
      data = df_Distress)
 
 reg_Safe_cpv <- felm(forn_cpv ~ cv_int + cv_cre + cv_mat + cv_dec + roe + alav
                      + cobJuros + altman + logMktCap + mtb + RD_at
                      + firm_age + analysts | ticker + year | 0 | SIC_2dig, 
                      data = df_Safe)
 
 #rl
 reg_Distress_rl <- felm(forn_rl ~ cv_int + cv_cre + cv_mat + cv_dec + roe + alav
                          + cobJuros + altman + logMktCap + mtb + RD_at
                          + firm_age + analysts | ticker + year | 0 | SIC_2dig, 
                          data = df_Distress)
 
 reg_Safe_rl <- felm(forn_rl ~ cv_int + cv_cre + cv_mat + cv_dec + roe + alav
                      + cobJuros + altman + logMktCap + mtb + RD_at
                      + firm_age + analysts | ticker + year | 0 | SIC_2dig, 
                      data = df_Safe)
 
 
 # tabela - CPV
 stargazer(reg_Distress_cpv, reg_Safe_cpv,
           type = "text", 
           title = "Regression Results by Financial Distress Level - CPV",
           style = "AER",
           intercept.bottom = FALSE, # To move the intercept to the top
           omit = "Constant", # To omit the constant term from the table
           column.labels = c("High Distress", "Low Distress"),
           dep.var.caption = "", # Removes the caption for the dependent variable
           dep.var.labels.include = FALSE, # Removes the dependent variable label
           digits = 3, # Number of digits to display
           no.space = TRUE, # Remove extra space
           omit.table.layout = "n", # Remove notes
           star.cutoffs = c(0.05, 0.01, 0.001), # Significance levels for stars
           align = TRUE) # Alignment of the columns
 
  # tabela - RL
 stargazer(reg_Distress_rl, reg_Safe_rl,
           type = "text", 
           title = "Regression Results by Financial Distress Level - CPV",
           style = "AER",
           intercept.bottom = FALSE, # To move the intercept to the top
           omit = "Constant", # To omit the constant term from the table
           column.labels = c("High Distress", "Low Distress"),
           dep.var.caption = "", # Removes the caption for the dependent variable
           dep.var.labels.include = FALSE, # Removes the dependent variable label
           digits = 3, # Number of digits to display
           no.space = TRUE, # Remove extra space
           omit.table.layout = "n", # Remove notes
           star.cutoffs = c(0.05, 0.01, 0.001), # Significance levels for stars
           align = TRUE) # Alignment of the columns
 
 
 #####
 
 # Função para calcular a diferença simples dos coeficientes, o valor t e o p-valor
 calculate_diff1 <- function(coef1, se1, coef2, se2, df) {
   diff_coef_simple <- coef2 - coef1
   se_diff <- sqrt(se1^2 + se2^2)
   t_value <- diff_coef_simple / se_diff
   p_value <- 2 * pt(-abs(t_value), df)
   return(c(diff_coef_simple, t_value, p_value))
 }
 
 # Coeficientes e erros padrão para as variáveis na parte forn_cpv da tabela
 # Aqui, nós usamos c() para criar vetores diretamente
 cpv_coefs <- list(
   cv_int = c(coef1 = 0.022, se1 = 0.028, coef2 = 0.074, se2 = 0.039),
   cv_cre = c(coef1 = 0.059, se1 = 0.021, coef2 = 0.043, se2 = 0.034),
   cv_mat = c(coef1 = 0.036, se1 = 0.023, coef2 = 0.013, se2 = 0.023),
   cv_dec = c(coef1 = 0.053, se1 = 0.088, coef2 = 0.001, se2 = 0.025)
 )
 
 # Graus de liberdade estimados - substitua por seus valores reais
 df_estimated <- 120  # Este é um valor arbitrário para exemplo
 
 # Calculando as diferenças e p-valores para forn_cpv
 results_cpv <- list()
 for (var in names(cpv_coefs)) {
   coeffs <- cpv_coefs[[var]]
   results_cpv[[var]] <- calculate_diff(coeffs[1], coeffs[2], coeffs[3], coeffs[4], df_estimated)
 }
 
 # Mostrando os resultados para forn_cpv
 cat("Results for forn_cpv:\n")
 for (var in names(results_cpv)) {
   cat(var, 
       "Diferença simples dos coeficientes:", results_cpv[[var]][1], 
       "Valor t:", results_cpv[[var]][2], 
       "P-valor:", results_cpv[[var]][3], "\n")
 }
 
 
 # Função para calcular a diferença simples dos coeficientes, o valor t e o p-valor
 calculate_diff <- function(coef1, se1, coef2, se2, df) {
   diff_coef_simple <- coef2 - coef1
   se_diff <- sqrt(se1^2 + se2^2)
   t_value <- diff_coef_simple / se_diff
   p_value <- 2 * pt(-abs(t_value), df)
   return(c(diff_coef_simple, t_value, p_value))
 }
 
 # Coeficientes e erros padrão para as variáveis na parte forn_rl da tabela
 rl_coefs <- list(
   cv_int = c(coef1 = 0.022, se1 = 0.012, coef2 = 0.020, se2 = 0.012),
   cv_cre = c(coef1 = 0.019, se1 = 0.009, coef2 = 0.006, se2 = 0.006),
   cv_mat = c(coef1 = 0.008, se1 = 0.011, coef2 = -0.008, se2 = 0.004),
   cv_dec = c(coef1 = -0.001, se1 = 0.037, coef2 = -0.002, se2 = 0.007)
 )
 
 # Graus de liberdade estimados - substitua por seus valores reais
 df_estimated_rl <- 120  # Este é um valor arbitrário para exemplo
 
 # Calculando as diferenças e p-valores para forn_rl
 results_rl <- list()
 for (var in names(rl_coefs)) {
   coeffs <- rl_coefs[[var]]
   results_rl[[var]] <- calculate_diff(coeffs[1], coeffs[2], coeffs[3], coeffs[4], df_estimated_rl)
 }
 
 # Mostrando os resultados para forn_rl
 cat("Results for forn_rl:\n")
 for (var in names(results_rl)) {
   cat(var, 
       "Diferença simples dos coeficientes:", results_rl[[var]][1], 
       "Valor t:", results_rl[[var]][2], 
       "P-valor:", results_rl[[var]][3], "\n")
 }
 
 
 
 