pacman::p_load(
    "readxl", "dplyr", "ggplot2", "tidyr",
    "kableExtra", "ggcorrplot", "psych", "purrr",
    "caret", "gvlma", "lmtest", "leaps"
)
source("funcoes_aux.r", encoding = "utf8")


data <- read_xls("dados.xls") %>%
    rename_all(~ c(
        "id", "Cidade", "Estado", "Área da Cidade",
        "População", "População18_34", "População65",
        "Médicos", "Leitos", "Crimes",
        "EM_completo", "Bacharéis", "Pobres",
        "Desempregados", "Renda p/c", "Renda Total",
        "Região geográfica"
    )) %>%
    select(-c(id, Cidade, Estado)) %>%
    mutate("Região geográfica" = factor(`Região geográfica`, levels = 1:4))

n <- nrow(data)
set.seed(2022)
train_i <- sample(1:n, n / 2, replace = FALSE)
val_i <- which(!(1:n %in% train_i))

train_df <- data[train_i, ]
val_df <- data[val_i, ]

data_sem_regiao <- select(train_df, -`Região geográfica`)

# visualmente lineares
lin <- c("Crimes", "Leitos", "População", "Renda Total")
train_df_medicos <- select(train_df, c("Médicos", "Região geográfica", lin)) %>% dummy_reg()
val_df_medicos <- select(val_df, c("Médicos", "Região geográfica", lin)) %>% dummy_reg()

intercepto_medicos <- lm(Médicos ~ 1, data = train_df_medicos)
completo_medicos <- lm(Médicos ~ ., data = train_df_medicos)
both_medicos <- step(intercepto_medicos, direction = "both", scope = formula(completo_medicos), trace = 1)

both_medicos$coefficients
both_medicos$anova

val_mod <- lm(formula = Médicos ~ `Renda Total` + Leitos + População, data = val_df_medicos)
val_fit <- predict(both_medicos, val_df_medicos)
val_res <- val_df_medicos$Médicos - val_fit

val_SSTO <- sum((val_df_medicos$Médicos - mean(val_df_medicos$Médicos))^2)
val_SSE <- sum(val_res)^2
val_R2a <- 1 - (n / 2 - 1) / (n - length(val_mod$coefficients)) * val_SSE / val_SSTO

final_mod <- lm(formula = Médicos ~ `Renda Total` + Leitos + População, data = data)

#---------------------------------
data_crimes <- data %>%
    mutate("Crimes" = Crimes / População * 1e5) %>%
    select(-População)

tmp <- data_crimes[["Região geográfica"]]
data_crimes <- select(data_crimes, -`Região geográfica`) %>%
    log() %>%
    mutate("Região geográfica" = tmp)

train_df_crimes <- data[train_i, ] %>% dummy_reg()
val_df_crimes <- data[val_i, ] %>% dummy_reg()


selecao_crimes <- regsubsets(Crimes ~ ., data = train_df_crimes, nbest = 4)
resumo_sel_crimes <- summary(selecao_crimes)
n_parametros <- as.numeric(rownames(resumo_sel_crimes$which)) + 1

selecao_crimes_tabular <- regsubsets(Crimes ~ ., data = train_df_crimes, nbest = 2)
resumo_tab_crimes <- summary(selecao_crimes_tabular)
variaveis <- names(train_df_crimes)
n_vars <- length(variaveis)
mantidas <- apply(resumo_tab_crimes$which, 1, function(x) {
    paste0(variaveis[x[2:(n_vars)]], collapse = ", ")
})