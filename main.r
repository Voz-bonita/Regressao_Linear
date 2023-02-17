pacman::p_load(
    "readxl", "dplyr", "ggplot2", "tidyr",
    "kableExtra", "ggcorrplot", "psych", "purrr",
    "caret"
)
source("funcoes_aux.r", encoding="utf8")


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
both_medicos$anova