pacman::p_load(
    "readxl", "dplyr", "ggplot2", "tidyr",
    "kableExtra", "ggcorrplot", "psych", "purrr"
)
source("funcoes_aux.r")

data <- read_xls("dados.xls") %>%
    rename_all(~ c(
        "id", "Cidade", "Estado", "Área da Cidade",
        "População", "População18_34", "População65",
        "Médicos", "Leitos", "Crimes",
        "EM_completo", "Bacharéis", "Pobres",
        "Desempregados", "Renda p/c", "Renda Total",
        "Região geográfica"
    )) %>%
    select(-c(id, Cidade, Estado, `Região geográfica`))

# visualmente lineares
lin <- c("Crimes", "Leitos", "População", "Renda Total")
# visualmente não lineares
non_lin <- names(data)[!(names(data) %in% lin)]

cor_matrix_plot(data)
cor_matrix_plot(select(data, non_lin))
cor_matrix_plot(select(data, lin))

model_full <- lm(data = data, medicos ~ .)