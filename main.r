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
    select(-c(id, Cidade, Estado))

data$`Região geográfica` <- factor(data$`Região geográfica`, levels = 1:4)
data_sem_regiao <- select(data, -`Região geográfica`)
# visualmente lineares
lin <- c("Crimes", "Leitos", "População", "Renda Total")
# visualmente não lineares
non_lin <- names(data)[!(names(data) %in% lin)]

cor_matrix_plot(data_sem_regiao)
cor_matrix_plot(select(data_sem_regiao, non_lin))
cor_matrix_plot(select(data_sem_regiao, lin))