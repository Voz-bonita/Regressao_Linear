pacman::p_load(
    "readxl", "dplyr", "ggplot2", "tidyr",
    "kableExtra", "ggcorrplot"
)
source("funcoes_aux.r")

data <- read_xls("dados.xls") %>%
    rename_all(~ c(
        "id", "cidade", "estado", "area",
        "populacao", "pop18_34", "pop65",
        "medicos", "leitos", "crimes",
        "EM_completo", "bachareis", "pobres",
        "desempregados", "renda_pc", "renda_total",
        "regiao"
    )) %>%
    select(-c(id, cidade, estado, regiao))

ggplot(
    gather(select(data, -medicos)),
    aes(x = value, y = rep(data$medicos, ncol(data) - 1))
) +
    geom_point() +
    facet_wrap(~key, scales = "free_x") +
    theme_bw()

# visualmente lineares
lin <- c("crimes", "leitos", "populacao", "renda_total")

cov_matrix_plot(data)