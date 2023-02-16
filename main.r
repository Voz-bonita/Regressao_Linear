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