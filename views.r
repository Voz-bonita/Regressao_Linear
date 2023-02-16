head(select(data, -c(Médicos, População, Leitos, `Renda Total`))[, 1:5]) %>%
    format_tab("\\label{table:head}Algumas observações do banco de dados disponível.", digits = 2, "latex")

head(select(data, -c(Médicos, População, Leitos, `Renda Total`))[, 6:9]) %>%
    format_tab("Algumas observações do banco de dados disponível.", digits = 2, "latex")

head(select(data, c(Médicos, População, Leitos, `Renda Total`))) %>%
    format_tab("Algumas observações do banco de dados disponível.", digits = 2, "latex")