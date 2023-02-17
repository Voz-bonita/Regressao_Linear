head(select(data_sem_regiao, -c(Médicos, População, Leitos, `Renda Total`))[, 1:5]) %>%
    format_tab("Algumas observações do banco de dados disponível.", digits = 2, "latex")

head(select(data_sem_regiao, -c(Médicos, População, Leitos, `Renda Total`))[, 6:9]) %>%
    format_tab("Algumas observações do banco de dados disponível.", digits = 2, "latex")

head(select(data_sem_regiao, c(Médicos, População, Leitos, `Renda Total`))) %>%
    format_tab("\\label{table:head}Algumas observações do banco de dados disponível.", digits = 2, "latex")

describe(data_sem_regiao) %>%
    select(-c(vars, n, trimmed, mad, range)) %>%
    rename_all(~ c(
        "Média", "DP", "Mediana",
        "Min", "Max", "Skew", "Kurtosis", "EP"
    )) %>%
    format_tab("\\label{table:desc}Medidas descritivas das variáveis mantidas durante o estudo.", "latex", digits = 0)

(ggplot(
    gather(select(data_sem_regiao, -Médicos)),
    aes(x = value, y = rep(data_sem_regiao$Médicos, ncol(data_sem_regiao) - 1))
) +
    geom_point() +
    facet_wrap(~key, scales = "free_x") +
    theme_bw() +
    ylab("Número de médicos") +
    xlab("Valor")) %>%
    ggsave(filename = "assets/dispersao_y.png", .)

(ggplot(
    pivot_longer(data, cols = -c(`Região geográfica`, Médicos)),
    aes(x = log(value), y = log(Médicos), color = `Região geográfica`)
) +
    geom_point() +
    facet_wrap(~name, scales = "free_x") +
    theme_bw() +
    ylab("Número de médicos") +
    xlab("Valor") +
    theme(legend.position = "bottom")) %>%
    ggsave(filename = "assets/dispersao_logxlogy.png", .)


cor(data[, lin], data$Médicos) %>%
    cbind(cor(log(data[, lin]), log(data$Médicos))) %>%
    as.data.frame() %>%
    rename_all(~ c("r_1", "r_2")) %>%
    mutate("$Z_c$" = (atanh(r_1) - atanh(r_2)) / sqrt(2 / 437)) %>%
    round(2) %>%
    mutate("P(Z > $Z_c$)" = pnorm(`$Z_c$`, lower.tail = F)) %>%
    format_tab("\\label{tab:corteste}Teste de Fisher para correlação linear simples entre o número de médicos e algumas variáveis explicativas.", format = "latex")

ggsave(filename = "assets/cor_medicos.png", cor_matrix_plot(select(data_sem_regiao, lin)))