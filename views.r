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
    theme_bw() + ylab("Número de médicos") + xlab("Valor")) %>%
    ggsave(filename = "assets/dispersao_y.png", .)

(ggplot(
    pivot_longer(data, cols = -c(`Região geográfica`, Médicos)),
    aes(x = log(value), y = log(Médicos), color = `Região geográfica`)) +
    geom_point() +
    facet_wrap(~name, scales = "free_x") +
    theme_bw() + ylab("Número de médicos") + xlab("Valor") +
    theme(legend.position = "bottom")) %>%
    ggsave(filename = "assets/dispersao_logxlogy.png", .)

c1 <- cor(data$População, data$Médicos)
c2 <- cor(log(data$População), log(data$Médicos))
pnorm((atanh(c1) - atanh(c2))/sqrt(2/437), lower.tail = F)