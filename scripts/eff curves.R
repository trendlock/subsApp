
x <- list.files("/Users/rosseji/Downloads", pattern = "saved_jet_input", full.names = T)  %>% 
  map2(c("a", "b", "c"), ~ read_rds(.x) %>% 
         as_tibble() %>% 
         mutate(id = .y)) %>% 
  bind_rows()

x <- x %>% 
  mutate(kts = c(df$kts, df$kts, df$kts))

ggplot(x, aes(x = kts, y= value, col = id)) +
  geom_line()


smth <- x %>% 
  split(.$id) %>% 
  map( ~ lm(pull(.x, value) ~ poly(pull(.x, kts), 3))$fitted.values) %>% 
  unlist()
y <- x %>% 
  mutate(
         eff = smth)
ggplot(y, aes(x = kts, y= eff, col = id)) +
  geom_line()



# prop
n <-  5
x <- list.files("/Users/rosseji/Downloads", pattern = "saved_prop_input", full.names = T)  %>% 
  map2(letters[1:n], ~ read_rds(.x) %>% 
         as_tibble() %>% 
         mutate(id = .y)) %>% 
  bind_rows()

x <- x %>% 
  mutate(kts = rep(df$kts, n))

ggplot(x, aes(x = kts, y= value, col = id)) +
  geom_line() +
  ylim(0, 1)

ls <- letters[1:n] %>% 
  map( ~ )

a <- lm(x %>% filter(id == "a") %>% pull(value) ~ poly(pull(df, kts), 5))$fitted.values
b <- lm(x %>% filter(id == "b") %>% pull(value) ~ poly(pull(df, kts), 4))$fitted.values
c <- lm(x %>% filter(id == "c") %>% pull(value) ~ poly(pull(df, kts), 3))$fitted.values
d <- lm(x %>% filter(id == "d") %>% pull(value) ~ poly(pull(df, kts), 4))$fitted.values
e <- lm(x %>% filter(id == "e") %>% pull(value) ~ poly(pull(df, kts), 3))$fitted.values


new_vec <- letters[1:n] %>% 
  map( ~ x %>%  )

y <- x %>% 
  mutate(eff = c(a, 
                 b,
                 c,
                 d,
                 e
                 ))

ggplot(y, aes(x = kts, y= eff, col = id)) +
  geom_line() +
  ylim(0, 1)

plotly::ggplotly()
