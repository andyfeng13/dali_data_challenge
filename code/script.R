library(tidyverse)
library(lubridate)
library(ggplot2)
library(maps)
library(usmap)
library(ggthemes)

df <- read_csv("Sample - Superstore.csv") %>% 
  mutate(`Order Date` = mdy(`Order Date`),
         `Ship Date` = mdy(`Ship Date`),
         State = tolower(State))

colnames(df)

# Orders by state ---------------------------------------------------------
df1 <- df %>% count(State)
map <- map_data("state") %>% inner_join(df1, by = c("region" = "State"))

ggplot(map) +
  geom_polygon(aes(long, lat, group = group, fill = n), color = "gray60") +
  theme_map() +
  scale_fill_continuous_tableau() +
  labs(title = "Total Orders by State",
       subtitle = "California, New York, and Texas have the most orders",
       fill = "Total Orders") +
  theme(legend.position = "bottom",
        plot.title = element_text(size=16))

ggsave("figures/orders_by_state.pdf", width = 8, height = 6)


# Orders over time --------------------------------------------------------
df1 <- df %>% 
  mutate(month = month(`Order Date`), year = year(`Order Date`)) %>% 
  group_by(year, month) %>% 
  summarise(n = n(), sales = sum(Sales), profit = sum(Profit))

ggplot(df1) +
  geom_col(aes(month, n, fill = sales)) +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  facet_wrap(~year) +
  theme_bw() +
  labs(title = "Total Orders by Month and Year",
       subtitle = "People buy more and sales are highest near the year end",
       x = "Month", y = "Total Orders", fill = "Total Sales")

ggsave("figures/orders_by_time.pdf", width = 8, height = 6)


# Bar chart of sales/profit by region -------------------------------------
df1 <- df %>% group_by(Region) %>% 
  summarise(Sales = sum(Sales), Profit = sum(Profit)) %>% 
  pivot_longer(cols = c(Sales, Profit), names_to = "type") %>% 
  mutate(type = fct_relevel(type, c("Sales", "Profit")),
         value = value / 100000)

ggplot(df1) +
  geom_col(aes(reorder(Region, -value), value, fill=type), position = "dodge") +
  scale_fill_manual(values = c("#f4b41a", "#143d59")) +
  theme_clean() +
  labs(title = "Sales and Profits by Region", fill = NULL,
       x = "Region", y = "Total Amount (in hundred thousands)") +
  theme(legend.position = c(.81, .88))

ggsave("figures/sales_profit_by_region.pdf", width = 8, height = 6)


# Most profitable products -----------------------------------------------
df1 <- df %>% group_by(`Sub-Category`, Category) %>% 
  summarise(sales = sum(Sales), profit = sum(Profit)) %>% ungroup() %>% 
  mutate(pct = profit/sales)

ggplot(df1) +
  geom_col(aes(reorder(`Sub-Category`, pct), pct, fill = Category), alpha = .8) +
  scale_fill_manual(values = c("#C03C11", "#076A73", "#021E51")) +
  coord_flip() + 
  theme_bw() +
  labs(title = "Most and Least Profitable Products per Order",
       subtitle = "Office supplies are more profitable while furniture is least",
       y = "Profit / Sales", x = NULL, fill = "Category")

ggsave("figures/most_profitable.pdf", width = 8, height = 6)


# Orders by ship mode -----------------------------------------------------
df1 <- df %>% count(`Ship Mode`) %>% 
  mutate(pct = round(n/sum(n), 3) * 100,
         `Ship Mode` = 
           fct_relevel(`Ship Mode`, c("Same Day", "First Class",
                                      "Second Class", "Standard Class")))

ggplot(df1, aes(x="", y=pct, fill=`Ship Mode`)) +
  geom_bar(color= "grey40", stat = "identity") +
  coord_polar("y", start=0) +
  geom_text(aes(label = pct),
            position = position_stack(vjust = 0.5), size=5) +
  theme_void() +
  scale_fill_brewer()+
  labs(title = "Most Popular Shipping Modes",
       subtitle = "Standard shipping is the overwhelming majority",
       fill = "Ship Mode")

ggsave("figures/ship_modes.pdf", width = 8, height = 6)


# Profits by city start letter --------------------------------------------
df1 <- df %>% 
  mutate(letters = str_extract(City, "."),
         start = ifelse(letters=="P", 1, 0))
model <- lm(Profit ~ start, df1)
summary(model)

df2 <- df1 %>% 
  group_by(letters) %>% 
  summarise(profit = sum(Profit), start) %>% distinct()

ggplot(df2) +
  geom_col(aes(reorder(letters, profit), profit, fill = start)) +
  coord_flip() + 
  theme_light() +
  labs(title = "Profits vs City Start Letter",
       y = "Total Profits", x = NULL,
       subtitle = "Cities that start with 'p' have greater profit losses") +
  theme(legend.position = "none")

ggsave("figures/profits_by_letter.pdf", width = 8, height = 6)


# Discounts by category ---------------------------------------------------
model <- lm(Profit ~ Discount, df)
summary(model)

df1 <- df %>% 
  group_by(`Sub-Category`) %>% 
  summarise(mean = mean(Discount) * 100,
            sales = sum(Sales)/1000, profit = sum(Profit)/1000)

ggplot(df1) +
  geom_point(aes(sales, profit, color = `Sub-Category`, size = mean)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#e00006", size = .8) +
  annotate("text", x = 315, y = -3, label = "Zero-profits") +
  ggrepel::geom_label_repel(aes(sales, profit, label = `Sub-Category`),
                            hjust = 1, vjust = .5, size = 3) +
  theme_light() +
  scale_y_continuous(limits = c(-20, 60), breaks = seq(-20, 60, 20),
                     labels = c("Profit loss", "0", "20", "40", "Profit gain")) +
  scale_size_continuous(range = c(1, 8)) +
  guides(color = "none") +
  labs(title = "Sales and Profits by Category",
       subtitle = "Larger circles indicate greater discounts on average",
       x = "Total Sales (in thousands)", y = "Total Profits (in thousands)",
       size = "Average \nDiscount (%)")

ggsave("figures/profits_by_category.pdf", width = 8, height = 6)







