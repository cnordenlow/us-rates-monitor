#####################################################################################################
### Script for parsing rates
#####################################################################################################

### Info
#Data have been parsed once and for each time it is parsed, it is saved in a csv-file. Look over how it think around year turn.


# ------------------------------------------------------------------------------- 
# Pages, years to parse, create empty df
# ------------------------------------------------------------------------------- 

pages = c("https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yieldYear&year=",
        # "https://www.treasury.gov/resource-center/data-chart-center/interest-rates/pages/TextView.aspx?data=billRatesYear&year=",
        "https://www.treasury.gov/resource-center/data-chart-center/interest-rates/pages/TextView.aspx?data=realyieldYear&year=")

#base_url = "https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yieldYear&year="

years = year(Sys.Date())

#years = c("2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009","2008")

#des_name = sub('.*data=', '', base_url)
#des_name = sub('&year*.', '', des_name)

tableUsRates <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(tableUsRates) <- c( 'date', 'term', 'rate', 'des')


# ------------------------------------------------------------------------------- 
# Parsing function
# ------------------------------------------------------------------------------- 
parse_us_rates <- function(year) {
  url = paste(base_url, year, sep="")
  parse_rates <- read_html(url)
  
  df <- parse_rates %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  df <- df[[2]]
  return(df)
  
}

# ------------------------------------------------------------------------------- 
# Parse all pages and dates
# ------------------------------------------------------------------------------- 
for (base_url in pages) {

des_name = sub('.*data=', '', base_url)
des_name = sub('&year*.', '', des_name)
  

  for (year in years) {
    Sys.sleep(sample(1:3, 1, replace=T))
    df <- parse_us_rates(year)
    
    
    #rename first column to date
    df <- rename(df, "date" = 1)
    df$date <- as.Date(df$date, format = "%m/%d/%y") ##convert dates
    
    ###Create df with dates, from first each year to last, or to sys.date during current

    first_date = paste(year,"-01-01",sep="")
    if (paste(year,"-12-31",sep="") < Sys.Date()) {
      last_date = paste(year,"-12-31",sep="")
    } else {
      last_date = Sys.Date()
    }

    dates <- data.frame(
      date = seq(as.Date(first_date), as.Date(last_date), by = 'days')
    )
    df <- merge(dates, df, by ="date", all.x = TRUE)
    
    df <- df %>%
      fill(names(df), .direction = "down") %>%
      fill(names(df), .direction = "up")
    
    
    
    ###dplyr table
    df <- df %>%
      pivot_longer(
        !date,
        names_to = "term",
        values_to ="rate", values_ptypes = list(rate = 'character')
      ) %>%
      mutate(des = des_name) %>%
      mutate(info = "") 
  
    tableUsRates <- bind_rows(mutate_all(tableUsRates, as.character), mutate_all(df, as.character)) 
    
    #Clean

    tableUsRates$rate <- as.numeric(tableUsRates$rate)
    #df$date <- as.Date(df$date, format = "%m/%d/%y") ##convert dates
    
    
    
 
  }     


}


# ------------------------------------------------------------------------------- 
# Tidying
# ------------------------------------------------------------------------------- 
tableUsRates <- tableUsRates %>%
  mutate(t1 = as.numeric(gsub("([0-9]+).*$", "\\1", term)))%>%
  mutate(maturity = case_when(
    grepl(" yr", term, fixed = TRUE) ~ (t1 *365 / 365),
    grepl(" YR", term, fixed = TRUE) ~ (t1 *365 / 365),
    
    #bills
    grepl("1 mo", term, fixed= TRUE) ~ (4 * 7 /365), ##365 days because bond equivalent
    grepl("2 mo", term, fixed= TRUE) ~ (8 * 7 /365), ##365 days because bond equivalent
    grepl("3 mo", term, fixed= TRUE) ~ (13 * 7 /365), ##365 days because bond equivalent
    grepl("6 mo", term, fixed= TRUE) ~ (26 * 7 /365) ##365 days because bond equivalent
    
  ))%>%

  ##Fix term column so names are the same
  mutate(term = case_when(
    grepl(" YR", term, fixed = TRUE) ~ paste(t1, "y", sep=""),
    grepl(" yr", term, fixed = TRUE) ~ paste(t1, "y", sep=""),
    grepl(" m", term, fixed = TRUE) ~ paste(t1, "m", sep="")
  ))%>%
  select(-t1)



###Delete dates tableUsRates false values
tableUsRates <- tableForShiny %>%
  filter(date < "2017-04-14" | date > "2017-04-16") 

