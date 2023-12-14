library(jsonlite)
library(tidyverse)
library(dplyr)

############################################################################################################

#sport per minute merged data

############################################################################################################

# Odczyt pliku JSON
ścieżka_do_pliku_kroki_calorie_na_minute <- "C:/Users/macie/Desktop/STUDIA/SEMESTR3/Techniki Wizualizacji Danych/PROJEKTY/Project2/MM/data/HUAWEI_HEALTH_20231211144413 (1)/Sport per minute merged data & description/sport per minute merged data.json"

dane_json_kroki_calorie_na_minute <- fromJSON(ścieżka_do_pliku_kroki_calorie_na_minute)
# str(dane_json)
# if (is.list(dane_json)) {
#   ramka_danych <- do.call(rbind, dane_json)
# } else {
#   ramka_danych <- as.data.frame(dane_json)
# }
dane_json_kroki_calorie_na_minute <- dane_json_kroki_calorie_na_minute  %>%  select(-timeZone, -version)
# Przetworzenie ramki danych
kroki_calorie_na_minute <- dane_json_kroki_calorie_na_minute %>%
  # Rozwijanie kolumny sportDataUserData
  unnest_wider(sportDataUserData) %>%
  # Rozwijanie kolumny sportBasicInfos
  unnest_longer(sportBasicInfos)

# Sortowanie danych
kroki_calorie_na_minute <- kroki_calorie_na_minute %>%
  arrange(recordDay)
kroki_calorie_na_minute <- kroki_calorie_na_minute %>% select(-c(dataId, appType, sportType, sportDataSource,timeZone, startTime, endTime, deviceCode, version))

# Wyświetlenie przetworzonych danych
View(kroki_calorie_na_minute)

x <- kroki_calorie_na_minute %>% group_by(recordDay) %>% 
  summarise(n = sum(sportBasicInfos$steps)) %>% arrange(-n)
view(x)
y <- kroki_calorie_na_minute %>% group_by(recordDay) %>% 
  summarise(n = sum(sportBasicInfos$calorie)) %>% arrange(-n)
View(y)
 ############################################################################################################

#health detail data

############################################################################################################


ścieżka_do_pliku_health_dane <- "C:/Users/macie/Desktop/STUDIA/SEMESTR3/Techniki Wizualizacji Danych/PROJEKTY/Project2/MM/data/HUAWEI_HEALTH_20231211144413 (1)/Health detail data & description/health detail data.json"
dane_json_health_dane <- fromJSON(ścieżka_do_pliku_health_dane)
health_dane <- dane_json_health_dane %>% unnest_longer(samplePoints)
health_dane$startTime <- as.POSIXct(health_dane$startTime / 1000, origin = "1970-01-01", tz = "UTC")
health_dane$endTime <- as.POSIXct(health_dane$endTime / 1000, origin = "1970-01-01", tz = "UTC")
View(health_dane)


############################################################################################################

#motion path detail

############################################################################################################


scieżka_do_pliku_motion_path <- "C:/Users/macie/Desktop/STUDIA/SEMESTR3/Techniki Wizualizacji Danych/PROJEKTY/Project2/MM/data/HUAWEI_HEALTH_20231211144413 (1)/Motion path detail data & description/motion path detail data.json"
dane_json_motion_path <- fromJSON(scieżka_do_pliku_motion_path)

head(dane_json_motion_path)
blabla<-dane_json_motion_path %>% select(startTime, endTime, totalDistance, totalTime,totalCalories, attribute)
View(blabla)
