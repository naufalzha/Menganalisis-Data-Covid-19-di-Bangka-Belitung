#import library
library(httr)
library(dplyr)
library(hrbrthemes)
library(ggplot2)
library(lubridate)
library(tidyr)

#Mengambil Data dari API
resp_babel <- GET("https://data.covid19.go.id/public/api/prov_detail_KEPULAUAN_BANGKA_BELITUNG.json")
cov_babel_raw <- content(resp_babel, as = "parsed", simplifyVector = TRUE)
names(cov_babel_raw)

#mengambil Informasi 
cov_babel_raw$kasus_total #kasus total covid di Kepulauan Bangka Belitung
cov_babel_raw$meninggal_persen #Persentasi meningggal karena Covid di Kepulauan Bangka Belitung
cov_babel_raw$sembuh_persen #Persentasi sembuh dari covid 

#Membersihkan Tabel
covid_babel <- cov_babel_raw$list_perkembangan
cov_babel <-
  covid_babel %>%
  select(-contains("DIRAWAT_OR_ISOLASI")) %>%
  select(-starts_with("AKUMULASI")) %>%
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>%
  mutate(
    tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )
str(cov_babel)

#Visualisasi Data
#grafik kasus_baru
ggplot(data = cov_babel, aes(tanggal, kasus_baru)) +
  geom_col(fill = "red4") +
  labs(
    x = "Bulan",
    y = "Jumlah Kasus",
    title = "Grafik Kasus Konfirmasi Harian Positif di Provinsi Kepulauan Bangka Belitung",
    subtitle = "Update 10 September 2020",
    caption = "sumber : data.covid19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 15,
    grid = "Y",
    ticks = TRUE
  )

#Grafik Kasus Sembuh
ggplot(data = cov_babel, aes(tanggal, sembuh)) +
  geom_col(fill= "olivedrab2") +
  labs(
    x = "Bulan",
    y = "Jumlah Kasus",
    title = "Grafik Kasus Konfirmasi Harian Sembuh di Provinsi Kepulauan Bangka Belitung",
    subtitle = "Update 10 September 2020",
    caption = "sumber : data.covid19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 15,
    grid = "Y",
    ticks = TRUE
  )
##Persentasi sembuh bedasarkan  jenis kelamin
sembuh_jk <- cov_babel_raw$data$sembuh$jenis_kelamin$list_data
sembuh_jk <- rename(sembuh_jk, Jenis_kelamin = key)
sembuh_jk <- sembuh_jk %>% 
  arrange(desc(Jenis_kelamin)) %>%
  mutate(prop = doc_count / sum(doc_count)*100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(sembuh_jk, aes(x="", y=prop, fill=Jenis_kelamin)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  geom_text(aes(y = ypos, label = round(doc_count, digits = 2)), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")

##persentasi Kesembuhan bedasarkan Umur
sembuh_umur <- cov_babel_raw$data$sembuh$kelompok_umur$list_data
sembuh_umur <- rename(sembuh_umur, Kelompok_umur = key)
sembuh_umur <- sembuh_umur %>% 
  arrange(desc(Kelompok_umur)) %>%
  mutate(prop = doc_count / sum(doc_count)*100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(sembuh_umur, aes(x="", y=prop, fill=Kelompok_umur)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  geom_text(aes(y = ypos, label = round(doc_count, digits = 2)),
            color = "white", size=6) +
  scale_fill_brewer(palette="Set1")

#Perbandingan Kasus Pekanan
cov_babel_pekanan <- cov_babel %>%
  count(
    tahun = year(tanggal),
    pekan_ke = week(tanggal),
    wt = kasus_baru,
    name = "Jumlah"
  )
glimpse(cov_babel_pekanan)

#akumulasi Positif
cov_babel_akumulasi <-
  cov_babel %>%
  transmute(
    tanggal,
    akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
    akumulasi_sembuh = cumsum(sembuh),
    akumulasi_meninggal = cumsum(meninggal)
  )
ggplot(data = cov_babel_akumulasi, aes(x = tanggal, y = akumulasi_aktif)) +
  geom_line(color = "red4") +
  labs(
    x = "Bulan",
    y = "Jumlah Kasus",
    title = "Grafik Kasus Kumulatif Positif di Provinsi Kepulauan Bangka Belitung",
    subtitle = "Update 10 September 2020",
    caption = "sumber : data.covid19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 15,
    grid = "Y",
    ticks = TRUE
  )

#Konparasi Kasus Positif, Sembuh dan meninggal
cov_babel_akumulasi_pivot <- 
  cov_babel_akumulasi %>% 
  gather(
    key = "kategori",
    value = "jumlah",
    -tanggal
  ) %>% 
  mutate(
    kategori = sub(pattern = "akumulasi_", replacement = "", kategori)
  )

dim(cov_babel_akumulasi_pivot)
glimpse(cov_babel_akumulasi_pivot)
#plot
ggplot(cov_babel_akumulasi_pivot, aes(tanggal, jumlah, colour = (kategori))) +
  geom_line(size = 0.9) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
  scale_colour_manual(
    values = c(
      "aktif" = "salmon",
      "meninggal" = "darkslategray4",
      "sembuh" = "olivedrab2"
    ),
    labels = c("Aktif", "Meninggal", "Sembuh")
  ) +
  labs(
    x = "Bulan",
    y = "Jumlah kasus akumulasi",
    colour = NULL,
    title = "Dinamika Kasus COVID-19 di Bangka Belitung",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )
