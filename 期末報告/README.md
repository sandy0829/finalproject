台灣各地區登革熱分析
================

讀入資料
--------

``` r
#這是R Code Chunk
library(readr)
```

    ## Warning: package 'readr' was built under R version 3.3.3

``` r
#99~106年高雄登革熱資料
MosIndex_Kaohsiung <- read_csv("C:/Users/user/Downloads/MosIndex_Kaohsiung.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   Date = col_date(format = ""),
    ##   County = col_character(),
    ##   Town = col_character(),
    ##   Village = col_character(),
    ##   VillageID = col_character(),
    ##   VillageLon = col_double(),
    ##   VillageLat = col_double(),
    ##   AreaType = col_character(),
    ##   InspectType = col_character(),
    ##   BI = col_double(),
    ##   AIAeg = col_double(),
    ##   AIAlb = col_double(),
    ##   HI = col_double(),
    ##   HIAeg = col_double(),
    ##   CI = col_double(),
    ##   LI = col_double(),
    ##   AI = col_double(),
    ##   Con100HH = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
#104年高雄各地區各月份病例資料
X104dengueinfection <- read_csv("C:/Users/user/Downloads/104dengueinfection.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   行政區 = col_character(),
    ##   `1月` = col_integer(),
    ##   `2月` = col_integer(),
    ##   `3月` = col_integer(),
    ##   `4月` = col_integer(),
    ##   `5月` = col_integer(),
    ##   `6月` = col_integer(),
    ##   `7月` = col_integer(),
    ##   `8月` = col_integer(),
    ##   `9月` = col_integer(),
    ##   `10月` = col_integer(),
    ##   `11月` = col_integer(),
    ##   `12月` = col_integer(),
    ##   總計 = col_integer()
    ## )

``` r
#台南市、高雄市、屏東縣快篩診所資料
ns1hosp_20160603 <- read_csv("C:/Users/user/Downloads/ns1hosp_20160603.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   city = col_character(),
    ##   hospName = col_character(),
    ##   hospID = col_character(),
    ##   hospAddress = col_character(),
    ##   lat = col_double(),
    ##   lng = col_double(),
    ##   hospTel = col_character()
    ## )

``` r
#99~106年全台登革熱資料
MosIndex_All <- read_csv("C:/Users/user/Downloads/MosIndex_All.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   Date = col_date(format = ""),
    ##   County = col_character(),
    ##   Town = col_character(),
    ##   Village = col_character(),
    ##   VillageID = col_character(),
    ##   VillageLon = col_character(),
    ##   VillageLat = col_character(),
    ##   AreaType = col_character(),
    ##   InspectType = col_character(),
    ##   BI = col_double(),
    ##   AIAeg = col_double(),
    ##   AIAlb = col_double(),
    ##   HI = col_double(),
    ##   HIAeg = col_double(),
    ##   CI = col_double(),
    ##   LI = col_double(),
    ##   AI = col_double(),
    ##   Con100HH = col_double()
    ## )
    ## See spec(...) for full column specifications.

``` r
#104年全台各鄉鎮市區人口密度
opendata104N010 <- read_csv("C:/Users/user/Downloads/opendata104N010.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   statistic_yyy = col_character(),
    ##   site_id = col_character(),
    ##   people_total = col_character(),
    ##   area = col_character(),
    ##   population_density = col_character()
    ## )

資料處理與清洗、資料視覺化(99~106年全台登革熱資料)
--------------------------------------------------

``` r
#這是R Code Chunk
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.3.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
MosIndex_Allc<-select(MosIndex_All,Date:AreaType)
MosIndex_Allc<-MosIndex_Allc[complete.cases(MosIndex_Allc),]
MosIndex_Allc$VillageLon<-as.numeric(MosIndex_Allc$VillageLon)
```

    ## Warning: 強制變更過程中產生了 NA

``` r
MosIndex_Allc$VillageLat<-as.numeric(MosIndex_Allc$VillageLat)
```

    ## Warning: 強制變更過程中產生了 NA

``` r
MosIndex_Allc2010<-MosIndex_Allc[grepl("2010",MosIndex_Allc$Date),]
MosIndex_Allc2011<-MosIndex_Allc[grepl("2011",MosIndex_Allc$Date),]
MosIndex_Allc2012<-MosIndex_Allc[grepl("2012",MosIndex_Allc$Date),]
MosIndex_Allc2013<-MosIndex_Allc[grepl("2013",MosIndex_Allc$Date),]
MosIndex_Allc2014<-MosIndex_Allc[grepl("2014",MosIndex_Allc$Date),]
MosIndex_Allc2015<-MosIndex_Allc[grepl("2015",MosIndex_Allc$Date),]
MosIndex_Allc2016<-MosIndex_Allc[grepl("2016",MosIndex_Allc$Date),]
MosIndex_Allc2017<-MosIndex_Allc[grepl("2017",MosIndex_Allc$Date),]
library(ggmap)
```

    ## Warning: package 'ggmap' was built under R version 3.3.3

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 3.3.3

``` r
Taiwanmap <- get_map(location = "Taiwan", zoom = 8)
```

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=Taiwan&zoom=8&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false

    ## Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Taiwan&sensor=false

``` r
densityMap0<-ggmap(Taiwanmap,extent = "device")+ 
  geom_density2d(data = MosIndex_Allc2010, aes(x = VillageLon, y = VillageLat), size = 0.3)+
  stat_density2d(data = MosIndex_Allc2010, 
                 aes(x = VillageLon, y = VillageLat, 
                     fill = ..level.., alpha = ..level..), 
                 size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", 
                      high = "red", guide = FALSE) + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)
```

    ## Warning: `panel.margin` is deprecated. Please use `panel.spacing` property
    ## instead

``` r
densityMap0
```

    ## Warning: Removed 456 rows containing non-finite values (stat_density2d).

    ## Warning: Removed 456 rows containing non-finite values (stat_density2d).

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
Taiwanmap <- get_map(location = "Taiwan", zoom = 8)
```

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=Taiwan&zoom=8&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false
    ## Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Taiwan&sensor=false

``` r
densityMap1<-ggmap(Taiwanmap,extent = "device")+ 
  geom_density2d(data = MosIndex_Allc2011, aes(x = VillageLon, y = VillageLat), size = 0.3)+
  stat_density2d(data = MosIndex_Allc2011, 
                 aes(x = VillageLon, y = VillageLat, 
                     fill = ..level.., alpha = ..level..), 
                 size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", 
                      high = "red", guide = FALSE) + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)
```

    ## Warning: `panel.margin` is deprecated. Please use `panel.spacing` property
    ## instead

``` r
densityMap1
```

    ## Warning: Removed 502 rows containing non-finite values (stat_density2d).

    ## Warning: Removed 502 rows containing non-finite values (stat_density2d).

![](README_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
Taiwanmap <- get_map(location = "Taiwan", zoom = 8)
```

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=Taiwan&zoom=8&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false
    ## Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Taiwan&sensor=false

``` r
densityMap2<-ggmap(Taiwanmap,extent = "device")+ 
  geom_density2d(data = MosIndex_Allc2012, aes(x = VillageLon, y = VillageLat), size = 0.3)+
  stat_density2d(data = MosIndex_Allc2012, 
                 aes(x = VillageLon, y = VillageLat, 
                     fill = ..level.., alpha = ..level..), 
                 size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", 
                      high = "red", guide = FALSE) + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)
```

    ## Warning: `panel.margin` is deprecated. Please use `panel.spacing` property
    ## instead

``` r
densityMap2
```

    ## Warning: Removed 412 rows containing non-finite values (stat_density2d).

    ## Warning: Removed 412 rows containing non-finite values (stat_density2d).

![](README_files/figure-markdown_github/unnamed-chunk-2-3.png)

``` r
Taiwanmap <- get_map(location = "Taiwan", zoom = 8)
```

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=Taiwan&zoom=8&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false
    ## Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Taiwan&sensor=false

``` r
densityMap3<-ggmap(Taiwanmap,extent = "device")+ 
  geom_density2d(data = MosIndex_Allc2013, aes(x = VillageLon, y = VillageLat), size = 0.3)+
  stat_density2d(data = MosIndex_Allc2013, 
                 aes(x = VillageLon, y = VillageLat, 
                     fill = ..level.., alpha = ..level..), 
                 size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", 
                      high = "red", guide = FALSE) + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)
```

    ## Warning: `panel.margin` is deprecated. Please use `panel.spacing` property
    ## instead

``` r
densityMap3
```

    ## Warning: Removed 316 rows containing non-finite values (stat_density2d).

    ## Warning: Removed 316 rows containing non-finite values (stat_density2d).

![](README_files/figure-markdown_github/unnamed-chunk-2-4.png)

``` r
densityMap4<-ggmap(Taiwanmap,extent = "device")+ 
  geom_density2d(data = MosIndex_Allc2014, aes(x = VillageLon, y = VillageLat), size = 0.3)+
  stat_density2d(data = MosIndex_Allc2014, 
                 aes(x = VillageLon, y = VillageLat, 
                     fill = ..level.., alpha = ..level..), 
                 size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", 
                      high = "red", guide = FALSE) + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)
```

    ## Warning: `panel.margin` is deprecated. Please use `panel.spacing` property
    ## instead

``` r
densityMap4
```

    ## Warning: Removed 319 rows containing non-finite values (stat_density2d).

    ## Warning: Removed 319 rows containing non-finite values (stat_density2d).

![](README_files/figure-markdown_github/unnamed-chunk-2-5.png)

``` r
Taiwanmap <- get_map(location = "Taiwan", zoom = 8)
```

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=Taiwan&zoom=8&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false
    ## Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Taiwan&sensor=false

``` r
densityMap5<-ggmap(Taiwanmap,extent = "device")+ 
  geom_density2d(data = MosIndex_Allc2015, aes(x = VillageLon, y = VillageLat), size = 0.3)+
  stat_density2d(data = MosIndex_Allc2015, 
                 aes(x = VillageLon, y = VillageLat, 
                     fill = ..level.., alpha = ..level..), 
                 size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", 
                      high = "red", guide = FALSE) + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)
```

    ## Warning: `panel.margin` is deprecated. Please use `panel.spacing` property
    ## instead

``` r
densityMap5
```

    ## Warning: Removed 336 rows containing non-finite values (stat_density2d).

    ## Warning: Removed 336 rows containing non-finite values (stat_density2d).

![](README_files/figure-markdown_github/unnamed-chunk-2-6.png)

``` r
Taiwanmap <- get_map(location = "Taiwan", zoom = 8)
```

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=Taiwan&zoom=8&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false
    ## Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Taiwan&sensor=false

``` r
densityMap6<-ggmap(Taiwanmap,extent = "device")+ 
  geom_density2d(data = MosIndex_Allc2016, aes(x = VillageLon, y = VillageLat), size = 0.3)+
  stat_density2d(data = MosIndex_Allc2016, 
                 aes(x = VillageLon, y = VillageLat, 
                     fill = ..level.., alpha = ..level..), 
                 size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", 
                      high = "red", guide = FALSE) + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)
```

    ## Warning: `panel.margin` is deprecated. Please use `panel.spacing` property
    ## instead

``` r
densityMap6
```

    ## Warning: Removed 986 rows containing non-finite values (stat_density2d).

    ## Warning: Removed 986 rows containing non-finite values (stat_density2d).

![](README_files/figure-markdown_github/unnamed-chunk-2-7.png)

``` r
Taiwanmap <- get_map(location = "Taiwan", zoom = 8)
```

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=Taiwan&zoom=8&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false
    ## Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Taiwan&sensor=false

``` r
densityMap7<-ggmap(Taiwanmap,extent = "device")+ 
  geom_density2d(data = MosIndex_Allc2017, aes(x = VillageLon, y = VillageLat), size = 0.3)+
  stat_density2d(data = MosIndex_Allc2017, 
                 aes(x = VillageLon, y = VillageLat, 
                     fill = ..level.., alpha = ..level..), 
                 size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", 
                      high = "red", guide = FALSE) + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)
```

    ## Warning: `panel.margin` is deprecated. Please use `panel.spacing` property
    ## instead

``` r
densityMap7
```

    ## Warning: Removed 263 rows containing non-finite values (stat_density2d).

    ## Warning: Removed 263 rows containing non-finite values (stat_density2d).

![](README_files/figure-markdown_github/unnamed-chunk-2-8.png)

資料處理與清洗、資料視覺化(99~106年高雄登革熱資料)
--------------------------------------------------

``` r
library(dplyr)
MosIndex_Kaohsiungc<-select(MosIndex_Kaohsiung,Date:AreaType)
MosIndex_Kaohsiungc2015<-MosIndex_Kaohsiungc[grepl("2015",MosIndex_Kaohsiungc$Date),]
MosIndex_Kaohsiungc2016<-MosIndex_Kaohsiungc[grepl("2016",MosIndex_Kaohsiungc$Date),]
library(ggmap)
khcmap <- get_map(location ='Kaohsiung', 
                 zoom = 10,
                 language = "zh-TW", maptype = 'roadmap')
```

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=Kaohsiung&zoom=10&size=640x640&scale=2&maptype=roadmap&language=zh-TW&sensor=false

    ## Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Kaohsiung&sensor=false

``` r
khcmap1 <- ggmap(khcmap)+ 
  geom_point(data=MosIndex_Kaohsiungc, 
             aes(x=VillageLon, y=VillageLat),colour="red")
khcmap1
```

    ## Warning: Removed 284 rows containing missing values (geom_point).

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png) \#\#資料處理與清洗、資料視覺化(104年高雄各地區各月份病例資料)

``` r
X104dengueinfectionC<-select(X104dengueinfection,行政區:`12月`)
library(reshape2)
```

    ## Warning: package 'reshape2' was built under R version 3.3.3

``` r
X104dengueinfectionM<-melt(X104dengueinfectionC,id.vars = "行政區")
X104dengueinfectionM1<-X104dengueinfectionM[grepl("區",X104dengueinfectionM$行政區),]
#104年各月份高雄地區的病例數
test<-group_by(X104dengueinfectionM1,variable) %>%
  summarise(次數=sum(value,na.rm=T))%>%
  arrange(desc(次數))
test
```

    ## # A tibble: 12 × 2
    ##    variable  次數
    ##      <fctr> <int>
    ## 1      11月  8909
    ## 2      10月  5139
    ## 3      12月  2953
    ## 4       9月  2112
    ## 5       8月   440
    ## 6       1月    70
    ## 7       6月    31
    ## 8       7月    29
    ## 9       5月    18
    ## 10      2月    10
    ## 11      3月     7
    ## 12      4月     5

``` r
ggplot()+geom_bar(data=test,aes(x= variable,y= 次數),
                  stat = "identity")+xlab("月份")
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
#104年高雄各地區的總病例
test1<-group_by(X104dengueinfectionM1,.dots=行政區) %>%
 summarise(病例=sum(value,na.rm=T)) %>%
    arrange(desc(病例))
test1<-rename(test1,site_id=.dots)
test1
```

    ## # A tibble: 36 × 2
    ##    site_id  病例
    ##      <chr> <int>
    ## 1   三民區  4692
    ## 2   前鎮區  2722
    ## 3   鳳山區  2512
    ## 4   苓雅區  2257
    ## 5   鼓山區  1380
    ## 6   左營區  1288
    ## 7   小港區   913
    ## 8   楠梓區   630
    ## 9   新興區   473
    ## 10  前金區   324
    ## # ... with 26 more rows

資料處理與清洗、資料視覺化(台南市、高雄市、屏東縣快篩診所資料)
--------------------------------------------------------------

``` r
ns1hosp_20160603K<- ns1hosp_20160603[grep("高雄市",ns1hosp_20160603$city),]
#登革熱快篩所與病例分布
library(ggmap)
Kaohsiungmap <- get_map(location = "Kaohsiung", zoom = 10,
                        language = "zh-TW", maptype = "roadmap")
```

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=Kaohsiung&zoom=10&size=640x640&scale=2&maptype=roadmap&language=zh-TW&sensor=false

    ## Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Kaohsiung&sensor=false

``` r
#紅色是病例，綠色是快篩所
Kaohsiungmap0 <- ggmap(Kaohsiungmap)+ 
  geom_point(data=MosIndex_Kaohsiungc2015, 
             aes(x = VillageLon, y = VillageLat),
             colour="red",size=1.2)+
  geom_point(data=ns1hosp_20160603K, 
             aes(x = lng, y = lat),
             colour="green",size=1.2)
Kaohsiungmap0
```

    ## Warning: Removed 3 rows containing missing values (geom_point).

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png) \#\#資料處理與清洗、資料視覺化(104年全台各鄉鎮市區人口密度)

``` r
opendata104N010C<-opendata104N010[grepl("高雄市",opendata104N010$site_id),]
opendata104N010C<-opendata104N010C[complete.cases(opendata104N010C$statistic_yyy),]
opendata104N010C$site_id<-substr(opendata104N010C$site_id,start = 4,stop = 7)
a<-full_join(opendata104N010C,test1,by="site_id")
a<-mutate(a,盛行率=病例/as.numeric(people_total))
```
