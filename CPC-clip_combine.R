library(lubridate)
library(ncdf4)
library(dplyr) 
library(openxlsx) 
library(terra)
library(raster)

####### CLIPPING TMIN & TMAX ########
#### ALL IN ONE (IMPORT & CLIP) #####
## list, in my case it gave an error do to lack off loading capacity
cpc_list <- list.files("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\1979-1983",pattern="\\.nc$",full=TRUE)
cpc_list <- rast(cpc_list,drivers="NETCDF")
cpc_r <- rotate(cpc_list)
shape <- vect("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\Honduras.shp")
cropext <- ext(-90,-82,12,17)
cpc_crop <- crop(cpc_r,cropext)
cpc_mask <-mask(cpc_crop,shape)

#### ALL TMAX INDIVIDUAL ####
shape <- vect("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\Honduras.shp")

CPC_1979 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.1979.nc")
CPC_1980 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.1980.nc")
CPC_1981 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.1981.nc")
CPC_1982 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.1982.nc")
CPC_1983 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.1983.nc")
CPC_1984 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.1984.nc")
CPC_1985 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.1985.nc")
CPC_1986 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.1986.nc")
CPC_1987 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.1987.nc")
CPC_1988 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.1988.nc")
CPC_1989 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.1989.nc")
CPC_1990 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.1990.nc") 
CPC_1991 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.1991.nc")
CPC_1992 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.1992.nc")
CPC_1993 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.1993.nc")
CPC_1994 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.1994.nc")
CPC_1995 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.1995.nc")
CPC_1996 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.1996.nc")
CPC_1997 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.1997.nc")
CPC_1998 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.1998.nc")
CPC_1999 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.1999.nc")
CPC_2000 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2000.nc")
CPC_2001 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2001.nc")
CPC_2002 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2002.nc")
CPC_2003 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2003.nc")
CPC_2004 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2004.nc")
CPC_2005 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2005.nc")
CPC_2006 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2006.nc")
CPC_2007 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2007.nc")
CPC_2008 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2008.nc")
CPC_2009 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2009.nc")
CPC_2010 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2010.nc")
CPC_2011 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2011.nc")
CPC_2012 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2012.nc")
CPC_2013 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2013.nc")
CPC_2014 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2014.nc")
CPC_2015 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2015.nc")
CPC_2016 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2016.nc")
CPC_2017 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2017.nc")
CPC_2018 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2018.nc")
CPC_2019 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2019.nc")
CPC_2020 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2020.nc")
CPC_2021 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2021.nc")
CPC_2022 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2022.nc")
CPC_2023 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmax.2023.nc")

CPC_1979 <- rotate(CPC_1979)
CPC_1980 <- rotate(CPC_1980)
CPC_1981 <- rotate(CPC_1981)
CPC_1982 <- rotate(CPC_1982)
CPC_1983 <- rotate(CPC_1983)
CPC_1984 <- rotate(CPC_1984)
CPC_1985 <- rotate(CPC_1985)
CPC_1986 <- rotate(CPC_1986)
CPC_1987 <- rotate(CPC_1987)
CPC_1988 <- rotate(CPC_1988)
CPC_1989 <- rotate(CPC_1989)
CPC_1990 <- rotate(CPC_1990)
CPC_1991 <- rotate(CPC_1991)
CPC_1992 <- rotate(CPC_1992)
CPC_1993 <- rotate(CPC_1993)
CPC_1994 <- rotate(CPC_1994)
CPC_1995 <- rotate(CPC_1995)
CPC_1996 <- rotate(CPC_1996)
CPC_1997 <- rotate(CPC_1997)
CPC_1998 <- rotate(CPC_1998)
CPC_1999 <- rotate(CPC_1999)
CPC_2000 <- rotate(CPC_2000)
CPC_2001 <- rotate(CPC_2001)
CPC_2002 <- rotate(CPC_2002)
CPC_2003 <- rotate(CPC_2003)
CPC_2004 <- rotate(CPC_2004)
CPC_2005 <- rotate(CPC_2005)
CPC_2006 <- rotate(CPC_2006)
CPC_2007 <- rotate(CPC_2007)
CPC_2008 <- rotate(CPC_2008)
CPC_2009 <- rotate(CPC_2009)
CPC_2010 <- rotate(CPC_2010)
CPC_2011 <- rotate(CPC_2011)
CPC_2012 <- rotate(CPC_2012)
CPC_2013 <- rotate(CPC_2013)
CPC_2014 <- rotate(CPC_2014)
CPC_2015 <- rotate(CPC_2015)
CPC_2016 <- rotate(CPC_2016)
CPC_2017 <- rotate(CPC_2017)
CPC_2018 <- rotate(CPC_2018)
CPC_2019 <- rotate(CPC_2019)
CPC_2020 <- rotate(CPC_2020)
CPC_2021 <- rotate(CPC_2021)
CPC_2022 <- rotate(CPC_2022)
CPC_2023 <- rotate(CPC_2023)

cropext <- ext(-90,-82,12,17)

CPC_1979 <- crop(CPC_1979,cropext)
CPC_1979 <- mask(CPC_1979,shape)
CPC_1980 <- crop(CPC_1980,cropext)
CPC_1980 <- mask(CPC_1980,shape)
CPC_1981 <- crop(CPC_1981,cropext)
CPC_1981 <- mask(CPC_1981,shape)
CPC_1982 <- crop(CPC_1982,cropext)
CPC_1982 <- mask(CPC_1982,shape)
CPC_1983 <- crop(CPC_1983,cropext)
CPC_1983 <- mask(CPC_1983,shape)
CPC_1984 <- crop(CPC_1984,cropext)
CPC_1984 <- mask(CPC_1984,shape)
CPC_1985 <- crop(CPC_1985,cropext)
CPC_1985 <- mask(CPC_1985,shape)
CPC_1986 <- crop(CPC_1986,cropext)
CPC_1986 <- mask(CPC_1986,shape)
CPC_1987 <- crop(CPC_1987,cropext)
CPC_1987 <- mask(CPC_1987,shape)
CPC_1988 <- crop(CPC_1988,cropext)
CPC_1988 <- mask(CPC_1988,shape)
CPC_1989 <- crop(CPC_1989,cropext)
CPC_1989 <- mask(CPC_1989,shape)
CPC_1990 <- crop(CPC_1990,cropext)
CPC_1990 <- mask(CPC_1990,shape)
CPC_1991 <- crop(CPC_1991,cropext)
CPC_1991 <-mask(CPC_1991,shape)
CPC_1992 <- crop(CPC_1992,cropext)
CPC_1992 <-mask(CPC_1992,shape)
CPC_1993 <- crop(CPC_1993,cropext)
CPC_1993 <-mask(CPC_1993,shape)
CPC_1994 <- crop(CPC_1994,cropext)
CPC_1994 <-mask(CPC_1994,shape)
CPC_1995 <- crop(CPC_1995,cropext)
CPC_1995 <-mask(CPC_1995,shape)
CPC_1996 <- crop(CPC_1996,cropext)
CPC_1996 <-mask(CPC_1996,shape)
CPC_1997 <- crop(CPC_1997,cropext)
CPC_1997 <-mask(CPC_1997,shape)
CPC_1998 <- crop(CPC_1998,cropext)
CPC_1998 <-mask(CPC_1998,shape)
CPC_1999 <- crop(CPC_1999,cropext)
CPC_1999 <-mask(CPC_1999,shape)
CPC_2000 <- crop(CPC_2000,cropext)
CPC_2000 <-mask(CPC_2000,shape)
CPC_2001 <- crop(CPC_2001,cropext)
CPC_2001 <-mask(CPC_2001,shape)
CPC_2002 <- crop(CPC_2002,cropext)
CPC_2002 <-mask(CPC_2002,shape)
CPC_2003 <- crop(CPC_2003,cropext)
CPC_2003 <-mask(CPC_2003,shape)
CPC_2004 <- crop(CPC_2004_max,cropext)
CPC_2004 <-mask(CPC_2004,shape)
CPC_2005 <- crop(CPC_2005,cropext)
CPC_2005 <-mask(CPC_2005,shape)
CPC_2006 <- crop(CPC_2006,cropext)
CPC_2006 <-mask(CPC_2006,shape)
CPC_2007 <- crop(CPC_2007,cropext)
CPC_2007 <-mask(CPC_2007,shape)
CPC_2008 <- crop(CPC_2008,cropext)
CPC_2008 <-mask(CPC_2008,shape)
CPC_2009 <- crop(CPC_2009,cropext)
CPC_2009 <-mask(CPC_2009,shape)
CPC_2010 <- crop(CPC_2010,cropext)
CPC_2010 <-mask(CPC_2010,shape)
CPC_2011 <- crop(CPC_2011,cropext)
CPC_2011 <-mask(CPC_2011,shape)
CPC_2012 <- crop(CPC_2012,cropext)
CPC_2012 <-mask(CPC_2012,shape)
CPC_2013 <- crop(CPC_2013,cropext)
CPC_2013 <-mask(CPC_2013,shape)
CPC_2014 <- crop(CPC_2014,cropext)
CPC_2014 <-mask(CPC_2014,shape)
CPC_2015 <- crop(CPC_2015,cropext)
CPC_2015 <-mask(CPC_2015,shape)
CPC_2016 <- crop(CPC_2016,cropext)
CPC_2016 <-mask(CPC_2016,shape)
CPC_2017 <- crop(CPC_2017,cropext)
CPC_2017 <-mask(CPC_2017,shape)
CPC_2018 <- crop(CPC_2018,cropext)
CPC_2018 <-mask(CPC_2018,shape)
CPC_2019 <- crop(CPC_2019,cropext)
CPC_2019 <-mask(CPC_2019,shape)
CPC_2020 <- crop(CPC_2020,cropext)
CPC_2020 <-mask(CPC_2020,shape)
CPC_2021 <- crop(CPC_2021,cropext)
CPC_2021 <-mask(CPC_2021,shape)
CPC_2022 <- crop(CPC_2022,cropext)
CPC_2022 <-mask(CPC_2022,shape)
CPC_2023 <- crop(CPC_2023,cropext)
CPC_2023 <-mask(CPC_2023,shape)

#check
plot(CPC_2004_max[[50]])
plot(shape, add=TRUE)

#### EXPORT  ####
# individual
ftif <- file.path("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\TX_", "CPC_1979.nc")
writeCDF(CPC_1979, ftif, overwrite=TRUE)

# all
# Loop through each year from 1979 to 2023
for (year in 1979:2023) {
  path <- file.path("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\TX", paste("CPC_TX_", year, ".nc", sep=""))
  variable_name <- get(paste("CPC_", year, sep=""))
  writeCDF(variable_name, path, overwrite=TRUE)
}

#### ALL TMIN INDIVIDUAL ####
CPC_1979 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.1979.nc")
CPC_1980 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.1980.nc")
CPC_1981 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.1981.nc")
CPC_1982 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.1982.nc")
CPC_1983 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.1983.nc")
CPC_1984 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.1984.nc")
CPC_1985 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.1985.nc")
CPC_1986 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.1986.nc")
CPC_1987 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.1987.nc")
CPC_1988 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.1988.nc")
CPC_1989 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.1989.nc")
CPC_1990 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.1990.nc") 
CPC_1991 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.1991.nc")
CPC_1992 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.1992.nc")
CPC_1993 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.1993.nc")
CPC_1994 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.1994.nc")
CPC_1995 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.1995.nc")
CPC_1996 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.1996.nc")
CPC_1997 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.1997.nc")
CPC_1998 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.1998.nc")
CPC_1999 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.1999.nc")
CPC_2000 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2000.nc")
CPC_2001 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2001.nc")
CPC_2002 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2002.nc")
CPC_2003 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2003.nc")
CPC_2004 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2004.nc")
CPC_2005 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2005.nc")
CPC_2006 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2006.nc")
CPC_2007 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2007.nc")
CPC_2008 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2008.nc")
CPC_2009 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2009.nc")
CPC_2010 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2010.nc")
CPC_2011 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2011.nc")
CPC_2012 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2012.nc")
CPC_2013 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2013.nc")
CPC_2014 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2014.nc")
CPC_2015 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2015.nc")
CPC_2016 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2016.nc")
CPC_2017 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2017.nc")
CPC_2018 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2018.nc")
CPC_2019 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2019.nc")
CPC_2020 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2020.nc")
CPC_2021 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2021.nc")
CPC_2022 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2022.nc")
CPC_2023 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\tmin.2023.nc")


CPC_1979 <- rotate(CPC_1979)
CPC_1980 <- rotate(CPC_1980)
CPC_1981 <- rotate(CPC_1981)
CPC_1982 <- rotate(CPC_1982)
CPC_1983 <- rotate(CPC_1983)
CPC_1984 <- rotate(CPC_1984)
CPC_1985 <- rotate(CPC_1985)
CPC_1986 <- rotate(CPC_1986)
CPC_1987 <- rotate(CPC_1987)
CPC_1988 <- rotate(CPC_1988)
CPC_1989 <- rotate(CPC_1989)
CPC_1990 <- rotate(CPC_1990)
CPC_1991 <- rotate(CPC_1991)
CPC_1992 <- rotate(CPC_1992)
CPC_1993 <- rotate(CPC_1993)
CPC_1994 <- rotate(CPC_1994)
CPC_1995 <- rotate(CPC_1995)
CPC_1996 <- rotate(CPC_1996)
CPC_1997 <- rotate(CPC_1997)
CPC_1998 <- rotate(CPC_1998)
CPC_1999 <- rotate(CPC_1999)
CPC_2000 <- rotate(CPC_2000)
CPC_2001 <- rotate(CPC_2001)
CPC_2002 <- rotate(CPC_2002)
CPC_2003 <- rotate(CPC_2003)
CPC_2004 <- rotate(CPC_2004)
CPC_2005 <- rotate(CPC_2005)
CPC_2006 <- rotate(CPC_2006)
CPC_2007 <- rotate(CPC_2007)
CPC_2008 <- rotate(CPC_2008)
CPC_2009 <- rotate(CPC_2009)
CPC_2010 <- rotate(CPC_2010)
CPC_2011 <- rotate(CPC_2011)
CPC_2012 <- rotate(CPC_2012)
CPC_2013 <- rotate(CPC_2013)
CPC_2014 <- rotate(CPC_2014)
CPC_2015 <- rotate(CPC_2015)
CPC_2016 <- rotate(CPC_2016)
CPC_2017 <- rotate(CPC_2017)
CPC_2018 <- rotate(CPC_2018)
CPC_2019 <- rotate(CPC_2019)
CPC_2020 <- rotate(CPC_2020)
CPC_2021 <- rotate(CPC_2021)
CPC_2022 <- rotate(CPC_2022)
CPC_2023 <- rotate(CPC_2023)

cropext <- ext(-90,-82,12,17)

CPC_1979 <- crop(CPC_1979,cropext)
CPC_1979 <- mask(CPC_1979,shape)
CPC_1980 <- crop(CPC_1980,cropext)
CPC_1980 <- mask(CPC_1980,shape)
CPC_1981 <- crop(CPC_1981,cropext)
CPC_1981 <- mask(CPC_1981,shape)
CPC_1982 <- crop(CPC_1982,cropext)
CPC_1982 <- mask(CPC_1982,shape)
CPC_1983 <- crop(CPC_1983,cropext)
CPC_1983 <- mask(CPC_1983,shape)
CPC_1984 <- crop(CPC_1984,cropext)
CPC_1984 <- mask(CPC_1984,shape)
CPC_1985 <- crop(CPC_1985,cropext)
CPC_1985 <- mask(CPC_1985,shape)
CPC_1986 <- crop(CPC_1986,cropext)
CPC_1986 <- mask(CPC_1986,shape)
CPC_1987 <- crop(CPC_1987,cropext)
CPC_1987 <- mask(CPC_1987,shape)
CPC_1988 <- crop(CPC_1988,cropext)
CPC_1988 <- mask(CPC_1988,shape)
CPC_1989 <- crop(CPC_1989,cropext)
CPC_1989 <- mask(CPC_1989,shape)
CPC_1990 <- crop(CPC_1990,cropext)
CPC_1990 <- mask(CPC_1990,shape)
CPC_1991 <- crop(CPC_1991,cropext)
CPC_1991 <-mask(CPC_1991,shape)
CPC_1992 <- crop(CPC_1992,cropext)
CPC_1992 <-mask(CPC_1992,shape)
CPC_1993 <- crop(CPC_1993,cropext)
CPC_1993 <-mask(CPC_1993,shape)
CPC_1994 <- crop(CPC_1994,cropext)
CPC_1994 <-mask(CPC_1994,shape)
CPC_1995 <- crop(CPC_1995,cropext)
CPC_1995 <-mask(CPC_1995,shape)
CPC_1996 <- crop(CPC_1996,cropext)
CPC_1996 <-mask(CPC_1996,shape)
CPC_1997 <- crop(CPC_1997,cropext)
CPC_1997 <-mask(CPC_1997,shape)
CPC_1998 <- crop(CPC_1998,cropext)
CPC_1998 <-mask(CPC_1998,shape)
CPC_1999 <- crop(CPC_1999,cropext)
CPC_1999 <-mask(CPC_1999,shape)
CPC_2000 <- crop(CPC_2000,cropext)
CPC_2000 <-mask(CPC_2000,shape)
CPC_2001 <- crop(CPC_2001,cropext)
CPC_2001 <-mask(CPC_2001,shape)
CPC_2002 <- crop(CPC_2002,cropext)
CPC_2002 <-mask(CPC_2002,shape)
CPC_2003 <- crop(CPC_2003,cropext)
CPC_2003 <-mask(CPC_2003,shape)
CPC_2004 <- crop(CPC_2004,cropext)
CPC_2004 <-mask(CPC_2004,shape)
CPC_2005 <- crop(CPC_2005,cropext)
CPC_2005 <-mask(CPC_2005,shape)
CPC_2006 <- crop(CPC_2006,cropext)
CPC_2006 <-mask(CPC_2006,shape)
CPC_2007 <- crop(CPC_2007,cropext)
CPC_2007 <-mask(CPC_2007,shape)
CPC_2008 <- crop(CPC_2008,cropext)
CPC_2008 <-mask(CPC_2008,shape)
CPC_2009 <- crop(CPC_2009,cropext)
CPC_2009 <-mask(CPC_2009,shape)
CPC_2010 <- crop(CPC_2010,cropext)
CPC_2010 <-mask(CPC_2010,shape)
CPC_2011 <- crop(CPC_2011,cropext)
CPC_2011 <-mask(CPC_2011,shape)
CPC_2012 <- crop(CPC_2012,cropext)
CPC_2012 <-mask(CPC_2012,shape)
CPC_2013 <- crop(CPC_2013,cropext)
CPC_2013 <-mask(CPC_2013,shape)
CPC_2014 <- crop(CPC_2014,cropext)
CPC_2014 <-mask(CPC_2014,shape)
CPC_2015 <- crop(CPC_2015,cropext)
CPC_2015 <-mask(CPC_2015,shape)
CPC_2016 <- crop(CPC_2016,cropext)
CPC_2016 <-mask(CPC_2016,shape)
CPC_2017 <- crop(CPC_2017,cropext)
CPC_2017 <-mask(CPC_2017,shape)
CPC_2018 <- crop(CPC_2018,cropext)
CPC_2018 <-mask(CPC_2018,shape)
CPC_2019 <- crop(CPC_2019,cropext)
CPC_2019 <-mask(CPC_2019,shape)
CPC_2020 <- crop(CPC_2020,cropext)
CPC_2020 <-mask(CPC_2020,shape)
CPC_2021 <- crop(CPC_2021,cropext)
CPC_2021 <-mask(CPC_2021,shape)
CPC_2022 <- crop(CPC_2022,cropext)
CPC_2022 <-mask(CPC_2022,shape)
CPC_2023 <- crop(CPC_2023,cropext)
CPC_2023 <-mask(CPC_2023,shape)

# all
# Loop through each year from 1979 to 2023
for (year in 1979:2023) {
  path <- file.path("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\mask\\TN", paste("CPC_TN_", year, ".nc", sep=""))
  variable_name <- get(paste("CPC_", year, sep=""))
  writeCDF(variable_name, path, overwrite=TRUE)
}


####### COMBINING NC FILES #######
### INDIVIDUAL ####
CPC_1979 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\mask\\TX\\CPC_TX_1979.nc")
CPC_1980 <- rast("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\mask\\TX\\CPC_TX_1980.nc")
combined_rasters <- c(CPC_1979, CPC_1980) 
output_file <- "C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\mask\\CPC_1979-1980.nc"
writeCDF(combined_rasters, output_file, overwrite = TRUE)

plot(combined_rasters[[712]], main="722")

### ALL TMIN ####
# input data as raster
input_folder <- "C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\mask\\TN"
output_folder <- "C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\mask"
years <- 1979:2024
file_paths <- file.path(input_folder, paste0("CPC_TN_", years, ".nc"))

# NetCDF files as rasters
rasters <- lapply(file_paths, rast)

# Combine all rasters
combined_rasters <- rast(rasters)

# Save as a single NetCDF file
output_file <- file.path(output_folder, "CPC_TN_1979-2024.nc")
writeCDF(combined_rasters, output_file, overwrite = TRUE)

### ALL TMAX ####
# input data as raster
input_folder <- "C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\mask\\TX"
output_folder <- "C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\mask"
years <- 1979:2024
file_paths <- file.path(input_folder, paste0("CPC_TX_", years, ".nc"))

# NetCDF files as rasters
rasters <- lapply(file_paths, rast)

# Combine all rasters
combined_rasters <- rast(rasters)

# Save as a single NetCDF file
output_file <- file.path(output_folder, "CPC_TX_1979-2024.nc")
writeCDF(combined_rasters, output_file, overwrite = TRUE)

####### TO TMAX & TMIN to excel ######
nc_data <- nc_open("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\mask\\CPC_TX_1979-2024.nc")
nc_data_tn <- nc_open("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\mask\\CPC_TN_1979-2024.nc")

start_date <- as.Date("1979-01-01")
end_date <- as.Date("2024-12-31")
date_list <- seq.Date(from = start_date, to = end_date, by = "day")

time <- date_list
lon <- ncvar_get(nc_data, "longitude")
lat <- ncvar_get(nc_data, "latitude")  

# extract the variable 'tmax' en 'tmin'
tmax <- ncvar_get(nc_data, "CPC_TX_1979-2024")
tmin <- ncvar_get(nc_data_tn, "CPC_TN_1979-2024")

# close the NetCDF file
nc_close(nc_data)
nc_close(nc_data_tn)

# create a data frame 
data <- expand.grid(lon = lon, lat = lat, time = time) %>%
  mutate(tmax = as.vector(tmax),
         tmin = as.vector(tmin))


# remove leap day and add day of the year (1 to 365)
data <- data %>% filter(format(time, "%m-%d") != "02-29")
data$mean <- rowMeans(data[, c("tmax", "tmin")])
data$day_of_year <- yday(data$time)
data$year <- year(data$time)

# save the data to an Excel file
output_file <- "C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\CPCfinal.xlsx"
write.xlsx(data, output_file)

#manually created a list of all coordinates with data, so not in ocean
lon_lat_cpc <- data.frame(
  lon = c(-86.75, -86.25, -88.25, -87.75, -87.25, -86.75, -86.25, -85.75, -85.25, -84.75, -84.25,
          -89.25, -88.75, -88.25, -87.75, -87.25, -86.75, -86.25, -85.75, -85.25, 
          -84.75, -84.25, -83.75, -83.25, -89.25, -88.75, -88.25, -87.75, -87.25,
          -86.75, -86.25, -85.75, -85.25, -84.75, -84.25, -83.75, -83.25, -89.25,
          -88.75, -88.25, -87.75, -87.25, -86.75, -86.25, -85.75, -85.25, -88.75, 
          -88.25, -87.75, -87.25, -86.75, -86.25, -85.75, -87.75, -87.25, -86.75,
          -87.25),
  lat = c(16.25,16.25, 15.75, 15.75, 15.75, 15.75, 15.75, 15.75, 15.75, 15.75, 15.75,
          15.25, 15.25, 15.25, 15.25, 15.25, 15.25, 15.25, 15.25, 15.25, 
          15.25, 15.25, 15.25, 15.25, 14.75, 14.75, 14.75, 14.75, 14.75,
          14.75, 14.75, 14.75, 14.75, 14.75, 14.75, 14.75, 14.75, 14.25, 
          14.25, 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, 13.75,
          13.75, 13.75, 13.75, 13.75, 13.75, 13.75, 13.25, 13.25, 13.25,
          12.75)
)

lon_lat_cpc <- lon_lat_cpc %>%
  mutate(coordinate_id = row_number())

# Assign coordinate_id
data_cpc$lon <- as.numeric(data_cpc$lon)
data_cpc$lat <- as.numeric(data_cpc$lat)
lon_lat_cpc$lon <- as.numeric(lon_lat_cpc$lon)
lon_lat_cpc$lat <- as.numeric(lon_lat_cpc$lat)

# Join and assign unique coordinate ID  
data_cpc <- data_cpc %>%
  inner_join(lon_lat_cpc, by = c("lon", "lat"))

######## TX90 & TN90 ########
# Filter reference period (1980-2009)
ref_data <- data_cpc %>% 
  filter(year >= 1980 & year <= 2009)

# Compute thresholds per coordinate
calculate_threshold <- function(df) {
  df %>%
    group_by(coordinate_id) %>%
    summarise(
      TX90 = quantile(tmax, 0.9, na.rm = TRUE),
      TN90 = quantile(tmin, 0.9, na.rm = TRUE),
      .groups = 'drop'
    )
}

temp_thresholds <- calculate_threshold(ref_data)

# Merge thresholds back
data_cpc <- data_cpc %>%
  left_join(temp_thresholds, by = "coordinate_id") %>%
  mutate(
    exceedance_TX90 = as.integer(tmax > TX90),
    exceedance_TN90 = as.integer(tmin > TN90)
  )

# Heatwave detection function using rle
detect_heatwave <- function(exceedance_vector) {
  r <- rle(exceedance_vector)
  r$values[r$values == 1 & r$lengths >= 3] <- 2  # Mark valid sequences
  r$values[r$values == 1] <- 0  # Remove short sequences
  r$values[r$values == 2] <- 1  # Final heatwave flag
  rep(r$values, r$lengths)
}

# Apply detection per group
data_cpc <- data_cpc %>%
  group_by(coordinate_id) %>%
  arrange(time) %>%
  mutate(
    heatwave_TX90 = detect_heatwave(exceedance_TX90),
    heatwave_TN90 = detect_heatwave(exceedance_TN90)
  ) %>%
  ungroup() %>%
  dplyr::select(-exceedance_TX90, -exceedance_TN90)  # Optional cleanup

# Save results
write.csv(data_cpc, "HONDURAS HEAT/CPC_data/data_cpc_90TX_90TN.csv", row.names = FALSE)

######## EHF ########
# Initialize list to store results
list_ehf_results <- vector("list", length = nrow(lon_lat_cpc))

# Loop through each coordinate
for (i in seq_len(nrow(lon_lat_cpc))) {
  if (i %% 10 == 0) cat("Processing coordinate", i, "of", nrow(lon_lat_cpc), "\n")
  
  lon_i <- lon_lat_cpc$lon[i]
  lat_i <- lon_lat_cpc$lat[i]
  
  # Subset data for this coordinate
  subset_data <- data_cpc %>%
    filter(lon == lon_i & lat == lat_i) %>%
    arrange(time)
  
  if (nrow(subset_data) == 0) next
  
  # Reference period (1980–2009)
  subset_rp <- subset_data %>% filter(year >= 1980 & year <= 2009)
  
  # 95th percentile of mean
  per95 <- quantile(subset_rp$mean, probs = 0.95, na.rm = TRUE)
  subset_data$per95 <- per95
  
  # Rolling means
  subset_data$rolling_avg3 <- rollapply(subset_data$mean, width = 3, FUN = mean, align = "right", fill = NA)
  subset_data$rolling_avg_past_30 <- rollapply(subset_data$mean, width = 30, FUN = mean, align = "right", fill = NA)
  
  # EHI significant
  subset_data$ehisig <- subset_data$rolling_avg3 - subset_data$per95
  
  # Heatwave detection: ehisig > 0 for 3+ consecutive days
  rle_obj <- rle(subset_data$ehisig > 0)
  flags <- rep(0, length(subset_data$ehisig))
  pos <- cumsum(c(1, head(rle_obj$lengths, -1)))
  for (j in seq_along(rle_obj$lengths)) {
    if (rle_obj$values[j] && rle_obj$lengths[j] >= 3) {
      idx_start <- pos[j]
      idx_end <- idx_start + rle_obj$lengths[j] - 1
      flags[idx_start:idx_end] <- 1
    }
  }
  subset_data$heatwave_ehisig <- flags
  
  # EHI accumulated
  subset_data$ehiacc <- ifelse(subset_data$heatwave_ehisig == 1,
                               subset_data$rolling_avg3 - subset_data$rolling_avg_past_30, NA)
  
  # EHF calculation
  subset_data$ehf <- ifelse(!is.na(subset_data$ehiacc) & subset_data$ehiacc > 1,
                            subset_data$ehisig * subset_data$ehiacc,
                            subset_data$ehisig * 1)
  
  # 85th percentile EHF
  ehf_per85 <- quantile(subset_data$ehf, probs = 0.85, na.rm = TRUE)
  subset_data$ehf_per85 <- ehf_per85
  
  # Heatwave flag based on EHF
  subset_data$heatwave_ehf <- ifelse(subset_data$ehf > ehf_per85, 1, 0)
  
  # Store result
  list_ehf_results[[i]] <- subset_data
}

# Combine all into one dataframe
df_all_coordinates_cpc <- do.call(rbind, list_ehf_results)

# Save to CSV
write.csv(df_all_coordinates_cpc, "HONDURAS HEAT/CPC_data/data_cpc_ehf.csv", row.names = FALSE)

