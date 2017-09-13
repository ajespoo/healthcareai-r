# Installing RODBC on macOS


- Cleanup your install and start with a fresh environment. In terminal:
```
brew uninstall freedtds unixodbc
brew update
```
In R:
```
remove.pacakges('RODBC')
```

- Install dependencies with switches. In terminal:
```
brew install unixodbc
brew install freetds --with-unixodbc
```

- If you want, set up Sublime 3 to open files from the command line. In terminal:
```
ln -s "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl" /usr/local/bin/sublime
sublime myFile.txt
```

- Fix the `odbcinst.ini` file. 
Find the location of the odbcinst.ini file using the command `odbcinst -j` in terminal.
Open it (it should be blank) and put the following into the file:
```
[FreeTDS]
Description=FreeTDS Driver for Linux & MSSQL
Driver=/usr/local/lib/libtdsodbc.so
Setup=/usr/local/lib/libtdsodbc.so
UsageCount=1
```
- in R, run `install.packages('RODBC', type = 'source')`. This installs RODBC from source and compiles it. Very important.

- Try pulling from an MSSQL database in R:
```
credentials <- 'server=myServer.database.windows.net;database=myDatabase;uid=myUserName;pwd=myPassword;Port=1433;driver=FreeTDS;TDS_Version=8.0;'
conn <- RODBC::odbcDriverConnect(connection = credentials)
query <- "SELECT COUNT(*) FROM myTable"
df <- RODBC::sqlQuery(conn, query)
```
}
