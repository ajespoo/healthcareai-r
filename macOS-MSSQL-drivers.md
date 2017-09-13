# Installing RODBC on macOS


- Cleanup your install
```
brew uninstall freedtds unixodbc
remove.pacakges(RODBC)
brew update
```

- Install dependencies with switches
```
brew install unixodbc
brew install freetds --with-unixodbc
```

- edit the odbdinst.ini found by running `odbcinst -j` to have the following:
```
[FreeTDS]
Description=FreeTDS Driver for Linux & MSSQL
Driver=/usr/local/lib/libtdsodbc.so
Setup=/usr/local/lib/libtdsodbc.so
UsageCount=1
```
- in R run `install.packages('RODBC', type = 'source')`

- Try the specified things.
