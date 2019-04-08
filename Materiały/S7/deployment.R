library(rsconnect)

rsconnect::setAccountInfo(name='kowaliks',
                          token='DFBFC5CAD89C2EE76538E9F63C01D0C9',
                          secret='uJKIftyHZvY4dPhFX9e5V4jetWkDdEZBWKLw//CJ')

rsconnect::deployApp(appDir = '/home/samba/kowaliks/Documents/magisterka/semestr1/wizualizacja-danych/WizualizacjaDanych2018/Materiały/S7/appp')
