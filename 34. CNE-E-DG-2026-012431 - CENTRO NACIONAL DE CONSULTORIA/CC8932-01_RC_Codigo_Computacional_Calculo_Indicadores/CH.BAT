C***** 
rem QUANTUM -c rcmarzo.PRG
rem QUANTUM rcmarzo.PRG rcambio1.dat trevisa1.LIS
rem QUANTUM rcmarzo.PRG rcambio2.dat trevisa2.LIS
rem QUANTUM rcmarzo.PRG rcambio3.dat trevisa3.LIS
rem QUANTUM rcmarzo.PRG rcambio4f.dat trevisa4.LIS
rem QUANTUM rcmarzo.PRG rcambiof.dat trevisaf.LIS
rem QUANTUM rcmarzo.PRG rcambiof.dat trevisap.LIS

C******Preliminar ponderado
rem QUANTUM rcmarzo.PRG rcambio4f.dat trevispr.LIS
REM QUANTUM rcmarzo.PRG rcambio4f.dat tdemogpr.LIS
rem QUANTUM rcmarzo.PRG rcambio4f.dat tPREG1pr.LIS


C***PROCESAMIENTO PRELIMINAR SIN PONDERAR
rem QUANTUM rcmarzo.PRG rcambio2.dat tdemogsp.LIS
rem QUANTUM rcmarzo.PRG rcambio3.dat tdemogsp.LIS

C*****PROCESAMIENTO FINAL PONDERADO
rem QUANTUM rcmarzo.PRG rcambiof.dat tdemog.LIS
rem QUANTUM rcmarzo.PRG rcambiof.dat tpreg1.LIS
rem QUANTUM rcmarzo.PRG rcambiof.dat tpreg15.LIS
rem QUANTUM rcmarzo.PRG rcambiof.dat tpreg18.LIS

C******CNE
QUANTUM rcmarzo.PRG rcambiof.dat tcNE.LIS



