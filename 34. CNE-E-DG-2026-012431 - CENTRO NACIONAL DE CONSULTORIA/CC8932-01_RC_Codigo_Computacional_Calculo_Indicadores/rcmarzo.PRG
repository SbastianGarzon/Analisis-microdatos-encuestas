C * ESTUDIO      : A:\U_NEGOCIOS_P\OPINION\REV_CAMBIO\2026_03_CC893201_PresidenciaIII
C * DIRECTOR     : MARIA JOSE ROLDAN
C * SISTEMAS     : CAPT: Carolina Suarez
C * PROGRAMA     : RCMARZO.PRG
C * BASE DE D.   : \BASE\RCAMBIOF.DAT
C * TEXTO        : RCAMBIOF.DAT

Struct;reclen=5000

ED

END

a;flush;FORCE;topc;op=2;NOpc;decp=1;spechar="-*";NOdate;nopage;notype;indent=2;side=40;PAGWID=500;linesaft=0;linesbef=0;paglen=250;NSW;WM=1
wm1 PESOP;pre=cx(97,116);factor;1

C** REVISI‡N- 
C*INCLUDE TABS;Cruce=Totalp
C*INCLUDE TABS;Cruce=REVI

C****tabulado
C*INCLUDE TABS;Cruce=DEMOGRAF
C*INCLUDE TABS;Cruce=Preg1
C*INCLUDE TABS;Cruce=Preg15
C*INCLUDE TABS;Cruce=Preg18


C***GENERACI‡N TABULADO CNE
*INCLUDE cabezo;F=Marzo 17-21
*INCLUDE TABS;Cruce=CNE



C************************* CRUCES
*Include Cruces

C*** EJES FACTOR DE EXPANSION
L PESOP
n01                                                       


C************************** EJES
L PERFIL 
*Include BAS
N03 Sexo 
FLD C29, C29:1
+ Masculino=1
+ Femenino=2
N03;COLTXT R+1=----
N05
N03;NOSORT
N11 = BASE EXPANDIDA 
N03 ®En quÇ grupo de edad se encuentra usted?;NOSORT 
N03;NOSORT 
FLD C33, C33:1
+ 18-24=1
+ 25-34=2
+ 35-44=3
+ 45-54=4
+ 55 o m†s=5
N03;COLTXT R+1=----
N05
N03;NOSORT
N11 BASE EXPANDIDA 
N03 Estrato;nosort 
N03;NOSORT 
fld C31, C31:1 
+ Bajo=1,2
+ Medio =3
+ Alto=4-6
N03;COLTXT R+1=----
N05
N03
N03 Regiones 
FLD C20, C20:1
+ Bogot†=1
+ Caribe=2
+ Centro Oriente=3
+ Centro Sur - Amazon°a=4
+ Eje cafetero=5
+ Llano=6
+ Pac°fico=7
N03;COLTXT R+1=----
N05
N03
N03 Tama§o
Fld C21, C21:2
+ Ciudades principales/Grandes=01-11
+ Medianos=12
+ Peque§os=13
N03;COLTXT R+1=----
N05
N03
N03 Zona
Fld C23, C23:1
+ Urbana=1
+ Rural=2
N03;COLTXT R+1=----
N05

L LOCALDAD
N00;C=C(45,46).GE.1 
*Include BAS2;X=Entrevistados que viven en Bogot†;Y=
N03 LOCALIDAD;Nosort 
Fld C45, C45:2
+ UsaquÇn=01
+ Chapinero=02
+ Santa Fe=03
+ San Crist¢bal=04
+ Usme=05
+ Tunjuelito=06
+ Bosa=07
+ Kennedy=08
+ Fontib¢n=09
+ Engativ†=10
+ Suba=11
+ Barrios Unidos=12
+ Teusaquillo=13
+ Los M†rtires=14
+ Antonio Nari§o=15
+ Puente Aranda=16
+ La Candelaria=17
+ Rafael Uribe Uribe=18
+ Ciudad Bolivar=19
N03;COLTXT R+1=----
N05

L PF1
TTL FILTRO
*Include BAS 
N03 PF1. Si las elecciones a la Presidencia fueran ma§ana, ®quÇ tan probable es que usted vote?
Col 34
+ Definitivamente s° votar°a=1
+ Probablemente s° votar°a=2
N03;COLTXT R+1=----
N05

L P1
TTL INTENCI‡N DE VOTO PRESIDENCIA
*Include BAS 
N03 P1.	®Por cu†l de las siguientes f¢rmulas presidenciales votar°a usted?;Nosort
Fld C35, C35:2
+ Iv†n Cepeda Castro - A°da QuilcuÇ=01
+ Abelardo de la Espriella - JosÇ Manuel Restrepo=02
+ Paloma Valencia - Juan Daniel Oviedo=03
+ Sergio Fajardo - Edna Bonilla=04
+ Claudia L¢pez - Leonardo Huerta=05
+ Roy Barreras - Martha Luc°a Zamora=06
+ Miguel Uribe Londo§o - Luisa Fernanda Villegas=07
+ Mauricio Lizcano - Pedro de la Torre=08
+ Clara L¢pez Obreg¢n - Mar°a Consuelo del R°o Mantilla=09
+ Luis Gilberto Murillo - Luz Mar°a Zapata=10
+ Carlos Caicedo - Omar Nelson Alarc¢n=11
+ Santiago Botero - Carlos Fernando Cuevas=12
+ Sondra McCollins - Leonardo Karam Helo=13
+ Gustavo Matamoros - Robinson Alonso Giraldo=14
+ Voto en Blanco=66;%Nosort
+ Ninguno=88;%Nosort	
+ NS/NR=99;%Nosort
N03;COLTXT R+1=----
N05

L P2
TTL INTENCI‡N DE VOTO PRESIDENCIA
*Include BAS 
N03 P2.	®El anuncio de los nombres que van a la vicepresidencia 
*Include sino;Col(A)=37;T=    influy¢ en la elecci¢n de su candidato a la presidencia?

L P3
TTL INTENCI‡N DE VOTO PRESIDENCIA
*Include BAS 
N03 P3.	Pensando en los nombres de las f¢rmulas vicepresidenciales;Nosort
N03     ®a usted cu†l le gusta m†s?;Nosort
*Include Rtasp3;Col(A)=38
N03;Nosort
N03 P4.	y a usted ®cu†l le gusta menos?;Nosort
*Include Rtasp3;Col(A)=40

L P5
TTL INTENCI‡N DE VOTO PRESIDENCIA
*Include BAS 
N03 P5.	En su opini¢n, ®cu†l es el principal problema que tiene Colombia en estos momentos?;Nosort
*Include Rtasp5;Col(A)=42

L P6
TTL INTENCI‡N DE VOTO PRESIDENCIA
*Include BAS 
N03 P6.	De los problemas que le mencionÇ, ®en cu†l quisiera que se enfocara el pr¢ximo presidente?;Nosort
*Include Rtasp5;Col(A)=44

L P7
TTL VOTO SEGUNDA VUELTA
*Include BAS 
N03 P7. Si los candidatos a segunda vuelta fueran: ....;Nosort
N03     ®usted por cu†l de los dos votar°a?;Nosort
*Include Rtasp7;Col(A)=46;B=Iv†n Cepeda;D=Abelardo de la Espriella

L P8
TTL VOTO SEGUNDA VUELTA
*Include BAS 
N03 P8. Y si los candidatos fueran: ....;Nosort
N03     ®usted por cu†l de los dos votar°a?;Nosort
*Include Rtasp7;Col(A)=47;B=Iv†n Cepeda;D=Paloma Valencia

L P9
TTL VOTO SEGUNDA VUELTA
*Include BAS 
N03 P9. Y si los candidatos fueran: ....;Nosort
N03     ®usted por cu†l de los dos votar°a?;Nosort
*Include Rtasp7;Col(A)=48;B=Iv†n Cepeda;D=Sergio Fajardo

L P10
TTL VOTO SEGUNDA VUELTA
*Include BAS 
N03 P10. Y si los candidatos fueran: ....;Nosort
N03     ®usted por cu†l de los dos votar°a?;Nosort
*Include Rtasp7;Col(A)=49;B=Iv†n Cepeda;D=Claudia L¢pez

L P11
TTL VOTO SEGUNDA VUELTA
*Include BAS 
N03 P11. Y si los candidatos fueran: ....;Nosort
N03     ®usted por cu†l de los dos votar°a?;Nosort
*Include Rtasp7;Col(A)=50;B=Iv†n Cepeda;D=Roy Barreras

L P12
TTL VOTO SEGUNDA VUELTA
*Include BAS 
N03 P12. Y si los candidatos fueran: ....;Nosort
N03     ®usted por cu†l de los dos votar°a?;Nosort
*Include Rtasp7;Col(A)=51;B=Iv†n Cepeda;D=Miguel Uribe Londo§o

L P13
TTL VOTO SEGUNDA VUELTA
*Include BAS 
N03 P13. Y si los candidatos fueran: ....;Nosort
N03     ®usted por cu†l de los dos votar°a?;Nosort
*Include Rtasp7;Col(A)=52;B=Iv†n Cepeda;D=Mauricio Lizcano

L P14
TTL VOTO SEGUNDA VUELTA
*Include BAS 
N03 P14. Y si los candidatos fueran: ....;Nosort
N03     ®usted por cu†l de los dos votar°a?;Nosort
*Include Rtasp7;Col(A)=53;B=Iv†n Cepeda;D=Luis Gilberto Murillo

L P15
TTL IMAGEN/GESTI‡N
*Include BAS 
N03 P15. ®Usted tiene una imagen positiva o negativa del presidente Gustavo Petro?
Col 54
+ Positiva=1
+ Negativa=2
+ NS/NR=9
N03;COLTXT R+1=----
N05


L P16
TTL PARTIDOS O MOVIMIENTOS POL÷TICOS
*Include BAS 
*Include sino;Col(A)=58;T=P16. ®El d°a de ayer usted habl¢ de pol°tica?

L P17
TTL VARIABLES SOCIODEMOGRµFICOS 
*Include BAS 
N03 P17. Tipo de vivienda;Nosort
Fld C59, C59:1
+ Casa=1
+ Apartamento=2
+ Cuarto(s)=3
+ Vivienda tradicional ind°gena=4
+ Otro (carpa/contenedor/vag¢n/embarcaci¢n/cueva/refugio natural/etc)=5
N03;COLTXT R+1=----
N05

L P18
TTL VARIABLES SOCIODEMOGRµFICOS 
*Include BAS 
N03 P18. ®En quÇ actividad ocup¢ la mayor parte del tiempo LA SEMANA PASADA?;Nosort
Fld C60, C60:1
+ Trabajando=1
+ Buscando trabajo=2
+ Estudiando=3
+ Oficios de hogar=4
+ Incapacitado permanente para trabajar=5
+ Otra actividad=6;%Nosort
N03;COLTXT R+1=----
N05

L P19
TTL VARIABLES SOCIODEMOGRµFICOS 
*Include BAS 
N03 P19. ®Cu†l es el nivel educativo m†s alto que ha alcanzado?
Fld C61, C61:2
+ Ninguno=01
+ Preescolar=02
+ B†sica primaria (1ro a 5to)=03
+ B†sica secundaria (6to a 9no)=04
+ Media (10mo a 13vo)=05
+ TÇcnico sin t°tulo=06
+ TÇcnico con t°tulo=07
+ Tecnol¢gico sin t°tulo=08
+ Tecnol¢gico con t°tulo=09
+ Universitario sin t°tulo=10
+ Universitario con t°tulo=11
+ Posgrado sin t°tulo=12
+ Posgrado con t°tulo=13
N03;COLTXT R+1=----
N05

L P20
TTL VARIABLES SOCIODEMOGRµFICOS 
*Include BAS 
N03 P20. ®A cu†l de los siguientes reg°menes de seguridad social en salud est† afiliado?;Nosort
Col 63
+ Contributivo (EPS)=1
+ Especial (Fuerzas Armadas/Ecopetrol/universidades p£blicas/magisterio)=2
+ Subsidiado (EPS-S)=3
+ NS/NR=9;%Nosort
N03;COLTXT R+1=----
N05


