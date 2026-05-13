* Encoding: windows-1252.



DATASET CLOSE * .
GET FILE='C:\Diana\2026\OPINIÓN\009000260000 COLOMBIA OPINA No 20\PROCESAMIENTO\Data SPSS\009000260000 COLOMBIA OPINA No 20 Tendencias.sav' .
DATASET NAME Data_Tendencias WINDOW=FRONT .



/****************************************************************************
/*                  INFORME TOTAL 
/****************************************************************************

/* SCRIPT PARA TÍTULOS .

SCRIPT FILE =  'C:\Scripts\Macro Variable Labels.sbs' .

INSERT FILE = 'C:\Diana\2026\OPINIÓN\009000260000 COLOMBIA OPINA No 20\PROCESAMIENTO\Sintaxis\4_0_009000260000_Nets.sps' .


/**********************************************.
* MACRO QUE GENERA TABLA DE ÚNICA RESPUESTA . 
/**********************************************.


DEFINE @UNI_RESP_BASE      (VarCat     = !CHAREND('/')
                           /Título     = !CHAREND('/')
                           /SubTítulo  = !CMDEND)
TEMPORARY.
MISSING VALUES !VarCat (888).

CTABLES
  /FORMAT EMPTY='-' MISSING='-'
  /VLABELS VARIABLES = B_TOTAL F_TENDENCIA      	DISPLAY=NONE
  /VLABELS VARIABLES = !VarCat              	DISPLAY=NONE
  /TABLE
   !VarCat [C][COLPCT.COUNT ' ' F8.1, TOTALS[COLPCT.COUNT 'Total ' F8.0, COUNT 'Base Ponderada' F8.0, UCOUNT 'Base No Ponderada' F8.0]]
   BY F_TENDENCIA > (B_TOTAL [C])
  /SLABELS POSITION     = ROW                   VISIBLE=YES
  /CATEGORIES VARIABLES = !VarCat               ORDER=A KEY=VALUE EMPTY=EXCLUDE TOTAL=YES LABEL=' ' POSITION=AFTER MISSING=INCLUDE
  /CATEGORIES VARIABLES = F_TENDENCIA [202602]                          EMPTY = INCLUDE
  /CATEGORIES VARIABLES = B_TOTAL  EMPTY=INCLUDE
  /TITLES
   TITLE= !EVAL(!CONCAT(!Título,' ','!',!VarCat,!SubTítulo,' ')) .
!ENDDEFINE .


/**********************************************.
* MACRO QUE GENERA TABLA DE ÚNICA RESPUESTA CON TTB  . 
/**********************************************.

DEFINE @UNI_RESP_BASE_TTB             (Var    	=!CHAREND('/')
                /Cat    	=!CHAREND('/')
                /Titulo    	=!CHAREND('/')
                /SubTitulo 	=!CMDEND)

TEMPORARY.
MISSING VALUE !Var () .
COMPUTE !CONCAT('R_',!Var) = !Var .
MISSING VALUE !CONCAT("R_",!Var) (999) .

CTABLES
  /FORMAT EMPTY = '-' MISSING   = '-'
  /VLABELS VARIABLES = !Var	!CONCAT('R_',!Var) B_TOTAL 	F_TENDENCIA	DISPLAY=NONE
  /TABLE
   !Var [C][COLPCT.COUNT ' ' F20.1] +
   !CONCAT("R_",!Var) [C][TOTALS[COLPCT.COUNT 'Total' F20.0,MEAN 'Promedio' F20.2, MODE 'Moda' F20.0, MEDIAN 'Mediana' F20.0, COUNT 'Base Ponderada' F20.0, UCOUNT 'Base No Ponderada' F20.0]]
   BY F_TENDENCIA > (B_TOTAL [C] )
  /SLABELS POSITION     = ROW
  /CATEGORIES VARIABLES = !Var !EVAL(!CONCAT("!NET_",!Cat)) 				EMPTY=EXCLUDE  TOTAL = NO  POSITION = BEFORE
  /CATEGORIES VARIABLES = !CONCAT("R_",!Var) [HSUBTOTAL=' ',999,OTHERNM] 		EMPTY=INCLUDE  TOTAL = NO  POSITION = BEFORE
  /CATEGORIES VARIABLES = F_TENDENCIA [202602]                          EMPTY = INCLUDE
  /CATEGORIES VARIABLES = B_TOTAL                  EMPTY=INCLUDE
    /TITLES
   TITLE= !EVAL(!CONCAT(!Titulo,' !',!Var,!SubTitulo,' ')) .
!ENDDEFINE .


/**********************************************.
* MACRO QUE GENERA TABLA DE ÚNICA RESPUESTA CON NET DE SI/NO LO CONOCE .
/**********************************************.

DEFINE @UNI_RESP_NET    (VarCat     = !CHAREND('/')
                        /Título     = !CHAREND('/')
                        /SubTítulo  = !CMDEND)
TEMPORARY .
COMPUTE !CONCAT ("R",!VarCat) =!VarCat .
CTABLES
  /FORMAT EMPTY='-' MISSING='-'
  /VLABELS VARIABLES = B_TOTAL F_TENDENCIA     			DISPLAY=NONE
  /VLABELS VARIABLES=!VarCat !CONCAT ("R",!VarCat)     	DISPLAY=NONE
  /TABLE
   !VarCat [C][COLPCT.COUNT ' ' F8.1, TOTALS[COLPCT.COUNT ' ' F8.1]] +
   !CONCAT ("R",!VarCat) [C][COLPCT.COUNT 'TOTAL' F8.0, COUNT 'Base Ponderada' F8.0, UCOUNT 'Base No Ponderada' F8.0]
   BY F_TENDENCIA > (B_TOTAL [C] )
  /SLABELS POSITION=ROW                             VISIBLE=YES
  /CATEGORIES VARIABLES=!VarCat [SUBTOTAL ='  SI LO(A) CONOCE',1,2,999, HSUBTOTAL ='  NO LO(A) CONOCE',4,0]   EMPTY=EXCLUDE TOTAL=NO  LABEL='TOTAL' POSITION=BEFORE
  /CATEGORIES VARIABLES=!CONCAT ("R",!VarCat) [OTHERNM, HSUBTOTAL=' '] EMPTY=EXCLUDE TOTAL=NO LABEL=' ' POSITION=AFTER
  /CATEGORIES VARIABLES = F_TENDENCIA [202602]                          EMPTY = INCLUDE
  /CATEGORIES VARIABLES = B_TOTAL  EMPTY=INCLUDE
  /TITLES
   TITLE= !EVAL(!CONCAT(!Título,' ','!',!VarCat,!SubTítulo,' ')) .
!ENDDEFINE .


/**********************************************.
* MACRO QUE GENERA TABLA DE ÚNICA RESPUESTA CON NET DE FUE/ NO FUE VÍCTIMA .
/**********************************************.

DEFINE @UNI_RESP_NET_2    (VarCat     = !CHAREND('/')
                        /Título     = !CHAREND('/')
                        /SubTítulo  = !CMDEND)
TEMPORARY .
COMPUTE !CONCAT ("R",!VarCat) =!VarCat .
CTABLES
  /FORMAT EMPTY='-' MISSING='-'
  /VLABELS VARIABLES = B_TOTAL F_TENDENCIA     			DISPLAY=NONE
  /VLABELS VARIABLES=!VarCat !CONCAT ("R",!VarCat)     	DISPLAY=NONE
  /TABLE
   !VarCat [C][COLPCT.COUNT ' ' F8.1, TOTALS[COLPCT.COUNT ' ' F8.1]] +
   !CONCAT ("R",!VarCat) [C][COLPCT.COUNT 'TOTAL' F8.0, COUNT 'Base Ponderada' F8.0, UCOUNT 'Base No Ponderada' F8.0]
   BY F_TENDENCIA > (B_TOTAL [C])
  /SLABELS POSITION=ROW                             VISIBLE=YES
  /CATEGORIES VARIABLES=!VarCat [SUBTOTAL ='  FUE VÍCTIMA DE ALGÚN DELITO',1,2, HSUBTOTAL ='  NO FUE VÍCTIMA DE ALGÚN DELITO',4,0]   EMPTY=EXCLUDE TOTAL=NO  LABEL='TOTAL' POSITION=BEFORE
  /CATEGORIES VARIABLES=!CONCAT ("R",!VarCat) [OTHERNM, HSUBTOTAL=' '] EMPTY=EXCLUDE TOTAL=NO LABEL=' ' POSITION=AFTER
  /CATEGORIES VARIABLES = F_TENDENCIA [202602]                          EMPTY = INCLUDE
  /CATEGORIES VARIABLES = B_TOTAL  EMPTY=INCLUDE
  /TITLES
   TITLE= !EVAL(!CONCAT(!Título,' ','!',!VarCat,!SubTítulo,' ')) .
!ENDDEFINE .



/**********************************************.
* MACRO QUE GENERA TABLA DE ÚNICA RESPUESTA CON NET DE SE/ NO SE PUEDE CONECTAR A INTERNET .
/**********************************************.

DEFINE @UNI_RESP_NET_3    (VarCat     = !CHAREND('/')
                        /Título     = !CHAREND('/')
                        /SubTítulo  = !CMDEND)
TEMPORARY .
COMPUTE !CONCAT ("R",!VarCat) =!VarCat .
CTABLES
  /FORMAT EMPTY='-' MISSING='-'
  /VLABELS VARIABLES = B_TOTAL F_TENDENCIA     			DISPLAY=NONE
  /VLABELS VARIABLES=!VarCat !CONCAT ("R",!VarCat)     	DISPLAY=NONE
  /TABLE
   !VarCat [C][COLPCT.COUNT ' ' F8.1, TOTALS[COLPCT.COUNT ' ' F8.1]] +
   !CONCAT ("R",!VarCat) [C][COLPCT.COUNT 'TOTAL' F8.0, COUNT 'Base Ponderada' F8.0, UCOUNT 'Base No Ponderada' F8.0]
   BY F_TENDENCIA > (B_TOTAL [C])
  /SLABELS POSITION=ROW                             VISIBLE=YES
  /CATEGORIES VARIABLES=!VarCat [SUBTOTAL ='  SE PUEDE CONECTAR A INTERNET',1,2,3, HSUBTOTAL ='  NO SE PUEDE CONECTAR A INTERNET',4,0]   EMPTY=EXCLUDE TOTAL=NO  LABEL='TOTAL' POSITION=BEFORE
  /CATEGORIES VARIABLES=!CONCAT ("R",!VarCat) [OTHERNM, HSUBTOTAL=' '] EMPTY=EXCLUDE TOTAL=NO LABEL=' ' POSITION=AFTER
  /CATEGORIES VARIABLES = F_TENDENCIA [202602]                          EMPTY = INCLUDE
  /CATEGORIES VARIABLES = B_TOTAL  EMPTY=INCLUDE
  /TITLES
   TITLE= !EVAL(!CONCAT(!Título,' ','!',!VarCat,!SubTítulo,' ')) .
!ENDDEFINE .


/**********************************************.
* MACRO QUE GENERA TABLA DE ÚNICA RESPUESTA CON NET DE SI VOTÓ O NO .
/**********************************************.

DEFINE @UNI_RESP_NET_4    (VarCat     = !CHAREND('/')
                        /Título     = !CHAREND('/')
                        /SubTítulo  = !CMDEND)
TEMPORARY .
COMPUTE !CONCAT ("R",!VarCat) =!VarCat .
CTABLES
  /FORMAT EMPTY='-' MISSING='-'
  /VLABELS VARIABLES = B_TOTAL F_TENDENCIA     			DISPLAY=NONE
  /VLABELS VARIABLES=!VarCat !CONCAT ("R",!VarCat)     	DISPLAY=NONE
  /TABLE
   !VarCat [C][COLPCT.COUNT ' ' F8.1, TOTALS[COLPCT.COUNT ' ' F8.1]] +
   !CONCAT ("R",!VarCat) [C][COLPCT.COUNT 'TOTAL' F8.0, COUNT 'Base Ponderada' F8.0, UCOUNT 'Base No Ponderada' F8.0]
   BY F_TENDENCIA > (B_TOTAL [C])
  /SLABELS POSITION=ROW                             VISIBLE=YES
  /CATEGORIES VARIABLES=!VarCat [SUBTOTAL ='  SI VOTÓ',7,26,995,999, HSUBTOTAL ='  NO VOTÓ/ NO SABE',1000]   EMPTY=EXCLUDE TOTAL=NO  LABEL='TOTAL' POSITION=BEFORE
  /CATEGORIES VARIABLES=!CONCAT ("R",!VarCat) [OTHERNM, HSUBTOTAL=' '] EMPTY=EXCLUDE TOTAL=NO LABEL=' ' POSITION=AFTER
  /CATEGORIES VARIABLES = F_TENDENCIA [202602]                          EMPTY = INCLUDE
  /CATEGORIES VARIABLES = B_TOTAL  EMPTY=INCLUDE
  /TITLES
   TITLE= !EVAL(!CONCAT(!Título,' ','!',!VarCat,!SubTítulo,' ')) .
!ENDDEFINE .




/**********************************************.
* MACRO QUE GENERA TABLA DE ÚNICA RESPUESTA CON NET DE URBANO Y RURAL .
/**********************************************.

DEFINE @UNI_RESP_NET_5    (VarCat     = !CHAREND('/')
                        /Título     = !CHAREND('/')
                        /SubTítulo  = !CMDEND)
TEMPORARY .
COMPUTE !CONCAT ("R",!VarCat) =!VarCat .
CTABLES
  /FORMAT EMPTY='-' MISSING='-'
  /VLABELS VARIABLES = B_TOTAL F_TENDENCIA     			DISPLAY=NONE
  /VLABELS VARIABLES=!VarCat !CONCAT ("R",!VarCat)     	DISPLAY=NONE
  /TABLE
   !VarCat [C][COLPCT.COUNT ' ' F8.1, TOTALS[COLPCT.COUNT ' ' F8.1]] +
   !CONCAT ("R",!VarCat) [C][COLPCT.COUNT 'TOTAL' F8.0, COUNT 'Base Ponderada' F8.0, UCOUNT 'Base No Ponderada' F8.0]
   BY F_TENDENCIA > (B_TOTAL [C] )
  /SLABELS POSITION=ROW                             VISIBLE=YES
  /CATEGORIES VARIABLES=!VarCat [SUBTOTAL ='  URBANO',1,2,3,4,5,6,999, SUBTOTAL ='  RURAL',7,8,9,10,11,12,888]   EMPTY=EXCLUDE TOTAL=NO  LABEL='TOTAL' POSITION=BEFORE
  /CATEGORIES VARIABLES=!CONCAT ("R",!VarCat) [OTHERNM, HSUBTOTAL=' '] EMPTY=EXCLUDE TOTAL=NO LABEL=' ' POSITION=AFTER
  /CATEGORIES VARIABLES = F_TENDENCIA [202602]                          EMPTY = INCLUDE
  /CATEGORIES VARIABLES = B_TOTAL  EMPTY=INCLUDE
  /TITLES
   TITLE= !EVAL(!CONCAT(!Título,' ','!',!VarCat,!SubTítulo,' ')) .
!ENDDEFINE .



/**********************************************.
* MACRO QUE GENERA TABLA DE UNICA RESPUESTA CON PROMEDIO
/**********************************************.

DEFINE @UNIRES_PROM     (Var        = !CHAREND('/')
                        /Titulo     = !CHAREND('/')
                        /SubTitulo  = !CMDEND)

TEMPORARY .
MISSING VALUE !Var () .
COMPUTE !CONCAT("R_",!Var) = !Var .
MISSING VALUE !CONCAT("R_",!Var) (999) .

CTABLES
  /FORMAT EMPTY='-' MISSING='-'
  /VLABELS VARIABLES= B_TOTAL F_TENDENCIA   DISPLAY=NONE
  /VLABELS VARIABLES= !Var !CONCAT("R_",!Var)    DISPLAY=NONE
  /TABLE
   !Var [C][COLPCT.COUNT ' ' F20.0] +
   !CONCAT("R_",!Var) [C][TOTALS[COLPCT.COUNT 'Total' F20.0,MEAN 'Promedio' F20.2,COUNT 'Base Ponderada' F20.0, UCOUNT 'Base No Ponderada' F20.0]]
   BY F_TENDENCIA > (B_TOTAL [C] )
  /SLABELS POSITION=ROW VISIBLE=YES
  /CATEGORIES VARIABLES=!Var [5,4,SUBTOTAL='    Calificación 3/2/1' ,3,2,1,HSUBTOTAL='No sabe/ No responde',999] EMPTY=INCLUDE TOTAL = NO POSITION = BEFORE
  /CATEGORIES VARIABLES=!CONCAT("R_",!Var) [HSUBTOTAL=' ',999,OTHERNM]                                           EMPTY=INCLUDE TOTAL = NO POSITION = BEFORE
  /CATEGORIES VARIABLES = F_TENDENCIA [202602]                          EMPTY = INCLUDE
  /CATEGORIES VARIABLES = B_TOTAL   EMPTY=INCLUDE
  /TITLES
   TITLE= !EVAL(!CONCAT(!Titulo,' ','!',!Var,!SubTitulo,' ')) .
!ENDDEFINE .


/**********************************************.
/* MACRO QUE GENERA TABLA DE MÚLTIPLE RESPUESTA  .
/**********************************************.

DEFINE @MULT_RESP_BASE  (VarCat     = !CHAREND('/')
                        /Título     = !CHAREND('/')
                        /SubTítulo  = !CMDEND)
CTABLES
  /FORMAT EMPTY='-' MISSING='-'
  /VLABELS VARIABLES = B_TOTAL F_TENDENCIA     	DISPLAY=NONE
  /VLABELS VARIABLES =!CONCAT('$',!VarCat)      DISPLAY=NONE
  /TABLE
   !CONCAT('$',!VarCat) [COLPCT.RESPONSES.COUNT ' ' F8.1, TOTALS[COLPCT.RESPONSES.COUNT ' ' F8.0, COUNT 'Base Ponderada' F8.0, UCOUNT 'Base No Ponderada' F8.0]]
   BY F_TENDENCIA > (B_TOTAL [C] )
  /SLABELS POSITION=ROW                             VISIBLE=YES
  /CATEGORIES VARIABLES=!CONCAT('$',!VarCat)        ORDER=D KEY=COUNT EMPTY=EXCLUDE TOTAL=YES LABEL=' ' POSITION=AFTER
  /CATEGORIES VARIABLES = F_TENDENCIA [202602]                          EMPTY = INCLUDE
  /CATEGORIES VARIABLES = B_TOTAL   EMPTY=INCLUDE
  /TITLES
   TITLE= !EVAL(!CONCAT(!Título,' ','!',!VarCat,!SubTítulo,' ')) .
!ENDDEFINE .


/**********************************************.
/* MACRO QUE GENERA TABLA DE MÚLTIPLE RESPUESTA CON NET .
/**********************************************.

DEFINE @MULTIPLENET    (Var    	= !CHAREND('/')
                       /Cat    	= !CHAREND('/')
                       /Titulo    	= !CHAREND('/')
                       /SubTitulo = !CMDEND)

CTABLES
  /FORMAT EMPTY = '-' MISSING   = '-'
  /VLABELS VARIABLES = !CONCAT('$',!Var) !CONCAT('$R',!Var)  B_TOTAL F_TENDENCIA   DISPLAY = NONE
  /TABLE
   !CONCAT('$',!Var) [COLPCT.COUNT ' ' F8.1, TOTALS[COLPCT.VALIDN ' ' F8.1]] +
   !CONCAT('$R',!Var) [COLPCT.RESPONSES.COUNT 'Total' F8.0, COUNT 'Base Ponderada' F8.0, UCOUNT 'Base No Ponderada' F8.0]
   BY F_TENDENCIA > (B_TOTAL [C] )
  /SLABELS POSITION = ROW
  /CATEGORIES VARIABLES = !CONCAT('$',!Var) !EVAL(!CONCAT("!NET_",!Cat))        EMPTY =EXCLUDE TOTAL =NO POSITION =BEFORE
  /CATEGORIES VARIABLES = !CONCAT('$R',!Var) !EVAL(!CONCAT("!TOTAL_",!Cat))     EMPTY =EXCLUDE TOTAL =NO POSITION =AFTER
  /CATEGORIES VARIABLES = F_TENDENCIA [202602]                          EMPTY = INCLUDE
  /CATEGORIES VARIABLES = B_TOTAL  EMPTY=INCLUDE
  /TITLES
   TITLE= !EVAL(!CONCAT(!Titulo,' !',!Var,!SubTitulo,' ')) . 
!ENDDEFINE .


/**********************************************
/*        CUADROS RESUMEN .
/*********************************************.
DEFINE @RESUMEN_CAT     (Var    =	!CHAREND('/')
		/Estad  =	!CHAREND('/')
		/Cat    =	!CHAREND('/')
		/Titulo =	!CMDEND)
CTABLES
  /FORMAT EMPTY = '-' MISSING = '-'
  /VLABELS VARIABLES = !Var         DISPLAY = LABEL
  /TABLE
   !HEAD(!Var) [C][!Estad ' ' F8.1]
   !DO !I !IN (!TAIL(!Var)) + !I [C][!Estad ' ' F8.1] !DOEND
  /CLABELS ROWLABELS    = OPPOSITE
  /CATEGORIES VARIABLES = !Var !CONCAT("[",!Cat,"]")    EMPTY = INCLUDE POSITION =BEFORE LABEL=' '
  /TITLES
   TITLE = !Titulo .
!ENDDEFINE .


/*<***************************************************************************>.
/*<***************************************************************************>.
/*                 I N I C I O   T A B L A S   I N F O R M E                   .
/*<***************************************************************************>.

/* ACTIVO EL FACTOR DE PONDERACIÓN.

WEIGHT BY FACTOR_PONDERACION.

@UNI_RESP_BASE    VarCat = P23 		/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P24_GRUPOEDAD      /SubTítulo=' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P243 		/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P255 		/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P244 		/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P197 		/SubTítulo= ' ' 'Base: Total Encuestados' .

@MULT_RESP_BASE   VarCat = P228 	/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P245 		/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P247 		/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P231 		/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P232 		/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P233 		/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P256 		/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P242 		/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_NET    VarCat = P5A_3      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_25      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_11      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_88      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_87      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_100      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_45      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_98      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_102      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_38      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_104      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_106      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_7      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_30      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_90      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_97      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_44      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_2      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_50      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_27      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_55      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_95      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_65      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_109      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_60      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_110      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_112      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_113      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_250      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_251      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_252      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_20      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_6      	/Título='P5A. ¿Conoce usted o ha oído mencionar a ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .


USE ALL.
COMPUTE filter_$=(F_TENDENCIA = 202602).
FILTER BY filter_$.
EXECUTE .

@RESUMEN_CAT      Var= P5A_3 P5A_25 P5A_11 P5A_88 P5A_87 P5A_100 P5A_45 P5A_98 P5A_102 P5A_38 P5A_104 P5A_106 P5A_7 P5A_30 
    P5A_90 P5A_97 P5A_44 P5A_2 P5A_50 P5A_27 P5A_55 P5A_95 P5A_65 P5A_109 P5A_60 P5A_110 P5A_112 P5A_113 P5A_250 P5A_251 P5A_252 P5A_20 P5A_6
		/ESTAD=ROWPCT.TOTALN     /CAT=SUBTOTAL ='  SI LO(A) CONOCE',1,2,999, HSUBTOTAL ='  NO LO(A) CONOCE',4
		/Titulo='P6. ¿Tiene usted una opinión favorable o desfavorable de este Expresidente?'  	'' 'CUADRO RESUMEN' '' 'Base: Total Encuestados'.

FILTER OFF.



@UNI_RESP_BASE    VarCat = P1 		/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P2  	/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P183  	/SubTítulo= ' ' 'Base: Total Encuestados' .

@MULTIPLENET  	  Var = P4	/Cat = P4_OTRO  /SubTitulo = ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P155  	/SubTítulo= ' ' 'Base: Total Encuestados' .


@UNI_RESP_NET    VarCat = P6_1      /Título='P6. ¿Tiene usted una opinión favorable o desfavorable de este Expresidente?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P6_2      /Título='P6. ¿Tiene usted una opinión favorable o desfavorable de este Expresidente?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P6_3      /Título='P6. ¿Tiene usted una opinión favorable o desfavorable de este Expresidente?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P6_4      /Título='P6. ¿Tiene usted una opinión favorable o desfavorable de este Expresidente?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P6_5      /Título='P6. ¿Tiene usted una opinión favorable o desfavorable de este Expresidente?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P6_6      /Título='P6. ¿Tiene usted una opinión favorable o desfavorable de este Expresidente?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .

USE ALL.
COMPUTE filter_$=(F_TENDENCIA = 202602).
FILTER BY filter_$.
EXECUTE .

@RESUMEN_CAT      Var= P6_1 P6_2 P6_3 P6_4 P6_5 P6_6
		/ESTAD=ROWPCT.TOTALN     /CAT=SUBTOTAL ='  SI LO(A) CONOCE',1,2,999, HSUBTOTAL ='  NO LO(A) CONOCE',0
		/Titulo='P6. ¿Tiene usted una opinión favorable o desfavorable de este Expresidente?'  	'' 'CUADRO RESUMEN' '' 'Base: Total Encuestados'.

FILTER OFF.


@UNI_RESP_NET    VarCat = P5A_21      	/Título='P5A. ¿Conoce usted o ha oído mencionar a/al ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_77      	/Título='P5A. ¿Conoce usted o ha oído mencionar a/al ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_84      	/Título='P5A. ¿Conoce usted o ha oído mencionar a/al ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_85      	/Título='P5A. ¿Conoce usted o ha oído mencionar a/al ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_NET    VarCat = P5A_111      	/Título='P5A. ¿Conoce usted o ha oído mencionar a/al ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.' ' '/SubTítulo=' ' 'Base: Total Encuestados' .


USE ALL.
COMPUTE filter_$=(F_TENDENCIA = 202602).
FILTER BY filter_$.
EXECUTE .

@RESUMEN_CAT     Var= P5A_21 P5A_77 P5A_84 P5A_85  P5A_111
/ESTAD=ROWPCT.TOTALN     /CAT=SUBTOTAL ='  SI LO(A) CONOCE',1,2,999, HSUBTOTAL ='  NO LO(A) CONOCE',4
/Titulo='P5A. ¿Conoce usted o ha oído mencionar a/al ___. ¿Tiene usted una opinión favorable o desfavorable de esta persona?.'  '' 'CUADRO RESUMEN' '' 'Base: Total Encuestados'.

FILTER OFF.


@UNI_RESP_BASE    VarCat = P7_1       	/Título='P7. ¿Tiene usted una opinión favorable o desfavorable de ___?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = P7_2       	/Título='P7. ¿Tiene usted una opinión favorable o desfavorable de ___?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = P7_3       	/Título='P7. ¿Tiene usted una opinión favorable o desfavorable de ___?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = P7_4       	/Título='P7. ¿Tiene usted una opinión favorable o desfavorable de ___?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = P7_5       	/Título='P7. ¿Tiene usted una opinión favorable o desfavorable de ___?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = P7_6       	/Título='P7. ¿Tiene usted una opinión favorable o desfavorable de ___?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = P7_21      	/Título='P7. ¿Tiene usted una opinión favorable o desfavorable de ___?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = P7_8       	/Título='P7. ¿Tiene usted una opinión favorable o desfavorable de ___?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = P7_9       	/Título='P7. ¿Tiene usted una opinión favorable o desfavorable de ___?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = P7_10      	/Título='P7. ¿Tiene usted una opinión favorable o desfavorable de ___?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = P7_11      	/Título='P7. ¿Tiene usted una opinión favorable o desfavorable de ___?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = P7_12      	/Título='P7. ¿Tiene usted una opinión favorable o desfavorable de ___?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = P7_13      	/Título='P7. ¿Tiene usted una opinión favorable o desfavorable de ___?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = P7_14      	/Título='P7. ¿Tiene usted una opinión favorable o desfavorable de ___?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = P7_15      	/Título='P7. ¿Tiene usted una opinión favorable o desfavorable de ___?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = P7_17      	/Título='P7. ¿Tiene usted una opinión favorable o desfavorable de ___?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = P7_18      	/Título='P7. ¿Tiene usted una opinión favorable o desfavorable de ___?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = P7_20      	/Título='P7. ¿Tiene usted una opinión favorable o desfavorable de ___?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = P7_23      	/Título='P7. ¿Tiene usted una opinión favorable o desfavorable de ___?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .

USE ALL.
COMPUTE filter_$=(F_TENDENCIA = 202602).
FILTER BY filter_$.
EXECUTE .

@RESUMEN_CAT      Var= P7_1 P7_2 P7_3 P7_4 P7_5 P7_6 P7_21 P7_8 P7_9 P7_10 P7_11 P7_12 P7_13 P7_14 P7_15 P7_17 P7_18 P7_20 P7_23
		/ESTAD=ROWPCT.TOTALN     /CAT=1,2,999  /Titulo='P7. ¿Tiene usted una opinión favorable o desfavorable de ___?'  '' 'CUADRO RESUMEN' '' 'Base: Total Encuestados'.

FILTER OFF.

@UNI_RESP_BASE    VarCat = P185  	/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P165  	/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P234  	/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P166  	/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P182  	/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P10  	/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE_TTB   Var = P54    /Cat = P54  /SubTitulo ='' 'Base: Total Encuestados' .

@MULT_RESP_BASE   VarCat = P73 	/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P235  	/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P236  	/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P251  	/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P238_1       	/Título='P238. ¿Qué tan de acuerdo o en desacuerdo está usted con las siguienes frases?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = P238_8       	/Título='P238. ¿Qué tan de acuerdo o en desacuerdo está usted con las siguienes frases?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = P238_5       	/Título='P238. ¿Qué tan de acuerdo o en desacuerdo está usted con las siguienes frases?' ' '/SubTítulo=' ' 'Base: Total Encuestados' .


USE ALL.
COMPUTE filter_$=(F_TENDENCIA = 202602).
FILTER BY filter_$.
EXECUTE .

@RESUMEN_CAT      Var= P238_1  P238_8   P238_5 
		/ESTAD=ROWPCT.TOTALN     /CAT=1,2,999  /Titulo='P238. ¿Qué tan de acuerdo o en desacuerdo está usted con las siguienes frases?'  '' 'CUADRO RESUMEN' '' 'Base: Total Encuestados'.

FILTER OFF.



@UNI_RESP_BASE    VarCat = P241  	/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P164  	/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P20yP21 	/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P64yP65 	/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P149 	/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_NET_4    VarCat = RP150	/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P22 	/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P40 	/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_NET_5    VarCat = RP40 	/SubTítulo= ' ' 'Base: Total Encuestados' .

@MULT_RESP_BASE   VarCat = P42 	/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = P26 	/SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_NET_3    VarCat = P27 	/SubTítulo= ' ' 'Base: Total Encuestados' .

/*<***************************************************************************>.
* INFORMACIÓN DEMOGRÁFICA .

@UNI_RESP_BASE    VarCat = B_TAMANO_2	    /Título='INFORMACIÓN DEMOGRÁFICA' ' '	/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = B_REGION_2      /Título='INFORMACIÓN DEMOGRÁFICA' ' '	/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = COD_ZONA         /Título='INFORMACIÓN DEMOGRÁFICA' ' '	/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = SEXO                  /Título='INFORMACIÓN DEMOGRÁFICA' ' '	/SubTítulo=' ' 'Base: Total Encuestados' .
@UNI_RESP_BASE    VarCat = ESTRATO            /Título='INFORMACIÓN DEMOGRÁFICA' ' '	/SubTítulo=' ' 'Base: Total Encuestados' .




/*<***************************************************************************>.

OUTPUT SAVE
 OUTFILE = 'C:\Diana\2026\OPINIÓN\009000260000 COLOMBIA OPINA No 20\PROCESAMIENTO\Informe 1_009000260000 COLOMBIA OPINA No 20_Informe principal.spv' .

OUTPUT EXPORT
  /CONTENTS  EXPORT=VISIBLE  LAYERS=PRINTSETTING  MODELVIEWS=PRINTSETTING
  /XLSX  DOCUMENTFILE='C:\Diana\2026\OPINIÓN\009000260000 COLOMBIA OPINA No 20\RESULTADOS\Informe 1_009000260000 COLOMBIA OPINA No 20.xlsx'
     OPERATION=CREATESHEET SHEET='Informe principal'
     LOCATION=LASTCOLUMN  NOTESCAPTIONS=NO.

OUTPUT CLOSE * .

/*<***************************************************************************>.
/*<***************************************************************************>.


/* ACTIVO EL FACTOR DE PONDERACIÓN.

WEIGHT BY FACTOR_PONDERACION.
    


/*<***************************************************************************>.
/* VICTIMIZACION.

@UNI_RESP_BASE    VarCat = VICTIMIZACION          /SubTítulo= ' ' 'Base: Total Encuestados' .

@UNI_RESP_BASE    VarCat = VICTIMIZACION_1      /SubTítulo= ' ' 'Base: Total Encuestados' .

 * /*<***************************************************************************>.
 
OUTPUT SAVE
 OUTFILE = 'C:\Diana\2026\OPINIÓN\009000260000 COLOMBIA OPINA No 20\PROCESAMIENTO\Informe 1_009000260000 COLOMBIA OPINA No 20_Victimización.spv' .

OUTPUT EXPORT
  /CONTENTS  EXPORT=VISIBLE  LAYERS=PRINTSETTING  MODELVIEWS=PRINTSETTING
  /XLSX  DOCUMENTFILE='C:\Diana\2026\OPINIÓN\009000260000 COLOMBIA OPINA No 20\RESULTADOS\Informe 1_009000260000 COLOMBIA OPINA No 20.xlsx'
     OPERATION=CREATESHEET SHEET='Victimización'
     LOCATION=LASTCOLUMN  NOTESCAPTIONS=YES.

OUTPUT CLOSE * .
 


