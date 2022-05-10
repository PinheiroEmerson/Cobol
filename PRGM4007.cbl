      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM).
      * Date:      05/05/2022.
      * Purpose:   MELHORES USOS DO EVALUATE
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGM4007.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-DIAS-MES              PIC 9(02).
           88 WS-FEV-NB            VALUE 1 THRU 28.
           88 WS-FEV-B             VALUE 1 THRU 29.
           88 WS-MES-30            VALUE 1 THRU 30.
           88 WS-MES-31            VALUE 1 THRU 31.

       PROCEDURE DIVISION.

       PROCEDURE-MAIN.

           PERFORM P100-INICIO   THRU P100-INICIO-FIM.

           PERFORM P200-PROCESSA THRU P200-PROCESSA-FIM.

           PERFORM P900-FINALIZA THRU P900-FINALIZA-FIM.

       PROCEDURE-MAIN-FIM.

       P100-INICIO.

           DISPLAY 'INICIO PROCESSAMENTO'.
           DISPLAY 'INFORME UM DIA DO MES: '
           ACCEPT WS-DIAS-MES.

       P100-INICIO-FIM.

       P200-PROCESSA.
           EVALUATE WS-DIAS-MES

              WHEN 1 THRU 28

                    DISPLAY 'ESTE DIA PERTENCE A QQ MES DO ANO'


              WHEN 1 THRU 29

                    DISPLAY 'ESTE DIA PERTENCE A QQ MES DO ANO'
                    DISPLAY 'ESTE DIA EXISTIRA EM FEV SE ANO BISSEXTO'


              WHEN 1 THRU 30

                    DISPLAY 'ESTE DIA PERTENCE A QQ MES DO ANO'
                    DISPLAY 'ESTE DIA NAO EXISTIRA EM ANO BISSEXTO'


              WHEN 1 THRU 31

                    DISPLAY 'ESTE DIA SO EXISTE EM'
                    DISPLAY 'JAN MAR MAIO JULHO AGO OUT DEZ'


           END-EVALUATE.

       P200-PROCESSA-FIM.


       P900-FINALIZA.
           DISPLAY "FINALIZOU PROGRAMA.".
           GOBACK.
       P900-FINALIZA-FIM.

       END PROGRAM PRGM4007.
