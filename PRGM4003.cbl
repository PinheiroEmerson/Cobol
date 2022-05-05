      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM).
      * Date:      05/05/2022.
      * Purpose:   USO MATRIZ BIDIMENCIONAL ESTATICA.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGM4003.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MATRIZ.
           03 WS-LINHAS                OCCURS 3  TIMES.
               05 WS-LINHA             PIC X(06) VALUE 'LINHA'.
               05 WS-COLUNAS           OCCURS 5  TIMES.
                   07 WS-CELULA        PIC X(06) VALUE 'CELULA'.

       01  WS-LC.
           03  WS-L                    PIC 99.
           03  WS-C                    PIC 99.

       PROCEDURE DIVISION.

       PROCEDURE-MAIN.

           PERFORM P100-INICIO   THRU P100-INICIO-FIM.

           PERFORM P200-PROCESSA-L THRU P200-PROCESSA-L-FIM
                   VARYING WS-L FROM 1 BY 1
                   UNTIL   WS-L IS GREATER THAN 3.

           PERFORM P900-FINALIZA THRU P900-FINALIZA-FIM.

       PROCEDURE-MAIN-FIM.

       P100-INICIO.
           INITIALISE  WS-LC
               REPLACING NUMERIC       BY ZEROES
                         ALPHANUMERIC  BY SPACES.

           DISPLAY "INICIO PROCESSAMENTO.".

       P100-INICIO-FIM.

       P200-PROCESSA-L.
           PERFORM P250-PROCESSA-C THRU P250-PROCESSA-C-FIM
                   VARYING WS-C FROM 1 BY 1
                   UNTIL   WS-C IS GREATER THAN 5.
       P200-PROCESSA-L-FIM.

       P250-PROCESSA-C.
           DISPLAY WS-CELULA(WS-L, WS-C) ' ' WS-L ':' WS-C.
       P250-PROCESSA-C-FIM.

       P300-FINALIZA.
           DISPLAY "FINAL PROCESSAMENTO.".
       P300-FINALIZA-FIM.

       P350-ERRO.
           DISPLAY 'MEIO DO PROCESSAMENTO...'.
      *     DISPLAY "ERRO NO CALCULO.........:" WS-VALOR.
           PERFORM P900-FINALIZA THRU P900-FINALIZA-FIM.
       P350-ERRO-FIM.


       P900-FINALIZA.
           DISPLAY "FINALIZOU PROGRAMA.".
           GOBACK.
       P900-FINALIZA-FIM.

       END PROGRAM PRGM4003.
