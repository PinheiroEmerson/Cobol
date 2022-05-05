      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM).
      * Date:      05/05/2022.
      * Purpose:   USO MATRIZ BIDIMENCIONAL INDEXADA.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGM4004.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MATRIZ.
           03 WS-LINHAS                OCCURS 3  TIMES
                                       INDEXED BY I.
               05 WS-LINHA             PIC X(06) VALUE 'LINHA'.
               05 WS-COLUNAS           OCCURS 5  TIMES
                                       INDEXED BY J.
                   07 WS-CELULA        PIC X(06) VALUE 'CELULA'.

       01  WS-MSK.
           03 WS-I                     PIC 9.
           03 WS-J                     PIC 9.


       PROCEDURE DIVISION.

       PROCEDURE-MAIN.

           PERFORM P100-INICIO   THRU P100-INICIO-FIM.

           PERFORM P200-PROCESSA-L THRU P200-PROCESSA-L-FIM
                   VARYING I FROM 1 BY 1
                   UNTIL   I IS GREATER THAN 3.

           PERFORM P900-FINALIZA THRU P900-FINALIZA-FIM.

       PROCEDURE-MAIN-FIM.

       P100-INICIO.
           INITIALISE  J I WS-MSK
               REPLACING NUMERIC       BY ZEROES
                         ALPHANUMERIC  BY SPACES.

           DISPLAY "INICIO PROCESSAMENTO.".

       P100-INICIO-FIM.

       P200-PROCESSA-L.
           PERFORM P250-PROCESSA-C THRU P250-PROCESSA-C-FIM
                   VARYING J FROM 1 BY 1
                   UNTIL   J IS GREATER THAN 5.
       P200-PROCESSA-L-FIM.

       P250-PROCESSA-C.
           MOVE I TO WS-I.
           MOVE J TO WS-J.
           DISPLAY WS-CELULA(WS-I, WS-J) ' ' WS-I ':' WS-J.
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

       END PROGRAM PRGM4004.
