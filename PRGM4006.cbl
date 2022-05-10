      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM).
      * Date:      05/05/2022.
      * Purpose:   MELHORES USOS DO IF THE ELSE.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGM4006.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       77  WS-CODIGO                      PIC 9.
       77  WS-ALFA                        PIC X.
       01  WS-CALCULA.
           05 WS-NR01                     PIC S9(02).
           05 WS-NR02                     PIC S9(02).

       01  WS-STATUS                      PIC 99.
           88 WS-VERDADEIRO               VALUE 1, 2.
           88 WS-FALSO                    VALUE 0, 3 THRU 5.
           88 WS-OUTRO                    VALUE 6    THRU 9, 15.

       PROCEDURE DIVISION.

       PROCEDURE-MAIN.

           PERFORM P100-INICIO   THRU P100-INICIO-FIM.

           PERFORM P200-PROCESSA THRU P200-PROCESSA-FIM.

           PERFORM P900-FINALIZA THRU P900-FINALIZA-FIM.

       PROCEDURE-MAIN-FIM.

       P100-INICIO.

           DISPLAY "INICIO PROCESSAMENTO.".
           DISPLAY 'DIGITE O WS-CODIGO:'
           ACCEPT WS-CODIGO.
           DISPLAY 'DIGITE O WS-ALFA:'
           ACCEPT WS-ALFA.
           DISPLAY 'DIGITE O WS-STATUS:'
           ACCEPT WS-STATUS.
           DISPLAY 'DIGITE UM NUMERO:'
           ACCEPT WS-NR01.
           DISPLAY 'DIGITE OUTRO NUMERO:'
           ACCEPT WS-NR02.
       P100-INICIO-FIM.

       P200-PROCESSA.
           IF WS-CODIGO EQUALS TO ZERO THEN
               DISPLAY 'VOCE DIGITOU ZERO'
           ELSE
               DISPLAY 'VOCE DIGITOU MAIOR QUE ZERO'
           END-IF.
           IF WS-ALFA EQUALS TO 'A' THEN
               DISPLAY 'VOCE DIGITOU A'
           ELSE
               DISPLAY 'VOCE DIGITOU OUTRA LETRA'
           END-IF.
           IF WS-CODIGO IS NUMERIC THEN
               DISPLAY 'WS-CODIGO E NUMERICO'
           END-IF.
           IF WS-ALFA IS ALPHABETIC THEN
               DISPLAY 'WS-ALFA E ALFABETICO'
           END-IF.

           IF WS-VERDADEIRO THEN
               DISPLAY 'VERDADE'
           END-IF.

           IF WS-FALSO THEN
               DISPLAY 'FALSO'
           END-IF.

           IF WS-OUTRO THEN
               DISPLAY 'OUTRO'
           END-IF.

           IF WS-NR01 IS GREATER THAN WS-NR02
               DISPLAY 'NR01 E MAIOR QUE NR02'
           END-IF.
           IF WS-NR01 EQUALS TO WS-NR02
               DISPLAY 'NR01 E IGUAL A NR02'
           END-IF.
           IF WS-NR01 IS LESS THAN WS-NR02
               DISPLAY 'NR01 E MENOR QUE NR02'
           END-IF.
           IF WS-NR01 IS NOT POSITIVE THEN
               DISPLAY 'NAO POSITIVO'
           END-IF.
           IF WS-NR01 IS POSITIVE THEN
               DISPLAY 'POSITIVO'
           END-IF.
           IF WS-NR01 IS NOT NEGATIVE THEN
               DISPLAY 'NAO NEGATIVO'
           END-IF.
           IF WS-NR01 IS NEGATIVE THEN
               DISPLAY 'NEGATIVO'
           END-IF.
           IF WS-NR01 IS GREATER THAN (WS-NR01 * WS-NR02)
               DISPLAY 'VALOR WS-NR01  MAIOR'
           ELSE
               DISPLAY 'VALOR WS-NR01  WS-NR02'
           END-IF.

       P200-PROCESSA-FIM.

       P350-ERRO.
           DISPLAY 'ERRO DO PROCESSAMENTO...'.

           PERFORM P900-FINALIZA THRU P900-FINALIZA-FIM.
       P350-ERRO-FIM.


       P900-FINALIZA.
           DISPLAY "FINALIZOU PROGRAMA.".
           GOBACK.
       P900-FINALIZA-FIM.

       END PROGRAM PRGM4006.
