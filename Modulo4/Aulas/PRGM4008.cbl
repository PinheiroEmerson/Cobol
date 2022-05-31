      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM).
      * Date:      07/05/2022.
      * Purpose:   MELHORES DO ADD CORRESPONDENTE
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGM4008.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-REG-1.
           03 WS-COD                   PIC 9(02).
           03 WS-NOME                  PIC X(15).
           03 WS-TEL                   PIC X(09).

       01  WS-REG-2.
           03 WS-COD                   PIC 9(02).
           03 WS-NOME                  PIC X(15).
           03 WS-TEL                   PIC X(09).

       01  WS-NUM1.
           03 WS-NUM                   PIC 9(05).

       01  WS-NUM2.
           03 WS-NUM                   PIC 9(05).


       PROCEDURE DIVISION.

       PROCEDURE-MAIN.

           PERFORM P100-INICIO   THRU P100-INICIO-FIM.

           PERFORM P200-PROCESSA THRU P200-PROCESSA-FIM.

           PERFORM P900-FINALIZA THRU P900-FINALIZA-FIM.

       PROCEDURE-MAIN-FIM.

       P100-INICIO.

           DISPLAY 'INICIO PROCESSAMENTO'.


       P100-INICIO-FIM.

       P200-PROCESSA.
           MOVE '20CARLOS GOMES   985109610' TO WS-REG-1.

           MOVE WS-COD OF WS-REG-1 TO WS-COD OF WS-REG-2.
           MOVE WS-NOME OF WS-REG-1 TO WS-NOME OF WS-REG-2.
           MOVE WS-TEL OF WS-REG-1 TO WS-TEL OF WS-REG-2.

           DISPLAY WS-REG-1.
           DISPLAY WS-REG-2.

           MOVE '10CARLOS LOPES   985105555' TO WS-REG-1.

           ADD CORR WS-REG-1 TO WS-REG-2.

           DISPLAY WS-REG-1.
           DISPLAY WS-REG-2.

           ADD CORR WS-REG-1 TO WS-REG-2.

           DISPLAY WS-REG-1.
           DISPLAY WS-REG-2.

           MOVE CORR WS-REG-1 TO WS-REG-2.

           DISPLAY WS-REG-1.
           DISPLAY WS-REG-2.

       P200-PROCESSA-FIM.


       P900-FINALIZA.
           DISPLAY "FINALIZOU PROGRAMA.".
           GOBACK.
       P900-FINALIZA-FIM.

       END PROGRAM PRGM4008.
