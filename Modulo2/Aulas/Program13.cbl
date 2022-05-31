      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM)
      * Date:      11/04/2022 - 8:00 A.M.
      * Purpose:   TESTAR OPERADORES LÓGICOS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM13.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-NUM-1                        PIC S9(04).
       77  WS-NUM-2                        PIC S9(04).


       PROCEDURE DIVISION.

       P001-MAIN.
           PERFORM P100-INICIAL  THRU P100-INICIAL-FIM.
           PERFORM P200-PROCESSA THRU P200-PROCESSA-FIM.
           PERFORM P900-TERMINAL THRU P900-TERMINAL-FIM.

      *-----------------------------------------------------------------
       P100-INICIAL.
      *-----------------------------------------------------------------
           DISPLAY 'INICIO PROCESSAMENTO...'.
           INITIALISE WS-NUM-1 WS-NUM-2.
           DISPLAY 'DIGITE O PRIMEIRO NUMERO:'.
           ACCEPT WS-NUM-1.
           DISPLAY 'DIGITE O PRIMEIRO NUMERO:'.
           ACCEPT WS-NUM-2.

      *-----------------------------------------------------------------
       P100-INICIAL-FIM.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       P200-PROCESSA.
      *-----------------------------------------------------------------
           IF WS-NUM-1 EQUAL TO WS-NUM-2
              DISPLAY 'O NUMERO WS-NUM-1 ' WS-NUM-1
                      ' EH IGUAL A WS-NUM-2 ' WS-NUM-2
           END-IF.

           IF WS-NUM-1 GREATER THAN WS-NUM-2
              DISPLAY 'O NUMERO WS-NUM-1 ' WS-NUM-1
                      ' EH MAIOR QUE WS-NUM-2 ' WS-NUM-2
           END-IF.

           IF WS-NUM-1 LESS THAN WS-NUM-2
              DISPLAY 'O NUMERO WS-NUM-1 ' WS-NUM-1
                      ' EH MENOR QUE WS-NUM-2 ' WS-NUM-2
           END-IF.

           IF WS-NUM-1 NOT EQUAL TO WS-NUM-2
              DISPLAY 'O NUMERO WS-NUM-1 ' WS-NUM-1
                      ' NAO EH IGUAL A WS-NUM-2 ' WS-NUM-2
           END-IF.

           IF WS-NUM-1 IS NEGATIVE THEN
              DISPLAY 'O NUMERO WS-NUM-1 ' WS-NUM-1
                      ' EH NEGATIVO.'
           END-IF.

           IF WS-NUM-2 IS NEGATIVE THEN
              DISPLAY 'O NUMERO WS-NUM-2 ' WS-NUM-2
                      ' EH NEGATIVO.'
           END-IF.

           IF WS-NUM-1 IS POSITIVE THEN
              DISPLAY 'O NUMERO WS-NUM-1 ' WS-NUM-1
                      ' EH POSITIVO.'
           END-IF.

           IF WS-NUM-2 IS POSITIVE THEN
              DISPLAY 'O NUMERO WS-NUM-2 ' WS-NUM-2
                      ' EH POSITIVO.'
           END-IF.

           IF WS-NUM-1 IS ALPHABETIC THEN
              DISPLAY 'O NUMERO WS-NUM-1 ' WS-NUM-1
                      ' EH ALFABETICO.'
           END-IF.

           IF WS-NUM-2 IS ALPHABETIC THEN

              DISPLAY 'O NUMERO WS-NUM-2 ' WS-NUM-2
                      ' EH ALFABETICO.'
           END-IF.

           IF WS-NUM-1 IS NUMERIC THEN
              DISPLAY 'O NUMERO WS-NUM-1 ' WS-NUM-1
                      ' EH NUMERICO.'
           END-IF.

           IF WS-NUM-2 IS NUMERIC THEN
              DISPLAY 'O NUMERO WS-NUM-2 ' WS-NUM-2
                      ' EH NUMERICO.'
           END-IF.

      *-----------------------------------------------------------------
       P200-PROCESSA-FIM.
      *-----------------------------------------------------------------





      *-----------------------------------------------------------------
       P300-ERROR.
      *-----------------------------------------------------------------
           DISPLAY 'ERRO DE PROCESSAMENTO'.
           PERFORM P900-TERMINAL THRU P900-TERMINAL-FIM.
      *-----------------------------------------------------------------
       P300-ERROR-FIM.
      *-----------------------------------------------------------------


      *-----------------------------------------------------------------
       P900-TERMINAL.
      *-----------------------------------------------------------------
           DISPLAY 'FIM DO PROCESSAMENTO.'.
           GOBACK.
      *-----------------------------------------------------------------
       P900-TERMINAL-FIM.
      *-----------------------------------------------------------------

       END PROGRAM PROGRAM13.
