      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM)
      * Date:      11/04/2022 - 8:00 A.M.
      * Purpose:   TESTAR ADD SUBTRACT DIVIDE MULTIPLY COMPUTE
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM12.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-NUM-1                        PIC 9(02)V99 VALUE ZERO.
       77  WS-NUM-2                        PIC 9(02)V99 VALUE ZERO.

       01  WS-IDENTIFICADOR1.
           03 WS-CAMPO-1                  PIC 999.
           03 WS-CAMPO-2                  PIC 9.
           03 WS-CAMPO-3                  PIC 99.

       01  WS-IDENTIFICADOR2.
           03 WS-CAMPO-2                   PIC 9.
           03 WS-CAMPO-1                   PIC 999.
           03 WS-CAMPO-3                   PIC 99.

       PROCEDURE DIVISION.

       P001-MAIN.
           PERFORM P100-INICIAL  THRU P100-INICIAL-FIM.
           PERFORM P900-TERMINAL THRU P900-TERMINAL-FIM.

      *-----------------------------------------------------------------
       P100-INICIAL.
      *-----------------------------------------------------------------
           DISPLAY 'INICIO PROCESSAMENTO...'.
      *     SET WS-CAMPO-1 OF WS-IDENTIFICADOR1 TO 3.
      *     SET WS-CAMPO-2 OF WS-IDENTIFICADOR1 TO 6.
      *     SET WS-CAMPO-3 OF WS-IDENTIFICADOR1 TO 9.

      *     SET WS-CAMPO-1 OF WS-IDENTIFICADOR2 TO 5.
      *     SET WS-CAMPO-2 OF WS-IDENTIFICADOR2 TO 2.
      *     SET WS-CAMPO-3 OF WS-IDENTIFICADOR2 TO 8.

      *     ADD CORR  WS-IDENTIFICADOR1 TO WS-IDENTIFICADOR2.

      *     DISPLAY WS-IDENTIFICADOR1.
      *     DISPLAY WS-IDENTIFICADOR2.

           INITIALISE WS-NUM-1 WS-NUM-2.
           DISPLAY 'DIGITE O PRIMEIRO NUMERO:'.
           ACCEPT WS-NUM-1.
           DISPLAY 'DIGITE O SEGUNDO NUMERO:'.
           ACCEPT WS-NUM-2.

           PERFORM P200-CALCULA THRU P200-CALCULA-FIM.

      *-----------------------------------------------------------------
       P100-INICIAL-FIM.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       P200-CALCULA.
      *-----------------------------------------------------------------
           DISPLAY 'FUNCAO ADD.'.
           ADD WS-NUM-1        TO WS-NUM-2
                               ON SIZE ERROR
                               PERFORM P300-ERROR THRU P300-ERROR-FIM
           END-ADD.

           DISPLAY 'VALOR DE WS-NUM-1 EH ' WS-NUM-1 'E O VALOR DE'       /
           ' WS-NUM-2 EH ' WS-NUM-2.

           DISPLAY 'FUNCAO SUBTRACT.'.
           SUBTRACT WS-NUM-1   FROM WS-NUM-2
                               ON SIZE ERROR
                               PERFORM P300-ERROR THRU P300-ERROR-FIM
           END-SUBTRACT.

           DISPLAY 'VALOR DE WS-NUM-1 EH ' WS-NUM-1 'E O VALOR DE'       /
           ' WS-NUM-2 EH ' WS-NUM-2.

           DISPLAY 'FUNCAO MULTIPLY.'.
           MULTIPLY WS-NUM-1   BY WS-NUM-2
                               ON SIZE ERROR
                               PERFORM P300-ERROR THRU P300-ERROR-FIM
           END-MULTIPLY.

           DISPLAY 'VALOR DE WS-NUM-1 EH ' WS-NUM-1 'E O VALOR DE'       /
           ' WS-NUM-2 EH ' WS-NUM-2.

           DISPLAY 'FUNCAO DIVIDE.'.
           DIVIDE WS-NUM-2     BY   WS-NUM-1  GIVING WS-NUM-2
                               SIZE ERROR
                               PERFORM P300-ERROR THRU P300-ERROR-FIM
           END-DIVIDE.

           DISPLAY 'VALOR DE WS-NUM-1 EH ' WS-NUM-1 'E O VALOR DE'       /
           ' WS-NUM-2 EH ' WS-NUM-2.
      *-----------------------------------------------------------------
       P200-CALCULA-FIM.
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

       END PROGRAM PROGRAM12.
