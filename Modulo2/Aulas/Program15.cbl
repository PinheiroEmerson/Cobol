      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM)
      * Date:      11/04/2022 - 8:00 A.M.
      * Purpose:   TESTAR PERFORM E SUAS VARIACOES
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM15.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-VARIAVEIS.
           03 WS-NUMERO            PIC 9(04).
           03 WS-TOTAL             PIC 9(04).
           03 WS-INDICE            PIC 9(04).

       PROCEDURE DIVISION.

       P001-MAIN.
           PERFORM P100-INICIAL  THRU P100-INICIAL-FIM.
           PERFORM P900-TERMINAL THRU P900-TERMINAL-FIM.
       P001-MAIN-FIM.
      *-----------------------------------------------------------------
       P100-INICIAL.
      *-----------------------------------------------------------------
           DISPLAY 'INICIO PROCESSAMENTO...'.
           INITIALISE WS-VARIAVEIS REPLACING NUMERIC BY ZEROES.
           PERFORM P400-PROCESSA1     THRU P400-PROCESSA1-FIM WITH TEST
                                      BEFORE UNTIL WS-NUMERO = 3.
      *-----------------------------------------------------------------
       P100-INICIAL-FIM.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       P300-ERROR.
      *-----------------------------------------------------------------
           DISPLAY 'ERRO DE PROCESSAMENTO'.
           PERFORM P900-TERMINAL   THRU P900-TERMINAL-FIM.
      *-----------------------------------------------------------------
       P300-ERROR-FIM.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       P400-PROCESSA1.
      *-----------------------------------------------------------------
           ADD 1                   TO WS-NUMERO.
           DISPLAY '---WS-NUMERO ->' WS-NUMERO.

           PERFORM P500-PROCESSA2  THRU P500-PROCESSA2-FIM WITH TEST
                                   BEFORE UNTIL WS-TOTAL = 3.
      *     SET WS-TOTAL TO ZERO.
      *-----------------------------------------------------------------
       P400-PROCESSA1-FIM.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       P500-PROCESSA2.
      *-----------------------------------------------------------------
           ADD 1                   TO WS-TOTAL.
           DISPLAY '-----WS-TOTAL ->' WS-TOTAL.
      *-----------------------------------------------------------------
       P500-PROCESSA2-FIM.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       P900-TERMINAL.
      *-----------------------------------------------------------------
           DISPLAY 'FIM DO PROCESSAMENTO.'.
           GOBACK.
      *-----------------------------------------------------------------
       P900-TERMINAL-FIM.
      *-----------------------------------------------------------------

       END PROGRAM PROGRAM15.
