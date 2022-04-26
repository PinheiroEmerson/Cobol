      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM)
      * Date:      1/04/2022 - 8:00 A.M.
      * Purpose:   TESTAR COMANDO UNSTRING
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM09.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-NUM-1                      PIC 9(02) VALUE ZEROES.
       77 WS-NUM-2                      PIC 9(02) VALUE ZEROES.
       77 WS-RESULT                     PIC 9(02) VALUE ZEROES.

       PROCEDURE DIVISION.

       P001-MAIN.
           PERFORM P100-INICIAL  THRU P100-INICIAL-FIM.
           PERFORM P900-TERMINAL THRU P900-TERMINAL-FIM.

      *-----------------------------------------------------------------
       P100-INICIAL.
      *-----------------------------------------------------------------
           DISPLAY 'INICIO DO PROCESSAMENTO...'.

           DISPLAY "DIGITE O PRIMEIRO NUMERO: ".
           ACCEPT WS-NUM-1.

           DISPLAY "DIGITE O SEGUNDO NUMERO: ".
           ACCEPT WS-NUM-2.

           COMPUTE WS-RESULT = WS-NUM-1 * WS-NUM-2
                   ON SIZE ERROR PERFORM P200-ERRO THRU P200-ERRO-FIM
           END-COMPUTE.

           DISPLAY "CALCULO OK. RESULTADO..............:" WS-RESULT.

      *-----------------------------------------------------------------
       P100-INICIAL-FIM.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       P200-ERRO.
      *-----------------------------------------------------------------
           DISPLAY 'MEIO DO PROCESSAMENTO...'.
           DISPLAY "ERRO NO CALCULO.........:" WS-RESULT.
           DISPLAY "PROGRAMA FINALIZADO."
           PERFORM P900-TERMINAL THRU P900-TERMINAL-FIM.

      *-----------------------------------------------------------------
       P200-ERRO-FIM.
      *-----------------------------------------------------------------



      *-----------------------------------------------------------------
       P900-TERMINAL.
      *-----------------------------------------------------------------
           DISPLAY 'FIM DO PROCESSAMENTO.'.
           GOBACK.
      *-----------------------------------------------------------------
       P900-TERMINAL-FIM.
      *-----------------------------------------------------------------

       END PROGRAM PROGRAM09.
