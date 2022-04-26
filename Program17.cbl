      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM)
      * Date:      11/04/2022 - 8:00 A.M.
      * Purpose:   TESTAR CALL DINAMICO E ESTATICO.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM17.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-PGM-NAME              PIC X(80).
       01 WS-AREA-COM.
           03 WS-RESULTADO         PIC 9(04).
           03 WS-NUM-1             PIC 9(04).
           03 WS-NUM-2             PIC 9(04).

       PROCEDURE DIVISION.

       P001-MAIN.
           PERFORM P100-INICIAL  THRU P100-INICIAL-FIM.
           PERFORM P900-TERMINAL THRU P900-TERMINAL-FIM.
       P001-MAIN-FIM.
      *-----------------------------------------------------------------
       P100-INICIAL.
      *-----------------------------------------------------------------
           INITIALISE WS-AREA-COM REPLACING NUMERIC BY ZEROES.
           MOVE "Program18"       TO WS-PGM-NAME.
           DISPLAY 'INICIO PROCESSAMENTO...'.
           DISPLAY 'PROGRAMA CHAMADOR...'.

           DISPLAY 'ENTRE COM O PRIMEIRO NUMERO: '.
           ACCEPT WS-NUM-1.

           DISPLAY 'ENTRE COM O SEGUNDO NUMERO: '.
           ACCEPT WS-NUM-2.

           DISPLAY 'CHAMANDO PROGRAMA PROGRAM18.'.

      *     CALL 'D:\My Documents\Cobol\Programs\Program18\bin\Program18'
      *           USING WS-AREA-COM.
           CALL WS-PGM-NAME USING WS-AREA-COM.

           PERFORM P900-TERMINAL THRU P900-TERMINAL-FIM.

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
       P900-TERMINAL.
      *-----------------------------------------------------------------
           DISPLAY 'FIM DO PROCESSAMENTO.'.
           DISPLAY 'RESULTADO DO PROGRAMA CHAMADO: WS-RESULTADO '
                    WS-RESULTADO.
           DISPLAY 'PROGRAMA CHAMADO COM SUCESSO.'.
           GOBACK.
      *-----------------------------------------------------------------
       P900-TERMINAL-FIM.
      *-----------------------------------------------------------------

       END PROGRAM PROGRAM17.
