      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM)
      * Date:      11/04/2022 - 8:00 A.M.
      * Purpose:   TESTAR INSTRUCAO MOVE-CORR
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM11.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-LAYOUT-1.
           03 WS-EMAIL                     PIC X(20).
           03 WS-NOME                      PIC X(20).
           03 WS-ENDERECO                  PIC X(20).
           03 WS-TELEFONE                  PIC X(20).
           03 WS-CIDADE                    PIC X(30).
           03 WS-ESTADO                    PIC X(20).

       01  WS-LAYOUT-2.

           03 WS-ESTADO                    PIC X(20).
           03 WS-ENDERECO                  PIC X(20).
           03 WS-TEL                       PIC X(20).
           03 WS-EMAIL                     PIC X(20).
           03 WS-CIDADE                    PIC X(30).
           03 WS-NOME                      PIC X(20).

       PROCEDURE DIVISION.

       P001-MAIN.
           PERFORM P100-INICIAL  THRU P100-INICIAL-FIM.
           PERFORM P900-TERMINAL THRU P900-TERMINAL-FIM.

      *-----------------------------------------------------------------
       P100-INICIAL.
      *-----------------------------------------------------------------
           DISPLAY 'INICIO PROCESSAMENTO...'

           MOVE 'EMERSON PINHEIRO'         TO WS-NOME     OF WS-LAYOUT-1.
           MOVE 'RUA 10'                   TO WS-ENDERECO OF WS-LAYOUT-1.
           MOVE '555555555'                TO WS-TELEFONE OF WS-LAYOUT-1.
           MOVE 'TESTSE@TETE.COM'          TO WS-EMAIL    OF WS-LAYOUT-1.
           MOVE 'SAO JOSE'                 TO WS-CIDADE   OF WS-LAYOUT-1.
           MOVE 'RIO GRANDE DO'            TO WS-ESTADO   OF WS-LAYOUT-1.


           MOVE CORRESPONDING WS-LAYOUT-1 TO  WS-LAYOUT-2.
           DISPLAY WS-LAYOUT-1.
           DISPLAY WS-LAYOUT-2.
      *-----------------------------------------------------------------
       P100-INICIAL-FIM.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       P900-TERMINAL.
      *-----------------------------------------------------------------
           DISPLAY 'FIM DO PROCESSAMENTO.'.
           GOBACK.
      *-----------------------------------------------------------------
       P900-TERMINAL-FIM.
      *-----------------------------------------------------------------

       END PROGRAM PROGRAM11.
