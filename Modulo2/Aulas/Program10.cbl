      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM)
      * Date:      1/04/2022 - 8:00 A.M.
      * Purpose:   TESTAR INSTRUCAO LENGTH
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM10.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-TAMANHO          PIC 9(02) VALUE ZERO.
       01  WS-ENDERECO.
           03 WS-RUA           PIC X(20).
           03 WS-BAIRRO        PIC X(20).
           03 WS-CIDADE        PIC X(30).

       PROCEDURE DIVISION.

       P001-MAIN.
           PERFORM P100-INICIAL  THRU P100-INICIAL-FIM.
           PERFORM P900-TERMINAL THRU P900-TERMINAL-FIM.

      *-----------------------------------------------------------------
       P100-INICIAL.
      *-----------------------------------------------------------------
           DISPLAY 'O COMPRIMENTO DO GRUPO ENDERECO EH: '
                   LENGTH OF WS-ENDERECO.

           COMPUTE WS-TAMANHO = FUNCTION LENGTH (WS-ENDERECO).

           DISPLAY 'OUTRA FORMA: ' WS-TAMANHO.
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

       END PROGRAM PROGRAM10.
