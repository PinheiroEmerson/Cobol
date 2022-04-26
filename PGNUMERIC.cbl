      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGNUMERIC.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-VARIAVEIS.
           05 WS-NUM-INTEIRO               PIC  9(09)    VALUE ZEROES.
           05 WS-NUM-DECIMAL               PIC  9(09)V99 VALUE ZEROES.
           05 WS-NUM-VALOR                 PIC S9(09)V99 VALUE ZEROES.
       01 WS-MASCARAS.
           05 WS-MSK-INTEIRO-1             PIC 999B999B999.
           05 WS-MSK-INTEIRO-2             PIC 999,999,999.
           05 WS-MSK-INTEIRO-3             PIC ********9.
           05 WS-MSK-DECIMAL-1             PIC ***,***,**9.99.
           05 WS-MSK-DECIMAL-2             PIC ZZZ,ZZZ,ZZ9.99.
           05 WS-MSK-VALOR-1               PIC ---,---,--9.99.
           05 WS-MSK-VALOR-2               PIC $$$,$$$,$$9.99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM P100-INICIO     THRU P100-INICIO-FIM.
           PERFORM P200-PROCESSA01 THRU P200-PROCESSA01-FIM.
           PERFORM P200-PROCESSA02 THRU P200-PROCESSA02-FIM.
           PERFORM P200-PROCESSA03 THRU P200-PROCESSA03-FIM.
           PERFORM P900-FINALIZA   THRU P900-FINALIZA-FIM.

       P100-INICIO.
           DISPLAY     'INICIO DO PROCESSAMENTO.'
           INITIALISE  WS-VARIAVEIS WS-MASCARAS
                       REPLACING NUMERIC      BY ZEROES
                                 ALPHANUMERIC BY SPACES.

       P100-INICIO-FIM.

       P200-PROCESSA01.
           DISPLAY 'PROCESSAMENTO ORIGINAL'.
           SET WS-NUM-INTEIRO
               WS-NUM-DECIMAL
               WS-NUM-VALOR    TO 01.

           DIVIDE  WS-NUM-INTEIRO BY 2 GIVING WS-NUM-INTEIRO
                   ON SIZE ERROR
                   PERFORM P800-ERRO THRU P800-ERRO-FIM.

           DIVIDE  WS-NUM-DECIMAL BY 2 GIVING WS-NUM-DECIMAL
                   ON SIZE ERROR
                   PERFORM P800-ERRO THRU P800-ERRO-FIM.

           DIVIDE  WS-NUM-VALOR BY 2 GIVING WS-NUM-VALOR
                   ON SIZE ERROR
                   PERFORM P800-ERRO THRU P800-ERRO-FIM.


           DISPLAY 'WS-NUM-INTEIRO: ' WS-NUM-INTEIRO.
           DISPLAY 'WS-NUM-DECIMAL: ' WS-NUM-DECIMAL.
           DISPLAY 'WS-NUM-VALOR: ' WS-NUM-VALOR.

           PERFORM P100-INICIO         THRU P100-INICIO-FIM.
       P200-PROCESSA01-FIM.


       P200-PROCESSA02.
           DISPLAY 'PROCESSAMENTO SEM MASCARAS'.
           SET WS-NUM-INTEIRO
               WS-NUM-DECIMAL
               WS-NUM-VALOR    TO 01.

           DIVIDE  WS-NUM-INTEIRO BY 2 GIVING WS-NUM-INTEIRO
                   ON SIZE ERROR
                   PERFORM P800-ERRO THRU P800-ERRO-FIM.
           MULTIPLY WS-NUM-INTEIRO BY -1 GIVING WS-NUM-INTEIRO
                   ON SIZE ERROR
                   PERFORM P800-ERRO THRU P800-ERRO-FIM.

           DIVIDE  WS-NUM-DECIMAL BY 2 GIVING WS-NUM-DECIMAL
                   ON SIZE ERROR
                   PERFORM P800-ERRO THRU P800-ERRO-FIM.
           MULTIPLY WS-NUM-DECIMAL BY -1 GIVING WS-NUM-DECIMAL
                   ON SIZE ERROR
                   PERFORM P800-ERRO THRU P800-ERRO-FIM.

           DIVIDE  WS-NUM-VALOR BY 2 GIVING WS-NUM-VALOR
                   ON SIZE ERROR
                   PERFORM P800-ERRO THRU P800-ERRO-FIM.
           MULTIPLY WS-NUM-VALOR BY -1 GIVING WS-NUM-VALOR
                   ON SIZE ERROR
                   PERFORM P800-ERRO THRU P800-ERRO-FIM.

           DISPLAY 'WS-NUM-INTEIRO: ' WS-NUM-INTEIRO.
           DISPLAY 'WS-NUM-DECIMAL: ' WS-NUM-DECIMAL.
           DISPLAY 'WS-NUM-VALOR: ' WS-NUM-VALOR.

           PERFORM P100-INICIO         THRU P100-INICIO-FIM.
       P200-PROCESSA02-FIM.

       P200-PROCESSA03.
           DISPLAY 'PROCESSAMENTO COM MASCARAS'.
           DISPLAY 'DIGITE UM NUMERO INTEIRO: '.
           ACCEPT WS-NUM-INTEIRO.
           DISPLAY 'DIGITE UM NUMERO DECIMAL: '.
           ACCEPT WS-NUM-DECIMAL.
           DISPLAY 'DIGITE UM VALOR: '.
           ACCEPT WS-NUM-VALOR.
           MOVE    WS-NUM-INTEIRO TO  WS-MSK-INTEIRO-1
                                      WS-MSK-INTEIRO-2
                                      WS-MSK-INTEIRO-3.
           MOVE    WS-NUM-DECIMAL TO WS-MSK-DECIMAL-1
                                     WS-MSK-DECIMAL-2.
           MOVE    WS-NUM-VALOR   TO WS-MSK-VALOR-1
                                     WS-MSK-VALOR-2.

           DISPLAY 'WS-MSK-INTEIRO-1: ' WS-MSK-INTEIRO-1.
           DISPLAY 'WS-MSK-INTEIRO-2: ' WS-MSK-INTEIRO-2.
           DISPLAY 'WS-MSK-INTEIRO-3: ' WS-MSK-INTEIRO-2.

           DISPLAY 'WS-MSK-DECIMAL-1: ' WS-MSK-DECIMAL-1.
           DISPLAY 'WS-MSK-DECIMAL-2: ' WS-MSK-DECIMAL-2.

           DISPLAY 'WS-MSK-DECIMAL-1: ' WS-MSK-VALOR-1.
           DISPLAY 'WS-MSK-DECIMAL-2: ' WS-MSK-VALOR-2.

           PERFORM P100-INICIO         THRU P100-INICIO-FIM.
       P200-PROCESSA03-FIM.


       P800-ERRO.
           DISPLAY 'ERRO NO PROCESSAMENTO.'.
           PERFORM P900-FINALIZA       THRU P900-FINALIZA-FIM.
       P800-ERRO-FIM.


       P900-FINALIZA.
           DISPLAY 'FIM DO PROCESSAMENTO.'.
           GOBACK.
       P900-FINALIZA-FIM.

       END PROGRAM PGNUMERIC.
