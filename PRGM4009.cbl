      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM).
      * Date:      07/05/2022.
      * Purpose:   MELHORES DO LOOPING
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGM4009.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       77  WS-CONT                 PIC 999.
       77  WS-CONDICAO             PIC 999.

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
           DISPLAY 'DESEJA CONTAR ATE QUANTO?'
           ACCEPT WS-CONDICAO.
      *SEM DEPENDENCIA
           PERFORM P300-MOSTRA THRU P300-MOSTRA-FIM
                   VARYING WS-CONT FROM 1 BY 1
                   UNTIL WS-CONT GREATER THAN WS-CONDICAO.
           INITIALISE WS-CONT WS-CONDICAO
                      REPLACING NUMERIC BY ZEROS.
      *COM DEPENDENCIA
           DISPLAY 'DESEJA CONTAR ATE QUANTO?'
           ACCEPT WS-CONDICAO.
           PERFORM P400-MOSTRA THRU P400-MOSTRA-FIM
                   WITH TEST BEFORE
                   UNTIL WS-CONT GREATER THAN WS-CONDICAO.
                  INITIALISE WS-CONT WS-CONDICAO
                      REPLACING NUMERIC BY ZEROS.
      *COM DEPENDENCIA
           MOVE 1 TO WS-CONT.
           DISPLAY 'DESEJA CONTAR ATE QUANTO?'
           ACCEPT WS-CONDICAO.
           PERFORM P400-MOSTRA THRU P400-MOSTRA-FIM
                   WITH TEST AFTER
                   UNTIL WS-CONT GREATER THAN WS-CONDICAO.

       P200-PROCESSA-FIM.

       P300-MOSTRA.
           DISPLAY WS-CONT.
       P300-MOSTRA-FIM.

       P400-MOSTRA.

           DISPLAY WS-CONT.
           ADD 1 TO WS-CONT.
       P400-MOSTRA-FIM.

       P900-FINALIZA.
           DISPLAY "FINALIZOU PROGRAMA.".
           GOBACK.
       P900-FINALIZA-FIM.

       END PROGRAM PRGM4009.
