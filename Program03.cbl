      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM).
      * Date:      07/04/2022.
      * Purpose:   USO DO PERFORM.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM03.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.


           PERFORM P100-INICIO   THRU P100-INICIO-FIM.
           PERFORM P200-PROCESSA THRU P200-PROCESSA-FIM.
           PERFORM P300-FINALIZA THRU P300-FINALIZA-FIM.
           PERFORM P400-PARA THRU P400-PARA-FIM.


       P100-INICIO.

           DISPLAY "INICIO PROCESSAMENTO.".

       P100-INICIO-FIM.


       P200-PROCESSA.

           DISPLAY "MEIO PROCESSAMENTO.".

       P200-PROCESSA-FIM.

       P300-FINALIZA.

           DISPLAY "FINAL PROCESSAMENTO.".

       P300-FINALIZA-FIM.

       P400-PARA.
           DISPLAY "FINALIZOU PROGRAMA.".
           GOBACK.
       P400-PARA-FIM.

       END PROGRAM PROGRAM03.
