      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM).
      * Date:      07/04/2022.
      * Purpose:   USO DO PERFORM E SECTION.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM04.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.


       PERFORM S2.
       PERFORM S1.
       PERFORM S3.
       S2 SECTION.
       P100-INICIO.

           DISPLAY "INICIO PROCESSAMENTO.".

       P100-INICIO-FIM.


       P200-PROCESSA.

           DISPLAY "MEIO PROCESSAMENTO.".

       P200-PROCESSA-FIM.

       S1 SECTION.
       P300-FINALIZA.

           DISPLAY "FINAL PROCESSAMENTO.".

       P300-FINALIZA-FIM.

       S3 SECTION.
       P400-PARA.
           DISPLAY "FINALIZOU PROGRAMA.".
           GOBACK.
       P400-PARA-FIM.

       END PROGRAM PROGRAM04.
