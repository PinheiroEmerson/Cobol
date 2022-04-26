      ******************************************************************
      * Author:    EMERSON PINHEIRO - EMAIL: TIOEL@OUTLOOK.COM.
      * Date:      19/04/2022 20:32
      * Purpose:   TESTAR CALL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Program20.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01 PARAMETRES.
           03 WS-RESULTADO      PIC 9(04) VALUE ZERO.
           03 WS-NUM-1          PIC 9(04) VALUE ZERO.
           03 WS-NUM-2          PIC 9(04) VALUE ZERO.
       PROCEDURE DIVISION USING PARAMETRES.
       MAIN-PROCEDURE.
           DISPLAY 'PROGRAMA CHAMADO ENTROU PROGRAM20'.
           DISPLAY 'RECEBEU WS-NUM-1: ' WS-NUM-1.
           DISPLAY 'RECEBEU WS-NUM-2: ' WS-NUM-2.
           ADD WS-NUM-1, WS-NUM-2 TO WS-RESULTADO.
           DISPLAY 'PROGRAMA CHAMADO SAIU PROGRAM20.'.
           GOBACK.
       MAIN-PROCEDURE-FIM.
       END PROGRAM Program20.
