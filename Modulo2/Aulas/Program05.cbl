      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM).
      * Date:      07/04/2022.
      * Purpose:   USO DO COMANDO COMPUTE.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM05.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-RESULTADO               PIC 9(05) VALUE ZERO.
       77 WS-NUM-1                   PIC 9(02) VALUE ZERO.
       77 WS-NUM-2                   PIC 9(02) VALUE ZERO.

       PROCEDURE DIVISION.

       P100-INICIO.

           COMPUTE WS-RESULTADO = 5 + 5.
           COMPUTE WS-RESULTADO = WS-RESULTADO - 5.
           COMPUTE WS-RESULTADO = WS-RESULTADO ** 3.
           COMPUTE WS-RESULTADO = (5 * 5 - 5)

           DISPLAY WS-RESULTADO.

           DISPLAY "PRIMEIRO NUMERO "
           ACCEPT WS-NUM-1.

           DISPLAY "SEGUNDO NUMERO "
           ACCEPT WS-NUM-2.

           COMPUTE WS-RESULTADO = WS-NUM-1 * WS-NUM-2.
           DISPLAY "RESULTADO É " WS-RESULTADO.


           GOBACK.

       P100-INICIO-FIM.
       END PROGRAM PROGRAM05.
