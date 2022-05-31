      ******************************************************************
      * Author: Emerson Pinheiro
      * Date: 29/03/2022 - 20:49
      * Purpose: Programa Demon
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Demon.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-N1      PIC 9(02) VALUE ZEROES.
       77 WS-N2      PIC 9(02) VALUE ZEROES.
       77 WS-N3      PIC Z(03) VALUE ZEROES.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            MOVE 5 TO WS-N1
            MOVE 3 TO WS-N2
            COMPUTE WS-N3 = WS-N1 + WS-N2
            DISPLAY "O RESULTADO DA SOMA EH: " WS-N3
            STOP RUN.
       END PROGRAM Demon.
