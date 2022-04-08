      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM)
      * Date:      06/04/2022
      * Purpose:   TESTE DE COMANDOS DISPLAY ACCEPT MOVE SET
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM02.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 WS-DATA                  PIC X(10).
       01 FILLER                   REDEFINES WS-DATA.
           03 WS-DATA-DD           PIC 9(02).
           03 WS-DATA-MM           PIC 9(02).
           03 WS-DATA-AA           PIC 9(04).

       01 WS-DATA-AUX              PIC X(10).
       01 FILLER                   REDEFINES WS-DATA-AUX.
           03 WS-DATA-DD-AUX       PIC 9(02).
           03 WS-DATA-MM-AUX       PIC 9(02).
           03 WS-DATA-AA-AUX       PIC 9(04).

       01 WS-TIME                  PIC X(10).
       01 FILLER                   REDEFINES WS-TIME.
           03 WS-TIME-HOUR         PIC 9(02).
           03 WS-TIME-MIN          PIC 9(02).
           03 WS-TIME-SEC          PIC 9(02).

      * VARIAVEL BOOLEAN
       01 WS-PAGAMENTO             PIC 9(01) VALUE ZERO.
          88 WS-PAGAMENTO-OK                 VALUE 1 FALSE ZERO.

       77 WS-DAY-OF-WEEK           PIC 9(01) VALUE ZERO.
       77 WS-TESTE                 PIC X(10) VALUE ' MENSAGEM'.
       77 WS-DATE-FULL-SHOW        PIC X(10) VALUE SPACES.
       77 WS-CAMPO-COMPLETO        PIC X(20) VALUE 'EMERSON PINHEIRO'.
       77 WS-CAMPO-1               PIC X(10) VALUE SPACES.
       77 WS-CAMPO-2               PIC X(10) VALUE SPACES.
       77 WS-NUM-1                 PIC 9(02) VALUE ZERO.
       77 WS-NUM-2                 PIC 9(02) VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      * ESTUDAR AS VARIAVEIS DE SISTEMA PORQUE SAO MUITO UTEIS
      * ESTUDO DOS COMANDOS DISPLAY E ACCEPT

           DISPLAY "ESTUDO DO DISPLAY E ACCEPT"
           DISPLAY "Hello world " 45 WS-TESTE.
           ACCEPT  WS-DATA           FROM DATE YYYYMMDD.
           ACCEPT  WS-TIME           FROM TIME.
           ACCEPT  WS-DAY-OF-WEEK    FROM DAY-OF-WEEK.
           DISPLAY WS-DATA WS-TIME WS-DAY-OF-WEEK.
           ACCEPT  WS-DATE-FULL-SHOW FROM DAY YYYYDDD.
           DISPLAY WS-DATE-FULL-SHOW.
           ACCEPT  WS-DATE-FULL-SHOW FROM DATE YYYYMMDD.
           DISPLAY WS-DATE-FULL-SHOW.
           DISPLAY "======================================="
      * ESTUDO DO COMANDO MOVE
           DISPLAY "ESTUDO DO MOVE"
           MOVE WS-CAMPO-COMPLETO(1:7) TO WS-CAMPO-1.
           MOVE WS-CAMPO-COMPLETO(9:8) TO WS-CAMPO-2.
           DISPLAY WS-CAMPO-COMPLETO WS-CAMPO-1 WS-CAMPO-2.

           MOVE "12"        TO WS-DATA(01:02).
           MOVE "/"         TO WS-DATA(03:01).
           MOVE "04"        TO WS-DATA(04:02).
           MOVE "/"         TO WS-DATA(06:01).
           MOVE "2022"      TO WS-DATA(07:04).
           DISPLAY WS-DATA.

           MOVE WS-DATA(07:04) TO WS-DATA-AA-AUX.
           DISPLAY WS-DATA-AA-AUX.
           DISPLAY "======================================="
      *SET - MAIS RECOMENDÁVEL QUE O MOVE, INCLUSIVE PARA BOOLEAN
      *IMPORTANTE: SUBSTITUE O CONTEUDO DA VARIAVEL. NÃO O QUE
      *ESTA NA AREA DE PONTEIRO DE MEMORIA.
           DISPLAY "ESTUDO DO SET"
           DISPLAY "WS-NUM-1 " WS-NUM-1 " WS-NUM-2 " WS-NUM-2.
           SET WS-NUM-1 TO 5.
           SET WS-NUM-2 TO 10.
           DISPLAY "WS-NUM-1 " WS-NUM-1 " WS-NUM-2 " WS-NUM-2.
           SET WS-NUM-2 TO WS-NUM-1.
           DISPLAY "WS-NUM-1 " WS-NUM-1 " WS-NUM-2 " WS-NUM-2.
           DISPLAY "======================================="
      *TRABALHANDO COM BOOLEAN
           DISPLAY "ESTUDO DO BOOLEAN"
           DISPLAY "WS-PAGAMENTO AINDA NAO PAGO: " WS-PAGAMENTO.
           SET WS-PAGAMENTO-OK TO TRUE.
           DISPLAY "WS-PAGAMENTO FEITO: " WS-PAGAMENTO.

           SET WS-PAGAMENTO-OK TO FALSE.
           DISPLAY "WS-PAGAMENTO ESTORNADO: " WS-PAGAMENTO.
           DISPLAY "======================================="

           GOBACK.
       END PROGRAM PROGRAM02.
