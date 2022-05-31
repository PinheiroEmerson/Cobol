      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM)
      * Date:      02/04/2022 - 8:00 A.M.
      * Purpose:   TESTAR COMANDOS INSPECT E INITIALISE
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM06.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 WS-ANO-BISSEXTO.
           03 WS-ANO               PIC 9(04).
           03 WS-RES4              PIC 9(04).
           03 WS-RES400            PIC 9(04).
           03 WS-RES100            PIC 9(04).
           03 WS-BISSEXTO          PIC 9(01).
           03 WS-BISSEXTO-NAO      PIC 9(01).
           03 WS-QUOCIENTE         PIC 9(04).
           03 WS-RESTO             PIC 9(04).

       01 WS-DATA                  PIC X(10).
       01 FILLER                   REDEFINES WS-DATA.
           03 WS-DATA-DD           PIC 9(02).
           03 FILLER               PIC X(01).
           03 WS-DATA-MM           PIC 9(02).
           03 FILLER               PIC X(01).
           03 WS-DATA-AA           PIC 9(04).


       01 WS-DIAS-MES              PIC 9(02).
           88 WS-FEV-NB            VALUE 1 THRU 28.
           88 WS-FEV-B             VALUE 1 THRU 29.
           88 WS-MES-30            VALUE 1 THRU 30.
           88 WS-MES-31            VALUE 1 THRU 31.

       01 WS-MESES-ANO             PIC 9(02).
          88 WS-MES-ATUAL          VALUE 1 THRU 12.

       01 WS-NOME.
          03 WS-PRIMEIRO-NOME      PIC X(10) VALUE 'EMERSON'.
          03 WS-ULTIMO-NOME        PIC X(10) VALUE 'PINEIRO'.
          03 FILLER                PIC 9(02) VALUE 99.
          03 WS-MEIO-NOME          PIC X(10).


       01 WS-NOME-2.
          03 FILLER                PIC X(10) VALUE 'EMERSON'.
          03 FILLER                PIC X(10) VALUE 'PINEIRO'.
          03 WS-NUMERO             PIC 9(02) VALUE 88.


       77 WS-NOME-MES-AUX          PIC X(15) VALUE SPACE.
       77 WS-TECLA-AUX             PIC X(01) VALUE SPACE.
       77 WS-VALIDA-DATA           PIC 9(01) VALUE ZERO.
       77 WS-TOTAL                 PIC 9(02).


       PROCEDURE DIVISION.

           PERFORM P100-INICIAL  THRU P100-INICIAL-FIM.
           PERFORM P900-TERMINAL THRU P900-TERMINAL-FIM.

      *-----------------------------------------------------------------
       P100-INICIAL.
      *-----------------------------------------------------------------

           DISPLAY "INICIO DO PROCESSAMENTO.".

           SET WS-DATA-DD TO 12.
           MOVE "/"       TO WS-DATA(03:01).
           SET WS-DATA-MM TO 05.
           MOVE "/"       TO WS-DATA(06:01).
           SET WS-DATA-AA TO 2021.

           DISPLAY WS-DATA.

           INSPECT WS-DATA TALLYING WS-TOTAL FOR ALL "/"
                           BEFORE INITIAL "21".

           INSPECT WS-DATA REPLACING ALL "/" BY "-"
                           AFTER "12".

           DISPLAY WS-TOTAL.
           DISPLAY WS-DATA.

           DISPLAY 'SEM INITIALISE'.
           DISPLAY 'SEU CONTEUDO EH ' WS-DATA.

           DISPLAY 'COM INITIALISE'.
           INITIALISE WS-DATA.
           DISPLAY 'SEU CONTEUDO EH ' WS-DATA.

           DISPLAY 'ANTES DO INITIALIZE WS-NOME ' WS-NOME.
           INITIALISE WS-NOME.
           DISPLAY 'DEPOIS DO INITIALIZE WS NOME ' WS-NOME.


           DISPLAY 'ANTES DO INITIALIZE WS-NOME-2 ' WS-NOME-2.
           INITIALISE WS-NOME-2 REPLACING ALPHANUMERIC BY SPACES
                                          NUMERIC      BY ZEROES.
           DISPLAY 'DEPOIS DO INITIALIZE WS WS-NOME-2 ' WS-NOME-2.

           MOVE    'DALILA'       TO WS-NOME(23:10).
           SET     WS-NUMERO      TO 77.
           DISPLAY WS-NOME.
           DISPLAY WS-NOME-2.

           MOVE    'SANSAO'        TO WS-NOME-2(01:10).
           DISPLAY WS-NOME-2.
      *-----------------------------------------------------------------
       P100-INICIAL-FIM.
      *-----------------------------------------------------------------


      *-----------------------------------------------------------------
       P900-TERMINAL.
      *-----------------------------------------------------------------
           DISPLAY "FIM DO PROCESSAMENTO."
           GOBACK.
      *-----------------------------------------------------------------
       P900-TERMINAL-FIM.
      *-----------------------------------------------------------------

       END PROGRAM PROGRAM06.
