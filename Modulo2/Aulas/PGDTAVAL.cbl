      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM)
      * Date:      02/04/2022 - 8:00 A.M.
      * Purpose:   VALIDAR UMA DATA QUALQUER
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGDTAVAL.
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

       77 WS-NOME-MES-AUX          PIC X(15) VALUE SPACE.
       77 WS-TECLA-AUX             PIC X(01) VALUE SPACE.
       77 WS-VALIDA-DATA           PIC 9(01) VALUE ZERO.

       PROCEDURE DIVISION.

       P001-MAIN.
           PERFORM P100-INICIAL       THRU P100-INICIAL-FIM.
           PERFORM P150-VALIDA-DATA   THROUGH P150-VALIDA-DATA-FIM            /
                   UNTIL WS-TECLA-AUX EQUALS TO "N" OR                      /
                   WS-TECLA-AUX       EQUALS TO "n".
           PERFORM P900-TERMINAL      THRU  P900-TERMINAL-FIM.
       P001-MAIN-FIM.
      *-----------------------------------------------------------------
       P100-INICIAL.
      *-----------------------------------------------------------------
      *INICIALIZANDO VARIÁVEIS INICIO
           INITIALISE WS-ANO-BISSEXTO, WS-DATA, WS-DIAS-MES,
                      WS-VALIDA-DATA, WS-NOME-MES-AUX, WS-TECLA-AUX
                      REPLACING
                      ALPHANUMERIC BY SPACES
                      NUMERIC      BY ZEROS.
      *-----------------------------------------------------------------
       P100-INICIAL-FIM.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       P150-VALIDA-DATA.
      *-----------------------------------------------------------------
      *MENSAGENS DE TELA E ATRIBUICAO DE VALORES
           DISPLAY "INICIO DO PROCESSAMENTO...".
           DISPLAY "INFORME UMA DATA: ".

      *RECEBE DATA NA TELA
           ACCEPT  WS-DATA.

      *MOVE CONTEUDO RECEBIDO EM WS-DATA
           MOVE    WS-DATA-MM TO WS-MESES-ANO.
           MOVE    WS-DATA-DD TO WS-DIAS-MES.
           MOVE    WS-DATA-AA TO WS-ANO.

      *CALCULO DE VALIDACAO DA DATA
           PERFORM P200-CALCULA-BISSEXTO THRU P200-CALCULA-BISSEXTO-FIM.
           PERFORM P300-VALIDA-MES-DIA   THRU P300-VALIDA-MES-DIA-FIM.
           PERFORM P400-RESULTADO        THRU P400-RESULTADO-FIM.
           PERFORM P100-INICIAL          THRU P100-INICIAL-FIM.

      *DISPONIBILIZA NOVO TESTE DE DATA
           DISPLAY "DESEJA VALIDAR OUTRA DATA? ".
           ACCEPT  WS-TECLA-AUX.

      *-----------------------------------------------------------------
       P150-VALIDA-DATA-FIM.
      *-----------------------------------------------------------------

      *VALIDA SE É ANO BISSEXTO OU NAO BISSEXTO
      *CRÉDITOS: DORNELLES, CARLOS ALBERTO.
      *-----------------------------------------------------------------
       P200-CALCULA-BISSEXTO.
      *-----------------------------------------------------------------
           DIVIDE WS-ANO BY 4   GIVING WS-QUOCIENTE REMAINDER
                                       WS-RES4.
           DIVIDE WS-ANO BY 400 GIVING WS-QUOCIENTE REMAINDER
                                       WS-RES400.
           DIVIDE WS-ANO BY 100 GIVING WS-QUOCIENTE REMAINDER
                                       WS-RES100.

           EVALUATE
              (WS-RES4   EQUAL ZERO)  ALSO                                 /
              (WS-RES400 EQUAL ZERO)  ALSO
              (WS-RES100 IS GREATER THAN ZERO)
                  WHEN TRUE ALSO TRUE ALSO ANY
                  WHEN TRUE ALSO TRUE ALSO TRUE
                  WHEN TRUE ALSO ANY  ALSO TRUE
                       MOVE 1 TO WS-BISSEXTO
                  WHEN OTHER
                       MOVE 1 TO WS-BISSEXTO-NAO
           END-EVALUATE.
      *-----------------------------------------------------------------
       P200-CALCULA-BISSEXTO-FIM.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       P300-VALIDA-MES-DIA.
      *-----------------------------------------------------------------
           EVALUATE WS-DIAS-MES

              WHEN 1 THRU 28
                 IF WS-MESES-ANO > 0
                    MOVE 1 TO WS-VALIDA-DATA
                 END-IF

              WHEN 1 THRU 29
                 IF WS-MESES-ANO EQUAL 2 AND WS-BISSEXTO = 1
                    MOVE 1 TO WS-VALIDA-DATA
                 END-IF

              WHEN 1 THRU 30
                 IF WS-MESES-ANO NOT EQUAL 2
                    MOVE 1 TO WS-VALIDA-DATA
                 END-IF

              WHEN 1 THRU 31
                 IF WS-MESES-ANO = 1  OR
                    WS-MESES-ANO = 3  OR
                    WS-MESES-ANO = 5  OR
                    WS-MESES-ANO = 7  OR
                    WS-MESES-ANO = 8  OR
                    WS-MESES-ANO = 10 OR
                    WS-MESES-ANO = 12
                    MOVE 1 TO WS-VALIDA-DATA
                 END-IF

           END-EVALUATE.

      *-----------------------------------------------------------------
       P300-VALIDA-MES-DIA-FIM.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       P400-RESULTADO.
      *-----------------------------------------------------------------
           IF WS-VALIDA-DATA EQUAL ZERO
               DISPLAY "A DATA INFORMADA EH INVALIDA"
           ELSE
               DISPLAY "A DATA INFORMADA EH VALIDA"
           END-IF.
      *-----------------------------------------------------------------
       P400-RESULTADO-FIM.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       P900-TERMINAL.
      *-----------------------------------------------------------------
           DISPLAY "FIM DO PROCESSAMENTO."
           GOBACK.
      *-----------------------------------------------------------------
       P900-TERMINAL-FIM.
      *-----------------------------------------------------------------

       END PROGRAM PGDTAVAL.
