      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM)
      * Date:      1/04/2022 - 8:00 A.M.
      * Purpose:   TESTAR COMANDO UNSTRING
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM08.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-NOME-COMPLETO           PIC X(50) VALUE SPACES.
       77  WS-MOSTRA                  PIC X(50) VALUE SPACES.

       01 WS-NOME.
          03 WS-PRIM-NOME             PIC X(10) VALUE SPACES.
          03 WS-MEIO-NOME             PIC X(10) VALUE SPACES.
          03 WS-ULT-NOME              PIC X(10) VALUE SPACES.
          03 WS-RESTO                 PIC X(20) VALUE SPACES.

       01 WS-CONTADORES.
          03 WS-PONTEIRO              PIC 9(02) VALUE ZEROES.
          03 WS-TOT-CAMPOS            PIC 9(02) VALUE ZEROES.
          03 WS-TAM-1                 PIC 9(02) VALUE ZEROES.
          03 WS-TAM-2                 PIC 9(02) VALUE ZEROES.
          03 WS-TAM-3                 PIC 9(02) VALUE ZEROES.
          03 WS-TAM-4                 PIC 9(02) VALUE ZEROES.

       PROCEDURE DIVISION.

           PERFORM P100-INICIAL  THRU P100-INICIAL-FIM.
           PERFORM P900-TERMINAL THRU P900-TERMINAL-FIM.

      *-----------------------------------------------------------------
       P100-INICIAL.
      *-----------------------------------------------------------------
           DISPLAY 'INICIO DO PROCESSAMENTO...'.

           DISPLAY "FORMA 1".
           INITIALISE WS-NOME WS-NOME-COMPLETO WS-MOSTRA
                      WS-CONTADORES REPLACING
                                  ALPHANUMERIC BY SPACES
                                  NUMERIC      BY ZEROES.
           MOVE 'EMERSON  FERNANDO  LOPES PNHEIRO TROUXA D+' TO
           WS-NOME-COMPLETO.
           DISPLAY WS-NOME-COMPLETO.

           UNSTRING
             WS-NOME-COMPLETO
             DELIMITED BY ALL SPACE
             INTO WS-PRIM-NOME WS-MEIO-NOME WS-ULT-NOME WS-RESTO
           END-UNSTRING.
           DISPLAY "PRIMEIRO NOME: " WS-PRIM-NOME.
           DISPLAY "PRIMEIRO NOME: " WS-MEIO-NOME.
           DISPLAY "PRIMEIRO NOME: " WS-ULT-NOME.
           DISPLAY "RESTO:         " WS-RESTO.
      *-----------------------------------------------------------

           DISPLAY "FORMA 2 COM PONTEIRO".
           INITIALISE WS-NOME WS-NOME-COMPLETO WS-MOSTRA
                      WS-CONTADORES REPLACING
                                  ALPHANUMERIC BY SPACES
                                  NUMERIC      BY ZEROES.

           MOVE 1                                       TO WS-PONTEIRO.
           MOVE 'EMERSON   FERNANDO   LOPES   PINHEIRO' TO
                                       WS-NOME-COMPLETO.
           DISPLAY "NOME COMPLETO: "   WS-NOME-COMPLETO.
           DISPLAY "PONTEIRO: "        WS-PONTEIRO.
           DISPLAY "TOTAL DE CAMPOS: " WS-TOT-CAMPOS.


           UNSTRING
             WS-NOME-COMPLETO
             DELIMITED BY ALL SPACE
             INTO WS-PRIM-NOME WS-MEIO-NOME WS-ULT-NOME WS-RESTO
             WITH POINTER WS-PONTEIRO
             TALLYING IN  WS-TOT-CAMPOS
           END-UNSTRING.
           DISPLAY "PRIMEIRO NOME: " WS-PRIM-NOME.
           DISPLAY "MEIO     NOME: " WS-MEIO-NOME.
           DISPLAY "ULTIMO   NOME: " WS-ULT-NOME.
           DISPLAY "RESTO:         " WS-RESTO.
           DISPLAY "PONTEIRO: "        WS-PONTEIRO.
           DISPLAY "TOTAL DE CAMPOS: " WS-TOT-CAMPOS.

      *-----------------------------------------------------------

           DISPLAY "FORMA 3 COM DELIMITADOR DIFERENCIADO".
           INITIALISE WS-NOME WS-NOME-COMPLETO WS-MOSTRA
                      WS-CONTADORES REPLACING
                                  ALPHANUMERIC BY SPACES
                                  NUMERIC      BY ZEROES.

           MOVE 1                                  TO WS-PONTEIRO.
           MOVE 'EMERSON*FERNANDO;LOPES-PINHEIRO' TO
                                       WS-NOME-COMPLETO.
           DISPLAY "NOME COMPLETO: "   WS-NOME-COMPLETO.
           DISPLAY "PONTEIRO: "        WS-PONTEIRO.
           DISPLAY "TOTAL DE CAMPOS: " WS-TOT-CAMPOS.


           UNSTRING
             WS-NOME-COMPLETO
             DELIMITED BY "*" OR ";" OR "-"
             INTO WS-PRIM-NOME WS-MEIO-NOME WS-ULT-NOME WS-RESTO
             WITH POINTER WS-PONTEIRO
             TALLYING IN  WS-TOT-CAMPOS
           END-UNSTRING.
           DISPLAY "PRIMEIRO NOME: " WS-PRIM-NOME.
           DISPLAY "MEIO     NOME: " WS-MEIO-NOME.
           DISPLAY "ULTIMO   NOME: " WS-ULT-NOME.
           DISPLAY "RESTO:         " WS-RESTO.
           DISPLAY "PONTEIRO: "        WS-PONTEIRO.
           DISPLAY "TOTAL DE CAMPOS: " WS-TOT-CAMPOS.
      *-----------------------------------------------------------

           DISPLAY "FORMA 4 COM DELIMITADOR DIFERENCIADO".
           INITIALISE WS-NOME WS-NOME-COMPLETO WS-MOSTRA
                      WS-CONTADORES REPLACING
                                  ALPHANUMERIC BY SPACES
                                  NUMERIC      BY ZEROES.

           MOVE 1                                  TO WS-PONTEIRO.
           MOVE 'PEDRO BICHO *PICA-FUMO BOM;COSTA SILVA'  TO
                                       WS-NOME-COMPLETO.
           DISPLAY "NOME COMPLETO: "   WS-NOME-COMPLETO.
           DISPLAY "PONTEIRO: "        WS-PONTEIRO.
           DISPLAY "TOTAL DE CAMPOS: " WS-TOT-CAMPOS.


           UNSTRING
             WS-NOME-COMPLETO
             DELIMITED BY "*" OR ";" OR "-"
             INTO         WS-PRIM-NOME COUNT IN WS-TAM-1
                          WS-MEIO-NOME COUNT IN WS-TAM-2
                          WS-ULT-NOME  COUNT IN WS-TAM-3
                          WS-RESTO     COUNT IN WS-TAM-4
             WITH POINTER WS-PONTEIRO
             TALLYING IN  WS-TOT-CAMPOS

           END-UNSTRING.
           DISPLAY "PRIMEIRO NOME: " WS-PRIM-NOME.
           DISPLAY "MEIO     NOME: " WS-MEIO-NOME.
           DISPLAY "ULTIMO   NOME: " WS-ULT-NOME.
           DISPLAY "RESTO:         " WS-RESTO.
           DISPLAY "PONTEIRO: "        WS-PONTEIRO.
           DISPLAY "TOTAL DE CAMPOS: " WS-TOT-CAMPOS.
           DISPLAY "WS-TAM-1 " WS-TAM-1.
           DISPLAY "WS-TAM-2 " WS-TAM-2.
           DISPLAY "WS-TAM-3 " WS-TAM-3.
           DISPLAY "WS-TAM-4 " WS-TAM-4.
           DISPLAY "WS-PONTEIRO " WS-PONTEIRO.

      *-----------------------------------------------------------

           DISPLAY "FORMA 5 ".
           INITIALISE WS-NOME WS-NOME-COMPLETO WS-MOSTRA
                      WS-CONTADORES REPLACING
                                  ALPHANUMERIC BY SPACES
                                  NUMERIC      BY ZEROES.

           MOVE 1                                  TO WS-PONTEIRO.
           MOVE 'MARIO;;;;;;CEZAR*CASTRO******TROUXA;'  TO
                                       WS-NOME-COMPLETO.
           DISPLAY "NOME COMPLETO: "   WS-NOME-COMPLETO.
           DISPLAY "PONTEIRO: "        WS-PONTEIRO.
           DISPLAY "TOTAL DE CAMPOS: " WS-TOT-CAMPOS.


           UNSTRING
             WS-NOME-COMPLETO
             DELIMITED BY ALL "*" OR ALL ";"
             INTO         WS-PRIM-NOME COUNT IN WS-TAM-1
                          WS-MEIO-NOME COUNT IN WS-TAM-2
                          WS-ULT-NOME  COUNT IN WS-TAM-3
                          WS-RESTO     COUNT IN WS-TAM-4
             WITH POINTER WS-PONTEIRO
             TALLYING IN  WS-TOT-CAMPOS

           END-UNSTRING.
           DISPLAY "PRIMEIRO NOME: " WS-PRIM-NOME.
           DISPLAY "MEIO     NOME: " WS-MEIO-NOME.
           DISPLAY "ULTIMO   NOME: " WS-ULT-NOME.
           DISPLAY "RESTO:         " WS-RESTO.
           DISPLAY "PONTEIRO: "        WS-PONTEIRO.
           DISPLAY "TOTAL DE CAMPOS: " WS-TOT-CAMPOS.
           DISPLAY "WS-TAM-1 " WS-TAM-1.
           DISPLAY "WS-TAM-2 " WS-TAM-2.
           DISPLAY "WS-TAM-3 " WS-TAM-3.
           DISPLAY "WS-TAM-4 " WS-TAM-4.
           DISPLAY "WS-PONTEIRO " WS-PONTEIRO.

      *-----------------------------------------------------------------
       P100-INICIAL-FIM.
      *-----------------------------------------------------------------


      *-----------------------------------------------------------------
       P900-TERMINAL.
      *-----------------------------------------------------------------
           DISPLAY 'FIM DO PROCESSAMENTO.'.
           GOBACK.
      *-----------------------------------------------------------------
       P900-TERMINAL-FIM.
      *-----------------------------------------------------------------

       END PROGRAM PROGRAM08.
