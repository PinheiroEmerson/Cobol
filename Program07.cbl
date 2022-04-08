      ******************************************************************
      * Author:    EMERSON PINHEIRO (TIO.EL@OUTLOOK.COM)
      * Date:      02/04/2022 - 8:00 A.M.
      * Purpose:   TESTAR COMANDO STRING
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM07.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-CONTEUDO                PIC X(30) VALUE SPACES.
       77  WS-TEXTO                   PIC X(30) VALUE SPACES.
       77  WS-PONTEIRO                PIC 9(02) VALUE ZERO.
       PROCEDURE DIVISION.

           PERFORM P100-INICIAL  THRU P100-INICIAL-FIM.
           PERFORM P900-TERMINAL THRU P900-TERMINAL-FIM.

      *-----------------------------------------------------------------
       P100-INICIAL.
      *-----------------------------------------------------------------
           DISPLAY 'INICIO DO PROCESSAMENTO...'.

           INITIALISE WS-CONTEUDO WS-TEXTO.

           STRING
             'ANDRE'
             ' '
             'PEDRO'
             DELIMITED BY SIZE INTO WS-CONTEUDO
           END-STRING.

           DISPLAY WS-CONTEUDO.


           INITIALISE WS-CONTEUDO WS-TEXTO.

           MOVE 'ANDRE COSTA MINISTRA CURSO' TO WS-TEXTO.

           STRING
             WS-TEXTO
             DELIMITED BY 'U' INTO WS-CONTEUDO
           END-STRING.
           DISPLAY WS-CONTEUDO.

           INITIALISE WS-CONTEUDO WS-TEXTO.

           MOVE 'ANDRE COSTA MINISTRA CURSO' TO WS-TEXTO.

           STRING
             WS-TEXTO(01:05)
             WS-TEXTO(21:10)
             DELIMITED BY SIZE INTO WS-CONTEUDO
           END-STRING.
           DISPLAY WS-CONTEUDO.

           MOVE 'O             MINISTRA CURSO' TO WS-TEXTO.

           SET WS-PONTEIRO TO 3.
           DISPLAY WS-PONTEIRO '  PONTEIRO'
           STRING
             'ANDRE COSTA'
             DELIMITED BY SIZE INTO WS-TEXTO
             WITH POINTER WS-PONTEIRO
           END-STRING.

           DISPLAY WS-TEXTO.
           DISPLAY WS-PONTEIRO '  PONTEIRO'.
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

       END PROGRAM PROGRAM07.
