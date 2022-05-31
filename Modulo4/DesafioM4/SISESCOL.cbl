      ******************************************************************
      * Author:    EMERSON PINHEIRO - EMAIL. TIO.EL@OUTLOOK.COM.
      * Date:      04/05/2022.
      * Purpose:   SISTEMA ESCOLAR.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SISESCOL.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       77  WS-OPCAO                    PIC X      VALUE SPACE.

       01  WS-COM-AREA.
           03 WS-COM-MENSAGEM          PIC X(40)  VALUE SPACES.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM P100-INICIO     THRU P100-INICIO-FIM.
           PERFORM P200-PROCESSA   THRU P200-PROCESSA-FIM
                   UNTIL WS-OPCAO  EQUAL '9'.
           PERFORM P900-FINALIZA   THRU P900-FINALIZA-FIM.
       MAIN-PROCEDURE-FIM.

       P100-INICIO.
           DISPLAY 'INICIO DO PROCESSAMENTO.'
           END-DISPLAY.
           INITIALISE WS-OPCAO
               REPLACING NUMERIC       BY ZEROES
                         ALPHANUMERIC  BY SPACES.
       P100-INICIO-FIM.

       P200-PROCESSA.
           PERFORM P430-MONTA-TELA     THRU P430-MONTA-TELA-FIM.
           ACCEPT WS-OPCAO
           END-ACCEPT.
           EVALUATE WS-OPCAO
               WHEN '1'
                   MOVE ' *** MENU DE ALUNOS *** '
                                                   TO WS-COM-MENSAGEM
                   CALL
                   'D:\My Documents\Cobol\Programs\bin\NTALUMEN'
                   USING  WS-COM-AREA

               WHEN '2'
                MOVE ' *** MENU DE MATERIAS *** '
                                                   TO WS-COM-MENSAGEM
                   CALL
                   'D:\My Documents\Cobol\Programs\bin\NTMATMEN'
                   USING  WS-COM-AREA
               WHEN '3'
                MOVE ' *** MENU DE NOTAS *** '
                                                   TO WS-COM-MENSAGEM
                   CALL
                   'D:\My Documents\Cobol\Programs\bin\NTNOTMEN'
                   USING  WS-COM-AREA

               WHEN '9'
                   DISPLAY 'PROGRAMA ENCERRADO'
                   END-DISPLAY
               WHEN OTHER
                   PERFORM P800-ERRO   THRU P800-ERRO-FIM
           END-EVALUATE.

       P200-PROCESSA-FIM.

       P430-MONTA-TELA.
           DISPLAY '*******************************'.
           DISPLAY '****** SISTEMA  ESCOLAR *******'.
           DISPLAY '*******************************'.
           DISPLAY '|      ESCOLHA:               |'.
           DISPLAY '|<1> - MENU ALUNOS            |'.
           DISPLAY '|<2> - MENU MATERIAS          |'.
           DISPLAY '|<3> - MENU NOTAS             |'.
           DISPLAY '|<9> - SAIR DO SISTEMA        |'.
           DISPLAY '*******************************'.

       P430-MONTA-TELA-FIM.

       P800-ERRO.
           DISPLAY 'OPCAO INVALIDA.'
           END-DISPLAY.
       P800-ERRO-FIM.


       P900-FINALIZA.
           DISPLAY 'FIM DO PROCESSAMENTO.'
           END-DISPLAY.
           GOBACK.
       P900-FINALIZA-FIM.

       END PROGRAM SISESCOL.
