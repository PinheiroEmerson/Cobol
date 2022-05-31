      ******************************************************************
      * Author:    EMERSON PINHEIRO - TIO.EL@OUTLOOK.COM.
      * Date:      16/05/2022
      * Purpose:   DESAFIO MODULO 2 - MEDIA DO ALUNO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NTNOTMEN.
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       77  WS-OPCAO                    PIC X      VALUE SPACE.

       LINKAGE SECTION.
       01  LK-COM-AREA.
           03 WS-COM-MENSAGEM          PIC X(40).

       PROCEDURE DIVISION
           USING LK-COM-AREA.

       MAIN-PROCEDURE.
           PERFORM P100-INICIO     THRU P100-INICIO-FIM.
           PERFORM P200-PROCESSA   THRU P200-PROCESSA-FIM
                   UNTIL WS-OPCAO  EQUAL '9'.
           PERFORM P900-FINALIZA   THRU P900-FINALIZA-FIM.
       MAIN-PROCEDURE-FIM.

       P100-INICIO.
           DISPLAY 'INICIO DO PROCESSAMENTO.'
           END-DISPLAY.
           INITIALISE LK-COM-AREA WS-OPCAO
               REPLACING NUMERIC       BY ZEROES
                         ALPHANUMERIC  BY SPACES.
       P100-INICIO-FIM.

       P200-PROCESSA.
           PERFORM P430-MONTA-TELA     THRU P430-MONTA-TELA-FIM.
           ACCEPT WS-OPCAO
           END-ACCEPT.
           EVALUATE WS-OPCAO
               WHEN '1'
                   MOVE ' ** PROCESSAR MEDIA ALUNO ** '
                                                   TO WS-COM-MENSAGEM
                   CALL
                   'D:\My Documents\Cobol\Programs\bin\NTNOTINC'
                   USING LK-COM-AREA
               WHEN '2'
                   MOVE ' ** LISTAR APROVADOS ** '
                                                   TO WS-COM-MENSAGEM
                   CALL
                   'D:\My Documents\Cobol\Programs\bin\NTLISAPR'
                   USING LK-COM-AREA
               WHEN '3'
                   MOVE ' ** LISTAR TODOS ** '
                                                   TO WS-COM-MENSAGEM
                   CALL
                   'D:\My Documents\Cobol\Programs\bin\NTNOTLIS'
                   USING LK-COM-AREA
               WHEN '9'
                   DISPLAY 'PROGRAMA PRINCIPAL ENCERRADO'
                   END-DISPLAY
               WHEN OTHER
                   PERFORM P800-ERRO   THRU P800-ERRO-FIM
           END-EVALUATE.

       P200-PROCESSA-FIM.

       P430-MONTA-TELA.
           DISPLAY '*******************************'.
           DISPLAY '***** SISTEMA  DE  NOTAS ******'.
           DISPLAY '*******************************'.
           DISPLAY '|      ESCOLHA:               |'.
           DISPLAY '|<1> - PROCESSAR MEDIA ALUNO  |'.
           DISPLAY '|<2> - LISTAR APROVADOS       |'.
           DISPLAY '|<3> - LISTAR TODOS           |'.
           DISPLAY '|<9> - SISTEMA PRINCIPAL      |'.
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

       END PROGRAM NTNOTMEN.
