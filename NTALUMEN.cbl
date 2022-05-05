      ******************************************************************
      * Author:    EMERSON PINHEIRO - EMAIL. TIO.EL@OUTLOOK.COM.
      * Date:      04/05/2022.
      * Purpose:   MENU CADASTRO DE ALUNOS.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NTALUMEN.

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
                   MOVE ' *** INCLUSAO DE ALUNO *** '
                                                   TO WS-COM-MENSAGEM
                   CALL
                   'D:\My Documents\Cobol\Programs\bin\NTALUINC'
                   USING LK-COM-AREA
               WHEN '2'
                   MOVE ' *** CONSULTA DE ALUNO *** '
                                                   TO WS-COM-MENSAGEM
                   CALL
                   'D:\My Documents\Cobol\Programs\bin\NTALUCON'
                   USING LK-COM-AREA
               WHEN '3'
                   MOVE ' *** ALTERACAO DE ALUNO *** '
                                                   TO WS-COM-MENSAGEM
                   CALL
                   'D:\My Documents\Cobol\Programs\bin\NTALUALT'
                   USING LK-COM-AREA
               WHEN '4'
                   MOVE ' *** EXCLUSAO DE ALUNO *** '
                                                   TO WS-COM-MENSAGEM
                   CALL
                   'D:\My Documents\Cobol\Programs\bin\NTALUEXC'
                   USING LK-COM-AREA
               WHEN '5'
                   MOVE ' *** LISTAGEM DE ALUNO *** '
                                                   TO WS-COM-MENSAGEM
                   CALL
                   'D:\My Documents\Cobol\Programs\bin\NTALULIS'
                   USING LK-COM-AREA
               WHEN '9'
                   DISPLAY 'PROGRAMA PRINCIPAL ENCERRADO'
                   END-DISPLAY
               WHEN OTHER
                   PERFORM P800-ERRO   THRU P800-ERRO-FIM
           END-EVALUATE.

       P200-PROCESSA-FIM.

       P430-MONTA-TELA.
           DISPLAY '******************************'.
           DISPLAY '***** SISTEMA  DE  ALUNOS ****'.
           DISPLAY '******************************'.
           DISPLAY '|      ESCOLHA:               |'.
           DISPLAY '|<1> - CADASTRAR ALUNO        |'.
           DISPLAY '|<2> - CONSULTAR ALUNO        |'.
           DISPLAY '|<3> - ALTERAR ALUNO          |'.
           DISPLAY '|<4> - EXCLUIR ALUNO          |'.
           DISPLAY '|<5> - LISTAR  ALUNOS         |'.
           DISPLAY '|<9> - SISTEMA PRINCIPAL      |'.
           DISPLAY '******************************'.

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

       END PROGRAM NTALUMEN.
