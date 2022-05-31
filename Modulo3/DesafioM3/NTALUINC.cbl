      ******************************************************************
      * Author:    EMERSON PINHEIRO - EMAIL. TIO.EL@OUTLOOK.COM.
      * Date:      27/04/2022.
      * Purpose:   CADASTRO DE ALUNO.
      * Updata:    TRANSFORMADO DE EXECUTAVEL PARA MODULO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NTALUINC.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ALUNOS
           ASSIGN TO
           'D:\My Documents\Cobol\Modulo3\DesafioM3\ALUNOS.DAT'
           ORGANIZATION IS INDEXED
           ACCESS  MODE IS RANDOM
           RECORD KEY IS ID-ALUNO
           FILE  STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  ALUNOS.
           COPY CFPK0001.

       WORKING-STORAGE SECTION.

       01  WS-ALUNO                    PIC X(32) VALUE SPACES.
       01  FILLER REDEFINES WS-ALUNO.
           03 WS-ID-ALUNO              PIC 9(03).
           03 WS-NM-ALUNO              PIC X(20).
           03 WS-TL-ALUNO.
               05 WS-FONEAREA        PIC 9(02).
               05 WS-FONENUMERO      PIC 9(09).

       77  WS-FS                       PIC 99.
           88 WS-FS-OK                 VALUE 0.

       77  WS-EOF                      PIC X.
           88 WS-EOF-OK                VALUE 'S' FALSE 'N'.

       77  WS-EXIT                     PIC X.
           88 WS-EXIT-OK               VALUE 'S' FALSE 'N'.

       LINKAGE SECTION.
       01  LK-COM-AREA.
           03 WS-COM-MENSAGEM          PIC X(40).

       PROCEDURE DIVISION
           USING LK-COM-AREA.
       MAIN-PROCEDURE.

           PERFORM P100-INICIO     THRU P100-INICIO-FIM.
           PERFORM P200-PROCESSA   THRU P200-PROCESSA-FIM
                   UNTIL WS-EXIT-OK.
           PERFORM P900-FINALIZA   THRU P900-FINALIZA-FIM.

       MAIN-PROCEDURE-FIM.

       P100-INICIO.
           DISPLAY 'INICIO DO PROCESSAMENTO.'
           END-DISPLAY.
           INITIALISE WS-FS WS-ALUNO
               REPLACING NUMERIC       BY ZEROES
                         ALPHANUMERIC  BY SPACES.
           SET WS-EOF-OK               TO FALSE.
           SET WS-EXIT-OK              TO FALSE.

           DISPLAY WS-COM-MENSAGEM
           END-DISPLAY.
           PERFORM P400-ABRE-ARQ   THRU P400-ABRE-ARQ-FIM.
       P100-INICIO-FIM.

       P200-PROCESSA.
           PERFORM P430-MONTA-TELA THRU P430-MONTA-TELA-FIM.
           PERFORM P410-GRAVA-REGISTRO THRU
                   P410-GRAVA-REGISTRO-FIM
           DISPLAY 'TECLE: '
                   '<QUALQUER TECLA> PARA CONTINUAR, OU'
                   ' <S> PARA SAIR'
           END-DISPLAY.
           ACCEPT WS-EXIT
           END-ACCEPT.
       P200-PROCESSA-FIM.

       P400-ABRE-ARQ.
           OPEN I-O ALUNOS.
      *VE SE O ARQUIVO EXISTE. SE NAO EXISTE (35) CRIA ARQUIVO.
           IF NOT WS-FS-OK THEN
               OPEN OUTPUT ALUNOS
           END-IF.
       P400-ABRE-ARQ-FIM.

       P410-GRAVA-REGISTRO.
           MOVE WS-ID-ALUNO TO ID-ALUNO.
           MOVE WS-NM-ALUNO TO NM-ALUNO.
           MOVE WS-TL-ALUNO TO TL-ALUNO.

           WRITE REG-ALUNO
               INVALID KEY
                   DISPLAY 'ALUNO JAH CADASTRADO.'
                   END-DISPLAY
               NOT INVALID KEY
                   DISPLAY'ALUNO SALVO COM SUCESSO.'
                   END-DISPLAY
           END-WRITE.
       P410-GRAVA-REGISTRO-FIM.

       P420-FECHA-ARQ.
           CLOSE ALUNOS.
       P420-FECHA-ARQ-FIM.

       P430-MONTA-TELA.
           DISPLAY 'PARA REGISTRAR UM ALUNO, INFORME:'
           END-DISPLAY.
           DISPLAY 'UM NUMERO PARA ID: '
           END-DISPLAY.
           ACCEPT WS-ID-ALUNO
           END-ACCEPT.
           DISPLAY 'NOME DO ALUNO: '
           END-DISPLAY.
           ACCEPT WS-NM-ALUNO
           END-ACCEPT.
           DISPLAY 'TELEFONE DO ALUNO: '
           END-DISPLAY.
           ACCEPT WS-TL-ALUNO
           END-ACCEPT.
       P430-MONTA-TELA-FIM.

       P800-ERRO.
           DISPLAY 'ERRO DE GRAVACAO. APLICACAO FINALIZADA.'
           END-DISPLAY.
           DISPLAY 'FILE STATUS: ' WS-FS
           END-DISPLAY.
           PERFORM P900-FINALIZA  THRU P900-FINALIZA-FIM.
       P800-ERRO-FIM.


       P900-FINALIZA.
           PERFORM P420-FECHA-ARQ THRU P420-FECHA-ARQ-FIM.
           DISPLAY 'FIM DO PROCESSAMENTO.'
           END-DISPLAY.
           GOBACK.
       P900-FINALIZA-FIM.

       END PROGRAM NTALUINC.
