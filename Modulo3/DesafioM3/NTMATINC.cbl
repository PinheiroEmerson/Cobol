      ******************************************************************
      * Author:    EMERSON PINHEIRO - EMAIL. TIO.EL@OUTLOOK.COM.
      * Date:      27/04/2022.
      * Purpose:   CADASTRO DE MATERIA.
      * Updata:    TRANSFORMADO DE EXECUTAVEL PARA MODULO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NTMATINC.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MATERIAS
           ASSIGN TO
           'D:\My Documents\Cobol\Modulo3\DesafioM3\MATERIAS.DAT'
           ORGANIZATION IS INDEXED
           ACCESS  MODE IS RANDOM
           RECORD KEY IS ID-MATERIA
           FILE  STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  MATERIAS.
           COPY CFPK0002.

       WORKING-STORAGE SECTION.

       01  WS-MATERIA                    PIC X(27) VALUE SPACES.
       01  FILLER REDEFINES WS-MATERIA.
           03 WS-ID-MATERIA              PIC 9(03).
           03 WS-NM-MATERIA              PIC X(20).
           03 WS-NT-APROVACAO            PIC 9(02)V99.

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
           INITIALISE WS-FS WS-MATERIA
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
           OPEN I-O MATERIAS.
      *VE SE O ARQUIVO EXISTE. SE NAO EXISTE (35) CRIA ARQUIVO.
           IF NOT WS-FS-OK THEN
               OPEN OUTPUT MATERIAS
           END-IF.
       P400-ABRE-ARQ-FIM.

       P410-GRAVA-REGISTRO.
           MOVE WS-ID-MATERIA   TO ID-MATERIA.
           MOVE WS-NM-MATERIA   TO NM-MATERIA.
           MOVE WS-NT-APROVACAO TO NT-APROVACAO.

           WRITE REG-MATERIA
               INVALID KEY
                   DISPLAY 'MATERIA JAH CADASTRADO.'
                   END-DISPLAY
               NOT INVALID KEY
                   DISPLAY'MATERIA SALVO COM SUCESSO.'
                   END-DISPLAY
           END-WRITE.
       P410-GRAVA-REGISTRO-FIM.

       P420-FECHA-ARQ.
           CLOSE MATERIAS.
       P420-FECHA-ARQ-FIM.

       P430-MONTA-TELA.
           DISPLAY 'PARA REGISTRAR UM MATERIA, INFORME:'
           END-DISPLAY.
           DISPLAY 'UM NUMERO PARA ID: '
           END-DISPLAY.
           ACCEPT WS-ID-MATERIA
           END-ACCEPT.
           DISPLAY 'UM NOME PARA O MATERIA: '
           END-DISPLAY.
           ACCEPT WS-NM-MATERIA
           END-ACCEPT.
           DISPLAY 'NOTA DE APROVACAO DA MATERIA: '
           END-DISPLAY.
           ACCEPT WS-NT-APROVACAO
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

       END PROGRAM NTMATINC.
