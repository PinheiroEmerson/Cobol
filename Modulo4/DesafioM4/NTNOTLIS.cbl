      ******************************************************************
      * Author:    EMERSON PINHEIRO - TIO.EL@OUTLOOK.COM.
      * Date:      30/05/2022
      * Purpose:   DESAFIO MODULO 2 - LISTAGEM GERAL DE ALUNOS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NTNOTLIS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT TODOS-ALUNOS
           ASSIGN TO
               'D:\My Documents\Cobol\Modulo3\DesafioM3\ALUTODOS.DAT'
           ORGANIZATION IS INDEXED
           ACCESS  MODE IS SEQUENTIAL
           RECORD  KEY IS ID-ALUNO OF REG-TODOS
           FILE  STATUS IS WS-FS-T.

       DATA DIVISION.
       FILE SECTION.

       FD  TODOS-ALUNOS.
           COPY CFPK0004.

       WORKING-STORAGE SECTION.

       01  WS-REG-TODOS.
           03 WS-ID-ALUNO            PIC 9(03).
           03 WS-NM-ALUNO            PIC X(20).
           03 WS-TL-ALUNO.
              05 WS-FONEAREA         PIC 9(02).
              05 WS-FONENUMERO       PIC 9(09).
           03 WS-ID-MATERIA          PIC 9(03).
           03 WS-NM-MATERIA          PIC X(20).
           03 WS-NT-APROVACAO        PIC 9(02)V99.
           03 WS-MD-ALUNO            PIC 9(02)V99.
           03 WS-ST-APROVACAO        PIC X(10).

      *CONTROLE ARQUIVO TODOS-ALUNOS
       77  WS-EOF-T                  PIC X.
           88 WS-EOF-OK              VALUE 'S' FALSE 'N'.
       77  WS-FS-T                   PIC 99.
           88 WS-FS-OK               VALUE 0.

      *CONTROLES AUXILIARES / LOOPING
       77  WS-EXIT                   PIC X.
           88 WS-EXIT-OK             VALUE 'S' FALSE 'N'.

       77  WS-CONTA-REG                PIC 9(04) VALUE ZEROS.

       LINKAGE SECTION.
       01  LK-COM-AREA.
           03 WS-COM-MENSAGEM          PIC X(40).

       PROCEDURE DIVISION
           USING LK-COM-AREA.

       MAIN-PROCEDURE.
           PERFORM P100-INICIO     THRU P100-INICIO-FIM.
           PERFORM P200-PROCESSA   THRU P200-PROCESSA-FIM.
           PERFORM P900-FINALIZA   THRU P900-FINALIZA-FIM.
       MAIN-PROCEDURE-FIM.

       P100-INICIO.
           DISPLAY 'INICIO DO PROCESSAMENTO.'
           END-DISPLAY.
           INITIALISE WS-FS-T  WS-CONTA-REG WS-REG-TODOS
               REPLACING NUMERIC       BY ZEROES
                         ALPHANUMERIC  BY SPACES.
           SET WS-EOF-OK               TO FALSE.
           SET WS-EXIT-OK              TO FALSE.
           DISPLAY WS-COM-MENSAGEM
           END-DISPLAY.
       P100-INICIO-FIM.

       P200-PROCESSA.
           PERFORM P400-ABRE-ARQ       THRU P400-ABRE-ARQ-FIM.
           PERFORM P410-LE-REGISTRO    THRU P410-LE-REGISTRO-FIM
                   WITH TEST
                       BEFORE UNTIL    WS-EOF-OK
           PERFORM P420-FECHA-ARQ      THRU P420-FECHA-ARQ-FIM.
       P200-PROCESSA-FIM.

       P400-ABRE-ARQ.
      *VE SE O ARQUIVO EXISTE.
           OPEN INPUT TODOS-ALUNOS.
           IF NOT WS-FS-OK THEN
               PERFORM P800-ERRO        THRU P800-ERRO-FIM
           END-IF.
       P400-ABRE-ARQ-FIM.

       P410-LE-REGISTRO.
           READ TODOS-ALUNOS INTO WS-REG-TODOS
               AT END
                   SET WS-EOF-OK TO TRUE
               NOT AT END
                   PERFORM P430-LISTA-REGISTRO
                           THRU P430-LISTA-REGISTRO-FIM
           END-READ.
       P410-LE-REGISTRO-FIM.

       P420-FECHA-ARQ.
           CLOSE TODOS-ALUNOS.
       P420-FECHA-ARQ-FIM.

       P430-LISTA-REGISTRO.
           ADD 1 TO WS-CONTA-REG.
           DISPLAY 'REGISTRO:' FUNCTION TRIM (WS-CONTA-REG)
                   ' - ALUNO:' FUNCTION TRIM (WS-NM-ALUNO)
                   ' - MATERIA:' FUNCTION TRIM (WS-NM-MATERIA)
                   ' - MEDIA ALUNO:' FUNCTION TRIM (WS-MD-ALUNO)
                   ' - SITUACAO:' FUNCTION TRIM (WS-ST-APROVACAO)
           END-DISPLAY.
       P430-LISTA-REGISTRO-FIM.

       P800-ERRO.
           DISPLAY 'ERRO DE LEITURA. APLICACAO FINALIZADA.'
           END-DISPLAY.
           DISPLAY 'FILE STATUS: ' WS-FS-T
           END-DISPLAY.
           PERFORM P420-FECHA-ARQ THRU P420-FECHA-ARQ-FIM.
           PERFORM P900-FINALIZA  THRU P900-FINALIZA-FIM.
       P800-ERRO-FIM.

       P900-FINALIZA.
           DISPLAY 'TOTAL DE REGISTROS LIDOS:...' WS-CONTA-REG
           END-DISPLAY.
           DISPLAY 'FIM DO PROCESSAMENTO.'
           END-DISPLAY.
           GOBACK.
       P900-FINALIZA-FIM.

       END PROGRAM NTNOTLIS.
