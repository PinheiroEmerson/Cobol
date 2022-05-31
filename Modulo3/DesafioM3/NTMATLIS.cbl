      ******************************************************************
      * Author:    EMERSON PINHEIRO - EMAIL. TIO.EL@OUTLOOK.COM.
      * Date:      29/04/2022.
      * Purpose:   LISTAGEM DE MATERIAS.
      * Update:    TRANSFORMADO DE EXECUTAVEL EM MODULO.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NTMATLIS.

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
           ACCESS  MODE IS SEQUENTIAL
           RECORD KEY IS ID-MATERIA
           FILE  STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  MATERIAS.
           COPY CFPK0002.

       WORKING-STORAGE SECTION.

       01  WS-MATERIA                   PIC X(27) VALUE SPACES.
       01  FILLER REDEFINES WS-MATERIA.
           03 WS-ID-MATERIA             PIC 9(03).
           03 WS-NM-MATERIA             PIC X(20).
           03 WS-NT-APROVACAO           PIC 9(02)V99.

       77  WS-EOF                      PIC X.
           88 WS-EOF-OK                VALUE 'S' FALSE 'N'.

       77  WS-CONTA-REG                PIC 9(04) VALUE ZEROS.

       77  WS-FS                       PIC 99.
           88 WS-FS-OK                 VALUE 0.


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
           INITIALISE WS-FS WS-MATERIA WS-CONTA-REG
               REPLACING NUMERIC       BY ZEROES
                         ALPHANUMERIC  BY SPACES.
           SET WS-EOF-OK               TO FALSE.
           DISPLAY WS-COM-MENSAGEM
           END-DISPLAY.
       P100-INICIO-FIM.

       P200-PROCESSA.
           PERFORM P400-ABRE-ARQ       THRU P400-ABRE-ARQ-FIM.
           PERFORM P410-LE-REGISTRO    THRU P410-LE-REGISTRO-FIM
                   WITH TEST
                       BEFORE UNTIL WS-EOF-OK
           PERFORM P420-FECHA-ARQ      THRU P420-FECHA-ARQ-FIM.
       P200-PROCESSA-FIM.

       P400-ABRE-ARQ.
      *VE SE O ARQUIVO EXISTE.
           OPEN INPUT MATERIAS.
           IF NOT WS-FS-OK THEN
               PERFORM P800-ERRO        THRU P800-ERRO-FIM
           END-IF.
       P400-ABRE-ARQ-FIM.

       P410-LE-REGISTRO.
           READ MATERIAS INTO WS-MATERIA
               AT END
                   SET WS-EOF-OK TO TRUE
               NOT AT END
                   PERFORM P430-LISTA-REGISTRO
                           THRU P430-LISTA-REGISTRO-FIM
           END-READ.
       P410-LE-REGISTRO-FIM.

       P420-FECHA-ARQ.
           CLOSE MATERIAS.
       P420-FECHA-ARQ-FIM.

       P430-LISTA-REGISTRO.
           ADD 1 TO WS-CONTA-REG.
           DISPLAY 'REGISTRO.: ' WS-CONTA-REG
                   ' - MATERIA ID.......: ' WS-ID-MATERIA
                   ' - MATERIA NOME.....: ' WS-NM-MATERIA
                   ' - NOTA DE APROVACAO: ' WS-NT-APROVACAO
           END-DISPLAY.
       P430-LISTA-REGISTRO-FIM.

       P800-ERRO.
           DISPLAY 'ERRO DE LEITURA. APLICACAO FINALIZADA.'
           END-DISPLAY.
           DISPLAY 'FILE STATUS: ' WS-FS
           END-DISPLAY.
           PERFORM P420-FECHA-ARQ THRU P420-FECHA-ARQ-FIM.
           PERFORM P900-FINALIZA  THRU P900-FINALIZA-FIM.
       P800-ERRO-FIM.

       P900-FINALIZA.
           DISPLAY 'FIM DO PROCESSAMENTO.'
           END-DISPLAY.
           GOBACK.
       P900-FINALIZA-FIM.

       END PROGRAM NTMATLIS.
