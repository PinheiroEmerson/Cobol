      ******************************************************************
      * Author:    EMERSON PINHEIRO - EMAIL. TIO.EL@OUTLOOK.COM.
      * Date:      29/04/2022.
      * Purpose:   LISTAGEM DE CONTATOS.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADCONTL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTATOS
           ASSIGN TO 'D:\My Documents\Cobol\Modulo3\bin\CONTATOS.DAT'
           ORGANIZATION IS INDEXED
           ACCESS  MODE IS SEQUENTIAL
           RECORD KEY IS ID-CONTATO
           FILE  STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  CONTATOS.
           COPY FD_CONTT.

       WORKING-STORAGE SECTION.
       77  WS-FS                       PIC 99.
           88 WS-FS-OK                 VALUE 0.

       01  WS-REGISTRO                 PIC X(22) VALUE SPACES.
       01  FILLER REDEFINES WS-REGISTRO.
           03 WS-ID-CONTATO            PIC 9(02).
           03 WS-NM-CONTATO            PIC X(20).

       77  WS-EOF                      PIC X.
           88 WS-EOF-OK                VALUE 'S' FALSE 'N'.

       77  WS-CONTA-REG                PIC 9(04) VALUE ZEROS.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM P100-INICIO     THRU P100-INICIO-FIM.
           PERFORM P200-PROCESSA   THRU P200-PROCESSA-FIM.
           PERFORM P900-FINALIZA   THRU P900-FINALIZA-FIM.
       MAIN-PROCEDURE-FIM.

       P100-INICIO.
           DISPLAY 'INICIO DO PROCESSAMENTO.'
           END-DISPLAY.
           INITIALISE WS-FS WS-REGISTRO WS-CONTA-REG
               REPLACING NUMERIC       BY ZEROES
                         ALPHANUMERIC  BY SPACES.
           SET WS-EOF-OK               TO FALSE.
           DISPLAY '*** LISTAGEM DE CONTATOS***'
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
           OPEN INPUT CONTATOS.
           IF NOT WS-FS-OK THEN
               PERFORM P800-ERRO        THRU P800-ERRO-FIM
           END-IF.
       P400-ABRE-ARQ-FIM.

       P410-LE-REGISTRO.
           READ CONTATOS INTO WS-REGISTRO
               AT END
                   SET WS-EOF-OK TO TRUE
               NOT AT END
                   PERFORM P430-LISTA-REGISTRO
                           THRU P430-LISTA-REGISTRO-FIM
           END-READ.
       P410-LE-REGISTRO-FIM.

       P420-FECHA-ARQ.
           CLOSE CONTATOS.
       P420-FECHA-ARQ-FIM.

       P430-LISTA-REGISTRO.
           ADD 1 TO WS-CONTA-REG.
           DISPLAY 'REGISTRO: ' WS-CONTA-REG
                   ' - CONTATO ID: ' WS-ID-CONTATO
                   ' - CONTATO NOME: ' WS-NM-CONTATO
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

       END PROGRAM CADCONTL.
