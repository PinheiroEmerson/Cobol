      ******************************************************************
      * Author:    EMERSON PINHEIRO - EMAIL. TIO.EL@OUTLOOK.COM.
      * Date:      27/04/2022.
      * Purpose:   CONSULTA DE CONTATOS.
      * Update:    TRANSORMADO DE EXECUTAVEL EM MODULO.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADCONTC.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTATOS
           ASSIGN TO 'D:\My Documents\Cobol\Modulo3\bin\CONTATOS.DAT'
           ORGANIZATION IS INDEXED
           ACCESS  MODE IS RANDOM
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
           PERFORM P420-FECHA-ARQ  THRU P420-FECHA-ARQ-FIM.
           PERFORM P900-FINALIZA   THRU P900-FINALIZA-FIM.

       MAIN-PROCEDURE-FIM.

       P100-INICIO.
           DISPLAY 'INICIO DO PROCESSAMENTO.'
           END-DISPLAY.
           INITIALISE WS-FS WS-REGISTRO
               REPLACING NUMERIC       BY ZEROES
                         ALPHANUMERIC  BY SPACES.
           SET WS-EOF-OK               TO FALSE.
           SET WS-EXIT-OK              TO FALSE.

           DISPLAY WS-COM-MENSAGEM
           END-DISPLAY.
           PERFORM P400-ABRE-ARQ   THRU P400-ABRE-ARQ-FIM.
       P100-INICIO-FIM.

       P200-PROCESSA.
           PERFORM P430-MONTA-TELA     THRU P430-MONTA-TELA-FIM.
           PERFORM P410-LE-REGISTRO    THRU P410-LE-REGISTRO-FIM.
           DISPLAY 'TECLE: '
                   '<QUALQUER TECLA> CONSULTA OUTRO CONTATO OU'
                   '<S> PARA SAIR'
           END-DISPLAY.
           ACCEPT WS-EXIT
           END-ACCEPT.
       P200-PROCESSA-FIM.

       P400-ABRE-ARQ.
           OPEN INPUT CONTATOS.
      *VE SE O ARQUIVO EXISTE, ENCERRA O PROGRAMA.
           IF NOT WS-FS-OK THEN
               PERFORM P800-ERRO       THRU P800-ERRO-FIM
           END-IF.
       P400-ABRE-ARQ-FIM.

       P410-LE-REGISTRO.
           READ CONTATOS INTO WS-REGISTRO
               KEY IS ID-CONTATO
               INVALID KEY
                   PERFORM P450-REG-NAO-LOCALIZADO
                           THRU    P450-REG-NAO-LOCALIZADO-FIM
               NOT INVALID KEY
                   PERFORM P440-MOSTRA-REGISTRO
                           THRU P440-MOSTRA-REGISTRO
           END-READ.
       P410-LE-REGISTRO-FIM.

       P420-FECHA-ARQ.
           CLOSE CONTATOS.
       P420-FECHA-ARQ-FIM.

       P430-MONTA-TELA.
           DISPLAY 'INFORME O CODIGO DO CONTATO: '
           END-DISPLAY.
           ACCEPT ID-CONTATO
           END-ACCEPT.
       P430-MONTA-TELA-FIM.

       P440-MOSTRA-REGISTRO.
           DISPLAY 'OS DADO DO CONTATO SAO: '
           END-DISPLAY.
           DISPLAY 'ID DO CONTATO..: ' WS-ID-CONTATO
                   ' - NOME DO CONTATO: ' WS-NM-CONTATO
           END-DISPLAY.
       P440-MOSTRA-REGISTRO-FIM.

       P450-REG-NAO-LOCALIZADO.
           DISPLAY 'CONTATO NAO LOCALIZADO.'
           END-DISPLAY.
       P450-REG-NAO-LOCALIZADO-FIM.

       P800-ERRO.
           DISPLAY 'ERRO DE LEITURA. ARQUIVO NAO EXISTE.'
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

       END PROGRAM CADCONTC.
