      ******************************************************************
      * Author:    EMERSON PINHEIRO - EMAIL. TIO.EL@OUTLOOK.COM.
      * Date:      27/04/2022.
      * Purpose:   CADASTRO DE CONTATOS.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADCONTT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTATOS
           ASSIGN TO 'D:\My Documents\Cobol\Modulo3\bin\CONTATOS.TXT'
           ORGANIZATION IS SEQUENTIAL
           ACCESS  MODE IS SEQUENTIAL
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

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM P100-INICIO     THRU P100-INICIO-FIM.
           PERFORM P200-PROCESSA   THRU P200-PROCESSA-FIM
                   UNTIL WS-EXIT-OK.
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

           DISPLAY '*** CADASTRO DE CONTATOS***'
           END-DISPLAY.
           PERFORM P400-ABRE-ARQ THRU P400-ABRE-ARQ-FIM.
       P100-INICIO-FIM.

       P200-PROCESSA.
           DISPLAY 'PARA REGISTRAR UM CONTATO, INFORME:'
           END-DISPLAY.
           DISPLAY 'UM NUMERO PARA ID: '
           END-DISPLAY.
           ACCEPT WS-ID-CONTATO
           END-ACCEPT.
           DISPLAY 'UM NOME PARA O CONTATO: '
           END-DISPLAY.
           ACCEPT WS-NM-CONTATO
           END-ACCEPT.
           PERFORM P410-GRAVA-REGISTRO THRU
                   P410-GRAVA-REGISTRO-FIM
           DISPLAY 'TECLE: '
                   '<QUALQUER TECLA> PARA CONTINUAR, OU'
                   '<S> PARA SAIR'
           END-DISPLAY.
           ACCEPT WS-EXIT
           END-ACCEPT.

       P200-PROCESSA-FIM.

       P400-ABRE-ARQ.
           OPEN EXTEND CONTATOS.
      *VE SE O ARQUIVO EXISTE. SE NAO EXISTE (35) CRIA ARQUIVO.
           IF NOT WS-FS-OK THEN
               OPEN OUTPUT CONTATOS
           END-IF.
       P400-ABRE-ARQ-FIM.

       P410-GRAVA-REGISTRO.
           MOVE WS-ID-CONTATO TO ID-CONTATO.
           MOVE WS-NM-CONTATO TO NM-CONTATO.

           WRITE REG-CONTATOS
           END-WRITE.
           DISPLAY 'CONTATO GRAVADO COM SUCESSO.'
           END-DISPLAY.
       P410-GRAVA-REGISTRO-FIM.

       P420-FECHA-ARQ.
           CLOSE CONTATOS.
       P420-FECHA-ARQ-FIM.

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

       END PROGRAM CADCONTT.
