      ******************************************************************
      * Author:    EMERSON PINHEIRO - EMAIL. TIO.EL@OUTLOOK.COM.
      * Date:      04/05/2022.
      * Purpose:   CONSULTA DE MATERIAS.
      * Update:    TRANSORMADO DE EXECUTAVEL EM MODULO.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NTALUCON.

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

       01  WS-MATERIA                 PIC X(32) VALUE SPACES.
       01  FILLER REDEFINES WS-MATERIA.
           03 WS-ID-MATERIA            PIC 9(03).
           03 WS-NM-MATERIA            PIC X(20).
           03 WS-TL-MATERIA            PIC X(09).

       77  WS-EOF                      PIC X.
           88 WS-EOF-OK                VALUE 'S' FALSE 'N'.

       77  WS-EXIT                     PIC X.
           88 WS-EXIT-OK               VALUE 'S' FALSE 'N'.

       77  WS-FS                       PIC 99.
           88 WS-FS-OK                 VALUE 0.

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
           PERFORM P430-MONTA-TELA     THRU P430-MONTA-TELA-FIM.
           PERFORM P410-LE-MATERIA    THRU P410-LE-MATERIA-FIM.
           DISPLAY 'TECLE: '
                   '<QUALQUER TECLA> CONSULTA OUTRO MATERIA OU'
                   ' <S> PARA SAIR'
           END-DISPLAY.
           ACCEPT WS-EXIT
           END-ACCEPT.
       P200-PROCESSA-FIM.

       P400-ABRE-ARQ.
           OPEN INPUT MATERIAS.
      *VE SE O ARQUIVO EXISTE, ENCERRA O PROGRAMA.
           IF NOT WS-FS-OK THEN
               PERFORM P800-ERRO       THRU P800-ERRO-FIM
           END-IF.
       P400-ABRE-ARQ-FIM.

       P410-LE-MATERIA.
           READ MATERIAS INTO WS-MATERIA
               KEY IS ID-MATERIA
               INVALID KEY
                   PERFORM P450-REG-NAO-LOCALIZADO
                           THRU    P450-REG-NAO-LOCALIZADO-FIM
               NOT INVALID KEY
                   PERFORM P440-MOSTRA-MATERIA
                           THRU P440-MOSTRA-MATERIA
           END-READ.
       P410-LE-MATERIA-FIM.

       P420-FECHA-ARQ.
           CLOSE MATERIAS.
       P420-FECHA-ARQ-FIM.

       P430-MONTA-TELA.
           DISPLAY 'INFORME O CODIGO DO MATERIA: '
           END-DISPLAY.
           ACCEPT ID-MATERIA
           END-ACCEPT.
       P430-MONTA-TELA-FIM.

       P440-MOSTRA-MATERIA.
           DISPLAY 'OS DADO DO MATERIA SAO: '
           END-DISPLAY.
           DISPLAY 'ID DO MATERIA..: ' WS-ID-MATERIA
                   ' - NOME DO MATERIA: ' WS-NM-MATERIA
           END-DISPLAY.
       P440-MOSTRA-MATERIA-FIM.

       P450-REG-NAO-LOCALIZADO.
           DISPLAY 'MATERIA NAO LOCALIZADO.'
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

       END PROGRAM NTALUCON.
