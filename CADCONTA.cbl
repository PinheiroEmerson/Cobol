      ******************************************************************
      * Author:    EMERSON PINHEIRO - EMAIL. TIO.EL@OUTLOOK.COM.
      * Date:      27/04/2022.
      * Purpose:   CONSULTA DE CONTATOS.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADCONTA.

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

       77  WS-AUX-ALTERA               PIC X.
           88 WS-AUX-ALTERA-OK         VALUE 'S' FALSE 'N'.

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
           SET WS-AUX-ALTERA-OK        TO FALSE.

           DISPLAY '*** ALTERACAO DE CONTATOS***'
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
           OPEN I-O CONTATOS.
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
           DISPLAY 'DESEJA ALTERAR O NOME DO CONTATO?'
           END-DISPLAY.
           ACCEPT WS-AUX-ALTERA
           END-ACCEPT.
           IF WS-AUX-ALTERA-OK THEN
               PERFORM P460-ALTERA-REGISTRO
                       THRU P460-ALTERA-REGISTRO-FIM
           END-IF.
       P440-MOSTRA-REGISTRO-FIM.

       P450-REG-NAO-LOCALIZADO.
           DISPLAY 'CONTATO NAO LOCALIZADO.'
                   'TENTE UM CODIGO VALIDO.'
           END-DISPLAY.
       P450-REG-NAO-LOCALIZADO-FIM.

       P460-ALTERA-REGISTRO.
           DISPLAY 'DIGIGTE O NOVO NOME DO CONTATO: '
           END-DISPLAY.
           ACCEPT NM-CONTATO
           END-ACCEPT.
           REWRITE REG-CONTATOS
               INVALID KEY
                   PERFORM P800-ERRO   THRU P800-ERRO-FIM
               NOT INVALID KEY
                   DISPLAY 'CONTATO ALTERADO COM SUCESSO.'
                   END-DISPLAY
           END-REWRITE.
       P460-ALTERA-REGISTRO-FIM.

       P800-ERRO.
           DISPLAY 'FILE STATUS: ' WS-FS
           END-DISPLAY.
           IF WS-FS = 35
               DISPLAY 'ERRO. NAO ACHOU O ARQUIVO.'
               END-DISPLAY
           ELSE
               DISPLAY 'NAO FOI POSSIVEL ATUALIZAR O REGISTRO.'
               END-DISPLAY
           END-IF
           PERFORM P900-FINALIZA  THRU P900-FINALIZA-FIM.
       P800-ERRO-FIM.


       P900-FINALIZA.
           PERFORM P420-FECHA-ARQ THRU P420-FECHA-ARQ-FIM.
           DISPLAY 'FIM DO PROCESSAMENTO.'
           END-DISPLAY.
           GOBACK.
       P900-FINALIZA-FIM.

       END PROGRAM CADCONTA.
