      ******************************************************************
      * Author:    EMERSON PINHEIRO - TIO.EL@OUTLOOK.COM.
      * Date:      16/05/2022
      * Purpose:   DESAFIO MODULO 2 - INCLUSAO APROVADOS DO ALUNO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NTNOTINC.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ALUNOS
           ASSIGN TO
               'D:\My Documents\Cobol\Modulo3\DesafioM3\ALUNOS.DAT'
           ORGANIZATION IS INDEXED
           ACCESS  MODE IS RANDOM
           RECORD KEY IS ID-ALUNO OF REG-ALUNO
           FILE  STATUS IS WS-FS-A.

           SELECT MATERIAS
           ASSIGN TO
               'D:\My Documents\Cobol\Modulo3\DesafioM3\MATERIAS.DAT'
           ORGANIZATION IS INDEXED
           ACCESS  MODE IS RANDOM
           RECORD KEY IS ID-MATERIA OF REG-MATERIA
           FILE  STATUS IS WS-FS-M.

           SELECT APROVADOS
           ASSIGN TO
               'D:\My Documents\Cobol\Modulo3\DesafioM3\ALUAPROV.DAT'
           ORGANIZATION IS INDEXED
           ACCESS  MODE IS DYNAMIC
           RECORD  KEY IS ID-ALUNO OF REG-INTER
           FILE  STATUS IS WS-FS-N.

           SELECT TODOS-ALUNOS
           ASSIGN TO
               'D:\My Documents\Cobol\Modulo3\DesafioM3\ALUTODOS.DAT'
           ORGANIZATION IS INDEXED
           ACCESS  MODE IS DYNAMIC
           RECORD  KEY IS ID-ALUNO OF REG-TODOS
           FILE  STATUS IS WS-FS-T.

       DATA DIVISION.
       FILE SECTION.
       FD  ALUNOS.
           COPY CFPK0001.

       FD  MATERIAS.
           COPY CFPK0002.

       FD  APROVADOS.
           COPY CFPK0003.


       FD  TODOS-ALUNOS.
           COPY CFPK0004.

       WORKING-STORAGE SECTION.

       01  WS-ALUNO.
           03 WS-ID-ALUNO            PIC 9(03).
           03 WS-NM-ALUNO            PIC X(20).
           03 WS-TL-ALUNO.
               05 WS-FONEAREA        PIC 9(02).
               05 WS-FONENUMERO      PIC 9(09).

       01  WS-MATERIA.
           03 WS-ID-MATERIA          PIC 9(03).
           03 WS-NM-MATERIA          PIC X(20).
           03 WS-NT-APROVACAO        PIC 9(02)V99.

      *PARA ECONOMIZAR MEMORIA DE PROCESSAMENTO
      *SERÁ UTILIZADO O MESMO WS TANTO NA GRAVAÇÃO DO
      *ALUNO APROVADO COMO DO ALUNO REPROVADO.
       01  WS-REG-INTER.
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

       01  WS-RESULTADO              PIC X(10).
           88 WS-RESULTADO-F         VALUE 'APROVADO' FALSE 'REPROVADO'.

       01  WS-APURACAO.
           05 WS-NOTA-1              PIC 9(02)V99.
           05 WS-NOTA-2              PIC 9(02)V99.
           05 WS-NOTA-3              PIC 9(02)V99.
           05 WS-NOTA-4              PIC 9(02)V99.
           05 WS-NOTA-MD             PIC 9(02)V99.

      *CONTROLE ARQUIVO ALUNOS
       77  WS-EOF-A                  PIC X.
           88 WS-EOF-OK              VALUE 'S' FALSE 'N'.
       77  WS-FS-A                   PIC 99.
           88 WS-FS-OK               VALUE 0.

      *CONTROLE ARQUIVO MATERIAS
       77  WS-EOF-M                  PIC X.
           88 WS-EOF-OK              VALUE 'S' FALSE 'N'.
       77  WS-FS-M                   PIC 99.
           88 WS-FS-OK               VALUE 0.

      *CONTROLE ARQUIVO APROVADOS
       77  WS-EOF-N                  PIC X.
           88 WS-EOF-OK              VALUE 'S' FALSE 'N'.
       77  WS-FS-N                   PIC 99.
           88 WS-FS-OK               VALUE 0.

      *CONTROLE ARQUIVO TODOS-ALUNOS
       77  WS-EOF-T                  PIC X.
           88 WS-EOF-OK              VALUE 'S' FALSE 'N'.
       77  WS-FS-T                   PIC 99.
           88 WS-FS-OK               VALUE 0.

      *CONTROLES AUXILIARES / LOOPING
       77  WS-EXIT                   PIC X.
           88 WS-EXIT-OK             VALUE 'S' FALSE 'N'.

       77  WS-EXIT-ALUNO             PIC X.
           88 WS-EXIT-ALUNO-OK       VALUE 'S' FALSE 'N'.

       77  WS-EXIT-MATERIA           PIC X.
           88 WS-EXIT-MATERIA-OK     VALUE 'S' FALSE 'N'.

       77  WS-VALIDA-NOTA            PIC X.
           88 WS-VALIDA-NOTA-OK      VALUE 'S' FALSE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM P100-INICIO THRU P100-INICIO-FIM.
           PERFORM P200-PROCESSA THRU P200-PROCESSA-FIM
                   UNTIL WS-EXIT-OK.
           PERFORM P900-FINALIZA THRU P900-FINALIZA-FIM.

       P100-INICIO.
           PERFORM P850-LIMPA-VARIAVEIS           THRU
                   P850-LIMPA-VARIAVEIS-FIM.

           PERFORM P410-ABRE-ARQUIVO-ALUNO    THRU
                   P410-ABRE-ARQUIVO-ALUNO-FIM.

           PERFORM P450-ABRE-ARQUIVO-MATERIA  THRU
                   P450-ABRE-ARQUIVO-MATERIA-FIM.

           PERFORM P480-ABRE-ARQUIVO-APROVADO     THRU
                   P480-ABRE-ARQUIVO-APROVADO-FIM.

           PERFORM P490-ABRE-ARQUIVO-TODOS        THRU
                   P490-ABRE-ARQUIVO-TODOS-FIM.
       P100-INICIO-FIM.

       P200-PROCESSA.

           PERFORM P600-MONTA-TELA-ALUNO              THRU
                   P600-MONTA-TELA-ALUNO-FIM.

           PERFORM P420-LE-ARQUIVO-ALUNO              THRU
                   P420-LE-ARQUIVO-ALUNO-FIM
                   WITH TEST
                       AFTER
                       UNTIL WS-EXIT-ALUNO-OK.

           PERFORM P605-MONTA-TELA-MATERIA            THRU
                   P605-MONTA-TELA-MATERIA-FIM.

           PERFORM P450-LE-ARQUIVO-MATERIA            THRU
                   P450-LE-ARQUIVO-MATERIA-FIM
                   WITH TEST
                       AFTER
                       UNTIL WS-EXIT-MATERIA-OK.

           PERFORM P610-MONTA-TELA-APROVADOS          THRU
                   P610-MONTA-TELA-APROVADOS-FIM.

           PERFORM P400-CALC-MEDIA                    THRU
                   P400-CALC-MEDIA-FIM.

           PERFORM P620-MOSTRA-RESULTADO              THRU
                   P620-MOSTRA-RESULTADO-FIM.

           PERFORM P560-MOVE-CONTEUDO                 THRU
                   P560-MOVE-CONTEUDO-FIM

           EVALUATE TRUE
      *GRAVA NO ARQUIVO DE APROVADOS
      *GRAVA NO ARQUIVO DE TODOS OS ALUNOS
               WHEN WS-RESULTADO-F
                   PERFORM P550-GRAVA-REGISTRO        THRU
                           P550-GRAVA-REGISTRO-FIM

                   PERFORM P570-GRAVA-REGISTRO-TODOS  THRU
                           P570-GRAVA-REGISTRO-TODOS-FIM
               WHEN OTHER
      *GRAVA NO ARQUIVO DE TODOS OS ALUNOS
                   PERFORM P570-GRAVA-REGISTRO-TODOS  THRU
                           P570-GRAVA-REGISTRO-TODOS-FIM
           END-EVALUATE.

           PERFORM P850-LIMPA-VARIAVEIS               THRU
                   P850-LIMPA-VARIAVEIS-FIM.
           DISPLAY 'TECLE: '
                   '<QUALQUER TECLA> CALCULAR NOVA MEDIA OU'
                   ' <S> PARA SAIR'
           END-DISPLAY.
           ACCEPT WS-EXIT
           END-ACCEPT.
       P200-PROCESSA-FIM.

       P400-CALC-MEDIA.

           ADD    WS-NOTA-1 WS-NOTA-2 WS-NOTA-3 WS-NOTA-4
                  TO WS-NOTA-MD OF WS-APURACAO
                  ON SIZE ERROR
                  PERFORM P830-ERRO-CALC THRU P830-ERRO-CALC-FIM
           END-ADD.

           DIVIDE WS-NOTA-MD OF WS-APURACAO BY 4
                  GIVING WS-NOTA-MD OF WS-APURACAO
                  ON SIZE ERROR
                  PERFORM P830-ERRO-CALC THRU P830-ERRO-CALC-FIM
           END-DIVIDE.

           EVALUATE WS-NOTA-MD OF WS-APURACAO
               WHEN GREATER THAN OR EQUALS TO
                   WS-NT-APROVACAO OF WS-MATERIA
                   SET WS-RESULTADO-F TO TRUE
                   MOVE 'APROVADO' TO WS-ST-APROVACAO  OF WS-REG-INTER
               WHEN OTHER
                   SET WS-RESULTADO-F TO FALSE
                   MOVE 'REPROVADO' TO WS-ST-APROVACAO OF WS-REG-INTER
           END-EVALUATE.

       P400-CALC-MEDIA-FIM.

       P410-ABRE-ARQUIVO-ALUNO.
           OPEN INPUT ALUNOS.
           IF NOT WS-FS-OK OF WS-FS-A THEN
               PERFORM P810-ERRO-ARQ-ALUNO  THRU
                       P810-ERRO-ARQ-ALUNO-FIM
           END-IF.
       P410-ABRE-ARQUIVO-ALUNO-FIM.

       P420-LE-ARQUIVO-ALUNO.
           READ ALUNOS INTO WS-ALUNO
               KEY IS ID-ALUNO OF REG-ALUNO
               INVALID KEY
                   PERFORM P430-ALUNO-NAO-LOCALIZADO
                           THRU P430-ALUNO-NAO-LOCALIZADO-FIM
                   PERFORM P600-MONTA-TELA-ALUNO
                           THRU P600-MONTA-TELA-ALUNO-FIM

               NOT INVALID KEY
                   PERFORM P440-MOSTRA-ALUNO
                           THRU P440-MOSTRA-ALUNO-FIM
                   SET     WS-EXIT-ALUNO-OK TO TRUE
           END-READ.
       P420-LE-ARQUIVO-ALUNO-FIM.

       P430-ALUNO-NAO-LOCALIZADO.
           DISPLAY 'ALUNO NAO LOCALIZADO.'
           END-DISPLAY.
       P430-ALUNO-NAO-LOCALIZADO-FIM.

       P440-MOSTRA-ALUNO.
           DISPLAY 'NOME DO ALUNO: ' WS-NM-ALUNO     OF WS-ALUNO
           END-DISPLAY.
       P440-MOSTRA-ALUNO-FIM.

       P445-MOSTRA-MATERIA.
           DISPLAY 'MATERIA......: ' WS-NM-MATERIA   OF WS-MATERIA
           END-DISPLAY.
       P445-MOSTRA-MATERIA-FIM.

       P447-MOSTRA-MEDIA.
           DISPLAY 'NOTA FINAL...: ' WS-MD-ALUNO OF WS-REG-INTER
           END-DISPLAY.
       P447-MOSTRA-MEDIA.

       P450-ABRE-ARQUIVO-MATERIA.
           OPEN INPUT MATERIAS.
           IF NOT WS-FS-OK OF WS-FS-M THEN
               PERFORM P820-ERRO-ARQ-MATERIA
                       THRU P820-ERRO-ARQ-MATERIA-FIM
           END-IF.
       P450-ABRE-ARQUIVO-MATERIA-FIM.

       P450-LE-ARQUIVO-MATERIA.
           READ MATERIAS INTO WS-MATERIA
               KEY IS ID-MATERIA OF REG-MATERIA
               INVALID KEY
                   PERFORM P460-MATERIA-NAO-LOCALIZADO
                           THRU P460-MATERIA-NAO-LOCALIZADO-FIM
                   PERFORM P605-MONTA-TELA-MATERIA
                           THRU P605-MONTA-TELA-MATERIA-FIM

               NOT INVALID KEY
                   PERFORM P445-MOSTRA-MATERIA
                           THRU P445-MOSTRA-MATERIA-FIM
                   SET     WS-EXIT-MATERIA-OK TO TRUE
           END-READ.
       P450-LE-ARQUIVO-MATERIA-FIM.

       P460-MATERIA-NAO-LOCALIZADO.
           DISPLAY 'MATERIA NAO LOCALIZADA.'
           END-DISPLAY.
       P460-MATERIA-NAO-LOCALIZADO-FIM.

       P470-FECHA-ARQUIVOS.
           CLOSE ALUNOS.
           CLOSE MATERIAS.
           CLOSE APROVADOS.
           CLOSE TODOS-ALUNOS.
       P470-FECHA-ARQUIVOS-FIM.

       P480-ABRE-ARQUIVO-APROVADO.
           OPEN I-O APROVADOS.
      *VE SE O ARQUIVO EXISTE. SE NAO EXISTE (35) CRIA ARQUIVO.
           IF NOT WS-FS-OK OF WS-FS-N THEN
               OPEN OUTPUT APROVADOS
           END-IF.
       P480-ABRE-ARQUIVO-APROVADO-FIM.

       P490-ABRE-ARQUIVO-TODOS.
           OPEN I-O TODOS-ALUNOS.
      *VE SE O ARQUIVO EXISTE. SE NAO EXISTE (35) CRIA ARQUIVO.
           IF NOT WS-FS-OK OF WS-FS-T THEN
               OPEN OUTPUT TODOS-ALUNOS
           END-IF.
       P490-ABRE-ARQUIVO-TODOS-FIM.

       P500-VALIDA-N1.
           ACCEPT WS-NOTA-1
           END-ACCEPT.
           IF WS-NOTA-1 NOT IS LESS THAN OR EQUAL TO 10 THEN
               DISPLAY 'A NOTA DEVE SER ENTRE 0 E 10'
               END-DISPLAY
               DISPLAY 'DIGITE NOVAMENTE'
               END-DISPLAY
           ELSE
               SET WS-VALIDA-NOTA-OK TO TRUE
           END-IF.
       P500-VALIDA-N1-FIM.

       P510-VALIDA-N2.
           ACCEPT WS-NOTA-2
           END-ACCEPT.
           IF WS-NOTA-2 NOT IS LESS THAN OR EQUAL TO 10 THEN
               DISPLAY 'A NOTA DEVE SER ENTRE 0 E 10'
               END-DISPLAY
               DISPLAY 'DIGITE NOVAMENTE'
               END-DISPLAY
           ELSE
               SET WS-VALIDA-NOTA-OK TO TRUE
           END-IF.
       P510-VALIDA-N2-FIM.

       P520-VALIDA-N3.
           ACCEPT WS-NOTA-3
           END-ACCEPT.
           IF WS-NOTA-3 NOT IS LESS THAN OR EQUAL TO 10 THEN
               DISPLAY 'A NOTA DEVE SER ENTRE 0 E 10'
               END-DISPLAY
               DISPLAY 'DIGITE NOVAMENTE'
               END-DISPLAY
           ELSE
               SET WS-VALIDA-NOTA-OK TO TRUE
           END-IF.
       P520-VALIDA-N3-FIM.

       P530-VALIDA-N4.
           ACCEPT WS-NOTA-4
           END-ACCEPT.
           IF WS-NOTA-4 NOT IS LESS THAN OR EQUAL TO 10 THEN
               DISPLAY 'A NOTA DEVE SER ENTRE 0 E 10'
               END-DISPLAY
               DISPLAY 'DIGITE NOVAMENTE'
               END-DISPLAY
           ELSE
               SET WS-VALIDA-NOTA-OK TO TRUE
           END-IF.
       P530-VALIDA-N4-FIM.

       P550-GRAVA-REGISTRO.
           MOVE WS-REG-INTER TO REG-INTER.
           WRITE REG-INTER
               INVALID KEY
                   PERFORM P590-INFORMA-GRAVACAO-ERRO
                           THRU P590-INFORMA-GRAVACAO-ERRO-FIM
               NOT INVALID KEY
                   PERFORM P580-INFORMA-GRAVACAO
                           THRU P580-INFORMA-GRAVACAO-FIM
           END-WRITE.
       P550-GRAVA-REGISTRO-FIM.

       P560-MOVE-CONTEUDO.
           MOVE WS-ID-ALUNO        OF WS-ALUNO
                TO WS-ID-ALUNO     OF WS-REG-INTER.
           MOVE WS-NM-ALUNO        OF WS-ALUNO
                TO WS-NM-ALUNO     OF WS-REG-INTER.
           MOVE WS-TL-ALUNO        OF WS-ALUNO
                TO WS-TL-ALUNO     OF WS-REG-INTER.
           MOVE WS-ID-MATERIA      OF WS-MATERIA
                TO WS-ID-MATERIA   OF WS-REG-INTER.
           MOVE WS-NM-MATERIA      OF WS-MATERIA
                TO WS-NM-MATERIA   OF WS-REG-INTER.
           MOVE WS-NT-APROVACAO    OF WS-MATERIA
                TO WS-NT-APROVACAO OF WS-REG-INTER.
           MOVE WS-NOTA-MD         OF WS-APURACAO
                TO WS-MD-ALUNO     OF WS-REG-INTER.
       P560-MOVE-CONTEUDO-FIM.

       P570-GRAVA-REGISTRO-TODOS.
           MOVE WS-REG-INTER TO REG-TODOS.
           WRITE REG-TODOS
               INVALID KEY
                   PERFORM P590-INFORMA-GRAVACAO-ERRO
                           THRU P590-INFORMA-GRAVACAO-ERRO-FIM
               NOT INVALID KEY
                   PERFORM P580-INFORMA-GRAVACAO
                           THRU P580-INFORMA-GRAVACAO-FIM
           END-WRITE.
       P570-GRAVA-REGISTRO-TODOS-FIM.

       P580-INFORMA-GRAVACAO.
           DISPLAY 'REGISTRO SALVO COM SUCESSO.'
           END-DISPLAY.
       P580-INFORMA-GRAVACAO-FIM.

       P590-INFORMA-GRAVACAO-ERRO.
           DISPLAY 'REGISTRO JAH EXISTENTE EM APROVADOS OU TODOS.'
           END-DISPLAY.
       P590-INFORMA-GRAVACAO-ERRO-FIM.

       P600-MONTA-TELA-ALUNO.
           DISPLAY 'INFORME O CODIGO DO ALUNO..: '
           END-DISPLAY.
           ACCEPT  ID-ALUNO    OF REG-ALUNO
           END-ACCEPT.
       P600-MONTA-TELA-ALUNO-FIM.

       P605-MONTA-TELA-MATERIA.
           DISPLAY 'INFORME O CODIGO DA MATERIA............: '
           END-DISPLAY.
           ACCEPT  ID-MATERIA  OF REG-MATERIA
           END-ACCEPT.
       P605-MONTA-TELA-MATERIA-FIM.

       P610-MONTA-TELA-APROVADOS.
           DISPLAY 'ENTRE COM AS NOTAS DE ' WS-NM-MATERIA OF WS-MATERIA
           END-DISPLAY.
           DISPLAY 'PRIMEIRA NOTA: '
           END-DISPLAY.

           SET WS-VALIDA-NOTA-OK TO FALSE.
           PERFORM P500-VALIDA-N1 THRU P500-VALIDA-N1-FIM
                   UNTIL WS-VALIDA-NOTA-OK.
           DISPLAY 'SEGUNDA NOTA: '
           END-DISPLAY.

           SET WS-VALIDA-NOTA-OK TO FALSE.
           PERFORM P510-VALIDA-N2 THRU P510-VALIDA-N2-FIM
                   UNTIL WS-VALIDA-NOTA-OK.
           DISPLAY 'TERCIERA NOTA: '
           END-DISPLAY.

           SET WS-VALIDA-NOTA-OK TO FALSE.
           PERFORM P520-VALIDA-N3 THRU P520-VALIDA-N3-FIM
                   UNTIL WS-VALIDA-NOTA-OK.
           DISPLAY 'QUARTA NOTA: '
           END-DISPLAY.

           SET WS-VALIDA-NOTA-OK TO FALSE.
           PERFORM P530-VALIDA-N4 THRU P530-VALIDA-N4-FIM
                   UNTIL WS-VALIDA-NOTA-OK.
       P610-MONTA-TELA-APROVADOS-FIM.

       P620-MOSTRA-RESULTADO.
           DISPLAY '*** RESULTADO DO PROCESSAMENTO ***'
           END-DISPLAY.
           DISPLAY 'NOME DO ALUNO.....: ' WS-NM-ALUNO     OF WS-ALUNO
           END-DISPLAY.
           DISPLAY 'MATERIA...........: ' WS-NM-MATERIA   OF WS-MATERIA
           END-DISPLAY.
           DISPLAY 'MEDIA OBTIDA......: ' WS-NOTA-MD      OF WS-APURACAO
           END-DISPLAY.
           DISPLAY 'APROVADO/REPROVADO? ' WS-RESULTADO
           END-DISPLAY.
       P620-MOSTRA-RESULTADO-FIM.

       P810-ERRO-ARQ-ALUNO.
           DISPLAY 'ERRO DE LEITURA.'
           END-DISPLAY.
           DISPLAY 'FILE STATUS: ' WS-FS-A
           END-DISPLAY.
       P810-ERRO-ARQ-ALUNO-FIM.

       P820-ERRO-ARQ-MATERIA.
           DISPLAY 'ERRO DE LEITURA.'
           END-DISPLAY.
           DISPLAY 'FILE STATUS: ' WS-FS-M
           END-DISPLAY.
       P820-ERRO-ARQ-MATERIA-FIM.

       P830-ERRO-CALC.
           DISPLAY 'DADOS INCORRETOS OU REGISTRO NAO ENCONTRADO'
           END-DISPLAY.
       P830-ERRO-CALC-FIM.

       P850-LIMPA-VARIAVEIS.
           INITIALISE  WS-ALUNO  WS-APURACAO WS-MATERIA
                       WS-REG-INTER
           REPLACING   ALPHABETIC   BY SPACES
                       NUMERIC      BY ZERO.
           SET WS-EXIT-OK           TO FALSE.
           SET WS-EXIT-ALUNO-OK     TO FALSE.
           SET WS-EXIT-MATERIA-OK   TO FALSE.
           SET WS-VALIDA-NOTA-OK    TO FALSE.
           SET WS-RESULTADO-F       TO FALSE.
       P850-LIMPA-VARIAVEIS-FIM.

       P900-FINALIZA.
           PERFORM P470-FECHA-ARQUIVOS THRU
                   P470-FECHA-ARQUIVOS-FIM.
           DISPLAY 'FIM DO PROCESSAMENTO.'
           END-DISPLAY.
           GOBACK.
       P900-FINALIZA-FIM.

       END PROGRAM NTNOTINC.
