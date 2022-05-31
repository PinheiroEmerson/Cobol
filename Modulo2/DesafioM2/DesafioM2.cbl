      ******************************************************************
      * Author:    EMERSON PINHEIRO - TIO.EL@OUTLOOK.COM.
      * Date:      20/4/2022
      * Purpose:   DESAFIO MODULO 2
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIOM2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-DATA.
           05 WS-ANO               PIC 9(04).
           05 WS-MES               PIC 9(02).
           05 WS-DIA               PIC 9(02).

       01  WS-HORA.
           05 WS-HORAS             PIC 9(02).
           05 WS-MINUTOS           PIC 9(02).
           05 WS-SEGUNDOS          PIC 9(02).

       01  WS-NOTAS.
           05 WS-NOTA-1            PIC 9(03)V9.
           05 WS-NOTA-2            PIC 9(03)V9.
           05 WS-NOTA-3            PIC 9(03)V9.
           05 WS-NOTA-4            PIC 9(03)V9.
           05 WS-NOTA-MEDIA        PIC 9(03)V9.

       01  WS-DADOS-ALUNO.
           05 WS-NOME-ALUNO        PIC X(30).
           05 WS-NOME-DISCIPLINA   PIC X(20).

       01  WS-RESULTADO            PIC X(10).
           88 WS-APROVADO          VALUE 'APROVADO' FALSE 'REPROVADO'.

       01  WS-CONTINUA-CALC        PIC X(01).
           88 WS-CONTINUA          VALUE SPACE.

       SCREEN SECTION.

       01  TELA-CABECALHO.
           05 LINE 01 COLUMN 35.
           05 WS-TITULO-1   PIC X(27) VALUE
                            '---ESCOLA APRENDA COBOL---'.

           05 LINE 02 COLUMN 35.
           05 WS-TITULO-2   PIC X(29)   VALUE
                            '---SISTEMA MEDIA DO ALUNO---'.

           05 LINE 03 COLUMN 35.
           05 WS-DATA-LABEL PIC X(10)   VALUE
                            '---DATA: '.
           05 LINE 03 COLUMN 44 USING WS-DIA.
           05 LINE 03 COLUMN 46 PIC X(01) VALUE '/'.
           05 LINE 03 COLUMN 47 USING WS-MES.
           05 LINE 03 COLUMN 49 PIC X(01) VALUE '/'.
           05 LINE 03 COLUMN 50 USING WS-ANO.

           05 LINE 03 COLUMN 55.
           05 WS-HORA-LABEL PIC X(09)   VALUE
                            ' - HORA: '.
           05 LINE 03 COLUMN 63 USING WS-HORAS.
           05 LINE 03 COLUMN 65 PIC X(01) VALUE ':'.
           05 LINE 03 COLUMN 66 USING WS-MINUTOS.
           05 LINE 03 COLUMN 68 PIC X(01) VALUE ':'.
           05 LINE 03 COLUMN 69 USING WS-SEGUNDOS.

       01  TELA-DADOS.
           05 LINE 05 COLUMN 35.
           05 WS-TITULO-3   PIC X(32)   VALUE
                            '---DADOS DO ALUNO/DISCIPLINA---'.
           05 LINE 07 COLUMN 35.
           05 WS-TITULO-4   PIC X(28)   VALUE
                            'DIGITE O NOME DO ALUNO....:'.
           05 NOME-ALUNO    PIC X(30)   USING WS-NOME-ALUNO.
           05 LINE 09 COLUMN 35.
           05 WS-TITULO-5   PIC X(28)   VALUE
                            'DIGITE A DISCIPLINA.......:'.
           05 NOME-DISCIPLI PIC X(30)   USING WS-NOME-DISCIPLINA.

           05 LINE 11 COLUMN 35.
           05 WS-TITULO-6   PIC X(31)   VALUE
                            '--------NOTAS DO ALUNO--------'.
           05 LINE 12 COLUMN 35.
           05 WS-TITULO-7   PIC X(28)   VALUE
                            'PRIMEIRA NOTA............:'.
           05 NOTA-N-1      PIC  99,9    USING WS-NOTA-1.

           05 LINE 13 COLUMN 35.
           05 WS-TITULO-8   PIC X(28)   VALUE
                            'SEGUNDA NOTA.............:'.
           05 NOTA-N-2      PIC  99,9    USING WS-NOTA-2.

           05 LINE 14 COLUMN 35.
           05 WS-TITULO-9   PIC X(28)   VALUE
                            'TERCEIRA NOTA............:'.
           05 NOTA-N-3      PIC  99,9    USING WS-NOTA-3.

           05 LINE 15 COLUMN 35.
           05 WS-TITULO-A   PIC X(28)   VALUE
                            'QUARTA NOTA..............:'.
           05 NOTA-N-4      PIC  99,9    USING WS-NOTA-4.

       01  TELA-CONFIRMA.
           05 LINE 18 COLUMN 35.
           05 WS-TITULO-C   PIC X(28)   VALUE
                            'NOVO CALCULO (S/N)?......:'.
           05  CONTINUA     PIC X(01)   USING WS-CONTINUA-CALC.

       01  TELA-APURACAO.

           05 LINE 19 COLUMN 35.
           05 WS-TITULO-E   PIC X(30)   VALUE
                            '*****************************'.
           05 LINE 20 COLUMN 35.
           05 WS-TITULO-4   PIC X(28)   VALUE
                            'NOME DO ALUNO............:'.
           05 NOME-ALUN     PIC X(30)   USING WS-NOME-ALUNO.
           05 LINE 21 COLUMN 35.
           05 WS-TITULO-5   PIC X(28)   VALUE
                            'NOME DA DISCIPLINA.......:'.
           05 NOME-DISCIPL  PIC X(30)   USING WS-NOME-DISCIPLINA.

           05 LINE 22 COLUMN 35.
           05 WS-TITULO-B   PIC X(28)   VALUE
                            'APURACAO DA MEDIA........:' .
           05 MEDIA         PIC  99,9    USING WS-NOTA-MEDIA.

           05 LINE 23 COLUMN 35.
           05 WS-TITULO-D   PIC X(28)   VALUE
                            'APROVADO / REPROVADO.....:'.
           05  RESULTADO    PIC X(10)   USING WS-RESULTADO.

       01  TELA-ERRO.
           05 BLANK SCREEN.
           05 LINE 5 COLUMN 35.
           05 WS-TITULO-C   PIC X(60)   VALUE
           'ERRO NO PROCESSAMENTO - SEM DADOS OU DADOS INVALIDOS'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM P100-INICIO THRU P100-INICIO-FIM.
           PERFORM P200-PROCESSA THRU P200-PROCESSA-FIM
                   UNTIL WS-CONTINUA-CALC EQUALS TO 'N' OR 'n'.
           PERFORM P900-FINALIZA THRU P900-FINALIZA-FIM.

       P100-INICIO.
           INITIALISE  WS-DATA WS-HORAS WS-NOTAS WS-DADOS-ALUNO
                       WS-RESULTADO WS-CONTINUA-CALC
                       REPLACING   ALPHABETIC BY SPACES
                                   NUMERIC    BY ZERO.

           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATA.
           MOVE FUNCTION CURRENT-DATE(9:6) TO WS-HORA.

           DISPLAY TELA-CABECALHO.

       P100-INICIO-FIM.

       P200-PROCESSA.
      *TRABALHA COM O CONCEITO DE SCREEN SECTION
           DISPLAY TELA-DADOS.
           ACCEPT  TELA-DADOS.

           PERFORM P500-VALIDA-CAMPO  THRU P500-VALIDA-CAMPO-FIM.
           PERFORM P400-CALC-MEDIA    THRU P400-CALC-MEDIA-FIM.

           DISPLAY TELA-CONFIRMA.
           ACCEPT  TELA-CONFIRMA.

           PERFORM P100-INICIO        THRU P100-INICIO-FIM.

       P200-PROCESSA-FIM.

       P300-ERROR.
           DISPLAY TELA-ERRO.
           PERFORM P100-INICIO THRU P100-INICIO-FIM.
       P300-ERROR-FIM.

       P400-CALC-MEDIA.

           ADD    WS-NOTA-1 WS-NOTA-2 WS-NOTA-3 WS-NOTA-4
                  TO WS-NOTA-MEDIA
                  ON SIZE ERROR
                  PERFORM P300-ERROR THRU P300-ERROR-FIM
           END-ADD.

           DIVIDE WS-NOTA-MEDIA BY 4 GIVING WS-NOTA-MEDIA
                  ON SIZE ERROR
                  PERFORM P300-ERROR THRU P300-ERROR-FIM
           END-DIVIDE.

           EVALUATE WS-NOTA-MEDIA
               WHEN GREATER THAN OR EQUALS TO 7
                   SET WS-APROVADO TO TRUE
               WHEN OTHER
                   SET WS-APROVADO TO FALSE
           END-EVALUATE.

           DISPLAY TELA-APURACAO.

       P400-CALC-MEDIA-FIM.

       P500-VALIDA-CAMPO.

           IF WS-NOME-ALUNO EQUALS TO SPACES
               PERFORM P300-ERROR THRU P300-ERROR-FIM
           END-IF.

           IF WS-NOME-DISCIPLINA EQUALS TO SPACES
               PERFORM P300-ERROR THRU P300-ERROR-FIM
           END-IF.

           IF WS-NOTA-1 EQUALS TO ZERO OR GREATER THAN 10
               PERFORM P300-ERROR THRU P300-ERROR-FIM
           END-IF.

           IF WS-NOTA-2 EQUALS TO ZERO OR GREATER THAN 10
               PERFORM P300-ERROR THRU P300-ERROR-FIM
           END-IF.

           IF WS-NOTA-3 EQUALS TO ZERO OR GREATER THAN 10
               PERFORM P300-ERROR THRU P300-ERROR-FIM
           END-IF.

           IF WS-NOTA-4 EQUALS TO ZERO OR GREATER THAN 10
               PERFORM P300-ERROR THRU P300-ERROR-FIM
           END-IF.

       P500-VALIDA-CAMPO-FIM.

       P900-FINALIZA.
           DISPLAY 'FIM DO PROCESSAMENTO.'.
           GOBACK.
       P900-FINALIZA-FIM.


       END PROGRAM DESAFIOM2.
