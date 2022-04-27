      ******************************************************************
      * Author:    EMERSON PINHEIRO - EMAIL. TIO.EL@OUTLOOK.COM.
      * Date:      26/04/2022
      * Purpose:   LEITURA DE ARQUIVOS NO COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGLERARQ.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT
           ASSIGN TO 'D:\My Documents\Cobol\Modulo3\bin\STUDENT.TXT'
           ORGANISATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT.
       01  STUDENT-FILE.
           03 CD-STUDENT           PIC 9(05).
           03 NM-STUDENT           PIC X(20).

       WORKING-STORAGE SECTION.
       01  WS-DADOS                PIC X(25) VALUE SPACES.
       01  FILLER REDEFINES WS-DADOS.
           03 WS-CD-STUDENT        PIC 9(05).
           03 WS-NM-STUDENT        PIC X(20).

       01  WS-END-OF-FILE          PIC A(01).
           88 WS-EOF               VALUE 'S' FALSE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM P100-INICIO     THRU P100-INICIO-FIM.

           PERFORM UNTIL WS-END-OF-FILE EQUALS TO 'S'
                   READ STUDENT INTO WS-DADOS
                       AT  END
                           MOVE 'S' TO WS-END-OF-FILE
                       NOT AT END
                           PERFORM P200-PROCESSA THRU P200-PROCESSA-FIM                   END-READ
           END-PERFORM.

           PERFORM P900-FINALIZA   THRU P900-FINALIZA-FIM.

       MAIN-PROCEDURE-FIM.

       P100-INICIO.
           DISPLAY     'INICIO DO PROCESSAMENTO.'.
           INITIALISE WS-DADOS WS-END-OF-FILE
               REPLACING NUMERIC      BY ZEROES
                         ALPHANUMERIC BY SPACES.
           SET WS-EOF TO FALSE.
           OPEN INPUT STUDENT.
           DISPLAY 'ARQUIVO STUDENT FOI ABERTO. LENDO DADOS...'.
       P100-INICIO-FIM.

       P200-PROCESSA.
           DISPLAY 'CODIGO: ' WS-CD-STUDENT
                   ' - NOME: ' WS-NM-STUDENT.
       P200-PROCESSA-FIM.

       P800-ERRO.
           DISPLAY 'ERRO NO PROCESSAMENTO.'.
           PERFORM P900-FINALIZA       THRU P900-FINALIZA-FIM.
       P800-ERRO-FIM.


       P900-FINALIZA.
           DISPLAY 'FIM DO PROCESSAMENTO.'.
           DISPLAY 'FECHANDO ARQUIVO STUDENT...'.
           CLOSE STUDENT.
           GOBACK.
       P900-FINALIZA-FIM.


       END PROGRAM PGLERARQ.
