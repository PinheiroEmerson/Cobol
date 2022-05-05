      ******************************************************************
      * Author:    EMERSON PINHEIRO - EMAIL. TIO.EL@OUTLOOK.COM.
      * Date:      26/04/2022
      * Purpose:   SORT EXEMPLO CLASSIFAR 1 ARQUIVO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGSORT1.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
           SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT TEMPORARIO   ASSIGN TO
               'D:\My Documents\Cobol\Modulo3\Sort\TEMP01.DAT'
               ORGANIZATION IS SEQUENTIAL
                 ACCESS  MODE IS SEQUENTIAL.

           SELECT ENTRADA  ASSIGN TO
               'D:\My Documents\Cobol\Modulo3\Sort\CONTATOS.DAT'
               ORGANIZATION IS SEQUENTIAL
                 ACCESS  MODE IS SEQUENTIAL.

           SELECT SAIDA ASSIGN TO
               'D:\My Documents\Cobol\Modulo3\Sort\CONTSORT.DAT'
                ORGANIZATION IS SEQUENTIAL
                 ACCESS  MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       SD  TEMPORARIO.
       01  WORK-CONTATO.
           03 ID-CONTATO-W           PIC 9(02).
           03 NM-CONTATO-W           PIC X(20).

       FD  ENTRADA.
       01  INPUT-CONTATO.
           03 ID-CONTATO-I           PIC 9(02).
           03 NM-CONTATO-I           PIC X(20).

       FD  SAIDA.
       01  OUTPUT-CONTATO.
           03 ID-CONTATO-O           PIC 9(02).
           03 NM-CONTATO-O           PIC X(20).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM P100-INICIO     THRU P100-INICIO-FIM.

           PERFORM P200-PROCESSA   THRU P200-PROCESSA-FIM.

           PERFORM P900-FINALIZA   THRU P900-FINALIZA-FIM.

       MAIN-PROCEDURE-FIM.

       P100-INICIO.
           DISPLAY     'INICIO DO PROCESSAMENTO.'
           END-DISPLAY.
       P100-INICIO-FIM.

       P200-PROCESSA.
           SORT TEMPORARIO
                ON DESCENDING KEY ID-CONTATO-W
                USING ENTRADA
                GIVING SAIDA.
       P200-PROCESSA-FIM.

       P800-ERRO.
           DISPLAY 'ERRO NO PROCESSAMENTO.'
           END-DISPLAY.
           PERFORM P900-FINALIZA       THRU P900-FINALIZA-FIM.
       P800-ERRO-FIM.


       P900-FINALIZA.
           DISPLAY 'FIM DO PROCESSAMENTO.'
           END-DISPLAY.
           GOBACK.
       P900-FINALIZA-FIM.

       END PROGRAM PRGSORT1.
