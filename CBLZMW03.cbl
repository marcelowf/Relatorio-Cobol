       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLZMW03.
      *-----------------------------------------------------------------
      * Author: Marcelo Wzorek Filho
      * Date: 17/01/2024
      * Purpose: Programa com funcionalidade de criar relatorio a partir de uma base de dados.
      * Updates:
      * dataxx - Marcelo - Create Program
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT LISTA ASSIGN TO
           'C:\VOLVO_ESTAGIO\IDE_COBOL\Exercício03\Devedores.txt'
           FILE STATUS IS WK-STATUS-E.

       SELECT RELATORIO ASSIGN TO
           'C:\VOLVO_ESTAGIO\IDE_COBOL\Exercício03\Relatorio.txt'
           FILE STATUS IS WK-STATUS-S.

       DATA DIVISION.
       FILE SECTION.
       FD LISTA RECORDING MODE IS F BLOCK CONTAINS 0 RECORDS.

       01 FL-LISTA-ARQ PIC X(33).
       01 FILLER REDEFINES FL-LISTA-ARQ.
           02 ARQ-CNPJ PIC 9(14).
           02 ARQ-SITU PIC 9(02).
           02 ARQ-VALO PIC 9(13)V99.
           02 ARQ-FIM  PIC X(02).

       FD RELATORIO RECORDING MODE IS F BLOCK CONTAINS 0 RECORDS.

       01 FL-RELATORIO-ARQ PIC X(60).

       WORKING-STORAGE SECTION.
       01 WK-STATUS-E PIC 9(02) VALUE ZEROS.
       01 WK-STATUS-S PIC 9(02) VALUE ZEROS.

       01 WK-CABEC-L  PIC X(60) VALUE ALL '='.

       01 WK-CABEC01.
           02 WK-CABEC01-PROG PIC X(20) VALUE 'CBLZMW03'.
           02 WK-CABEC01-IMPR PIC X(20) VALUE 'VOLVO S.A.'.
           02 WK-CABEC01-MASK PIC XXXXXXXXXX.

       01 WK-DATA-SYS.
           02 WK-YEAR-SYS  PIC 9(04) VALUE ZEROS.
           02 WK-MONTH-SYS PIC 9(02) VALUE ZEROS.
           02 WK-DAY-SYS   PIC 9(02) VALUE ZEROS.

       01 WK-HORA-SYS.
           02 WK-HOUR-SYS   PIC 9(02) VALUE ZEROS.
           02 WK-MINUTE-SYS PIC 9(02) VALUE ZEROS.
           02 WK-SECOND-SYS PIC 9(02) VALUE ZEROS.


       01 WK-CABEC02.
           02 WK-CABEC02-HORA-MASK PIC XXXXXXXX.
           02 WK-CABEC02-SPAC PIC X(10) VALUE SPACES.
           02 WK-CABEC02-DESC PIC X(40) VALUE
               'MEU PRIMEIRO RELATORIO COBOL.'.

       01 WK-LINDIT01.
           02 WK-LINDIT01-CNPJ PIC X(20) VALUE 'CNPJ'.
           02 WK-LINDIT01-SITU PIC X(20) VALUE 'SITUACAO'.
           02 WK-LINDIT01-VALO PIC X(20) VALUE 'VALOR'.


       01 WK-LINDIT02.
           02 WK-LINDIT02-CNPJ PIC 9(14)    VALUE ZEROS.
           02 WK-LINDIT02-SP01 PIC X(06)    VALUE SPACES.
           02 WK-LINDIT02-SITU PIC 9(02)    VALUE ZEROS.
           02 WK-LINDIT02-SP02 PIC X(18)    VALUE SPACES.
           02 WK-LINDIT02-VALO PIC ZZZZZZZZZZZZZZZZZZV99 VALUE SPACES.

       01 WK-LINDIT02-VALO-MASK PIC 9(18)V99 VALUE ZEROS.

       01 WK-FIM-ARQ PIC X(01) VALUE 'N'.

       PROCEDURE DIVISION.
           PERFORM 1000-INICIALIZAR.
           PERFORM 2000-PROCESSAR UNTIL WK-FIM-ARQ = 'S'.
           PERFORM 3000-FINALIZAR.

      *-----------------------------------------------------------------
      * INICIALIZAR
      *-----------------------------------------------------------------
       1000-INICIALIZAR SECTION.
           ACCEPT WK-DATA-SYS FROM DATE YYYYMMDD.

           MOVE WK-DAY-SYS TO WK-CABEC01-MASK (1:2).
           MOVE WK-MONTH-SYS TO WK-CABEC01-MASK (4:2).
           MOVE WK-YEAR-SYS TO WK-CABEC01-MASK (7:4).
           MOVE '/' TO WK-CABEC01-MASK (3:1)
                       WK-CABEC01-MASK (6:1).

           ACCEPT WK-HORA-SYS FROM TIME.

           MOVE WK-HOUR-SYS TO WK-CABEC02-HORA-MASK (1:2).
           MOVE WK-MINUTE-SYS TO WK-CABEC02-HORA-MASK (4:2).
           MOVE WK-SECOND-SYS TO WK-CABEC02-HORA-MASK (7:2).
           MOVE ':' TO WK-CABEC02-HORA-MASK (3:1)
                       WK-CABEC02-HORA-MASK (6:1).

           OPEN INPUT LISTA.
           IF WK-STATUS-E NOT EQUAL ZEROS
               DISPLAY 'ERRO DE ABERTURA DE ARQUIVO ' WK-STATUS-E
           END-IF.

           OPEN OUTPUT RELATORIO.
           IF WK-STATUS-S NOT EQUAL ZEROS
               DISPLAY 'ERRO DE ABERTURA DE ARQUIVO ' WK-STATUS-S
           END-IF.

           MOVE WK-CABEC-L TO FL-RELATORIO-ARQ.
           WRITE FL-RELATORIO-ARQ AFTER ADVANCING 1 LINE.

           MOVE WK-CABEC01 TO FL-RELATORIO-ARQ.
           WRITE FL-RELATORIO-ARQ AFTER ADVANCING 1 LINE.

           MOVE WK-CABEC02 TO FL-RELATORIO-ARQ.
           WRITE FL-RELATORIO-ARQ AFTER ADVANCING 1 LINE.

           MOVE WK-CABEC-L TO FL-RELATORIO-ARQ.
           WRITE FL-RELATORIO-ARQ AFTER ADVANCING 1 LINE.

           MOVE WK-LINDIT01 TO FL-RELATORIO-ARQ.
           WRITE FL-RELATORIO-ARQ AFTER ADVANCING 1 LINE.

       1000-INICIALIZAR-FIM.
           EXIT.
      *-----------------------------------------------------------------
      * PROCESSAR
      *-----------------------------------------------------------------
       2000-PROCESSAR SECTION.
           READ LISTA.

           IF WK-STATUS-E EQUAL 04
               MOVE 'S' TO WK-FIM-ARQ
           ELSE
               IF WK-STATUS-E NOT EQUAL 00
                   DISPLAY 'ERRO DE LEITURA DE ARQUIVO ' WK-STATUS-E
               END-IF
           END-IF.

           MOVE ARQ-CNPJ TO WK-LINDIT02-CNPJ.
           MOVE ARQ-SITU TO WK-LINDIT02-SITU.
           MOVE ARQ-VALO TO WK-LINDIT02-VALO-MASK.
           MOVE WK-LINDIT02-VALO-MASK TO WK-LINDIT02-VALO.


           MOVE WK-LINDIT02 TO FL-RELATORIO-ARQ.
           WRITE FL-RELATORIO-ARQ AFTER ADVANCING 1 LINE.

       2000-PROCESSAR-FIM.
           EXIT.
      *-----------------------------------------------------------------
      * FINALIZAR
      *-----------------------------------------------------------------
       3000-FINALIZAR SECTION.
           CLOSE LISTA.
           IF WK-STATUS-E NOT EQUAL ZEROS
               DISPLAY 'ERRO AO FECHAR O ARQUIVO ' WK-STATUS-E
           END-IF.

           CLOSE RELATORIO.
           IF WK-STATUS-S NOT EQUAL ZEROS
               DISPLAY 'ERRO AO FECHAR O ARQUIVO ' WK-STATUS-S
           END-IF.

           DISPLAY 'Processo concluido.'

           STOP RUN.

       3000-FINALIZAR-FIM.
           EXIT.

       END PROGRAM CBLZMW03.
