      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ANNUITED_INSTALLMENTS.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
             SELECT FS_CREDIT_INF ASSIGN TO 'annuited_input.txt'
             ORGANISATION IS LINE SEQUENTIAL.

             SELECT FS_INSTALLMENTS_INF ASSIGN TO 'annuited_output.txt'
             ORGANISATION IS LINE SEQUENTIAL
             ACCESS IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
           FD FS_CREDIT_INF.
           01 FS_CREDIT_INF0.
               05 FS_C_DESCRIPTION PIC A(20).
               05 FS_C_VALUE PIC X(10).

           FD FS_INSTALLMENTS_INF.
           01 FS_I.
               05 FS_I_C PIC 9(10).
               05 FS_I_C_DOT PIC A(1).
               05 FS_I_C_V PIC v9(2).
               05 BREAK PIC A(3).
               05 FS_I_I PIC 9(10).
               05 FS_I_I_DOT PIC A(1).
               05 FS_I_I_V PIC v9(2).
               05 BREAK2 PIC A(3).
               05 FS_I_A PIC 9(10).
               05 FS_I_A_DOT PIC A(1).
               05 FS_I_A_V PIC v9(2).

       WORKING-STORAGE SECTION.
       01 WS-EOF PIC A(1).
       01 COUNTER PIC 9(1) VALUE 0.
       01 POW_COUNTER PIC 9(1) VALUE 1.

       01 CREDIT_INFORMATION.
           05 CREDIT_AMOUNT PIC 9(8)v9(8).
           05 BANK_RATE PIC 9(8)v9(8).
           05 INSTALLMENTS PIC 9(8).
           05 YEARS PIC 9(8).

       01 ALL_INSTALLMENTS PIC 9(3).
       01 ANNUITED_I PIC 9(10)v9(2).

       01 SERIES_SUM PIC 9(10)v9(8) VALUE 0.
       01 ONE_SERIES PIC 9(10)v9(8) VALUE 0.
       01 FRAGMENT_TO_POW PIC 9(10)v9(8) VALUE 0.

       01 CREDIT_PART PIC 9(10)v9(8) VALUE 0.
       01 INTEREST_PART PIC 9(10)v9(8) VALUE 0.
       01 TO_PAYOFF PIC 9(10)v9(8) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM PARA-0.
           MULTIPLY INSTALLMENTS BY YEARS GIVING ALL_INSTALLMENTS.
           ADD 1 TO ALL_INSTALLMENTS
           MOVE 1 TO COUNTER.
           PERFORM PARA-2 UNTIL COUNTER=ALL_INSTALLMENTS.
           DIVIDE CREDIT_AMOUNT BY SERIES_SUM GIVING ANNUITED_I.
           DISPLAY ANNUITED_I.
           MOVE CREDIT_AMOUNT TO TO_PAYOFF.
           MOVE 1 TO COUNTER.
           OPEN OUTPUT FS_INSTALLMENTS_INF
           PERFORM PARA-4 UNTIL COUNTER=ALL_INSTALLMENTS.
           CLOSE FS_INSTALLMENTS_INF.
           STOP RUN.

           PARA-0.
               OPEN INPUT FS_CREDIT_INF.
               PERFORM UNTIL WS-EOF='Y'
                   READ FS_CREDIT_INF
                       AT END MOVE 'Y' TO WS-EOF
                       NOT AT END DISPLAY FS_C_VALUE
                       PERFORM PARA-1
                       ADD 1 TO COUNTER
                   END-READ
               END-PERFORM.
               CLOSE FS_CREDIT_INF.

           PARA-1.
               EVALUATE COUNTER
                 WHEN 0 MOVE FS_C_VALUE TO CREDIT_AMOUNT
                 WHEN 1 MOVE FS_C_VALUE TO BANK_RATE
                 WHEN 2 MOVE FS_C_VALUE TO INSTALLMENTS
                 WHEN 3 MOVE FS_C_VALUE TO YEARS.

           PARA-2.
               MOVE 0 TO ONE_SERIES
               ADD INSTALLMENTS TO ONE_SERIES
               ADD BANK_RATE TO ONE_SERIES
               DIVIDE ONE_SERIES BY INSTALLMENTS GIVING ONE_SERIES

               IF COUNTER=1 THEN
                   MOVE ONE_SERIES TO FRAGMENT_TO_POW
               END-IF

               MOVE 1 TO POW_COUNTER
               PERFORM PARA-3 UNTIL POW_COUNTER=COUNTER
               DIVIDE 1 BY ONE_SERIES GIVING ONE_SERIES
               ADD ONE_SERIES TO SERIES_SUM
               ADD 1 TO COUNTER.

           PARA-3.
               MULTIPLY ONE_SERIES BY FRAGMENT_TO_POW GIVING ONE_SERIES
               ADD 1 TO POW_COUNTER.

           PARA-4.
               IF COUNTER=1 THEN
               MOVE "CREDIT PAR      INTEREST PART   ANNUITED" TO FS_I
                   WRITE FS_I
                   MOVE "   " TO BREAK
                   MOVE "   " TO BREAK2
                   MOVE "." TO FS_I_C_DOT
                   MOVE "." TO FS_I_I_DOT
                   MOVE "." TO FS_I_A_DOT
               END-IF

               MULTIPLY TO_PAYOFF BY BANK_RATE GIVING INTEREST_PART
               DIVIDE INTEREST_PART BY INSTALLMENTS GIVING INTEREST_PART
               SUBTRACT INTEREST_PART FROM ANNUITED_I GIVING CREDIT_PART
               SUBTRACT CREDIT_PART FROM TO_PAYOFF GIVING TO_PAYOFF
               ADD 1 TO COUNTER

               MOVE CREDIT_PART TO FS_I_C
               MOVE CREDIT_PART TO FS_I_C_V
               MOVE INTEREST_PART TO FS_I_I
               MOVE INTEREST_PART TO FS_I_I_V
               MOVE ANNUITED_I TO FS_I_A
               MOVE ANNUITED_I TO FS_I_A_V
               WRITE FS_I.

       END PROGRAM ANNUITED_INSTALLMENTS.
