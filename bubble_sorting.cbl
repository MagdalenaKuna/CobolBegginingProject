      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. hello.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 GROUP-OF-NUMBERS.
           05 TABLE_NUMBER PIC A(2) OCCURS 10 TIMES.
       01 CNTR PIC 9 VALUE 1.
       01 CNTR-2 PIC 9 VALUE 2.
       01 SWITCHING_NUMBER PIC A(2).
       01 SORTING_ON PIC 9 VALUE 1.
       01 DISPLAY_COUNTER PIC 9 VALUE 1.

       PROCEDURE DIVISION.
           MOVE '34903896222146759917' TO GROUP-OF-NUMBERS.
           PERFORM PARA-A UNTIL SORTING_ON=0.
           PERFORM PARA-C UNTIL DISPLAY_COUNTER=9.
           PERFORM PARA-D.
           STOP RUN.

           PARA-A.
               IF TABLE_NUMBER(CNTR) > TABLE_NUMBER(CNTR-2) THEN
                   PERFORM PARA-B 1 TIMES
                   ADD 1 TO SORTING_ON
                   ADD 1 TO CNTR
                   ADD 1 TO CNTR-2
               ELSE
                   ADD 1 TO CNTR
                   ADD 1 TO CNTR-2
               END-IF.
               IF CNTR-2 = 9 THEN
                  MOVE 1 TO CNTR
                  MOVE 2 TO CNTR-2
                  IF SORTING_ON = 1 THEN
                      MOVE 0 TO SORTING_ON
                  ELSE
                      MOVE 1 TO SORTING_ON
                  END-IF
               END-IF.

           PARA-B.
               MOVE TABLE_NUMBER(CNTR-2) TO SWITCHING_NUMBER.
               MOVE TABLE_NUMBER(CNTR) TO TABLE_NUMBER(CNTR-2).
               MOVE SWITCHING_NUMBER TO TABLE_NUMBER(CNTR).

           PARA-C.
               DISPLAY TABLE_NUMBER(DISPLAY_COUNTER).
               ADD 1 TO DISPLAY_COUNTER.

           PARA-D.
               DISPLAY TABLE_NUMBER(DISPLAY_COUNTER).

       END PROGRAM hello.
