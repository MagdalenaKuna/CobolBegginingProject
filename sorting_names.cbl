      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. sort_people.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
               SELECT PEOPLE ASSIGN TO 'people_names.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD PEOPLE.
       01 PEOPLE-FILE PIC A(30).

       WORKING-STORAGE SECTION.
       01 PERSON_DATA  PIC A(30).
       01 GROUP-OF-PEOPLE OCCURS 6 TIMES.
           05 PERSON-FN PIC A(15).
           05 PERSON-SURNAME PIC A(15).
       01 WS-EOF PIC A(1).

       01 CNTR PIC 9(2) VALUE 0.
       01 CNTR-2 PIC 9(2) VALUE 2.
       01 SWITCHING_PEOPLE PIC A(30).
       01 SORTING_ON PIC 9 VALUE 1.
       01 DISPLAY_COUNTER PIC 9(2) VALUE 1.

      * Program nie dzia³a z polskimi literami w imionach.
       PROCEDURE DIVISION.
           OPEN INPUT PEOPLE.
               PERFORM UNTIL WS-EOF='Y'
                 ADD 1 TO CNTR
                 READ PEOPLE INTO PERSON_DATA
                    AT END MOVE 'Y' TO WS-EOF
                    NOT AT END DISPLAY PERSON_DATA
                    MOVE PERSON_DATA TO GROUP-OF-PEOPLE(CNTR)
                 END-READ
               END-PERFORM.
             CLOSE PEOPLE.

           DISPLAY " "
           DISPLAY "SORTED BY NAME".
           MOVE 1 TO CNTR.
           PERFORM PARA-1 UNTIL SORTING_ON=0.
           PERFORM PARA-3 UNTIL DISPLAY_COUNTER=7.

           DISPLAY " "
           DISPLAY "SORTED BY SURNAME".
           MOVE 1 TO CNTR.
           MOVE 2 TO CNTR-2.
           MOVE 1 TO DISPLAY_COUNTER.
           MOVE 1 TO SORTING_ON.
           PERFORM PARA-4 UNTIL SORTING_ON=0.
           MOVE 1 TO DISPLAY_COUNTER.
           PERFORM PARA-3 UNTIL DISPLAY_COUNTER=7.
           STOP RUN.

           PARA-1.
               IF GROUP-OF-PEOPLE(CNTR) > GROUP-OF-PEOPLE(CNTR-2) THEN
                   PERFORM PARA-2 1 TIMES
                   ADD 1 TO SORTING_ON
               END-IF.

               ADD 1 TO CNTR
               ADD 1 TO CNTR-2

               IF CNTR-2 = 7 THEN
                  MOVE 1 TO CNTR
                  MOVE 2 TO CNTR-2
                  IF SORTING_ON = 1 THEN
                      MOVE 0 TO SORTING_ON
                  ELSE
                      MOVE 1 TO SORTING_ON
                  END-IF
               END-IF.

           PARA-2.
               MOVE GROUP-OF-PEOPLE(CNTR-2) TO SWITCHING_PEOPLE.
               MOVE GROUP-OF-PEOPLE(CNTR) TO GROUP-OF-PEOPLE(CNTR-2).
               MOVE SWITCHING_PEOPLE TO GROUP-OF-PEOPLE(CNTR).

           PARA-3.
           DISPLAY GROUP-OF-PEOPLE(DISPLAY_COUNTER).
               IF DISPLAY_COUNTER < 7 THEN
                   ADD 1 TO DISPLAY_COUNTER
               END-IF.

           PARA-4.
               IF PERSON-SURNAME(CNTR) > PERSON-SURNAME(CNTR-2) THEN
                   PERFORM PARA-2 1 TIMES
                   ADD 1 TO SORTING_ON
               END-IF.
               IF PERSON-SURNAME(CNTR) = PERSON-SURNAME(CNTR-2) THEN
                   IF PERSON-FN(CNTR) > PERSON-FN(CNTR-2) THEN
                       PERFORM PARA-2 1 TIMES
                       ADD 1 TO SORTING_ON
                   END-IF
               END-IF.

               ADD 1 TO CNTR
               ADD 1 TO CNTR-2

               IF CNTR-2 = 7 THEN
                  MOVE 1 TO CNTR
                  MOVE 2 TO CNTR-2
                  IF SORTING_ON = 1 THEN
                      MOVE 0 TO SORTING_ON
                  ELSE
                      MOVE 1 TO SORTING_ON
                  END-IF
               END-IF.

       END PROGRAM sort_people.
