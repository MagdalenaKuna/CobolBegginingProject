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

                   SELECT SORTEDPP ASSIGN TO 'sorted_people_names.txt'
                   ORGANIZATION IS LINE SEQUENTIAL
                   ACCESS IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD PEOPLE.
       01 P-F PIC A(30).

       FD SORTEDPP.
       01 WS-SORTED-PERSON.
           05 SORTED-PERSON-FN PIC A(15).
           05 SORTED-PERSON-SURNAME PIC A(15).

       WORKING-STORAGE SECTION.
       01 GROUP-OF-PEOPLE OCCURS 9 TIMES.
           05 PERSON-FN PIC A(15).
           05 PERSON-SURNAME PIC A(15).
       01 WS-EOF PIC A(1).

       01 CNTR PIC 9(2) VALUE 0.
       01 CNTR-2 PIC 9(2) VALUE 2.
       01 SWITCHING_PEOPLE PIC A(30).
       01 SORTING_ON PIC 9 VALUE 1.
       01 DISPLAY_COUNTER PIC 9(2) VALUE 1.
       01 MAX_READ_INPUTS PIC 9(2).
       01 SPIPPED_FILE_EALIER PIC 9(1) VALUE 0.

      * Program nie obsluguje polskich znakow w imionach
       PROCEDURE DIVISION.

           PERFORM PARA-0.
           MOVE CNTR TO MAX_READ_INPUTS.
           ADD SPIPPED_FILE_EALIER TO MAX_READ_INPUTS.
           DISPLAY MAX_READ_INPUTS.

           OPEN OUTPUT SORTEDPP
           MOVE "SORTED BY NAME:" TO WS-SORTED-PERSON.
           WRITE WS-SORTED-PERSON.

           DISPLAY " "
           DISPLAY "SORTED BY NAME".
           MOVE 1 TO CNTR.
           PERFORM PARA-1 UNTIL SORTING_ON=0.
           PERFORM PARA-3 UNTIL DISPLAY_COUNTER=MAX_READ_INPUTS.

           MOVE "                           " TO WS-SORTED-PERSON.
           WRITE WS-SORTED-PERSON.
           MOVE "SORTED BY SURNAME:" TO WS-SORTED-PERSON.
           WRITE WS-SORTED-PERSON.

           DISPLAY " "
           DISPLAY "SORTED BY SURNAME".
           MOVE 1 TO CNTR.
           MOVE 2 TO CNTR-2.
           MOVE 1 TO DISPLAY_COUNTER.
           MOVE 1 TO SORTING_ON.
           PERFORM PARA-4 UNTIL SORTING_ON=0.
           MOVE 1 TO DISPLAY_COUNTER.
           PERFORM PARA-3 UNTIL DISPLAY_COUNTER=MAX_READ_INPUTS.
           PERFORM PARA-7.
           STOP RUN.

           PARA-0.
               OPEN INPUT PEOPLE.
                   PERFORM UNTIL WS-EOF='Y'
                     ADD 1 TO CNTR
                         READ PEOPLE INTO GROUP-OF-PEOPLE(CNTR)
                            AT END MOVE 'Y' TO WS-EOF
                            NOT AT END DISPLAY P-F
                            IF CNTR > 8 THEN
                               MOVE 1 TO SPIPPED_FILE_EALIER
                               MOVE 'Y' TO WS-EOF
                            END-IF
                         END-READ
                   END-PERFORM.
                 CLOSE PEOPLE.

           PARA-1.
               IF GROUP-OF-PEOPLE(CNTR) > GROUP-OF-PEOPLE(CNTR-2) THEN
                   PERFORM PARA-2 1 TIMES
                   ADD 1 TO SORTING_ON
               END-IF
               PERFORM PARA-5.

           PARA-2.
               MOVE GROUP-OF-PEOPLE(CNTR-2) TO SWITCHING_PEOPLE.
               MOVE GROUP-OF-PEOPLE(CNTR) TO GROUP-OF-PEOPLE(CNTR-2).
               MOVE SWITCHING_PEOPLE TO GROUP-OF-PEOPLE(CNTR).

           PARA-3.
               DISPLAY GROUP-OF-PEOPLE(DISPLAY_COUNTER)
               PERFORM PARA-6
               IF DISPLAY_COUNTER < MAX_READ_INPUTS THEN
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
               PERFORM PARA-5.

           PARA-5.
               ADD 1 TO CNTR
               ADD 1 TO CNTR-2

               IF CNTR-2 = MAX_READ_INPUTS THEN
                  MOVE 1 TO CNTR
                  MOVE 2 TO CNTR-2
                  IF SORTING_ON = 1 THEN
                      MOVE 0 TO SORTING_ON
                  ELSE
                      MOVE 1 TO SORTING_ON
                  END-IF
               END-IF.

           PARA-6.
               MOVE GROUP-OF-PEOPLE(DISPLAY_COUNTER) TO WS-SORTED-PERSON
               WRITE WS-SORTED-PERSON.

           PARA-7.
               CLOSE SORTEDPP.

       END PROGRAM sort_people.
