       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEPRSUB2.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       LINKAGE SECTION.
       01   YEAR-WS       PICTURE 9999.
       01   MONTH-WS      PICTURE 99.
       01   DAY-WS        PICTURE 99.
       01   MONTH-OUT     PICTURE 99.
       01   DAY-OUT       PICTURE 99.
       01   YEAR-OUT      PICTURE 9999.

       PROCEDURE DIVISION USING YEAR-WS, MONTH-WS, DAY-WS,
                                MONTH-OUT, DAY-OUT, YEAR-OUT.

       100-MAINLINE.

           MOVE MONTH-WS TO MONTH-OUT
           MOVE DAY-WS TO DAY-OUT
           MOVE YEAR-WS TO YEAR-OUT.

           EXIT PROGRAM.

