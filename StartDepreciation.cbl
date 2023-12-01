       IDENTIFICATION DIVISION.
       PROGRAM-ID. STARTDEPRECIATION.
       AUTHOR. COLE DOMBROWSKI.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

            SELECT ASSET-INPUT-FILE ASSIGN TO 'ASSETDATA.DAT'
            ORGANIZATION IS LINE SEQUENTIAL.

            SELECT ASSET-OUTPUT-FILE ASSIGN TO 'ASSETOUT.DOC'
            ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ASSET-INPUT-FILE RECORDING MODE IS F.
       01                              PICTURE X(80).

       FD  ASSET-OUTPUT-FILE RECORDING MODE IS F.
       01  PRINT-A-SINGLE-LINE         PICTURE X(132).

       WORKING-STORAGE SECTION.
       01  WORKING-VARIABLES.
           05  EOF-ASSET-WS            PICTURE X(3)     VALUE 'NO'.
           05  TOTAL-DEPR-WS           PICTURE S9(6)V99 VALUE ZERO.
           05  YEARLY-DEPR-WS          PICTURE S9(5)V99 VALUE ZERO.

           05  HOLD-DATE-WS.
               10 YEAR-WS       PIC 9999.
               10 MONTH-WS      PIC 99.
               10 DAY-WS        PIC 99.

      ***************************************************
      *MAKE THE FOLLOWING 01 YOUR INPUT COPY
      *01  ASSET-INPUT-RECORD.
      *    05  NAME-IN             PICTURE X(10).
      *    05  PRICE-IN            PICTURE S9(5)V99.
      *    05  SALVAGE-IN          PICTURE S9(5)V99.
      *    05  USEFUL-LIFE-IN      PICTURE 99.
      ***************************************************
       COPY ASSETIN.CPY.
      ***************************************************
      *MAKE THE FOLLOWING 01 YOUR OUTPUT COPY
      *01  DETAILED-OUTPUT-LINE-SETUP.
      *    05  FILLER              PICTURE X       VALUE SPACE.
      *    05  NAME-OUT            PICTURE X(10).
      *    05  FILLER              PICTURE X(5)    VALUE SPACE.
      *    05  PRICE-OUT           PICTURE $$$,$$9.99.
      *    05  FILLER              PICTURE X(5)    VALUE SPACE.
      *    05  SALVAGE-OUT         PICTURE $$$,$$9.99.
      *    05  FILLER              PICTURE X(5)    VALUE SPACE.
      *    05  USEFUL-LIFE-OUT     PICTURE Z9.
      *    05  FILLER              PICTURE X(5)    VALUE SPACE.
      *    05  YEARLY-DEPR-OUT     PICTURE $$$,$$9.99.
      ****************************************************
       COPY DEPRRPT.CPY.
      ****************************************************

       01  REPORT-HEADER-LINE-SETUP.
           05                      PICTURE X       VALUE SPACES.
           05                      PICTURE X(10)   VALUE 'RUN DATE: '.
           05  REPORT-HEADER-DATE-OUT.

               10  MONTH-OUT       PICTURE 99.
               10                  PICTURE X       VALUE '/'.
               10  DAY-OUT         PICTURE 99.
               10                  PICTURE X       VALUE '/'.
               10  YEAR-OUT        PICTURE 9999.
           05                      PICTURE X(4)    VALUE SPACES.
           05                      PICTURE X(45)   VALUE
           'IRS INTERNATIONAL, INC.  AUDIT YEAR IS 2020'.


       01  COLUMN-HEADER-LINE-SETUP.
           05  FILLER              PICTURE X(4)    VALUE SPACE.
           05                      PICTURE X(4)    VALUE 'NAME'.
           05  FILLER              PICTURE X(10)   VALUE SPACES.
           05                      PICTURE X(5)    VALUE 'PRICE'.
           05  FILLER              PICTURE X(10)   VALUE SPACES.
           05                      PICTURE X(7)    VALUE 'SALVAGE'.
           05  FILLER              PICTURE X(5)    VALUE SPACES.
           05                      PICTURE X(4)    VALUE 'LIFE'.
           05  FILLER              PICTURE X(3)    VALUE SPACES.
           05                      PICTURE X(19)   VALUE
                                      'YEARLY DEPRECIATION'.
           05  FILLER              PICTURE X(5)    VALUE SPACES.


       01  ASSET-TOTAL-LINE.
           05  FILLER           PICTURE  X     VALUE SPACE.
           05                   PICTURE  X(50) VALUE
                 'TOTAL DEPRECIATION FOR ALL ASSETS IS '.
           05  TOTAL-DEPR-OUT   PICTURE  $$,$$$,$$9.99.
           05  FILLER           PICTURE  X(70) VALUE SPACES.

       PROCEDURE DIVISION.
       100-MAINLINE.
           PERFORM    200-OPEN
           PERFORM    300-PROCESS UNTIL EOF-ASSET-WS = 'YES'
           PERFORM    900-CLOSE
           STOP RUN.

       200-OPEN.
           OPEN INPUT ASSET-INPUT-FILE OUTPUT ASSET-OUTPUT-FILE
           PERFORM     250-READ-ONE-RECORD

           MOVE FUNCTION CURRENT-DATE TO HOLD-DATE-WS
      *    MOVE MONTH-WS TO MONTH-OUT
      *    MOVE DAY-WS TO DAY-OUT
      *    MOVE YEAR-WS TO YEAR-OUT
           CALL "DEPRSUB2" USING YEAR-WS, MONTH-WS, DAY-WS,
                                 MONTH-OUT, DAY-OUT, YEAR-OUT.

           PERFORM 500-HEADER.

       250-READ-ONE-RECORD.
           READ ASSET-INPUT-FILE INTO ASSET-INPUT-RECORD
              AT END MOVE 'YES' TO EOF-ASSET-WS
           END-READ.

       300-PROCESS.
      *****************************************************
      *INSTEAD OF PERFORMING THE 400 PARAGRAPH, CALL A SUBROUTINE
      *    PERFORM   400-CALCULATE-DEPRECIATION.
           CALL "DEPRSUB" USING PRICE-IN, SALVAGE-IN, USEFUL-LIFE-IN,
                          YEARLY-DEPR-WS, TOTAL-DEPR-WS.

      ****************************************************

           MOVE YEARLY-DEPR-WS TO YEARLY-DEPR-OUT
           MOVE NAME-IN        TO NAME-OUT
           MOVE PRICE-IN       TO PRICE-OUT
           MOVE SALVAGE-IN     TO SALVAGE-OUT
           MOVE USEFUL-LIFE-IN TO USEFUL-LIFE-OUT


           MOVE  DETAILED-OUTPUT-LINE-SETUP TO PRINT-A-SINGLE-LINE
           WRITE PRINT-A-SINGLE-LINE AFTER 1 LINE
           PERFORM  250-READ-ONE-RECORD.


      ***************************************************
      * THE 400-CALCULATE-DEPRECIATION PARAGRAPH IS REMOVED AND
      * ITS CODE IS PUT IN THE SUBROUTINE YOU CREATE AND CALL ABOVE
       400-CALCULATE-DEPRECIATION.

      *    COMPUTE YEARLY-DEPR-WS = (PRICE-IN - SALVAGE-IN)
      *            / USEFUL-LIFE-IN

      *    ADD   YEARLY-DEPR-WS  TO  TOTAL-DEPR-WS.
      ****************************************************

       500-HEADER.
           MOVE REPORT-HEADER-LINE-SETUP TO PRINT-A-SINGLE-LINE
           WRITE PRINT-A-SINGLE-LINE AFTER 1 LINE

           MOVE COLUMN-HEADER-LINE-SETUP TO PRINT-A-SINGLE-LINE
           WRITE PRINT-A-SINGLE-LINE  AFTER 2 LINES

           MOVE SPACES TO PRINT-A-SINGLE-LINE
           WRITE PRINT-A-SINGLE-LINE AFTER 1 LINE.

       900-CLOSE.
           MOVE  TOTAL-DEPR-WS  TO TOTAL-DEPR-OUT
           WRITE PRINT-A-SINGLE-LINE FROM ASSET-TOTAL-LINE AFTER PAGE
           CLOSE     ASSET-INPUT-FILE      ASSET-OUTPUT-FILE.
