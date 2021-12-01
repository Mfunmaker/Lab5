        IDENTIFICATION DIVISION.
        PROGRAM-ID. LAB5.
        AUTHOR. Martin Funmaker.
      * LAB EXERCISE 5.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'LAB5-INPUT'.
           SELECT PRNT-FILE ASSIGN TO 'LAB5-PRNT'.


        DATA DIVISION.


        FILE SECTION.
        FD INPUT-FILE
          BLOCK CONTAINS 0 RECORDS
          LABEL RECORDS ARE STANDARD.
        01 INPUT-REC PIC X(80).

        FD PRNT-FILE
          LABEL RECORDS ARE OMITTED.
        01 PRNT-REC PIC X(125).
        WORKING-STORAGE SECTION.
       01 MATH-DATA.
          03 TOT PIC 9(7).
          03 BAL PIC 9(7).
      **************************************************************
      * LAYOUT FOR THE INPUT FILE *
      **************************************************************
        01 INPUT-DATA.
          03 I-NAME PIC X(20).
          03 I-DEGREE PIC X(4).
          03 I-YEAR PIC X(4).
          03 I-LOAN PIC 9(7).
          03 I-PAYMENT1 PIC 9(6).
          03 I-PAYMENT2 PIC 9(6).
          03 I-PAYMENT3 PIC 9(6).
          03 I-PAYMENT4 PIC 9(6).
      **************************************************************
      * LAYOUT FOR THE 1ST DATA LINE OF REPORT PRNTING *
      **************************************************************
        01 PRNT-DATA1.
          03 P-NAME PIC X(25).
          03 P-DEGREE PIC X(7).
          03 P-YEAR PIC X(7).
          03 P-LOAN PIC 99999.99.
          03 FILLER PIC X(3).
          03 P-PAYMENT1 PIC 9999.99.
          03 FILLER PIC X(3).
          03 P-TOTAL PIC 99999.99.
          03 FILLER PIC X(3).
          03 P-BAL PIC 99999.99.
      01 PRNT-DATA2.
          03 FILLER PIC X(40)
          03 P-PAYMENT2 PIC 9999.99.
      01 PRNT-DATA3.
          03 FILLER PIC X(40)
          03 P-PAYMENT3 PIC 9999.99.
      01 PRNT-DATA4.
          03 FILLER PIC X(40)
          03 P-PAYMENT4 PIC 9999.99.
      01 PRNT-DATA5.
          03 FILLER X(30).
          03 PIC X(10) VALUE 'TOTAL PAID'.
          03 FILLER X(5)
          03 P-TOTAL PIC 99999.99.
          03 FILLER PIC X(10).
          03 PIC X(7) VALUE 'BALANCE'.
          03 FILLER X(5).
          03 P-BAL PIC 99999.99.

      **************************************************************
      * LAYOUT FOR THE 1ST HEADING LINE OF REPORT PRNTING *
      **************************************************************
       01 PRNT-HEADING1.
          03              PIC X(20) VALUE 'NAME'.
          03              PIC X(10) VALUE 'DEGREE'.
          03              PIC X(8) VALUE 'YEAR'.
          03              PIC X(10) VALUE 'LOAN'.
          03              PIC X(10) VALUE 'PAID'.

        01 MISC.
      **************************************************************
      * END OF FILE (EOF) SWITCHES *
      * 0 = NOT AT EOF 1 = AT EOF *
      **************************************************************
          03 EOF-I PIC 9 VALUE 0.
      **************************************************************
      * START OF PROCEDURE DIVISION *
      **************************************************************
        PROCEDURE DIVISION.
        000-MAINLINE.
          OPEN INPUT INPUT-FILE
            OUTPUT PRNT-FILE.
          PERFORM 2000-READ-INPUT.
          PERFORM 1400-PRINT-HEAD.
          PERFORM 1500-LOOP
             UNTIL EOF-I = 1.
          CLOSE INPUT-FILE
            PRNT-FILE.
          STOP RUN.
        1400-PRINT-HEAD.
          WRITE PRNT-REC FROM PRNT-HEADING1
            AFTER ADVANCING PAGE.
          MOVE SPACES TO PRNT-REC.
          WRITE PRNT-REC
            AFTER ADVANCING 1 LINE.
        1500-LOOP.
          PERFORM 1600-PRINT-NAMES.
           PERFORM 2000-READ-INPUT.
      **************************************************************
      * PRINTS THE SCHEDULE INFORMATION *
      **************************************************************
        1600-PRINT-NAMES.
           MOVE I-NAME TO P-NAME.
           MOVE I-DEGREE TO P-DEGREE.
           MOVE I-YEAR TO P-YEAR.
           MOVE I-LOAN TO P-LOAN.
           MOVE I-PAYMENT1 TO P-PAYMENT1.
           MOVE I-PAYMENT2 TO P-PAYMENT2.
           MOVE I-PAYMENT3 TO P-PAYMENT3.
           MOVE I-PAYMENT4 TO P-PAYMENT4.
           ADD I-PAYMENT1, I-PAYMENT2, I-PAYMENT3
           I-PAYMENT4 GIVING P-TOTAL.
           SET TOT TO P-TOTAL.
           SUBTRACT TOT FROM I-LOAN
           GIVING P-BAL.
          WRITE PRNT-REC FROM PRNT-DATA1
            AFTER ADVANCING 1 LINE.
          WRITE PRNT-REC FROM PRNT-DATA2
            AFTER ADVANCING 1 LINE.
          WRITE PRNT-REC FROM PRNT-DATA3
            AFTER ADVANCING 1 LINE.
          WRITE PRNT-REC FROM PRNT-DATA4
            AFTER ADVANCING 1 LINE.
          WRITE PRNT-REC FROM PRNT-DATA5
            AFTER ADVANCING 1 LINE.
      **************************************************************
      * READS THE INPUT FILE *
      **************************************************************
        2000-READ-INPUT.
          READ INPUT-FILE INTO INPUT-DATA
                  AT END MOVE 1 TO EOF-I.
