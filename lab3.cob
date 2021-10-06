      ******************************************************************
      *Author: David Nguyen
      *Date: October 6, 2021
      *Purpose: lab3
      ******************************************************************
       IDENTIFICATION DIVISION.
           PROGRAM-ID. lab3.
           AUTHOR. David Nguyen.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'DA-S-INPUT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRNT-FILE ASSIGN TO 'UR-S-PRNT'.
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD.
       01  INPUT-REC PIC X(80).
       FD  PRNT-FILE
               LABEL RECORDS ARE OMITTED.
       01  PRNT-REC PIC X(110).
       WORKING-STORAGE SECTION.
       01  INPUT-DATA.
           03 I-NAME PIC X(20).
           03 I-MAJOR PIC X(4).
           03 I-YEAR PIC X(4).
           03 I-LOAN.
               05 I-LOANWHOLE PIC 9(5).
               05 I-LOANDECIMAL PIC P9(2).
           03 I-PAID1.
               05 I-PAID1WHOLE PIC 9(4).
               05 I-PAID1DECIMAL PIC P9(2).
           03 I-PAID2.
               05 I-PAID2WHOLE PIC 9(4).
               05 I-PAID2DECIMAL PIC P9(2).
           03 I-PAID3.
               05 I-PAID3WHOLE PIC 9(4).
               05 I-PAID3DECIMAL PIC P9(2).
           03 I-PAID4.
               05 I-PAID3WHOLE PIC 9(4).
               05 I-PAID3DECIMAL PIC 9(2).
       01  DATA-TRANSFER.
           03 D-LOAN PIC 9(5)V9(2).
           03 D-PAID1 PIC 9(4)V9(2).
           03 D-PAID2 PIC 9(4)V9(2).
           03 D-PAID3 PIC 9(4)V9(2).
           03 D-PAID4 PIC 9(4)V9(2).
           03 D-TOTPAID PIC 9(5)V9(2).
           03 D-BALANCE PIC 9(5)V9(2).
       01  PRNT-DATA.
           03 L-NAME PIC X(20).
           03 L-MAJOR PIC X(10).
           03 L-YEAR PIC X(10).
           03 L-LOAN.
               05 L-LOANNUM PIC 9(5).99.
               05 L-LOANSPACES PIC X(2) VALUES SPACES.
           03 L-PAID1.
               05 L-PAID1NUM PIC 9(4).99.
               05 L-PAID1SPACES PIC X(3) VALUES SPACES.
           03 L-PAID2.
               05 L-PAID2NUM PIC 9(4).99.
               05 L-PAID2SPACES PIC X(3) VALUES SPACES.
           03 L-PAID3.
               05 L-PAID3NUM PIC 9(4).99.
               05 L-PAID3SPACES PIC X(3) VALUES SPACES.
           03 L-PAID4.
               05 L-PAID4NUM PIC 9(4).99.
               05 L-PAID4SPACES PIC X(3) VALUES SPACES.
           03 L-TOTPAID.
               05 L-TOTPAIDNUM PIC 9(5).99.
               05 L-TOTPAIDSPACES PIC X(2) VALUES SPACES.
           03 L-BALANCE.
               05 L-BALANCENUM PIC 9(5).99.
               05 L-BALANCESPACES PIC X(2) VALUES SPACES.
       01  PRNT-HEADING.
           03 FILLER PIC X(20) VALUES 'NAME'.
           03 FILLER PIC X(10) VALUES 'MAJOR'.
           03 FILLER PIC X(10) VALUES 'YEAR'.
           03 FILLER PIC X(10) VALUES 'LOAN'.
           03 FILLER PIC X(10) VALUES 'PAID1'.
           03 FILLER PIC X(10) VALUES 'PAID2'.
           03 FILLER PIC X(10) VALUES 'PAID3'.
           03 FILLER PIC X(10) VALUES 'PAID4'.
           03 FILLER PIC X(10) VALUES 'TOT PAID'.
           03 FILLER PIC X(10) VALUES 'BALANCE'.
       01  MISC.
           03 EOF-I PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       000-MAINLINE.
           OPEN INPUT INPUT-FILE
               OUTPUT PRNT-FILE
           PERFORM 2000-READ-INPUT.
           PERFORM 1400-PRINT-HEAD.
           PERFORM 1500-LOOP
               UNTIL EOF-I = 1;
           CLOSE INPUT-FILE
               PRNT-FILE.
           STOP RUN.
       1400-PRINT-HEAD.
           WRITE PRNT-REC FROM PRNT-HEADING.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC
               AFTER ADVANCING 1 LINE.
       1500-LOOP.
           PERFORM 1600-PRINT-NAMES.
           PERFORM 2000-READ-INPUT.
       1600-PRINT-NAMES.
           MOVE I-NAME TO L-NAME.
           MOVE I-MAJOR TO L-MAJOR.
           MOVE I-YEAR TO L-YEAR.
           MOVE I-LOAN TO D-LOAN.
           MOVE D-LOAN TO L-LOANNUM.
           MOVE I-PAID1 TO D-PAID1.
           MOVE D-PAID1 TO L-PAID1NUM.
           MOVE I-PAID2 TO D-PAID2.
           MOVE D-PAID2 TO L-PAID2NUM.
           MOVE I-PAID3 TO D-PAID3.
           MOVE D-PAID3 TO L-PAID3NUM.
           MOVE I-PAID4 TO D-PAID4.
           MOVE D-PAID4 TO L-PAID4NUM.
           COMPUTE D-TOTPAID = D-PAID1 + D-PAID2 + D-PAID3 + D-PAID4.
           MOVE D-TOTPAID TO L-TOTPAIDNUM.
           COMPUTE D-BALANCE = D-LOAN - D-TOTPAID.
           MOVE D-BALANCE TO L-BALANCENUM.
           WRITE PRNT-REC FROM PRNT-DATA
               AFTER ADVANCING 1 LINE.
       2000-READ-INPUT.
           READ INPUT-FILE INTO INPUT-DATA
               AT END MOVE 1 TO EOF-I.
       END PROGRAM lab3.
