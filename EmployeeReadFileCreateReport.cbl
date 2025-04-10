       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-READ-FILE-CREATE-REP.
       AUTHOR.     JARS.
       DATE-WRITTEN.              02/04/2025.
       DATE-COMPILED.             10/04/2025.
       SECURITY.                  NO ES CONFIDENCIAL.
      ***************************************************************
      *  This program reads a file containing employee information  *
      *   and prints a report and a file.                           *
      ***************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
       OBJECT-COMPUTER.
       SPECIAL-NAMES. C01 IS LINE-BREAK.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT EMPLOYEE-FILE ASSIGN TO 
               "./archivos/entrada/EMPLOYEE_INPUT_COMP-3.DAT"
              FILE STATUS IS FILE-CHECK-KEY
              ORGANIZATION IS LINE SEQUENTIAL.
            SELECT PRINT-FILE ASSIGN TO
                "./archivos/salida/EDITED_EMPLOYEE_REPORT.DAT".
            SELECT EMPLOYEE-OUTPUT-FILE ASSIGN TO 
               "./archivos/salida/EMPLOYEE_OUTPUT_FILE-COMP-3.DAT"
              ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.

-       01 EMPLOYEE-DETAILS.
            88 ENDOF-INPUTFILE VALUE HIGH-VALUES.
            05 EMPLEADO-ID       PIC 9(10).
            05 NOMBRE-EMPLEADO   PIC X(40).
            05 DIRECCION         PIC X(60).
            05 SUELDO            PIC S9(9)V99 COMP-3.
            05 TIPO-EMPLEADO     PIC X.

       FD PRINT-FILE.

       01  PRINT-LINE              PIC X(132).

       FD EMPLOYEE-OUTPUT-FILE.
       
       01 EMPLOYEE-OUTPUT-DETAILS.
            05 EMP-ID        PIC 9(10).
            05 NOMBRE-EMP    PIC X(40).
            05 DIRECCION-EMP PIC X(60).
            05 SUELDO-EMP    PIC S9(9)V99 COMP-3.
            05 TIPO-EMP      PIC X.
            05 RESULTADO     PIC X(9).

       WORKING-STORAGE SECTION.
       01 WS-CURRENT-DATE-DATA.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR         PIC 9(04).
               10  WS-CURRENT-MONTH        PIC 9(02).
               10  WS-CURRENT-DAY          PIC 9(02).
           05  WS-CURRENT-TIME.
               10  WS-CURRENT-HOURS        PIC 9(02).
               10  WS-CURRENT-MINUTE       PIC 9(02).
               10  WS-CURRENT-SECOND       PIC 9(02).
               10  WS-CURRENT-MILLISECONDS PIC 9(02).

       01  WS-FIELDS-CALCULADO. 
           05 WS-TOTAL-EMP-LEIDOS              PIC 9(10).
           05 WS-TOTAL-EMP-GRABADOS            PIC 9(10).
           05 WS-TOTAL-EMP-SUELDO-ACTUAL       PIC S9(9)V99.
           05 WS-TOTAL-EMP-SUELDO-INCREMENTADO PIC S9(9)V99.

       77  WS-INCREMENTO PIC 9V99 VALUE 1.05.
       77  WS-CALCULADO PIC X(9) VALUE "CALCULADO".

       01  WS-WORK-AREAS.
		   05  FILE-CHECK-KEY    PIC X(2).
           05 WS-NORMAL-NUMBER   PIC S9(9)V99.
           05 WS-COMP3-VALUE     PIC S9(9)V99 COMP-3.
           05 WS-ENABLE-DISPLAY  PIC X VALUE "N".
           05 WS-SEPARATOR       PIC X(30) VALUE ALL "*".
           05 WS-PAGE            PIC 9(3) VALUE 1.
           05 WS-COUNTER-LINES   PIC 9(2) VALUE ZEROS.
           77 WS-MAX-LINES  PIC 9(2) VALUE 60. 


       01  HEADING-LINE-TITLE-1.
           05 FILLER              PIC X(132)  VALUE ALL '*'.

       01  HEADING-LINE-TITLE-2.
           05 FILLER              PIC X VALUE '*'.
           05 FILLER              PIC X(49) VALUE SPACES.
           05 FILLER              PIC X(31)  
               VALUE 'Reporte de Empleados Eventuales'.
           05 FILLER              PIC X(38) VALUE SPACES.
           05 FILLER              PIC X(8) VALUE "Pagina: ".
           05 WS-HEADING-PAGE     PIC ZZ9.
           05 FILLER              PIC X VALUE SPACES.
           05 FILLER              PIC X VALUE '*'.

       01  HEADING-LINE.
           05 FILLER              PIC X(14) VALUE SPACES.
           05 FILLER              PIC X(15) VALUE 'NOMBRE EMPLEADO'.
           05 FILLER              PIC X(34) VALUE SPACES.
           05 FILLER              PIC X(9)  VALUE 'DIRECCION'.
           05 FILLER              PIC X(38) VALUE SPACES.
           05 FILLER              PIC X(6) VALUE 'SUELDO'.
           05 FILLER              PIC X(6) VALUE SPACES.
           05 HEAD-DATE.
              10 HEAD-DAY         PIC X(2).
              10 FILLER           PIC X VALUE '/'.
              10 HEAD-MONTH       PIC X(2).
              10 FILLER           PIC X VALUE '/'.
              10 HEAD-YEAR        PIC X(4).

       01  HEADING-LINE-2.
           05 FILLER              PIC X(5) VALUE SPACES.
           05 FILLER              PIC X(35) VALUE ALL '-'.
           05 FILLER              PIC X(10) VALUE SPACES.
           05 FILLER              PIC X(35)  VALUE ALL  '-'.
           05 FILLER              PIC X(21) VALUE SPACES.
           05 FILLER              PIC X(14) VALUE ALL '-'.
           05 FILLER              PIC X(8) VALUE SPACES.

       01  DETAIL-LINE.
           05 FILLER               PIC X(5)  VALUE SPACES.
           05 DET-NOMBRE-EMPLEADO PIC X(40).
           05 FILLER               PIC X(5)  VALUE SPACES.
           05 DET-DIRECCION           PIC X(55).
           05 FILLER               PIC X(1)  VALUE SPACES.
           05 DET-SUELDO      PIC $ZZ,ZZZ,ZZ9.99.
           05 FILLER               PIC X(2)  VALUE SPACES.

       01  TOTAL-CALCULADO.
           05 FILLER               PIC X(51)  VALUE SPACES.
           05 FILLER               PIC X(9)   VALUE "CALCULADO".
           05 FILLER               PIC X(72)  VALUE SPACES.

       01  TOTAL-LINE-EMP-LEIDOS.
           05 FILLER               PIC X(5)   VALUE SPACES.
           05 FILLER               PIC X(16)  VALUE SPACES.
           05 FILLER               PIC X(30)  VALUE SPACES.
           05 FILLER               PIC X(24)  
               VALUE 'Total Empleados Leidos: '.
           05 FILLER               PIC X(5)  VALUE SPACES.
           05 TOTAL-EMP-LEIDOS     PIC Z,ZZ9.
           05 FILLER               PIC X(48)  VALUE SPACES.
       
       01  TOTAL-LINE-EMP-GRABADOS.
           05 FILLER               PIC X(5)   VALUE SPACES.
           05 FILLER               PIC X(16)  VALUE SPACES.
           05 FILLER               PIC X(30)  VALUE SPACES.
           05 FILLER               PIC X(26)  
               VALUE 'Total Empleados Grabados: '.
           05 FILLER               PIC X(3)  VALUE SPACES.
           05 TOTAL-EMP-GRABADOS     PIC Z,ZZ9.
           05 FILLER               PIC X(50)  VALUE SPACES.
       
       01  TOTAL-LINE-SUELDO-ACTUAL.
           05 FILLER               PIC X(5)   VALUE SPACES.
           05 FILLER               PIC X(16)  VALUE SPACES.
           05 FILLER               PIC X(30)  VALUE SPACES.
           05 FILLER               PIC X(21)  
               VALUE 'Total Sueldo Actual: '.
           05 FILLER               PIC X(6)  VALUE SPACES.
           05 TOTAL-SUELDO-ACTUAL     PIC $Z,ZZZ,ZZZ,ZZ9.99.
           05 FILLER               PIC X(47)  VALUE SPACES.
       
       01  TOTAL-LINE-SUELDO-INCREMENTADO.
           05 FILLER               PIC X(5)   VALUE SPACES.
           05 FILLER               PIC X(16)  VALUE SPACES.
           05 FILLER               PIC X(30)  VALUE SPACES.
           05 FILLER               PIC X(27)  
               VALUE 'Total Sueldo Incrementado: '.
           05 TOTAL-SUELDO-INCREMENTADO PIC $Z,ZZZ,ZZZ,ZZ9.99.
           05 FILLER               PIC X(53)  VALUE SPACES.

       PROCEDURE DIVISION.

       0050-OPEN-FILE.
           OPEN INPUT EMPLOYEE-FILE.

           INITIALIZE WS-TOTAL-EMP-GRABADOS, WS-TOTAL-EMP-LEIDOS,
           WS-TOTAL-EMP-SUELDO-ACTUAL, WS-TOTAL-EMP-SUELDO-INCREMENTADO.

           OPEN OUTPUT PRINT-FILE.
           OPEN OUTPUT EMPLOYEE-OUTPUT-FILE.

           IF FILE-CHECK-KEY NOT= "00"
               DISPLAY "Error al leer el archivo, código: ", 
               FILE-CHECK-KEY
               PERFORM 0200-STOP-RUN
               END-IF.
           PERFORM 0100-PROCESS-RECORDS.
           PERFORM 0200-STOP-RUN.

       0100-PROCESS-RECORDS.
           
           PERFORM 0120-WRITE-HEADING-LINE-TITLE.
           PERFORM 0130-WRITE-HEADING-LINE.
           READ EMPLOYEE-FILE
                AT END SET ENDOF-INPUTFILE TO TRUE
                END-READ.
           PERFORM UNTIL ENDOF-INPUTFILE
              ADD 1 TO WS-TOTAL-EMP-LEIDOS
              MOVE SUELDO TO WS-NORMAL-NUMBER
              ADD WS-NORMAL-NUMBER TO WS-TOTAL-EMP-SUELDO-ACTUAL
              PERFORM 0105-PROCESS-RECORDS-DISPLAY
              IF TIPO-EMPLEADO="E"
                PERFORM 0110-PROCESS-RECORDS-TO-WRITE-EMP-E
              END-IF
   
              READ EMPLOYEE-FILE
              AT END SET ENDOF-INPUTFILE TO TRUE
              END-READ
           END-PERFORM.
           PERFORM 0150-WRITE-TOTAL-CALCULADO THRU 
                   0190-WRITE-TOTAL-LINE-SUELDO-INCREMENTADO.

       0105-PROCESS-RECORDS-DISPLAY.
           IF WS-ENABLE-DISPLAY="Y"
               DISPLAY EMPLEADO-ID
               DISPLAY NOMBRE-EMPLEADO
               DISPLAY DIRECCION
               DISPLAY SUELDO
               DISPLAY TIPO-EMPLEADO
               DISPLAY WS-SEPARATOR
           END-IF.

       0110-PROCESS-RECORDS-TO-WRITE-EMP-E.
           ADD 1 TO WS-TOTAL-EMP-GRABADOS.
           COMPUTE WS-NORMAL-NUMBER = SUELDO * WS-INCREMENTO.
           ADD WS-NORMAL-NUMBER TO WS-TOTAL-EMP-SUELDO-INCREMENTADO.
           PERFORM 0115-PROCESS-RECORDS-TO-WRITE-DISP
           MOVE NOMBRE-EMPLEADO TO DET-NOMBRE-EMPLEADO.
           MOVE DIRECCION TO DET-DIRECCION.
           MOVE SUELDO TO DET-SUELDO.
           PERFORM 0140-WRITE-DETAIL-LINE.
           PERFORM 0145-WRITE-DETAIL-OUTPUT-LINE.
           ADD 1 TO WS-COUNTER-LINES.
           IF WS-COUNTER-LINES GREATER WS-MAX-LINES then
               ADD 1 TO WS-PAGE
               PERFORM 0120-WRITE-HEADING-LINE-TITLE THRU 
                       0130-WRITE-HEADING-LINE
               MOVE 6 TO WS-COUNTER-LINES
           END-IF.


       0115-PROCESS-RECORDS-TO-WRITE-DISP.
           IF WS-ENABLE-DISPLAY = "Y"
               DISPLAY "SUELDO ACTUAL: ", SUELDO
               DISPLAY "SUELDO INCREMENTADO: ", WS-NORMAL-NUMBER
               DISPLAY WS-SEPARATOR
           END-IF.

       0120-WRITE-HEADING-LINE-TITLE.
      *************************************************************
      *     The following move commands get the current date      *
      *      and page for the report header.                      *
      *************************************************************.
           MOVE HEADING-LINE-TITLE-1 TO PRINT-LINE.
           IF WS-COUNTER-LINES NOT GREATER WS-MAX-LINES THEN
               WRITE PRINT-LINE
           ELSE
               WRITE PRINT-LINE AFTER ADVANCING 1 LINE
           END-IF.
      * AFTER ADVANCING 1 LINE.
           MOVE WS-PAGE TO WS-HEADING-PAGE.
           MOVE HEADING-LINE-TITLE-2 TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.
           MOVE HEADING-LINE-TITLE-1 TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.
           MOVE 6 TO WS-COUNTER-LINES.

       0130-WRITE-HEADING-LINE.
      *************************************************************
      *     The following move commands get the current date for  *
      *     the report header.                                    *
      *************************************************************
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA.
           MOVE WS-CURRENT-MONTH TO HEAD-MONTH.
           MOVE WS-CURRENT-DAY TO HEAD-DAY.
           MOVE WS-CURRENT-YEAR TO HEAD-YEAR.
           MOVE HEADING-LINE TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.
           MOVE HEADING-LINE-2 TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.

       0140-WRITE-DETAIL-LINE.
           MOVE DETAIL-LINE TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.

       0145-WRITE-DETAIL-OUTPUT-LINE.
           MOVE SUELDO TO WS-COMP3-VALUE.
           MOVE EMPLEADO-ID TO EMP-ID.
           MOVE NOMBRE-EMPLEADO TO NOMBRE-EMP.
           MOVE DIRECCION TO DIRECCION-EMP.
           MOVE SUELDO TO SUELDO-EMP.
           MOVE WS-COMP3-VALUE TO SUELDO-EMP.
           MOVE TIPO-EMPLEADO TO TIPO-EMP.
           MOVE WS-CALCULADO TO RESULTADO.
           WRITE EMPLOYEE-OUTPUT-DETAILS.
      * AFTER ADVANCING 1 LINE.

       0150-WRITE-TOTAL-CALCULADO.
           MOVE SPACES TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.
           MOVE TOTAL-CALCULADO TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.

       0160-WRITE-TOTAL-LINE-EMP-LEIDOS.
           MOVE WS-TOTAL-EMP-LEIDOS TO TOTAL-EMP-LEIDOS.
           MOVE TOTAL-LINE-EMP-LEIDOS TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.


       0170-WRITE-TOTAL-LINE-EMP-GRABADOS.
           MOVE WS-TOTAL-EMP-GRABADOS TO TOTAL-EMP-GRABADOS.
           MOVE TOTAL-LINE-EMP-GRABADOS TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.
       
       0180-WRITE-TOTAL-LINE-SUELDO-ACTUAL.
           MOVE WS-TOTAL-EMP-SUELDO-ACTUAL TO TOTAL-SUELDO-ACTUAL.
           MOVE TOTAL-LINE-SUELDO-ACTUAL TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.

       0190-WRITE-TOTAL-LINE-SUELDO-INCREMENTADO.
           MOVE WS-TOTAL-EMP-SUELDO-INCREMENTADO TO 
               TOTAL-SUELDO-INCREMENTADO.
           MOVE TOTAL-LINE-SUELDO-INCREMENTADO TO PRINT-LINE.
           WRITE PRINT-LINE AFTER ADVANCING 1 LINE.

       0200-STOP-RUN.
           CLOSE EMPLOYEE-FILE.
           CLOSE PRINT-FILE.
           CLOSE EMPLOYEE-OUTPUT-FILE
           STOP RUN.
           END PROGRAM EMPLOYEE-READ-FILE-CREATE-REP.
