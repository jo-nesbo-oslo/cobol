       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-CREATE-FILE.
       AUTHOR.     JARS.
      ***************************************************************
      *  This program reads a file containing employee person    *
      *   sales information and prints a report.                    *
      ***************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
       OBJECT-COMPUTER.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT EMPLOYEE-FILE ASSIGN TO 
               "./archivos/archive/EMPLOYEE_OUTPUT_COPY.DAT"
              FILE STATUS IS FILE-CHECK-KEY
              ORGANIZATION IS LINE SEQUENTIAL.
            SELECT EMPLOYEE-OUTPUT-FILE ASSIGN TO 
               "./archivos/entrada/EMPLOYEE_INPUT_COMP-3.DAT"
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

       FD EMPLOYEE-OUTPUT-FILE.
       
       01 EMPLOYEE-OUTPUT-DETAILS.
            05 EMP-ID        PIC 9(10).
            05 NOMBRE-EMP    PIC X(40).
            05 DIRECCION-EMP PIC X(60).
            05 SUELDO-EMP    PIC S9(9)V99 COMP-3.
            05 TIPO-EMP      PIC X.

       WORKING-STORAGE SECTION.

       77  WS-INCREMENTO PIC 9V99 VALUE 1.05.

       01  WS-WORK-AREAS.
		   05  FILE-CHECK-KEY    PIC X(2).
           05  WS-INCREASE       PIC 9V99 VALUE 1.05.
           05 WS-NORMAL-NUMBER   PIC S9(9)V99.
           05 WS-COMP3-VALUE    PIC S9(9)V99 COMP-3.


       PROCEDURE DIVISION.

       0050-OPEN-FILE.
           OPEN INPUT EMPLOYEE-FILE.
           INITIALIZE WS-NORMAL-NUMBER
           OPEN OUTPUT EMPLOYEE-OUTPUT-FILE.
           IF FILE-CHECK-KEY NOT= "00"
               DISPLAY "Error al leer el archivo, código: ", 
               FILE-CHECK-KEY
               PERFORM 0200-STOP-RUN
               END-IF.
           PERFORM 0100-PROCESS-RECORDS.
           PERFORM 0200-STOP-RUN.

       0100-PROCESS-RECORDS.

           READ EMPLOYEE-FILE
                AT END SET ENDOF-INPUTFILE TO TRUE
                END-READ.
           PERFORM UNTIL ENDOF-INPUTFILE
              MOVE SUELDO TO WS-NORMAL-NUMBER
              DISPLAY "SUELDO ACTUAL: ", WS-NORMAL-NUMBER
              DISPLAY "SUELDO INCREMENTADO: ", SUELDO
              PERFORM 0125-WRITE-DETAIL-OUTPUT-LINE

              DISPLAY EMPLEADO-ID
              DISPLAY NOMBRE-EMPLEADO
              DISPLAY DIRECCION
              DISPLAY SUELDO
              DISPLAY TIPO-EMPLEADO
              DISPLAY "*****"
   
              READ EMPLOYEE-FILE
              AT END SET ENDOF-INPUTFILE TO TRUE
              END-READ
           END-PERFORM.
  

       0125-WRITE-DETAIL-OUTPUT-LINE.
           MOVE SUELDO TO WS-COMP3-VALUE.
           MOVE EMPLEADO-ID TO EMP-ID.
           MOVE NOMBRE-EMPLEADO TO NOMBRE-EMP.
           MOVE DIRECCION TO DIRECCION-EMP.
           MOVE WS-COMP3-VALUE TO SUELDO-EMP.
           MOVE TIPO-EMPLEADO TO TIPO-EMP.
           WRITE EMPLOYEE-OUTPUT-DETAILS.

       0200-STOP-RUN.
           CLOSE EMPLOYEE-FILE.
           CLOSE EMPLOYEE-OUTPUT-FILE
           STOP RUN.
           END PROGRAM EMPLOYEE-CREATE-FILE.
