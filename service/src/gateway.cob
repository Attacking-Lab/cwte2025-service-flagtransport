                           IDENTIFICATION DIVISION.
       PROGRAM-ID. FlagTransport-Gateway.

                           ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT User-Input ASSIGN TO KEYBOARD
           ORGANIZATION LINE SEQUENTIAL.
           SELECT OPTIONAL Vehichles ASSIGN TO "data/vehichles.dat"
           ORGANIZATION INDEXED
           ACCESS DYNAMIC
           RECORD KEY VE-Number.
           SELECT OPTIONAL Cargoes ASSIGN TO "data/cargoes.dat"
           ORGANIZATION INDEXED
           ACCESS DYNAMIC
           RECORD KEY CA-ID
           ALTERNATE RECORD KEY CA-Warehouse WITH DUPLICATES
           ALTERNATE RECORD KEY CA-Vehichle WITH DUPLICATES.

                           DATA DIVISION.
       FILE SECTION.
       FD User-Input.
       01 User-Input-Line PIC X(500).
       FD Vehichles.
       01 Vehichle.
           05 VE-Number PIC 9(15).
               88 VE-Number-Invalid VALUE ZERO.
           05 VE-Password PIC X(50).
           05 Location.
               10 X PIC 9(8).
               10 Y PIC 9(8).
       FD Cargoes.
       01 Cargo.
           05 CA-ID PIC X(30).
               88 CA-ID-Invalid VALUE SPACES.
           05 Origin PIC X(50).
           05 Dest PIC X(50).
           05 Note PIC X(100).
           05 Stored-State PIC 9.
               88 Stored VALUE 0.
               88 Loaded VALUE 1.
           05 CA-Warehouse PIC X(50).
           05 CA-Vehichle PIC 9(15).
       WORKING-STORAGE SECTION.
       01 Gateway-Key-Index USAGE INDEX.
       01 Gateway-Key-Temp PIC 9(20).
       01 Gateway-Key PIC X(50).
       01 Sent-Gateway-Key PIC X(50).
           88 Unauthenticated VALUE SPACES.
       01 Sent-VE-Number PIC 9(15).
           88 Sent-VE-Number-Invalid VALUE ZERO.
       01 Verb PIC X.
           88 Update-Location VALUE "L".
           88 Retrieve-Status VALUE "S".
       01 Sent-Location.
           05 X PIC 9(8).
           05 Y PIC 9(8).

                           PROCEDURE DIVISION.
           SET VE-Number-Invalid TO TRUE
           SET CA-ID-Invalid TO TRUE
           OPEN INPUT User-Input
           PERFORM HANDLE-REQUEST.
       HANDLE-REQUEST.
           READ User-Input END EXIT PARAGRAPH END-READ
           UNSTRING User-Input-Line
               DELIMITED BY ":" INTO
               Sent-VE-Number
               Sent-Gateway-Key
               Verb
               X OF Sent-Location Y OF Sent-Location
           END-UNSTRING
           IF Sent-VE-Number-Invalid
               PERFORM SEND-ERROR
               EXIT PARAGRAPH
           END-IF
           ADD Sent-VE-Number TO VE-Number
           PERFORM LOAD-VEHICHLE
           IF VE-Number-Invalid
               PERFORM SEND-ERROR
               EXIT PARAGRAPH
           END-IF
           PERFORM COMPUTE-GATEWAY-KEY
           IF Sent-Gateway-Key IS NOT EQUAL Gateway-Key
               AND NOT Unauthenticated
               PERFORM SEND-ERROR
               EXIT PARAGRAPH
           END-IF
           EVALUATE TRUE
           WHEN Update-Location
               IF Unauthenticated
                   PERFORM SEND-ERROR
                   EXIT PARAGRAPH
               END-IF
               MOVE Sent-Location TO Location OF Vehichle
               PERFORM UPDATE-VEHICHLE
               PERFORM SEND-OK
           WHEN Retrieve-Status
               DISPLAY ":LOC:" X OF Vehichle ":" Y OF Vehichle ":"
               MOVE Sent-VE-Number TO CA-Vehichle
               OPEN I-O Cargoes
               START Cargoes KEY IS EQUAL CA-Vehichle
                   INVALID CONTINUE
                   NOT INVALID PERFORM FOREVER
                   READ Cargoes AT END EXIT PERFORM END-READ
                   IF Sent-VE-Number IS NOT EQUAL CA-Vehichle
                       EXIT PERFORM
                   END-IF
                   IF NOT Loaded EXIT PERFORM CYCLE END-IF
                   IF Unauthenticated
                       DISPLAY
                           ":CAR:" FUNCTION TRIM(CA-ID)
                           ":" FUNCTION TRIM(Dest)
                           ":"
                           ":"
                   ELSE
                       DISPLAY
                           ":CAR:" FUNCTION TRIM(CA-ID)
                           ":" FUNCTION TRIM(Dest)
                           ":" FUNCTION TRIM(Note OF Cargo)
                           ":"
                   END-IF
                   END-PERFORM
               END-START
               CLOSE Cargoes
               PERFORM SEND-OK
           WHEN OTHER
               PERFORM SEND-ERROR
           END-EVALUATE
           PERFORM FINISH-REQUEST.
       SEND-ERROR.
           DISPLAY ":ERR:".
       SEND-OK.
           DISPLAY ":OK:".
       FINISH-REQUEST.
           DISPLAY ":END:"
           STOP RUN.
       LOAD-VEHICHLE.
           OPEN I-O Vehichles
           READ Vehichles
               INVALID SET VE-Number-Invalid TO TRUE
           END-READ
           CLOSE Vehichles.
       UPDATE-VEHICHLE.
           OPEN I-O Vehichles
           REWRITE Vehichle
           CLOSE Vehichles.
       COMPUTE-GATEWAY-KEY.
           PERFORM LOAD-VEHICHLE.
           PERFORM VARYING Gateway-Key-Index FROM 1 BY 1
               UNTIL Gateway-Key-Index > 50
               COMPUTE Gateway-Key-Temp =
                   26 * FUNCTION ORD(VE-Password(Gateway-Key-Index:1))
                   + Gateway-Key-Index * VE-Number
                   + VE-Number / Gateway-Key-Index
               MOVE FUNCTION CHAR(
                   66 + FUNCTION MOD(Gateway-Key-Temp, 26)
               ) TO Gateway-Key(Gateway-Key-Index:1)
           END-PERFORM.
