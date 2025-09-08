                           IDENTIFICATION DIVISION.
       PROGRAM-ID. FlagTransport-Controller.

                           ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT User-Input ASSIGN TO KEYBOARD
           ORGANIZATION LINE SEQUENTIAL.
           SELECT OPTIONAL Warehouses ASSIGN TO "data/warehouses.dat"
           ORGANIZATION INDEXED
           ACCESS DYNAMIC
           RECORD KEY WA-Name
           ALTERNATE RECORD KEY Location OF Warehouse WITH DUPLICATES.
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
           SELECT OPTIONAL Warehouse-Log ASSIGN TO Log-Path
           ORGANIZATION LINE SEQUENTIAL.

                           DATA DIVISION.
       FILE SECTION.
       FD User-Input.
       01 User-Input-Line PIC X(100).
       FD Warehouses.
       01 Warehouse.
           05 WA-Name PIC X(50).
               88 WA-Name-Invalid VALUE SPACES.
           05 WA-Password PIC X(50).
           05 Location.
               10 X PIC 9(8).
               10 Y PIC 9(8).
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
       FD Warehouse-Log.
       01 Log-Entry PIC X(300).
       WORKING-STORAGE SECTION.
       01 Command PIC X(30).
           88 Command-Warehouse-Register VALUE "REGISTER WAREHOUSE".
           88 Command-Warehouse-Manage VALUE "MANAGE WAREHOUSE".
           88 Command-Cargo-Register VALUE "REGISTER CARGO".
           88 Command-Cargo-List VALUE "LIST CARGO".
           88 Command-Log-Read VALUE "READ LOG".
           88 Command-Vehichle-Register VALUE "REGISTER VEHICHLE".
           88 Command-Vehichle-Manage VALUE "MANAGE VEHICHLE".
           88 Command-Gateway-Key VALUE "GATEWAY KEY".
           88 Command-Cargo-Load VALUE "LOAD CARGO".
           88 Command-Cargo-Unload VALUE "UNLOAD CARGO".
           88 Command-Exit VALUE "EXIT".
       01 WS-Location.
           05 X PIC 9(8).
           05 Y PIC 9(8).
      * Record for working warehouse data.
      * This record also acts as a "currently managed warehouse".
       01 WS-Warehouse.
           05 WS-WA-Name PIC X(50).
               88 WS-WA-Name-Invalid VALUE SPACES.
           05 WS-WA-Password PIC X(50).
           05 Location.
               10 X PIC 9(8).
               10 Y PIC 9(8).
      * Record for working vehichle data.
      * This record also acts as a "currently managed vehichle".
       01 WS-Vehichle.
           05 WS-VE-Number PIC 9(15).
               88 WS-VE-Number-Invalid VALUE ZERO.
           05 WS-VE-Password PIC X(50).
           05 Location.
               10 X PIC 9(8).
               10 Y PIC 9(8).
       01 WS-Cargo.
           05 WS-CA-ID PIC X(30).
               88 WS-CA-ID-Invalid VALUE SPACES.
           05 Origin PIC X(50).
           05 Dest PIC X(50).
           05 Note PIC X(100).
           05 Stored-State PIC 9.
               88 Stored VALUE 0.
               88 Loaded VALUE 1.
           05 WS-CA-Warehouse PIC X(50).
           05 WS-CA-Vehichle PIC 9(15).
       01 Now-Date.
           05  Now-Year PIC 9999.
           05  Now-Month PIC 99.
           05  Now-Day PIC 99.
       01 Now-Time.
           05  Now-Hour PIC 99.
           05  Now-Min PIC 99.
           05  Now-Sec PIC 99.
       01 Log-Entry-Temp PIC X(300).
       01 Log-Path.
           05 Path-Chars PIC X
               OCCURS 10 TO 1000 DEPENDING ON Log-Path-Length.
       01 Log-Path-Length USAGE INDEX.

       01 Gateway-Key-Index USAGE INDEX.
       01 Gateway-Key-Temp PIC 9(20).
       01 Gateway-Key PIC X(50).

                           PROCEDURE DIVISION.
           SET WA-Name-Invalid TO TRUE
           SET WS-WA-Name-Invalid TO TRUE
           SET VE-Number-Invalid TO TRUE
           SET WS-VE-Number-Invalid TO TRUE
           SET CA-ID-Invalid TO TRUE
           SET WS-CA-ID-Invalid TO TRUE
           DISPLAY "$>> FlagTransport" END-DISPLAY
           DISPLAY "$>>       Controller" END-DISPLAY
           DISPLAY "$>>             Console" END-DISPLAY
           OPEN INPUT User-Input
           PERFORM FOREVER

           READ User-Input END EXIT PERFORM END-READ
           MOVE User-Input-Line TO Command
           EVALUATE TRUE

           WHEN Command-Warehouse-Register
           PERFORM ASK-WAREHOUSE-LOGIN THROUGH ASK-LOCATION
           IF WS-WA-Name-Invalid
               DISPLAY "!>> Registration failed" END-DISPLAY
               EXIT PERFORM CYCLE
           END-IF
           MOVE WS-Warehouse TO Warehouse
           MOVE WS-Location TO Location OF Warehouse
           OPEN I-O Warehouses
           WRITE Warehouse
               INVALID
               DISPLAY "!>> Registration failed" END-DISPLAY
               CLOSE Warehouses
               EXIT PERFORM CYCLE
           END-WRITE
           CLOSE Warehouses
           MOVE "Warehouse registered" TO Log-Entry
           PERFORM ADD-LOG-ENTRY
           DISPLAY "/>> Registration success" END-DISPLAY

           WHEN Command-Warehouse-Manage
           PERFORM ASK-WAREHOUSE-LOGIN
           PERFORM LOAD-WAREHOUSE
           IF WS-WA-Name-Invalid
               OR WS-WA-Password IS NOT EQUAL WA-Password
               SET WS-WA-Name-Invalid TO TRUE
               DISPLAY "!>> Authentication failed" END-DISPLAY
           ELSE
               MOVE Warehouse TO WS-Warehouse
               DISPLAY "/>> Authentication success" END-DISPLAY
           END-IF

           WHEN Command-Cargo-Register
           IF WS-WA-Name-Invalid
               DISPLAY "!>> Not authenticated" END-DISPLAY
               EXIT PERFORM CYCLE
           END-IF
           PERFORM ASK-CARGO-ID THRU ASK-CARGO-DETAILS
           MOVE WS-WA-Name TO Origin OF WS-Cargo
           SET Stored OF WS-Cargo TO TRUE
           MOVE WS-WA-Name TO WS-CA-Warehouse
           MOVE WS-Cargo TO Cargo
           OPEN I-O Cargoes
           WRITE Cargo
               INVALID
               DISPLAY "!>> Registration failed" END-DISPLAY
               SET WS-CA-ID-Invalid TO TRUE
               NOT INVALID
               DISPLAY "/>> Registration success" END-DISPLAY
           END-WRITE
           CLOSE Cargoes

           WHEN Command-Cargo-List
           IF WS-WA-Name-Invalid
               DISPLAY "!>> Not authenticated" END-DISPLAY
               EXIT PERFORM CYCLE
           END-IF
           MOVE WS-WA-Name TO CA-Warehouse
           DISPLAY "/>> Cargo list start"
           OPEN I-O Cargoes
           START Cargoes KEY IS EQUAL CA-Warehouse
               INVALID CONTINUE
               NOT INVALID PERFORM FOREVER
               READ Cargoes AT END EXIT PERFORM END-READ
               IF CA-Warehouse IS NOT EQUAL WS-WA-Name
                   EXIT PERFORM
               END-IF
               DISPLAY
                   "*>> Cargo "
                   FUNCTION TRIM(CA-ID)
                   ": "
                   FUNCTION TRIM(Note OF Cargo)
               END-PERFORM
           END-START
           DISPLAY "/>> Cargo list end"
           CLOSE Cargoes

           WHEN Command-Log-Read
           IF WS-WA-Name-Invalid
               DISPLAY "!>> Not authenticated"
               EXIT PERFORM CYCLE
           END-IF
           DISPLAY "/>> Log start"
           PERFORM READ-LOG-ENTRIES
           DISPLAY "/>> Log end"

           WHEN Command-Vehichle-Register
           PERFORM ASK-VEHICHLE-LOGIN
           PERFORM ASK-LOCATION
           IF WS-VE-Number-Invalid
               DISPLAY "!>> Registration failed" END-DISPLAY
               EXIT PERFORM CYCLE
           END-IF
           MOVE WS-Vehichle TO Vehichle
           MOVE WS-Location TO Location OF Vehichle
           OPEN I-O Vehichles
           WRITE Vehichle
               INVALID
               DISPLAY "!>> Registration failed" END-DISPLAY
               SET WS-VE-Number-Invalid TO TRUE
               NOT INVALID
               DISPLAY "/>> Registration success" END-DISPLAY
           END-WRITE
           CLOSE Vehichles

           WHEN Command-Vehichle-Manage
           PERFORM ASK-VEHICHLE-LOGIN
           PERFORM LOAD-VEHICHLE
           IF WS-VE-Number-Invalid
               OR WS-VE-Password IS NOT EQUAL VE-Password
               SET WS-VE-Number-Invalid TO TRUE
               DISPLAY "!>> Authentication failed" END-DISPLAY
           ELSE
               MOVE Vehichle TO WS-Vehichle
               DISPLAY "/>> Authentication success" END-DISPLAY
           END-IF

           WHEN Command-Gateway-Key
           IF WS-VE-Number-Invalid
               DISPLAY "!>> Not authenticated" END-DISPLAY
               EXIT PERFORM CYCLE
           END-IF
           PERFORM COMPUTE-GATEWAY-KEY
           DISPLAY "/>> Key: " Gateway-Key

           WHEN Command-Cargo-Load
           IF WS-WA-Name-Invalid OR WS-VE-Number-Invalid
               DISPLAY "!>> Not authenticated" END-DISPLAY
               EXIT PERFORM CYCLE
           END-IF
           PERFORM ASK-CARGO-ID
           PERFORM LOAD-WAREHOUSE THROUGH LOAD-CARGO
           IF WS-CA-ID-Invalid
               OR NOT Stored OF Cargo
               OR CA-Warehouse IS NOT EQUAL WS-WA-Name
               *> We don't allow to load cargo which is already at its final destination
               OR CA-Warehouse IS EQUAL Dest OF Cargo
               OR Location OF Warehouse 
                   IS NOT EQUAL Location OF Vehichle
               DISPLAY "!>> Invalid operation" END-DISPLAY
               EXIT PERFORM CYCLE
           END-IF
           SET Loaded OF Cargo TO TRUE
           MOVE WS-VE-Number TO CA-Vehichle
           PERFORM UPDATE-CARGO
           DISPLAY "/>> Cargo loaded to the vehichle" END-DISPLAY

           WHEN Command-Cargo-Unload
           IF WS-WA-Name-Invalid OR WS-VE-Number-Invalid
               DISPLAY "!>> Not authenticated" END-DISPLAY
               EXIT PERFORM CYCLE
           END-IF
           PERFORM ASK-CARGO-ID
           PERFORM LOAD-WAREHOUSE THROUGH LOAD-CARGO
           IF WS-CA-ID-Invalid
               OR NOT Loaded OF Cargo
               OR CA-Vehichle IS NOT EQUAL WS-VE-Number
               *> Don't allow to unload cargo at other warehouses than necessary
               OR WS-WA-Name IS NOT EQUAL Dest OF Cargo
               OR Location OF Warehouse
                   IS NOT EQUAL Location OF Vehichle
               DISPLAY "!>> Invalid operation" END-DISPLAY
               EXIT PERFORM CYCLE
           END-IF
           SET Stored OF Cargo TO TRUE
           MOVE WS-WA-Name TO CA-Warehouse
           PERFORM UPDATE-CARGO
           STRING
               "Vehichle #"
               FUNCTION TRIM(WS-VE-Number)
               " delived cargo "
               FUNCTION TRIM(CA-ID)
               ": "
               FUNCTION TRIM(Note OF Cargo)
               INTO Log-Entry
           END-STRING
           PERFORM ADD-LOG-ENTRY
           DISPLAY "/>> Cargo unloaded from vehichle" END-DISPLAY

           WHEN Command-Exit EXIT PERFORM
           WHEN OTHER DISPLAY "!>> Invalid command" END-EVALUATE

           END-PERFORM.
       INPUT-CLOSED.
           DISPLAY "/>> Console closed" END-DISPLAY
           STOP RUN.

       ASK-VEHICHLE-LOGIN.
           DISPLAY "?>> Vehichle number:" END-DISPLAY
           READ User-Input END GO TO INPUT-CLOSED END-READ
           MOVE User-Input-Line TO WS-VE-Number
           DISPLAY "?>> Vehichle password:" END-DISPLAY
           READ User-Input END GO TO INPUT-CLOSED END-READ
           MOVE User-Input-Line TO WS-VE-Password.
       ASK-WAREHOUSE-LOGIN.
           DISPLAY "?>> Warehouse name:" END-DISPLAY
           READ User-Input END GO TO INPUT-CLOSED END-READ
           *> Path traversals are an issue so this should deal with them.
           MOVE FUNCTION SUBSTITUTE(User-Input-Line, "/", "_", ".", "_")
               TO WS-WA-Name
           IF FUNCTION LENGTH(FUNCTION TRIM(WS-WA-Name)) LESS OR EQUAL 5
               SET WS-WA-Name-Invalid TO TRUE
           END-IF
           DISPLAY "?>> Warehouse password:" END-DISPLAY
           READ User-Input END GO TO INPUT-CLOSED END-READ
           MOVE User-Input-Line TO WS-WA-Password.
       ASK-LOCATION.
           DISPLAY "?>> Location:" END-DISPLAY
           READ User-Input END GO TO INPUT-CLOSED END-READ
           UNSTRING User-Input-Line
               DELIMITED BY ALL SPACES INTO
                   X OF WS-Location
                   Y OF WS-Location
           END-UNSTRING.
       ASK-CARGO-ID.
           DISPLAY "?>> Cargo ID:" END-DISPLAY
           READ User-Input END GO TO INPUT-CLOSED END-READ
           MOVE User-Input-Line TO WS-CA-ID.
       ASK-CARGO-DETAILS.
           DISPLAY "?>> Cargo destination:" END-DISPLAY
           READ User-Input END GO TO INPUT-CLOSED END-READ
           MOVE User-Input-Line TO Dest OF WS-Cargo
           DISPLAY "?>> Cargo note:" END-DISPLAY
           READ User-Input END GO TO INPUT-CLOSED END-READ
           MOVE User-Input-Line TO Note OF WS-Cargo.

      * These procedures load data by relevant key in working storage record.
      * They clear key in working storage record on fail.
       LOAD-WAREHOUSE.
           MOVE WS-WA-Name TO WA-Name
           OPEN I-O Warehouses
           READ Warehouses
               INVALID SET WS-WA-Name-Invalid TO TRUE
           END-READ
           CLOSE Warehouses.
       LOAD-VEHICHLE.
           MOVE WS-VE-Number TO VE-Number
           OPEN I-O Vehichles
           READ Vehichles
               INVALID SET WS-VE-Number-Invalid TO TRUE
           END-READ
           CLOSE Vehichles.
       LOAD-CARGO.
           MOVE WS-CA-ID TO CA-ID
           OPEN I-O Cargoes
           READ Cargoes
               INVALID SET WS-CA-ID-Invalid TO TRUE
           END-READ
           CLOSE Cargoes.

      * This procedure takes data from file section.
      * Procedure assumes existence of previous record.
       UPDATE-CARGO.
           OPEN I-O Cargoes
           REWRITE Cargo
           CLOSE Cargoes.

      * Procedures for warehouse log handling.
       SET-LOG-PATH.
           *> Most of the records/fields are fixed length in COBOL.
           *> COBOL deals with this by padding value to its max length with some padding character.
           *> This unfortunatelly means, that we would have uneccessary filenames as they would be always padded to max lenght.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-WA-Name))
               TO Log-Path-Length
           STRING
               "logs/"
               FUNCTION TRIM(WS-WA-Name)
               ".log"
               INTO Log-Path
           END-STRING.
       ADD-LOG-ENTRY.
           ACCEPT Now-Date FROM DATE YYYYMMDD
           ACCEPT Now-Time FROM TIME
           MOVE SPACES TO Log-Entry-Temp
           STRING 
               "["
                   Now-Year "/" Now-Month "/" Now-Day
               " "
                   Now-Hour ":" Now-Min ":" Now-Sec
               "] "
               Log-Entry
               INTO Log-Entry-Temp
           END-STRING
           MOVE Log-Entry-Temp TO Log-Entry.
           PERFORM SET-LOG-PATH
           OPEN EXTEND Warehouse-Log
           WRITE Log-Entry
           CLOSE Warehouse-Log.
       READ-LOG-ENTRIES.
           PERFORM SET-LOG-PATH
           OPEN INPUT Warehouse-Log
           PERFORM FOREVER
               READ Warehouse-Log 
                   AT END EXIT PERFORM
                   NOT AT END DISPLAY FUNCTION TRIM(Log-Entry)
               END-READ
           END-PERFORM
           CLOSE Warehouse-Log.

      * Computes Gateway key for authenticated vehichle 
       COMPUTE-GATEWAY-KEY.
           PERFORM LOAD-VEHICHLE.
           PERFORM VARYING Gateway-Key-Index FROM 1 BY 1
               UNTIL Gateway-Key-Index > FUNCTION LENGTH(VE-Password)
               COMPUTE Gateway-Key-Temp =
                   26 * FUNCTION ORD(VE-Password(Gateway-Key-Index:1))
                   + Gateway-Key-Index * VE-Number + Gateway-Key-Index
               MOVE FUNCTION CHAR(
                   66 + FUNCTION MOD(Gateway-Key-Temp, 26)
               ) TO Gateway-Key(Gateway-Key-Index:1)
           END-PERFORM.
