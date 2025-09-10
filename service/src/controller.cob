000000                     IDENTIFICATION DIVISION.
000010 PROGRAM-ID. FlagTransport-Controller.
000020
000030                     ENVIRONMENT DIVISION.
000040 CONFIGURATION SECTION.
000050 SPECIAL-NAMES.
000060
000070 INPUT-OUTPUT SECTION.
000080 FILE-CONTROL.
000090     SELECT User-Input ASSIGN TO KEYBOARD
000100     ORGANIZATION LINE SEQUENTIAL.
000110     SELECT OPTIONAL Warehouses ASSIGN TO "data/warehouses.dat"
000120     ORGANIZATION INDEXED
000130     ACCESS DYNAMIC
000140     RECORD KEY WA-Name.
000150     SELECT OPTIONAL Vehichles ASSIGN TO "data/vehichles.dat"
000160     ORGANIZATION INDEXED
000170     ACCESS DYNAMIC
000180     RECORD KEY VE-Number.
000190     SELECT OPTIONAL Cargoes ASSIGN TO "data/cargoes.dat"
000200     ORGANIZATION INDEXED
000210     ACCESS DYNAMIC
000220     RECORD KEY CA-ID
000230     ALTERNATE RECORD KEY CA-Warehouse WITH DUPLICATES
000240     ALTERNATE RECORD KEY CA-Vehichle WITH DUPLICATES.
000250     SELECT OPTIONAL Warehouse-Log ASSIGN TO Log-Path
000260     ORGANIZATION LINE SEQUENTIAL.
000270
000280                     DATA DIVISION.
000290 FILE SECTION.
000300 FD User-Input.
000310 01 User-Input-Line PIC X(100).
000320 FD Warehouses.
000330 01 Warehouse.
000340     05 WA-Name PIC X(50).
000350         88 WA-Name-Invalid VALUE SPACES.
000360     05 WA-Password PIC X(50).
000370     05 Location.
000380         10 X PIC 9(8).
000390         10 Y PIC 9(8).
000400 FD Vehichles.
000410 01 Vehichle.
000420     05 VE-Number PIC 9(15).
000430         88 VE-Number-Invalid VALUE ZERO.
000440     05 VE-Password PIC X(50).
000450     05 Location.
000460         10 X PIC 9(8).
000470         10 Y PIC 9(8).
000480 FD Cargoes.
000490 01 Cargo.
000500     05 CA-ID PIC X(30).
000510         88 CA-ID-Invalid VALUE SPACES.
000520     05 Origin PIC X(50).
000530     05 Dest PIC X(50).
000540     05 Note PIC X(100).
000550     05 Stored-State PIC 9.
000560         88 Stored VALUE 0.
000570         88 Loaded VALUE 1.
000580     05 CA-Warehouse PIC X(50).
000590     05 CA-Vehichle PIC 9(15).
000600 FD Warehouse-Log.
000610 01 Log-Entry PIC X(300).
000620 WORKING-STORAGE SECTION.
000630 01 Command PIC X(30).
000640     88 Command-Warehouse-Register VALUE "REGISTER WAREHOUSE".
000650     88 Command-Warehouse-Manage VALUE "MANAGE WAREHOUSE".
000660     88 Command-Cargo-Register VALUE "REGISTER CARGO".
000670     88 Command-Cargo-List VALUE "LIST CARGO".
000680     88 Command-Log-Read VALUE "READ LOG".
000690     88 Command-Vehichle-Register VALUE "REGISTER VEHICHLE".
000700     88 Command-Vehichle-Manage VALUE "MANAGE VEHICHLE".
000710     88 Command-Gateway-Key VALUE "GATEWAY KEY".
000720     88 Command-Cargo-Load VALUE "LOAD CARGO".
000730     88 Command-Cargo-Unload VALUE "UNLOAD CARGO".
000740     88 Command-Exit VALUE "EXIT".
000750 01 WS-Location.
000760     05 X PIC 9(8).
000770     05 Y PIC 9(8).
000780* Record for working warehouse data.
000790* This record also acts as a "currently managed warehouse".
000800 01 WS-Warehouse.
000810     05 WS-WA-Name PIC X(50).
000820         88 WS-WA-Name-Invalid VALUE SPACES.
000830     05 WS-WA-Password PIC X(50).
000840     05 Location.
000850         10 X PIC 9(8).
000860         10 Y PIC 9(8).
000870* Record for working vehichle data.
000880* This record also acts as a "currently managed vehichle".
000890 01 WS-Vehichle.
000900     05 WS-VE-Number PIC 9(15).
000910         88 WS-VE-Number-Invalid VALUE ZERO.
000920     05 WS-VE-Password PIC X(50).
000930     05 Location.
000940         10 X PIC 9(8).
000950         10 Y PIC 9(8).
000960 01 WS-Cargo.
000970     05 WS-CA-ID PIC X(30).
000980         88 WS-CA-ID-Invalid VALUE SPACES.
000990     05 Origin PIC X(50).
001000     05 Dest PIC X(50).
001010     05 Note PIC X(100).
001020     05 Stored-State PIC 9.
001030         88 Stored VALUE 0.
001040         88 Loaded VALUE 1.
001050     05 WS-CA-Warehouse PIC X(50).
001060     05 WS-CA-Vehichle PIC 9(15).
001070 01 Now-Date.
001080     05  Now-Year PIC 9999.
001090     05  Now-Month PIC 99.
001100     05  Now-Day PIC 99.
001110 01 Now-Time.
001120     05  Now-Hour PIC 99.
001130     05  Now-Min PIC 99.
001140     05  Now-Sec PIC 99.
001150 01 Log-Entry-Temp PIC X(300).
001160 01 Log-Path.
001170     05 Path-Chars PIC X
001180         OCCURS 10 TO 1000 DEPENDING ON Log-Path-Length.
001190 01 Log-Path-Length USAGE INDEX.
001200
001210 01 Gateway-Key-Index USAGE INDEX.
001220 01 Gateway-Key-Temp PIC 9(20).
001230 01 Gateway-Key PIC X(50).
001240
001250                     PROCEDURE DIVISION.
001260     SET WA-Name-Invalid TO TRUE
001270     SET WS-WA-Name-Invalid TO TRUE
001280     SET VE-Number-Invalid TO TRUE
001290     SET WS-VE-Number-Invalid TO TRUE
001300     SET CA-ID-Invalid TO TRUE
001310     SET WS-CA-ID-Invalid TO TRUE
001320     DISPLAY "$>> FlagTransport" END-DISPLAY
001330     DISPLAY "$>>       Controller" END-DISPLAY
001340     DISPLAY "$>>             Console" END-DISPLAY
001350     OPEN INPUT User-Input
001360     PERFORM FOREVER
001370
001380     READ User-Input END EXIT PERFORM END-READ
001390     MOVE User-Input-Line TO Command
001400     EVALUATE TRUE
001410
001420     WHEN Command-Warehouse-Register
001430     PERFORM ASK-WAREHOUSE-LOGIN THROUGH ASK-LOCATION
001440     IF WS-WA-Name-Invalid
001450         DISPLAY "!>> Registration failed" END-DISPLAY
001460         EXIT PERFORM CYCLE
001470     END-IF
001480     MOVE WS-Warehouse TO Warehouse
001490     MOVE WS-Location TO Location OF Warehouse
001500     OPEN I-O Warehouses
001510     WRITE Warehouse
001520         INVALID
001530         DISPLAY "!>> Registration failed" END-DISPLAY
001540         CLOSE Warehouses
001550         EXIT PERFORM CYCLE
001560     END-WRITE
001570     CLOSE Warehouses
001580     MOVE "Warehouse registered" TO Log-Entry
001590     PERFORM ADD-LOG-ENTRY
001600     DISPLAY "/>> Registration success" END-DISPLAY
001610
001620     WHEN Command-Warehouse-Manage
001630     PERFORM ASK-WAREHOUSE-LOGIN
001640     PERFORM LOAD-WAREHOUSE
001650     IF WS-WA-Name-Invalid
001660         OR WS-WA-Password IS NOT EQUAL WA-Password
001670         SET WS-WA-Name-Invalid TO TRUE
001680         DISPLAY "!>> Authentication failed" END-DISPLAY
001690     ELSE
001700         MOVE Warehouse TO WS-Warehouse
001710         DISPLAY "/>> Authentication success" END-DISPLAY
001720     END-IF
001730
001740     WHEN Command-Cargo-Register
001750     IF WS-WA-Name-Invalid
001760         DISPLAY "!>> Not authenticated" END-DISPLAY
001770         EXIT PERFORM CYCLE
001780     END-IF
001790     PERFORM ASK-CARGO-ID THRU ASK-CARGO-DETAILS
001800     MOVE WS-WA-Name TO Origin OF WS-Cargo
001810     SET Stored OF WS-Cargo TO TRUE
001820     MOVE WS-WA-Name TO WS-CA-Warehouse
001830     MOVE WS-Cargo TO Cargo
001840     OPEN I-O Cargoes
001850     WRITE Cargo
001860         INVALID
001870         DISPLAY "!>> Registration failed" END-DISPLAY
001880         SET WS-CA-ID-Invalid TO TRUE
001890         NOT INVALID
001900         DISPLAY "/>> Registration success" END-DISPLAY
001910     END-WRITE
001920     CLOSE Cargoes
001930
001940     WHEN Command-Cargo-List
001950     IF WS-WA-Name-Invalid
001960         DISPLAY "!>> Not authenticated" END-DISPLAY
001970         EXIT PERFORM CYCLE
001980     END-IF
001990     MOVE WS-WA-Name TO CA-Warehouse
002000     DISPLAY "/>> Cargo list start"
002010     OPEN I-O Cargoes
002020     START Cargoes KEY IS EQUAL CA-Warehouse
002030         INVALID CONTINUE
002040         NOT INVALID PERFORM FOREVER
002050         READ Cargoes AT END EXIT PERFORM END-READ
002060         IF CA-Warehouse IS NOT EQUAL WS-WA-Name
002070             EXIT PERFORM
002080         END-IF
002090         IF Loaded OF Cargo EXIT PERFORM CYCLE END-IF
002100         DISPLAY
002110             "*>> Cargo " FUNCTION TRIM(CA-ID)
002120             ": " FUNCTION TRIM(Note OF Cargo)
002130         END-PERFORM
002140     END-START
002150     DISPLAY "/>> Cargo list end"
002160     CLOSE Cargoes
002170
002180     WHEN Command-Log-Read
002190     IF WS-WA-Name-Invalid
002200         DISPLAY "!>> Not authenticated"
002210         EXIT PERFORM CYCLE
002220     END-IF
002230     DISPLAY "/>> Log start"
002240     PERFORM READ-LOG-ENTRIES
002250     DISPLAY "/>> Log end"
002260
002270     WHEN Command-Vehichle-Register
002280     PERFORM ASK-VEHICHLE-LOGIN
002290     PERFORM ASK-LOCATION
002300     IF WS-VE-Number-Invalid
002310         DISPLAY "!>> Registration failed" END-DISPLAY
002320         EXIT PERFORM CYCLE
002330     END-IF
002340     MOVE WS-Vehichle TO Vehichle
002350     MOVE WS-Location TO Location OF Vehichle
002360     OPEN I-O Vehichles
002370     WRITE Vehichle
002380         INVALID
002390         DISPLAY "!>> Registration failed" END-DISPLAY
002400         SET WS-VE-Number-Invalid TO TRUE
002410         NOT INVALID
002420         DISPLAY "/>> Registration success" END-DISPLAY
002430     END-WRITE
002440     CLOSE Vehichles
002450
002460     WHEN Command-Vehichle-Manage
002470     PERFORM ASK-VEHICHLE-LOGIN
002480     PERFORM LOAD-VEHICHLE
002490     IF WS-VE-Number-Invalid
002500         OR WS-VE-Password IS NOT EQUAL VE-Password
002510         SET WS-VE-Number-Invalid TO TRUE
002520         DISPLAY "!>> Authentication failed" END-DISPLAY
002530     ELSE
002540         MOVE Vehichle TO WS-Vehichle
002550         DISPLAY "/>> Authentication success" END-DISPLAY
002560     END-IF
002570
002580     WHEN Command-Gateway-Key
002590     IF WS-VE-Number-Invalid
002600         DISPLAY "!>> Not authenticated" END-DISPLAY
002610         EXIT PERFORM CYCLE
002620     END-IF
002630     PERFORM COMPUTE-GATEWAY-KEY
002640     DISPLAY "/>> Key: " Gateway-Key
002650
002660     WHEN Command-Cargo-Load
002670     IF WS-WA-Name-Invalid OR WS-VE-Number-Invalid
002680         DISPLAY "!>> Not authenticated" END-DISPLAY
002690         EXIT PERFORM CYCLE
002700     END-IF
002710     PERFORM ASK-CARGO-ID
002720     PERFORM LOAD-WAREHOUSE THROUGH LOAD-CARGO
002730     IF WS-CA-ID-Invalid OR NOT Stored OF Cargo
002740         OR CA-Warehouse IS NOT EQUAL WS-WA-Name
002750         *> We don't allow to load cargo which is already at its final destination
002760         OR CA-Warehouse IS EQUAL Dest OF Cargo
002770         OR Location OF Warehouse 
002780             IS NOT EQUAL Location OF Vehichle
002790         DISPLAY "!>> Invalid operation" END-DISPLAY
002800         EXIT PERFORM CYCLE
002810     END-IF
002820     SET Loaded OF Cargo TO TRUE
002830     ADD WS-VE-Number TO CA-Vehichle
002840     MOVE SPACES TO CA-Warehouse
002850     PERFORM UPDATE-CARGO
002860     DISPLAY "/>> Cargo loaded to the vehichle" END-DISPLAY
002870
002880     WHEN Command-Cargo-Unload
002890     IF WS-WA-Name-Invalid OR WS-VE-Number-Invalid
002900         DISPLAY "!>> Not authenticated" END-DISPLAY
002910         EXIT PERFORM CYCLE
002920     END-IF
002930     PERFORM ASK-CARGO-ID
002940     PERFORM LOAD-WAREHOUSE THROUGH LOAD-CARGO
002950     IF Location OF Warehouse IS NOT EQUAL Location OF Vehichle OR CA-Vehichle IS NOT EQUAL WS-VE-Number OR
002960         *> Don't allow to unload cargo back at the origin warehouse (to disallow spam of load/unload oprations)
002970         WS-WA-Name IS EQUAL Origin OF Cargo OR
002980         WS-CA-ID-Invalid OR NOT Loaded OF Cargo
002990         DISPLAY "!>> Invalid operation" END-DISPLAY
003000         EXIT PERFORM CYCLE
003010     END-IF
003020     STRING
003030         "Vehichle #" FUNCTION TRIM(WS-VE-Number)
003040         " delived cargo " FUNCTION TRIM(CA-ID)
003050         " from " FUNCTION TRIM(Origin OF Cargo)
003060         ": " FUNCTION TRIM(Note OF Cargo)
003070         INTO Log-Entry
003080     END-STRING
003090     PERFORM ADD-LOG-ENTRY
003100     SET Stored OF Cargo TO TRUE
003110     SUBTRACT WS-VE-Number FROM CA-Vehichle
003120     MOVE WS-WA-Name TO CA-Warehouse
003130     PERFORM UPDATE-CARGO
003140     DISPLAY "/>> Cargo unloaded from vehichle" END-DISPLAY
003150
003160     WHEN Command-Exit EXIT PERFORM
003170     WHEN OTHER DISPLAY "!>> Invalid command" END-EVALUATE
003180
003190     END-PERFORM.
003200 INPUT-CLOSED.
003210     DISPLAY "/>> Console closed" END-DISPLAY
003220     STOP RUN.
003230
003240 ASK-VEHICHLE-LOGIN.
003250     DISPLAY "?>> Vehichle number:" END-DISPLAY
003260     READ User-Input END GO TO INPUT-CLOSED END-READ
003270     MOVE User-Input-Line TO WS-VE-Number
003280     DISPLAY "?>> Vehichle password:" END-DISPLAY
003290     READ User-Input END GO TO INPUT-CLOSED END-READ
003300     MOVE User-Input-Line TO WS-VE-Password.
003310 ASK-WAREHOUSE-LOGIN.
003320     DISPLAY "?>> Warehouse name:" END-DISPLAY
003330     READ User-Input END GO TO INPUT-CLOSED END-READ
003340     *> Path traversals are an issue so this should deal with them.
003350     MOVE FUNCTION SUBSTITUTE(User-Input-Line, "/", "_", ".", "_")
003360         TO WS-WA-Name
003370     IF FUNCTION LENGTH(FUNCTION TRIM(WS-WA-Name)) LESS OR EQUAL 5
003380         SET WS-WA-Name-Invalid TO TRUE
003390     END-IF
003400     DISPLAY "?>> Warehouse password:" END-DISPLAY
003410     READ User-Input END GO TO INPUT-CLOSED END-READ
003420     MOVE User-Input-Line TO WS-WA-Password.
003430 ASK-LOCATION.
003440     DISPLAY "?>> Location:" END-DISPLAY
003450     READ User-Input END GO TO INPUT-CLOSED END-READ
003460     UNSTRING User-Input-Line
003470         DELIMITED BY ALL SPACES INTO
003480             X OF WS-Location
003490             Y OF WS-Location
003500     END-UNSTRING.
003510 ASK-CARGO-ID.
003520     DISPLAY "?>> Cargo ID:" END-DISPLAY
003530     READ User-Input END GO TO INPUT-CLOSED END-READ
003540     MOVE User-Input-Line TO WS-CA-ID.
003550 ASK-CARGO-DETAILS.
003560     DISPLAY "?>> Cargo destination:" END-DISPLAY
003570     READ User-Input END GO TO INPUT-CLOSED END-READ
003580     MOVE User-Input-Line TO Dest OF WS-Cargo
003590     DISPLAY "?>> Cargo note:" END-DISPLAY
003600     READ User-Input END GO TO INPUT-CLOSED END-READ
003610     MOVE User-Input-Line TO Note OF WS-Cargo.
003620
003630* These procedures load data by relevant key in working storage record.
003640* They clear key in working storage record on fail.
003650 LOAD-WAREHOUSE.
003660     MOVE WS-WA-Name TO WA-Name
003670     OPEN I-O Warehouses
003680     READ Warehouses
003690         INVALID SET WS-WA-Name-Invalid TO TRUE
003700     END-READ
003710     CLOSE Warehouses.
003720 LOAD-VEHICHLE.
003730     MOVE WS-VE-Number TO VE-Number
003740     OPEN I-O Vehichles
003750     READ Vehichles
003760         INVALID SET WS-VE-Number-Invalid TO TRUE
003770     END-READ
003780     CLOSE Vehichles.
003790 LOAD-CARGO.
003800     MOVE WS-CA-ID TO CA-ID
003810     OPEN I-O Cargoes
003820     READ Cargoes
003830         INVALID SET WS-CA-ID-Invalid TO TRUE
003840     END-READ
003850     CLOSE Cargoes.
003860
003870* This procedure takes data from file section.
003880* Procedure assumes existence of previous record.
003890 UPDATE-CARGO.
003900     IF CA-Vehichle IS NOT EQUAL ZEROS
003910         AND CA-Warehouse IS NOT EQUAL SPACES
003920         *> Data inconsitency
003930         CONTINUE
003940     ELSE
003950         OPEN I-O Cargoes
003960         REWRITE Cargo
003970         CLOSE Cargoes
003980     END-IF.
003990
004000* Procedures for warehouse log handling.
004010 SET-LOG-PATH.
004020     *> Most of the records/fields are fixed length in COBOL.
004030     *> COBOL deals with this by padding value to its max length with some padding character.
004040     *> This unfortunatelly means, that we would have uneccessary filenames as they would be always padded to max lenght.
004050     MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-WA-Name))
004060         TO Log-Path-Length
004070     STRING
004080         "logs/" FUNCTION TRIM(WS-WA-Name) ".log"
004090         INTO Log-Path
004100     END-STRING.
004110 ADD-LOG-ENTRY.
004120     ACCEPT Now-Date FROM DATE YYYYMMDD
004130     ACCEPT Now-Time FROM TIME
004140     MOVE SPACES TO Log-Entry-Temp
004150     STRING 
004160         "["
004170             Now-Year "/" Now-Month "/" Now-Day
004180         " "
004190             Now-Hour ":" Now-Min ":" Now-Sec
004200         "] "
004210         Log-Entry
004220         INTO Log-Entry-Temp
004230     END-STRING
004240     MOVE Log-Entry-Temp TO Log-Entry.
004250     PERFORM SET-LOG-PATH
004260     OPEN EXTEND Warehouse-Log
004270     WRITE Log-Entry
004280     CLOSE Warehouse-Log.
004290 READ-LOG-ENTRIES.
004300     PERFORM SET-LOG-PATH
004310     OPEN INPUT Warehouse-Log
004320     PERFORM FOREVER
004330         READ Warehouse-Log 
004340             AT END EXIT PERFORM
004350             NOT AT END DISPLAY FUNCTION TRIM(Log-Entry)
004360         END-READ
004370     END-PERFORM
004380     CLOSE Warehouse-Log.
004390
004400* Computes Gateway key for authenticated vehichle 
004410 COMPUTE-GATEWAY-KEY.
004420     PERFORM LOAD-VEHICHLE.
004430     PERFORM VARYING Gateway-Key-Index FROM 1 BY 1
004440         UNTIL Gateway-Key-Index > 50
004450         COMPUTE Gateway-Key-Temp =
004460             26 * FUNCTION ORD(VE-Password(Gateway-Key-Index:1))
004470             + Gateway-Key-Index * VE-Number
004480             + VE-Number / Gateway-Key-Index
004490         MOVE FUNCTION CHAR(
004500             66 + FUNCTION MOD(Gateway-Key-Temp, 26)
004510         ) TO Gateway-Key(Gateway-Key-Index:1)
004520     END-PERFORM.
004530