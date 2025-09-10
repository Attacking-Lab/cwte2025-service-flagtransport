Flag Transport
================

    <!-- The service name shouldn't be longer than 20 chars -->

Authors:
* Hackrrr

Categories:
* Misc

Overview
--------
A service for managing logitics/tranportation of cargo. Whole service is written in COBOL to make it clear that we are doing serious business...

Service has two parts - "controller" and "gateway". Both are exposed to the network and both are exposed on the same port - controller is running at TCP port 6256, gateway is running at UDP port 6256. Controller manages entities in the system - it allows to create/register warehouses, vehichles and cargoes and it also allows to load/unload cargoes to/from vehichles from/to warehouses. Gateway is endpoint intended for "customers" and vehichles - customers one can track location of given vehichle and vehichles can announce its location.


### Flag Store 1
Flags are stored as notes of delivered cargoes. Attack info name of the warehouse where is the "flag cargo" stored. 

Correct way to access this flag store is to be authenticated for the given warehouse which enables to:
1. List cargo present in the warehouse
2. Read warehouse log (which cotains info about delivered cargoes)


### Flag Store 2
Flags are stored as notes of cargo loaded in vehichle. Attack info number of the target vehichle. 

Correct way to access this flag store is to know the gateway key which can be used to obtain list of cargo of target vehichle.

Vulnerabilities
---------------

### Flag Store 1, Vuln 1
Code logic in contoller is flawed. "Authentication state" for warehouse is stored in `WS-WA-Name` variable - empty value mean that user is not authenticated, any other value idicates for which warehouse is user currently authenticated. "Unfortunatelly", `WS-WA-Name` is also used as "temporary" variable during registration but there is missing code to clear this upon registration failure. This makes possible to authenticate to any warehouse by trying to register warehouse with target warehouse's name (which fails, because such name already exist).  

* Difficulty: easy
* Discoverability: easy
* Patchability: easy
* Categories: misc

### Flag Store 1, Vuln 2
Controller incorrectly determines log path length as it considers only length of the warehouse name but ommites length of "logs/" (and ".log"). This results in log path "collision" for warehouses which differs only in last 5 characters and so we can access another warehouse log by creating warehouse only slightly different name.

* Difficulty: easy
* Discoverability: medium
* Patchability: easy
* Categories: misc

### Flag Store 2, Vuln 1
Gateway code flow is flawed and allows to send 2 request before "connection ends" (resp. program ends as we are talking about UDP here) when request ends with an error. This, together with incorrect handling of sent vehichle number, allows to desync internal state and thus get status of different vehihcle (which contains the cargo list) than we have key for.

Exploit steps for getting status of vehichle number `X` are as follow:
1. Create vehichle with number `A`
2. Create vehichle with number `B = A - X`
3. Send request with wrong key (to cause an error) as vehichle `B` - gateway doesn't stop the code execution but internal state remains as is
4. Send request for status as vehichle `X` but with valid key for vehichle `A` - gateway will incorrectly use vehichle `A` during key validation but it will use vehicle `X` when returning the vehichle status

* Difficulty: medium
* Discoverability: medium
* Patchability: easy
* Categories: misc

### Flag Store 2, Vuln 2
Gateway key generation is broken. Vehichle password is mixed into the key with multiplicative constant `26`... but number is then moduled by `26` and so vehichle password has no effect on the key. This makes possible to compute correct gateway key for any vehichle just from the vehichle number.

Code for generating the key based on vehichle number:
```py
def compute_gateway_key(vehichle_number: int) -> str:
    # COBOL code for key generation:
    #   PERFORM VARYING Gateway-Key-Index FROM 1 BY 1
    #       UNTIL Gateway-Key-Index > 50
    #       COMPUTE Gateway-Key-Temp =
    #           26 * FUNCTION ORD(VE-Password(Gateway-Key-Index:1))
    #           + Gateway-Key-Index * VE-Number
    #           + VE-Number / Gateway-Key-Index
    #       MOVE FUNCTION CHAR(
    #           66 + FUNCTION MOD(Gateway-Key-Temp, 26)
    #       ) TO Gateway-Key(Gateway-Key-Index:1)

    key = ""
    for i in range(1, 50 + 1):
        # Default rounding mode in COBOL in "truncation".
        # "chr(n)" in COBOL means "n-th" char in ASCII so we need to do `- 1`.
        key += chr(
            66 + ((i * vehichle_number + math.trunc(target_vehichle / i)) % 26) - 1
        )
    return key
```

* Difficulty: medium (due to quirks of COBOL)
* Discoverability: hard
* Patchability: medium
* Categories: crypto


### Flag Store 2, Vuln 3
Controller doens't check for correct vehichle when unloading the cargo. This is caused by the fact that COBOL ignores anything after column 73 and so condition `CA-Vehichle IS NOT EQUAL WS-VE-Number OR` is ignored by compiler. This allows us to get cargo from any vehichle.

Exploit steps:
1. Create warehouse
2. Create vehichle at same location as the warehouse
3. Get cargo ID(s) of target vehichle though unauthenticated gateway request
4. Try to unload target cargo ID
5. Read warehouse log or cargo list
6. Profit  

* Difficulty: easy
* Discoverability: medium
* Patchability: easy
* Categories: misc


Patches
-------

### Flag Store 1, Vuln 1
Correct way to patch this is to add either `SET WA-Name-Invalid TO TRUE` or `MOST SPACES TO WA-Name` to `WHEN Command-Warehouse-Register` branch after registration fails.

### Flag Store 1, Vuln 2
Add (at least) `+5` to `Log-Path-Length` when computing path length. Completely correct is to add `+9` but since path is never shown then nobody can notice missing `".log"` anyways... 

### Flag Store 2, Vuln 1
Easiest fix is to add `STOP RUN` at the end of `SEND-ERROR` procedure. "More correct" way would be to restructure the code but that is way more error prone than one would like.

### Flag Store 2, Vuln 2
Easiest way is to remove multiplication constant `26` used during key generation (`COMPUTE-GATEWAY-KEY` procedure) - key is not breakable by change code this way (since vehichle password will basically act as Vigenere cipher key).

Exploits can be also stoped by changing any parameter of the generation process as attackers expects original "algorithm" for key generation (and so the wouldn't be able to generate correct keys for modifed algorithm).

What makes this patch bit tricky is that it needs to be applied twice - once for `controller.cob` and once for `gateway.cob`.

### Flag Store 2, Vuln 3
Restructure code text so there is no code after column 73 (e.g. just put conditions on separate lines).


Work Packages
-------------

    <!--
    Brief description of each work package
    -->

### WP 1: Name

#### WP 1.1: Subtask

    <!--
    Short Description
    -->

#### WP 1.N: Subtask

...

### WP N: Name

    <!--
    Short Description
    -->

...
