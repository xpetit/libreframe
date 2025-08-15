-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      testapi_sql.adb
--  @copyright See authors list below and v22.copyrights file
--  @licence   GPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - API test program
--
--  @description
--  Build application and documentation
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Ada.Containers.Vectors;

with LibreFrame.Tio;

package body TestApi_Sql is

   procedure Run is

      use LibreFrame.Sql; -- for Sql.Database_Brand operators

      Result : Integer;
      --Result_Status : Sql.Database_Status;

      Money_Value : Money := 100.01;
      Bigint_Value : Long_Long_Integer;

      Open_Result : Database_Status_Type;
      Database_Record : Sql.Database_List_Type;

      DB_Test_Name_1 : String := "Db_Test_1";
      DB_Test_Name_2 : String := "Db_Test_2";

      TBL_Test_Name : String := "Tbl_Test";

      -------------------------------------------------------------------------
      procedure T1 is
      begin
         Log.New_Line;
         Tio.Put_Line ("Money is delta 0.01 digits 10 - Money'Image (Money'Last): " & From_Latin_1 (Money'Image (Money'Last)));
         Log.Info ("From_Money_To_DB (Money_Value = 100.01): " & From_Money_To_DB (Money_Value));
         Log.New_Line;
         Money_Value := -100.01;
         Log.Info ("From_Money_To_DB (Money_Value = -100.01): " & From_Money_To_DB (Money_Value));
         Bigint_Value := -10001;
         Log.Info (Bigint_Value);
         Log.Info (To_Money (Bigint_Value));
         Log.New_Line;
         Money_Value := 0.0;
         Log.Info (Money_Value);
         Log.Info ("1234567890");
         Log.Info (From_Latin_1 (Money'Image (Money_Value)));
         Log.Info (From_Latin_1 (Money'Image (-1.1)));
         Log.New_Line;
         Log.Info ("123: " & From_Money_To_DB ("123"));
         Log.Info ("123.: " & From_Money_To_DB ("123."));
         Log.Info ("123.1: " & From_Money_To_DB ("123.1"));
         Log.Info ("123.12: " & From_Money_To_DB ("123.12"));
         Log.Info ("123.123456: " & From_Money_To_DB ("123.123456"));
         Log.New_Line;
         Log.Info (".1: " & From_Money_To_DB (".1"));
         Log.Info (".12: " & From_Money_To_DB (".12"));
         Log.Info (".123456: " & From_Money_To_DB (".123456"));
         Log.Info ("-0.123456: " & From_Money_To_DB ("-0.123456"));
         Log.Info ("-.123456: " & From_Money_To_DB ("-.123456"));
         Log.Info ("-.: " & From_Money_To_DB ("-."));
         Log.Info ("-: " & From_Money_To_DB ("-"));
         Log.New_Line;
         Log.Info ("000: " & From_DB_To_Money_String ("000"));
         Log.Info ("001: " & From_DB_To_Money_String ("001"));
         Log.Info ("012: " & From_DB_To_Money_String ("012"));
         Log.Info ("12312: " & From_DB_To_Money_String ("12312"));
         Log.Info ("-001: " & From_DB_To_Money_String ("-001"));
         Log.Info ("-012: " & From_DB_To_Money_String ("-012"));
         Log.Info ("-12312: " & From_DB_To_Money_String ("-12312"));
         Log.New_Line;
      end T1;

      -------------------------------------------------------------------------
      procedure T2_T3_T4_T5 is
      begin

         Sql.Schema_Load (Sql.Schema_Table_Name,        TBL_Test_Name, Comment => "Clusters table", Version => "1.0");
         Sql.Schema_Load (Sql.Schema_Column_Name,       "Number", "INTEGER", "Cluster number (1) 1...240", "1.0");
         Sql.Schema_Load (Sql.Schema_Table_Constraint,  "Number", "PRIMARY KEY","1.0");
         Sql.Schema_Load (Sql.Schema_Column_Constraint, "Number", "NOT NULL","1.0");
         Sql.Schema_Load (Sql.Schema_Column_Constraint, "Number", "AUTO_INCREMENT","1.0");

         Sql.Schema_Load (Sql.Schema_Column_Name,       "Key_Name",            "TEXT",    "Cluster key name", "1.0");
         Sql.Schema_Load (Sql.Schema_Column_Name,       "Key_Private",         "BLOB",    "Cluster private key", "1.0");
         Sql.Schema_Load (Sql.Schema_Column_Name,       "Key_Public",          "BLOB",    "Cluster public key", "1.0");
         Sql.Schema_Load (Sql.Schema_Column_Name,       "Test_Varchar",        "VARCHAR(30)",    "Supervisor instance", "1.2");
         Sql.Schema_Load (Sql.Schema_Column_Name,       "Test_Varchar2",        "VARCHAR(30)",    "Supervisor instance2", "1.3");
         Sql.Schema_Load (Sql.Schema_Column_Name,       "Supervisor_Instance", "INTEGER",        "Supervisor instance", "1.1");
         Sql.Schema_Load (Sql.Schema_Column_Name,       "Test_Decimal",        "DECIMAL(11,2)", "Decimal test", "1.3");
         Sql.Schema_Load (Sql.Schema_Column_Name,       "Comment",             "TEXT",           "Comment", "1.0");

         --  Sql.Schema_Load (Sql.Column_Name, "Thingy1", "TEXT", "Thingy1 created later", "1.1"); -- Upgrade schema test
         --  Sql.Schema_Load (Sql.Column_Name, "Thingy2", "TEXT", "Thingy2 created later", "1.1"); -- Upgrade schema test

         Sql.Schema_Load (Sql.Schema_Index_Name,        "Idx_Test_Number", "Number");
         Sql.Schema_Load (Sql.Schema_Index_Constraint,  "Idx_Test_Number", "UNIQUE");

         if Open_Result = Open_Need_Update then
            Sql.Schema_Update (DB_Test_Name_1);
         end if;

         if Open_Result = Open_Need_Update or
           Open_Result = Open_Success then

            Log.Info ("Get_Version: " & Sql.Get_Version (DB_Test_Name_1));

            Log.Info ("Clear table");
            Sql.Clear_Table (DB_Test_Name_1, TBL_Test_Name);

            Log.Info ("Insert 15 rows");
            for I in 1..15 loop
               Sql.Insert (DB_Test_Name_1, TBL_Test_Name,
                           "Key_Name~Name record " & Trim_Left(To_String (I)) & "^" &
                             "Key_Private~Private record " & Trim_Left(To_String (I)) & "^" &
                             "Key_Public~Public record " & Trim_Left(To_String (I)) & "^" &
                             "Supervisor_Instance~" & Trim_Left(To_String (I)) & "^" &
                             "Test_Decimal~" & Trim_Left(To_String (I)) & "." & Trim_Left(To_String (I)) & "^" &
                             "Comment~Comment record " & Trim_Left(To_String (I)));
            end loop;

            Log.Info ("Delete row 10");
            Sql.Delete (DB_Test_Name_1, TBL_Test_Name,"Key_Name = 'Name record 10'");

            Log.Info ("Update rows 11 to 15");
            for I in 11..15 loop
               Sql.Update (DB_Test_Name_1, TBL_Test_Name,
                           "Key_Name~Name RECORD " & Trim_Left(To_String (I)) & "^" &
                           "Key_Private~Private RECORD " & Trim_Left(To_String (I)) & "^" &
                           "Key_Public~Public RECORD " & Trim_Left(To_String (I)) & "^" &
                           "Supervisor_Instance~" & Trim_Left(To_String (I+10)) & "^" &
                           "Test_Decimal~" & Trim_Left(To_String (I)) & "." & Trim_Left(To_String (I)) & "^" &
                           "Comment~Comment RECORD " & Trim_Left(To_String (I+10)),
                           "Key_Name = 'Name record " & Trim_Left(To_String (I)) & "'");
            end loop;
            Log.New_Line;

            Log.Info ("Test with empty recordsets");
            Log.Info ("Theses consecutive calls should not raise exceptions");
            Sql.Query (DB_Test_Name_1, "BEGIN");
            Sql.Query (DB_Test_Name_1, "SAVEPOINT SV_Index_Exists");
            Result := Sql.Query (DB_Test_Name_1, "SELECT * FROM " & TBL_Test_Name);
            Sql.Query (DB_Test_Name_1, "COMMIT");
            Log.Info ("Result for an existant index (must be 1): " & Image (Result));
            Log.New_Line;

            Log.Info ("Test using Query to insure good freeing of previous results");
            Log.Info ("Theses 10 consecutive Execute_Query calls should not raise exception");
            for I in 1 .. 10 loop
               Sql.Query (DB_Test_Name_1, "SELECT * FROM " & TBL_Test_Name);
            end loop;
            Log.New_Line;

            Field_Display (Sql.Read (DB_Test_Name_1, TBL_Test_Name, "Number, Key_Name, Key_Private, Supervisor_Instance, Comment"),
                           CD, RD, "Number, Key Name, Key Private, Supervisor instance, Comment");
            Log.New_Line;

            Log.Info ("Search - Existing 'Name RECORD 11' (must be TRUE): " &
                        Image (Sql.Search (DB_Test_Name_1, TBL_Test_Name, "Key_Name = 'Name RECORD 11'")));
            Log.Info ("Search - Non existing 'Name record 11' (must be FALSE): " &
                        Image (Sql.Search (DB_Test_Name_1, TBL_Test_Name, "Key_Name = 'Name RECORD **'")));
            Log.New_Line;

            Log.Info ("Table_Exists - Existing table (must be TRUE): " &
                        Image (Sql.Is_Table_Exists (DB_Test_Name_1, TBL_Test_Name)));
            Log.Info ("Table_Exists - Non existing table (must be FALSE): " &
                        Image (Sql.Is_Table_Exists (DB_Test_Name_1, "Tbl_non_existing")));
            Log.New_Line;

            Log.Info ("Index_Exists - Existing index (must be TRUE): " &
                        Image (Sql.Is_Index_Exists (DB_Test_Name_1, TBL_Test_Name, "Idx_Test_Number")));
            Log.Info ("Index_Exists - Non existing index (must be FALSE): " &
                        Image (Sql.Is_Index_Exists (DB_Test_Name_1, TBL_Test_Name, "Non_existing")));
            Log.New_Line;

            Log.Info ("Column_Exists - Existing column (must be TRUE): " &
                        Image (Sql.Is_Column_Exists (DB_Test_Name_1, TBL_Test_Name, "Key_Private")));
            Log.Info ("Column_Exists - Non existing column (must be FALSE): " &
                        Image (Sql.Is_Column_Exists (DB_Test_Name_1, TBL_Test_Name, "Non_existing")));
            Log.New_Line;

            Log.Info ("Row_Count (must be 14 as row 10 has been deleted): " &
                        Image (Sql.Count_Rows (DB_Test_Name_1, TBL_Test_Name)));
            Log.New_Line;

            Log.Info ("Set_Config - Write Test_Parameter with value 1: ");
            Sql.Set_Config ("Test_Parameter", "1");
            Log.Info ("Get_Config - Read Test_Parameter: " & Sql.Get_Config ("Test_Parameter"));
            Log.New_Line;

            if Sql.Get_Database_Properties (DB_Test_Name_1, Database_Record) then
               Log.Info ("Get_Properties - by database name: " & Database_Record.Name);
            end if;
            if (Sql.Get_Database_Properties (1, Database_Record)) then
               Log.Info ("Get_Properties - by index number: " & Database_Record.Name);
            end if;

         end if;
      end T2_T3_T4_T5;

   begin

      -------------------------------------------------------------------------
      Log.Set_Task ("SQL T1");
      Log.Title ("Money conversion to and from Long_Long_Integer");

      -- T1;

      ----------------------------------------------------------------------------
      Log.Set_Task ("SQL T2");
      Log.Title ("SQLite tests");
      Log.New_Line;

      --  Open_Result := Sql.Open_Database (SQLite, "file:" & DB_Test_Name_1 & ".db", "1.3");
      --  -- Upgrade schema test
      --  --  Open_Result := Sql.Open (SQLite, "file:" & DB_Test_Name & ".db", "1.1");
      --
      --  T2_T3_T4_T5;
      --
      --  if Sql.Open_Database (SQLite, "file:" & DB_Test_Name_2 & ".db", "1.0") = Open_Need_Update then
      --     Sql.Schema_Load (Sql.Schema_Table_Name,        "Tbl_Test", Comment => "Clusters table");
      --     Sql.Schema_Load (Sql.Schema_Column_Name,       "Number", "INTEGER", "Cluster number (1) 1...240");
      --     Sql.Schema_Load (Sql.Schema_Column_Constraint, "Number", "UNIQUE");
      --     Sql.Schema_Load (Sql.Schema_Table_Constraint,  "Number", "PRIMARY KEY");
      --
      --     Sql.Schema_Load (Sql.Schema_Column_Name,       "Key_Name", "TEXT", "Cluster key name");
      --
      --     Sql.Schema_Update (DB_Test_Name_2);
      --     if Sql.Get_Database_Properties (DB_Test_Name_2, Database_Record) then
      --        Log.Info ("Get_Properties - by database name: " & Database_Record.Name);
      --     end if;
      --     if (Sql.Get_Database_Properties (2, Database_Record)) then
      --        Log.Info ("Get_Properties - by index number: " & Database_Record.Name);
      --     end if;
      --  end if;
      --  Log.New_Line;
      --
      --  --  Close all opened databases
      --  Sql.Close_Database (DB_Test_Name_1);
      --  Sql.Close_Database (DB_Test_Name_2);
      --  Log.New_Line;

      ----------------------------------------------------------------------------
      --  Log.Set_Task ("SQL T3");
      --  Log.Title ("MySQL tests");
      --  Log.New_Line;
      --
      --  Open_Result := Sql.Open_Database (MySQL, "db:" & DB_Test_Name_1 & "?host=rs3&port=3306&user=Db_Test_1user&password=Db_Test_1pwd", "1.3");
      --  -- Upgrade schema test
      --  --  Open_Result := Sql.Open (MySQL, "db:" & DB_Test_Name & "?host=rs3&port=3306&user=Db_Test_1usere&password=Db_Test_1pwd", "1.0");
      --
      --  T2_T3_T4_T5;
      --
      --  if Sql.Open_Database (MySQL, DB_Test_Name_2 & "?host=rs3&port=3306&user=Db_Test_2user&password=Db_Test_2pwd", "1.0") = Open_Need_Update then
      --     Sql.Schema_Load (Sql.Schema_Table_Name,        "Tbl_Test", Comment => "Clusters table");
      --     Sql.Schema_Load (Sql.Schema_Column_Name,       "Number", "INTEGER", "Cluster number (1) 1...240");
      --     Sql.Schema_Load (Sql.Schema_Column_Constraint, "Number", "UNIQUE");
      --     Sql.Schema_Load (Sql.Schema_Table_Constraint,  "Number", "PRIMARY KEY");
      --
      --     Sql.Schema_Load (Sql.Schema_Column_Name,       "Key_Name", "TEXT", "Cluster key name");
      --
      --     Sql.Schema_Update (DB_Test_Name_2);
      --     if Sql.Get_Database_Properties (DB_Test_Name_2, Database_Record) then
      --        Log.Info ("Get_Properties - by database name: " & Database_Record.Name);
      --     end if;
      --     if (Sql.Get_Database_Properties (2, Database_Record)) then
      --        Log.Info ("Get_Properties - by index number: " & Database_Record.Name);
      --     end if;
      --  end if;
      --  Log.New_Line;
      --
      --  --  Close all opened databases
      --  Sql.Close_Database (DB_Test_Name_1);
      --  --Sql.Close_Database (DB_Test_Name_2);
      --  Log.New_Line;

      ----------------------------------------------------------------------------
      Log.Set_Task ("SQL T4");
      Log.Title ("PostgreSQL tests");
      Log.New_Line;


      Open_Result := Sql.Open_Database (PostgreSQL, "db:" & DB_Test_Name_1 & "****?host=rs3****&port=5432&user=Db_Test_1usere&password=Db_Test_1pwd", "1.0");
      -- Upgrade schema test
      --  Open_Result := Sql.Open (PostgreSQL, "db:" & DB_Test_Name & "?host=rs3&port=3306&user=Db_Test_1usere&password=Db_Test_1pwd", "1.0");

      --  T2_T3_T4_T5;

      ----------------------------------------------------------------------------
      --  Log.Set_Task ("SQL T5");
      --  Log.Title ("Firebird tests");
      --  Log.New_Line;
      --
      --  Open_Result := Sql.Open (Firebird, "db:" & DB_Test_Name & "?host=rs3&port=3306&user=Db_Test_1usere&password=Db_Test_1pwd", "1.0");
      --  -- Upgrade schema test
      --  --  Open_Result := Sql.Open (Firebird, "db:" & DB_Test_Name & "?host=rs3&port=3306&user=Db_Test_1usere&password=Db_Test_1pwd", "1.0");
      --
      --  T2_T3_T4_T5;
      --
      ----------------------------------------------------------------------------
      --  Log.Set_Task ("SQL T6");
      --  Log.Title ("Non existent databse");
      --  Log.New_Line;
      --
      --  Log.Info ("Non existent database raising an exception");
      --
      --  if (Sql.Get_Properties (3, Database_Record)) then
      --     Log.Info ("Get_Properties - by index number: " & Database_Record.Name);
      --  else
      --     Log.Info ("Database record n°3 not found");
      --  end if;

   end Run;

-------------------------------------------------------------------------------
end TestApi_Sql;
-------------------------------------------------------------------------------
