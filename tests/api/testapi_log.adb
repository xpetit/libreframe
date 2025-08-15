-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      testapi_msg.ads
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
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

package body TestApi_Log is

   procedure Run is
      Set_Debug_State : Boolean := Log.Is_Debug;
   begin

      -------------------------------------------------------------------------
      Log.Set_Task ("LOG T1");
      Log.Title ("Log demo");
      Log.New_Line;

      Log.Info ("This is an information message");
      Log.Debug ("This first debug message should not appears");
      Log.Set_Debug;
      Log.Debug ("This is a debug message");
      Log.Set_Debug (False);
      Log.Debug ("This last debug message should not appears");
      Log.Error ("This is an error message");
      Log.Set_Disk (False);
      Log.Info ("This message should not be file logged (but displayed)");
      Log.Set_Disk (True);
      Log.Info ("This message should be truncated because it is really" &
                 "too long !");
      Log.Set_Task ("TASKTRUNCATED");
      Log.Title ("Task above and this title should be truncated it is" &
                   "really too long !");
      Log.New_Line;

      Log.Set_Debug (Set_Debug_State);

   end Run;

-------------------------------------------------------------------------------
end TestApi_Log;
-------------------------------------------------------------------------------
