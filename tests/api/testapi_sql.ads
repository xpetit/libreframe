-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      testapi_sql.ads
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

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

with UXStrings; use UXStrings;

with LibreFrame; use LibreFrame;
with LibreFrame.Log;
with LibreFrame.Sql;
--with LibreFrame.Sql.Drivers;
with LibreFrame.Tio;
with LibreFrame.Uxs; use LibreFrame.Uxs;

package TestApi_Sql is

   subtype String is UXString;

   procedure Run;

-------------------------------------------------------------------------------
end TestApi_Sql;
-------------------------------------------------------------------------------
