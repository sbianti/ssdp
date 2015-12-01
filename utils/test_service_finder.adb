--------------------------------------------------------------------------------
--  Test_Service_Finder 						      --
--  									      --
--  Copyright © 2015 Sébastien Bianti					      --
--  									      --
--  This program is free software; you can redistribute it and/or modify      --
--  it under the terms of the GNU General Public License version 2 as	      --
--  published by the Free Software Foundation.				      --
--  									      --
--  This program is distributed in the hope that it will be useful,	      --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of	      --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the	      --
--  GNU General Public License for more details.			      --
--  									      --
--  You should have received a copy of the GNU General Public License	      --
--  along with this program; if not, write to the Free Software	              --
--  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA --
--------------------------------------------------------------------------------

with Ada.Text_IO;
with SSDP.Service_Finder;

with Get_Options; --https://github.com/sbianti/GetOptions

procedure Test_Service_Finder is
   use SSDP.Service_Finder, Ada.Text_IO;

   procedure Default_Scheduling is
      Discover_Header: SSDP.Message_Header_Array :=
	(To_US("Toto: inutile:pardon"), To_US("Zak: important:rien"));

      Null_Header: SSDP.Message_Header_Array(1..0);

      UUID: constant String := "uuid:0dbcf247-96ca-4d58-b3de-a22cd083125b";

      Device: Finder_Device_Type := Initialize_Device("sncf:TGV", UUID);
      Str: String(1..10);
      Lg: Natural;
   begin
      Start_Listening;

      delay 0.5;
      M_Search(Device, Discover_Header);
      Put_Line("First discover sent");

      delay 1.0;
      M_Search(Device, NULL_Header);
      Put_Line("Second discover sent");

      Get_Line(Str, Lg);

      Stop_Listening;
   end Default_Scheduling;

begin
   Default_Scheduling;
end Test_Service_Finder;
