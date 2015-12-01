--------------------------------------------------------------------------------
--  Test_Service_Provider						      --
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
with SSDP.Service_Provider;

procedure Test_Service_Provider is
   use Ada.Text_IO, SSDP.Service_Provider;

    procedure Default_Scheduling is
      Notify_Header: SSDP.Message_Header_Array :=
	(To_US("Affaires: dentifrice"), To_US("DIY: courses:rien"));

      UUID: constant String := "822ccbbf-3aa6-44c2-80ef-0307f9673521";

      Null_Header: SSDP.Message_Header_Array(1..0);

      Device: Service_Provider_Device_Type :=
	Initialize_Device(Service_Type => "chauffage:central",
			  Universal_Serial_Number => UUID,
			  Location => "",
			  AL => "<http://halsensortester.mp.intel.com>",
			  Cache_Control => "max-age = 600",
			  Expires => "");

      Str: String(1..10);
      Lg: Natural;
   begin
      Start_Listening;

      delay 0.2;
      Notify_Alive(Device, Notify_Header);
      Put_Line("Notify sent");

      delay 2.5;
      Notify_Alive(Device, Null_Header);
      Put_Line("Second notify sent");

      Get_Line(Str, Lg);
      Put_Line("Bye bye !");
      Notify_Bye_Bye(Device);

      Stop_Listening;
   end Default_Scheduling;

begin
   Default_Scheduling;
end Test_Service_Provider;
