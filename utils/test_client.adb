--------------------------------------------------------------------------------
--  This file is part of SSDP package   				      --
--  									      --
--  Copyright © 2015 Sébastien Bianti					      --
--  									      --
--  This program is free software; you can redistribute it and/or modify      --
--  it under the terms of the GNU General Public License version 3 as	      --
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
with Ada.Exceptions;
with SSDP.Clients;
with SSDP.Command_Scheduling;

with Test_Utils;

with Get_Options; --https://github.com/sbianti/GetOptions

procedure Test_Client is
   use SSDP, Ada.Exceptions, Test_Utils;

   Client: Clients.SSDP_Client;

   Discover_Header: SSDP.Message_Header_Array :=
     (Clients.To_US("Toto: inutile:pardon"),
      Clients.To_US("Zak: important:rien"));

   Default_UUID: aliased constant String :=
     "uuid:0dbcf247-96ca-4d58-b3de-a22cd083125b";
   UUID_Value: access constant String;

   Default_Service_Type: aliased constant String := "sncf:TGV";
   Service_Type_Value: access constant String;

   Str: String(1..10);

   Lg: Natural;

   procedure Default_Scheduling is
      use SSDP.Clients, Ada.Text_IO;
   begin
      Client := Initialize(Service_Type_Value.all, UUID_Value.all);

      Start_Listening;

      delay 0.5;
      M_Search(Client, Discover_Header);
      Put_Line("First discover sent");

      delay 1.0;
      M_Search(Client, Null_Header_Array);
      Put_Line("Second discover sent");

      Get_Line(Str, Lg);

      Stop_Listening;
   end Default_Scheduling;

   type Test_Options is (Batch, Service_Type, UUID);

   package Get_Test_Options is new Get_Options(Test_Options);
   use Get_Test_Options;

   EOL: constant Character := Character'Val(10);

   Help_Section: constant Unbounded_String := To_US("Options:");

   Help_Header: constant String :=
     "   Test program for SSDP clients API" & EOL & EOL &
     "   usage: " & Command_Name &
     " [--batch «batch_line»][--service_type=oven:micro_wave]" &
     "[--uuid [type:]unique_value]" & EOL & EOL &
     "     batch_line ≡ command [command ]*" & EOL &
     "     command ≡ command_name[,occurence_number[,random_time_range]" &
     "|[,fix_delay]]" & EOL &
     "       command_name ∈ {discover, sleep}" & EOL &
     "       occurence_number ≡ INTEGER_VALUE" & EOL &
     "       fix_delay ≡ DECIMAL_VALUE" & EOL &
     "       random_time_range ≡ lower_bound,upper_bound" & EOL &
     "       lower_bound and upper_bound ∈ DECIMAL_VALUES";

   Result: Option_Result_Array;

   Setting: Option_Setting_Array :=
     (Batch =>
	(Short_Name => No_Short_Name,
	 Needs_Value => Yes,
	 Value_Form =>
	   To_US("""discover,3,0.5,2.5 sleep,10.0 discover,3,1.0,3.0"""),
	 Short_Description =>
	   To_US("Three DISCOVER sent with a random delay between 0”5 and 2”5" &
		   EOL & "followed by a delay of 10”" & EOL &
		   "followed by three DISCOVER spaced by a random duration " &
		   "between 1” and 3”")),

      Service_Type =>
	(Short_Name => 't',
	 Needs_Value => Yes,
	 Value_Form => To_US(Default_Service_Type),
	 Short_Description => To_US("The device service type to search for " &
				      "(previous value is the default)")),

      UUID =>
	(Short_Name => 'i',
	 Needs_Value => Yes,
	 Short_Description => To_US("Set the uuid of the declared client"),
	 Value_Form => To_US("uuid:AAAABBBB-1111-2222-3333-CCDDEEFF0055"))
     );

   package Scheduling is new SSDP.Command_Scheduling(Client_Command_Name_Type);
   use Scheduling;

   Schedule: Schedule_Type;

begin
   Result := Parse(Setting, Help_Header, "", Help_Sections =>
		     (Batch => Help_Section,
		      others => Null_Unbounded_String));

   if Result(Service_Type).Is_Set then
      Service_Type_Value := new String'(Get_Value(Result(Service_Type), 1));
   else
      Service_Type_Value := Default_Service_Type'Access;
   end if;

   if Result(UUID).Is_Set then
      UUID_Value := new String'(Get_Value(Result(UUID), 1));
   else
      UUID_Value := Default_UUID'Access;
   end if;

   if not Result(Batch).Is_Set then
      Default_Scheduling;
   else
      Client := Clients.Initialize(Service_Type_Value.all, UUID_Value.all);

      Clients.Start_Listening;

      Schedule := Parse(Get_Value(Result(Batch), 1));

      Batch(Client, Discover_Header, Schedule);

      -- Waiting for user interaction:
      Ada.Text_IO.Get_Line(Str, Lg);
      Clients.Stop_Listening;
   end if;

exception
   when End_Of_Program_With_Help_Menu =>
      Clients.Stop_Listening;

   when E: Scheduling.Parsing_Error =>
      Pl_Error(Exception_Message(E));
      Clients.Stop_Listening;
end Test_Client;
