--------------------------------------------------------------------------------
--  This file is part of SSDP package			        	      --
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
with SSDP.Services;
with SSDP.Command_Scheduling;

with Test_Utils;

with Get_Options; --https://github.com/sbianti/GetOptions

procedure Test_Service is
   use SSDP, Ada.Exceptions, Test_Utils;

   subtype SSDP_Service is Services.SSDP_Service;

   Service: SSDP_Service;

   Notify_Header: SSDP.Message_Header_Array :=
     (Services.To_US("Affaires: dentifrice"),
      Services.To_US("DIY: courses:rien"));

   Default_UUID: aliased constant String :=
     "uuid:822ccbbf-3aa6-44c2-80ef-0307f9673521";
   UUID_Value: access constant String;

   Default_Service_Type: aliased constant String := "chauffage:central";
   Service_Type_Value: access constant String;

   Default_Lifetime: constant Positive := 600;
   Lifetime_Value: Positive;

   Str: String(1..10);

   Lg: Natural;

   function Default_Initialization return SSDP_Service is
      use SSDP.Services;
   begin
      return Initialize(Service_Type => Service_Type_Value.all,
			Universal_Serial_Number => UUID_Value.all,
			Location => "",
			AL => "<http://halsensortester.mp.intel.com>",
			Cache_Control => Lifetime_Value,
			Expires => "");
   end Default_Initialization;

   procedure Default_Scheduling is
      use SSDP.Services, Ada.Text_IO;
   begin

      Service := Default_Initialization;

      Start_Listening;

      delay 0.2;
      Notify_Alive(Service, Notify_Header);
      Put_Line("Notify sent");

      delay 2.5;
      Notify_Alive(Service, Null_Header_Array);
      Put_Line("Second notify sent");

      Get_Line(Str, Lg);
      Put_Line("Bye bye !");
      Notify_Bye_Bye(Service);

      Stop_Listening;
   end Default_Scheduling;

   type Test_Options is (Batch, Service_Type, UUID, Bye_Bye_On_Exit, Lifetime);

   package Get_Test_Options is new Get_Options(Test_Options);
   use Get_Test_Options;

   EOL: constant Character := Character'Val(10);

   Help_Section: constant Unbounded_String := To_US("Options:");

   Help_Header: constant String :=
     "   Test program for SSDP services API" & EOL & EOL &
     "   usage: " & Command_Name &
     " [--batch «batch_line»][--service_type=oven:micro_wave]" &
     "[--bye_bye_on_exit] [--uuid [type:]unique_value]" & EOL & EOL &
     "     batch_line ≡ command [command ]*" & EOL &
     "     command ≡ command_name[,occurence_number[,random_time_range]" &
     "|[,fix_delay]]" & EOL &
     "       command_name ∈ {alive, sleep, byebye}" & EOL &
     "       occurence_number ≡ INTEGER_VALUE" & EOL &
     "       fix_delay ≡ DECIMAL_VALUE" & EOL &
     "       random_time_range ≡ lower_bound,upper_bound" & EOL &
     "       lower_bound and upper_bound ∈ DECIMAL_VALUES";

   Result: Option_Result_Array;

   Setting: Option_Setting_Array :=
     (Batch =>
	(Short_Name => No_Short_Name,
	 Needs_Value => Yes,
	 Short_Description =>
	   To_US("Five ALIVE sent with a random delay between 1”5 and 4”" &
		   EOL & "followed by a delay of 12”" & EOL &
		   "followed by three ALIVE spaced by a fix duration of 2”" &
		   EOL & "followed by two BYEBYE messages sent without delay"),
	 Value_Form =>
	   To_US("""alive,5,1.5,4.0 sleep,12.0 alive,3,2.0 byebye,2""")),

      Service_Type =>
	(Short_Name => 't',
	 Needs_Value => Yes,
	 Short_Description => To_US("The device service type to search for " &
				      "(previous value is the default)"),
	 Value_Form => To_US(Default_Service_Type)),

      Bye_Bye_On_Exit =>
	(Short_Name => 'b',
	 Needs_Value => Optional,
	 Short_Description =>
	   To_US("One, or many bye-byes are sent when exiting normaly"),
	 Value_Form => To_US("2")),

      UUID =>
	(Short_Name => 'i',
	 Needs_Value => Yes,
	 Short_Description => To_US("Set the uuid of the declared service"),
	 Value_Form => To_US("uuid:AAAABBBB-1111-2222-3333-CCDDEEFF0055")),

      Lifetime =>
	(Short_Name => 'l',
	 Needs_Value => Yes,
	 Short_Description => To_US("The amount of time (in second) after " &
				      "which the service is considered gone"),
	 Value_Form => To_US("300"))
     );

   package Scheduling is new Command_Scheduling(Service_Command_Name_Type);
   use Scheduling;

   Schedule: Schedule_Type;

   package Natural_IO is new Ada.Text_IO.Integer_IO(Positive);
   Last: Natural;
begin
   Result := Parse(Setting, Help_Header, "", Help_Sections =>
		     (Batch => Help_Section, others => Null_Unbounded_String));

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

   if Result(Lifetime).Is_Set then
      Natural_IO.Get(Result(Lifetime).Value.all, Lifetime_Value, Last);
   else
      Lifetime_Value := Default_Lifetime;
   end if;

   if not Result(Batch).Is_Set then
      Default_Scheduling;
   else
      Service := Default_Initialization;

      Services.Start_Listening;

      Schedule := Parse(Get_Value(Result(Batch), 1));

      Batch(Service, Notify_Header, Schedule);

      -- Waiting for user interaction:
      Ada.Text_IO.Get_Line(Str, Lg);

      if Result(Bye_Bye_On_Exit).Is_Set then
	 declare
	    Bye_Bye_Value: String := Get_Value(Result(Bye_Bye_On_Exit), 1);
	    Bye_Bye_Number: Positive;
	 begin

	    if Bye_Bye_Value = "" then
	       Bye_Bye_Number := 1;
	    else
	       Natural_IO.Get(Bye_Bye_Value, Bye_Bye_Number, Last);

	       if Bye_Bye_Number > 30 then
		  Pl_Warning("Bye-Bye number limited to 30 to avoid flooding");
		  Bye_Bye_Number := 30;
	       end if;
	    end if;

	    Pl_Debug("Sending" & Bye_Bye_Number'Img & " final ByeBye");
	    for I in 1..Bye_Bye_Number loop
	       SSDP.Services.Notify_Bye_Bye(Service);
	    end loop;

	 exception
	    when E: Ada.Text_IO.Data_Error =>
	       Pl_Error(Exception_Name(E) & ": Bad bye_bye value '" &
			  Bye_Bye_Value & "' should be a positive number." &
			  " Sending one ByeBye");
	       SSDP.Services.Notify_Bye_Bye(Service);
	    when E: others =>
	       Pl_Debug(Exception_Information(E));
	 end;
      end if;

      Services.Stop_Listening;
   end if;

exception
   when End_Of_Program_With_Help_Menu => Services.Stop_Listening;

   when E: Scheduling.Parsing_Error =>
      Pl_Error(Exception_Message(E));
      Services.Stop_Listening;
end Test_Service;
