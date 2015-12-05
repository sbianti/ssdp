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

with Ada.Streams;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Exceptions;
with Ada.Containers.Vectors;

with Gnat.Sockets;

with SSDP.Utils;

package body SSDP.Service_Provider is
   use SSDP.Utils, Ada.Containers;

   subtype SPD_Type is SSDP_Service;
   -- Each service_provider initialized has its USN kept in this list
   -- this allows to know if a discover message should be responded
   -- knowing its service_type.
   subtype Device_Count_Type is Count_Type range 1..100;
   package Device_Vectors is new Vectors(Device_Count_Type, SPD_Type);
   Device_Vector: Device_Vectors.Vector;

   type Device_Type_Array is array (Device_Count_Type range <>) of SPD_Type;

   function Matching_Devices(Service_Type: in String)
			    return Device_Type_Array is
      Count: Device_Count_Type'Base := 0;
   begin
      for I in 1..Device_Vector.Length loop
	 if Device_Vector.Element(I).Service_Type = To_US(Service_Type) then
	    Count := Count + 1;
	 end if;
      end loop;

      declare
	 Devices: Device_Type_Array(1..Count);
	 N: Device_Count_Type := 1;
      begin
	 if Count = 0 then
	    return Devices;
	 end if;

	 for I in 1..Device_Vector.Length loop
	    if Device_Vector.Element(I).Service_Type = To_US(Service_Type) then
	       Devices(N) := Device_Vector.Element(I);
	       N := N + 1;
	    end if;
	 end loop;

	 return Devices;
      end;
   end Matching_Devices;

   procedure Remove_Devices(USN: in Unbounded_String) is
      -- Removes all devices with this USN (could be many)
      Count: Natural := 0;
   begin
      for I in 1..Device_Vector.Length loop
	 if Device_Vector.Element(I).Universal_Serial_Number = USN then
	    Pl_Debug("Delete device:" & To_String
		       (Device_Vector.Element(I).Universal_Serial_Number)
		    );
	    Device_Vector.Delete(I);
	 end if;
      end loop;

      Pl_Debug("removed" & Count'Img & " device(s) with USN " & To_String(USN));
   end Remove_Devices;

   function Initialize_Device(Service_Type, Universal_Serial_Number,
				Location, AL, -- only one is required
				Cache_Control, Expires: String) -- dito
			     return SSDP_Service is

      function Service_Already_Exists(USN, ST: in String) return Boolean is
      begin
	 for I in 1..Device_Vector.Length loop
	    if Device_Vector(I).Universal_Serial_Number = USN and
	      Device_Vector(I).Service_Type = ST then
	       return True;
	    end if;
	 end loop;

	 return False;
      end Service_Already_Exists;

      Device: SSDP_Service;
   begin
      if Service_Type = "" or Universal_Serial_Number = "" then
	 raise Header_Malformed
	   with "service_type AND universal_serial_number should be not null";
      end if;

      if Location = "" and AL = "" then
	 raise Header_Malformed
	   with "Location or AL should be set";
      end if;

      if Cache_Control = "" and Expires = "" then
	 raise Header_Malformed
	   with "Cache_Control or Expires should be set";
      end if;

      if Service_Already_Exists(Universal_Serial_Number, Service_Type) then
	 raise Bad_Service
	   with "A service with USN=" & Universal_Serial_Number &
	   " and ST=" & Service_Type & " already exists";
      end if;

      Activate_Multicast_Connection;

      Device := (To_US(Service_Type), To_US(Universal_Serial_Number),
		 To_US(Location), To_US(AL),
		 To_US(Cache_Control), To_US(Expires));

      Device_Vector.Append(Device);
      Pl_Debug("Adding service:" & Universal_Serial_Number & " / " &
		 Service_Type);

      return Device;
   end Initialize_Device;

   procedure M_Search_Response(Device: in SSDP_Service;
			       USN_Requester: in String;
			       Other_Headers: in Message_Header_Array) is
      Required_Part, USN_Variable_Part: Unbounded_String;
   begin
      if Device.Service_Type = "" then raise Header_Malformed
	with "Header «ST» (Service Type) is missing";
      end if;

      if Device.Universal_Serial_Number = "" then raise Header_Malformed
	with "Header «USN» (Universal Service Type) is missing";
      end if;

      if Device.Cache_Control = "" and Device.Expires = "" then
	 raise Header_Malformed
	   with "Cache-control or Expires missing (at least one is required)";
      end if;

      if USN_Requester = "" then
	 Pl_Debug("Optional header «S» (Universal Service Type of " &
		    "the requester) is missing");
	 USN_Variable_Part := To_US("");
      else
	 USN_Variable_Part := To_US("S: " & USN_Requester & EOL);
      end if;

      Required_Part := To_US(Status_Line & EOL) & USN_Variable_Part &
	To_US("USN: ") & Device.Universal_Serial_Number &
	To_US(EOL & "ST: ") & Device.Service_Type & To_US(EOL);

      if Device.Cache_Control /= "" then
	 Append(Required_Part, To_US("Cache-Control: ") &
		  Device.Cache_Control & To_US(EOL));
      end if;

      if Device.Expires /= "" then
	 Append(Required_Part, To_US("Expires: ") &
		  Device.Expires & To_US(EOL));
      end if;

      Send_Message(Create_Message(To_String(Required_Part), Other_Headers));
   end M_Search_Response;

   procedure Notify_Alive(Device: in SSDP_Service;
			  Other_Headers: in Message_Header_Array) is
      Required_Part: Unbounded_String;
   begin
      if Device.Service_Type = "" then raise Header_Malformed
	with "Header «NT» (Service Type) is missing";
      end if;

      if Device.Universal_Serial_Number = "" then raise Header_Malformed
	with "Header «USN» (Universal Service Type) is missing";
      end if;

      if Device.Location = "" and Device.AL = "" then raise Header_Malformed
	with "Both headers «Location» and «AL» are missing," &
	" at least one is required";
      end if;

      if Device.Cache_Control = "" and Device.Expires = "" then
	 raise Header_Malformed
	   with "Both headers «Cache-Control» and «Expires» are missing," &
	   " at least one is required";
      end if;

      Required_Part := To_US(Notify_Line & EOL & "NT: ") & Device.Service_Type &
	To_US(EOL & "USN: ") & Device.Universal_Serial_Number &
	To_US(EOL & "NTS: ssdp:alive" & EOL);

      if Device.Location /= "" then
	 Append(Required_Part, To_US("Location: ") &
		  Device.Location & To_US(EOL));
      end if;

      if Device.AL /= "" then
	 Append(Required_Part, To_US("AL: ") & Device.AL & To_US(EOL));
      end if;

      if Device.Cache_Control /= "" then
	 Append(Required_Part, To_US("Cache-Control: ") &
		  Device.Cache_Control & To_US(EOL));
      end if;

      if Device.Expires /= "" then
	 Required_Part := Required_Part & To_US("Expires: ") &
	   Device.Expires & To_US(EOL);
      end if;

      Send_Message(Create_Message(To_String(Required_Part), Other_Headers));
   end Notify_Alive;

   procedure Notify_Bye_Bye(Device: in SSDP_Service) is
      Start_Line: constant String := Notify_Line & EOL;
   begin
      if Device.Service_Type = "" then raise Header_Malformed
	with "Header «NT» (Service Type) is missing";
      end if;

      if Device.Universal_Serial_Number = "" then
	 raise Header_Malformed
	   with "Header «USN» (Universal Service Type) is missing";
      end if;

      Remove_Devices(Device.Universal_Serial_Number);

      Send_Message(Start_Line & "NT: " & To_String(Device.Service_Type) & EOL &
		     "USN: " & To_String(Device.Universal_Serial_Number) & EOL &
		     "NTS: ssdp:byebye" & EOL & EOL);
   end Notify_Bye_Bye;

   procedure Service_Provider_Job is
      use Ada.Streams, Ada.Exceptions, Gnat.Sockets;

      Msg: Stream_Element_Array(1..500);
      Last: Stream_Element_Offset;
      Addr: Sock_Addr_Type;

      procedure Parse_Message(Message: in String) is
	 use Ada.Characters.Handling, Ada.Strings.Fixed, Ada.Strings;

	 procedure Get_M_Search_Info(Lines: in Line_Array) is
	    Posn: Natural;
	    First, Last: Natural;
	    Man_Line, ST_Line, S_Line: Natural := 0;
	 begin
	    for I in Lines'Range loop
	       Posn := Index(To_Upper(Lines(I).all), "MAN:");
	       if Posn = 1 then
		  Man_Line := I;
		  goto Continue;
	       end if;

	       Posn := Index(To_Upper(Lines(I).all), "ST:");
	       if Posn = 1 then
		  ST_Line := I;
		  goto Continue;
	       end if;

	       Posn := Index(To_Upper(Lines(I).all), "S:");
	       if Posn = 1 then
		  S_Line := I;
		  goto Continue;
	       end if;

	       Pl_Debug("Extra info: [" & Lines(I).all & "]");
	   <<Continue>>
	    end loop;

	    if Man_Line = 0 then raise SSDP_Message_Malformed
	      with "No Man field found. " &
	      "Should be present with the value ssdp:discover";
	    elsif ST_Line = 0 then raise SSDP_Message_Malformed
	      with "No ST (Service Type) field found.";
	    elsif S_Line = 0 then
	       Pl_Debug("No optional S field (Universal Service Name) found.");
	    end if;

	    First := Lines(Man_Line)'First + 4;
	    Last := Lines(Man_Line)'Last;
	    if Trim(Lines(Man_Line)(First..Last),
		    Both) /= """ssdp:discover""" then
	       raise SSDP_Message_Malformed
		 with "unmanaged M-Search message with Man field ≠ " &
		 "ssdp:discover. Here:" &
		 Trim(Lines(Man_Line)(First..Last), Both);
	    end if;

	    First := Lines(ST_Line)'First + 3;
	    Last := Lines(ST_Line)'Last;

	    declare
	       Devices: Device_Type_Array :=
		 Matching_Devices(Trim(Lines(ST_Line)(First..Last), Both));
	       S_Line_First: Natural;
	       USN_M_Search: Unbounded_String;
	    begin
	       if S_Line /= 0 then
		  S_Line_First := Lines(S_Line)'First + 2;
		  USN_M_Search := To_US
		    (Trim(Lines(S_Line)(S_Line_First..Lines(S_Line)'Last),
			  Both));
	       else
		  USN_M_Search := To_US("");
	       end if;

	       for I in Devices'Range loop
		  Pl_Debug("Envoie du device:" &
			     To_String(Devices(I).Universal_Serial_Number) &
			     " avec: " & To_String(Devices(I).Cache_Control));
		  M_Search_Response(Devices(I), To_String(USN_M_Search),
				    (1 => To_US("Que-dale:rien")));
	       end loop;
	    exception
	       when E: Header_Malformed => Pl_Debug(Exception_Message(E));
	    end;
	 end Get_M_Search_Info;

	 Lines: Line_Array := Parse_Lines(Message);
	 Posn_M_SEARCH, Posn_Reply_M_SEARCH: Natural;
      begin
	 Posn_M_SEARCH := Index(To_Upper(Lines(1).all),
				M_Search_Star_Line);
	 if Posn_M_SEARCH > 1 then raise SSDP_Message_Malformed
	   with "M-SEARCH line doesn't begin at character 0";
	 elsif Posn_M_SEARCH = 1 then
	    Pl_Debug("M-search received");
	    Get_M_Search_Info(Lines(2..Lines'Last));
	    return;
	 end if;

	 Posn_Reply_M_SEARCH := Index(To_Upper(Lines(1).all),
				      Status_Line);
	 if Posn_Reply_M_SEARCH > 1 then raise SSDP_Message_Malformed
	   with "Reply to M-SEARCH line doesn't begin at character 0";
	 elsif Posn_Reply_M_SEARCH = 1 then
	    Pl_Debug("Reply to M-SEARCH received");
	    return;
	 end if;

	 -- Uninteresting message --
      exception
	 when Ex: SSDP_Message_Malformed =>
	    Pl_Debug(Exception_Name(Ex) & ": " & Exception_Message(Ex));
      end Parse_Message;
   begin
      Set_Socket_Option(Global_Multicast_Connection.Socket,
			Ip_Protocol_For_Ip_Level, (Multicast_Loop, True));

      loop
	 begin
	    Receive_Socket(Global_Multicast_Connection.Socket, Msg, Last, Addr);
	    Pl_Debug("____________________________________________________");
	    Pl_Debug("From " & Image(Addr));
	    Parse_Message(To_String(Msg(1..Last)));
	    Pl_Debug("¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯");
	 exception
	    when E: Not_An_SSDP_Message | SSDP_Message_Malformed =>
	       Pl_Debug(Exception_Message(E));
	 end;
      end loop;
   end Service_Provider_Job;

   procedure Start_Listening is
   begin
      SSDP.Utils.Start_Listening(Service_Provider_Job'access);
   end Start_Listening;

   procedure Stop_Listening is
   begin
      SSDP.Utils.Stop_Listening;
   end Stop_Listening;

end Ssdp.Service_Provider;
