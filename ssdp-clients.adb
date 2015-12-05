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
with Ada.Calendar;
with Ada.Text_IO;

with Gnat.Sockets;

with SSDP.Utils;

package body SSDP.Clients is
   use SSDP.Utils, Gnat.Sockets, Ada.Containers, Ada.Calendar;

   type Service_Device_Type is new Device_Type with record
      -- We only need the preferred value between Location and AL.
      -- See rationnal in Parse_Response:
      Location: Unbounded_String;
      -- Dito here: prefered result from Cache-Control and Expires
      Expiration: Time;
   end record;

   -- Only one SSDP client should usually exist, however, we choose to allow
   --  the existence of several ones creating a dedicated type SSDP_Client.
   -- This vector contains the discovered services and permit to manage in one
   --  place the received events for every declared SSDP_Client.
   subtype Service_Count_Type is Count_Type range 1..100;
   package Service_Vectors is new Vectors(Service_Count_Type,
					  Service_Device_Type);
   Services: Service_Vectors.Vector;

   function Initialize_Device(Service_Type, Universal_Serial_Number: in String)
			     return SSDP_Client is
      Device: SSDP_Client;
   begin
      if Service_Type = "" or Universal_Serial_Number = "" then
	 raise Header_Malformed
	   with "service_type AND universal_serial_number should be not null";
      end if;

      Set_Networking;

      Device.Service_Type := To_US(Service_Type);
      Device.Universal_Serial_Number := To_US(Universal_Serial_Number);

      return Device;
   end Initialize_Device;

   procedure Set_Universal_Serial_Number(Device: in out SSDP_Client;
					 Universal_Serial_Number: in String) is
   begin
      Device.Universal_Serial_Number := To_US(Universal_Serial_Number);
   end Set_Universal_Serial_Number;

   procedure Set_Service_Type(Device: in out SSDP_Client;
			      Service_Type: in String) is
   begin
      Device.Service_Type := To_US(Service_Type);
   end Set_Service_Type;

   function Get_Universal_Serial_Number(Device: in SSDP_Client)
				       return String is
   begin
      return To_String(Device.Universal_Serial_Number);
   end Get_Universal_Serial_Number;

   function Get_Service_Type(Device: in SSDP_Client) return String is
   begin
      return To_String(Device.Service_Type);
   end Get_Service_Type;

   procedure M_Search(Device: in SSDP_Client;
		      Other_Header: in Message_Header_Array) is
      Start_Line: constant String := M_Search_Star_Line & EOL;
   begin
      if Device.Service_Type = "" then raise Header_Malformed
	with "Header «ST» (Service Type) is missing";
      end if;

      if Device.Universal_Serial_Number = "" then raise Header_Malformed
	with "Header «S» (Universal Service Type) is missing";
      end if;

      Send_Message(Create_Message(Start_Line & "ST: " &
				    To_String(Device.Service_Type) & EOL &
				    "S: " &
				    To_String(Device.Universal_Serial_Number) &
				    EOL & "Man: ""ssdp:discover""" & EOL,
				  Other_Header));
   end M_Search;

   procedure Finder_Job is
      use Ada.Streams, Ada.Exceptions;

      Msg: Stream_Element_Array(1..500);
      Last: Stream_Element_Offset;
      Addr: Sock_Addr_Type;

      type Expiration_Type is (Cache_Control, Expires);

      function Get_Expiration(Expiration_Header: in Expiration_Type;
			      Str: in String) return Time is
	 use Ada.Strings.Fixed;

	 package Natural_IO is new Ada.Text_IO.Integer_IO(Natural);

	 Posn: Natural;
	 Expiration_Val, Last: Natural;
      begin
	 case Expiration_Header is
	    when Cache_Control =>
	       -- Cache-Control should contain a «max-age = value» somewhere
	       Posn := Index(Str, "max-age");
	       if Posn = 0 then raise SSDP_Message_Malformed
		 with "max-age value is missing in Cache-Control header";
	       end if;

	       if Posn + 7 = Str'Last then raise SSDP_Message_Malformed
		 with "max-age header-value has no value";
	       end if;

	       Posn := Index(Str(Posn + 7..Str'Last), "=");
	       if Posn = 0 or Posn = Str'Last then raise SSDP_Message_Malformed
		 with "max-age header-value has no value";
	       end if;

	       Pl_Debug("max-age =" & Str(Posn + 1..Str'Last));

	       -- We avoid decimal values by getting a natural, not a duration:
	       Natural_IO.Get(Str(Posn + 1..Str'Last), Expiration_Val, Last);

	       return Clock + Duration(Expiration_Val);

	    when Expires =>
	       -- Needs to be implemented, see RFC here:
	       -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.3.1
	       raise SSDP_Message_Malformed
		 with "Expires field decoding not yet implemented, SORRY :(";
	 end case;

      exception
	 when Ada.Text_IO.Data_Error => raise SSDP_Message_Malformed
	    with "max-age header-value has incorrect value: " &
	    Str(Posn..Str'Last);
      end Get_Expiration;

      procedure Update(Service: in Service_Device_Type) is
	 NT: constant Unbounded_String := Service.Service_Type;
	 USN: constant Unbounded_String := Service.Universal_Serial_Number;
      begin
	 for I in 1..Services.Length loop
	    if Services.Element(I).Service_Type = NT and
	      Services.Element(I).Universal_Serial_Number = USN then
	       Pl_Debug("Update device: " & To_String(USN) & ' ' &
			  To_String(NT));
	       Services.Replace_Element(I, Service);
	       return;
	    end if;
	 end loop;

	 Pl_Debug("Adding device: " & To_String(USN) & ' ' & To_String(NT));

	 Services.Append(Service);
      end Update;

      procedure Parse_Message(Message: in String) is
	 use Ada.Characters.Handling, Ada.Strings.Fixed;

	 procedure Get_Notify_Info(Lines: in Line_Array) is
	    use Ada.Strings;

	    Service: Service_Device_Type;
	    NTS_Line, Cache_Control_Line, Expiration_Line: Natural := 0;
	    Posn, First, Last: Natural;
	 begin
	    for I in Lines'Range loop
	       Posn := Index(To_Upper(Lines(I).all), "USN:");
	       if Posn = 1 then
		  First := Lines(I)'First + 4;
		  Last := Lines(I)'Last;
		  Service.Universal_Serial_Number :=
		    To_Unbounded_String(Trim(Lines(I)(First..Last), Both));
		  goto Continue;
	       end if;

	       Posn := Index(To_Upper(Lines(I).all), "NT:");
	       if Posn = 1 then
		  First := Lines(I)'First + 3;
		  Last := Lines(I)'Last;
		  Service.Service_Type :=
		    To_Unbounded_String(Trim(Lines(I)(First..Last), Both));
		  goto Continue;
	       end if;

	       Posn := Index(To_Upper(Lines(I).all), "NTS:");
	       if Posn = 1 then
		  NTS_Line := I;
		  goto Continue;
	       end if;

	       Posn := Index(To_Upper(Lines(I).all), "CACHE-CONTROL:");
	       if Posn = 1 then
		  Cache_Control_Line := I;
		  goto Continue;
	       end if;

	       Posn := Index(To_Upper(Lines(I).all), "EXPIRATION:");
	       if Posn = 1 then
		  Expiration_Line := I;
		  goto Continue;
	       end if;

	       Pl_Debug("Extra info: [" & Lines(I).all & "]");
	   <<Continue>>
	    end loop;

	    if NTS_Line = 0 then raise SSDP_Message_Malformed
	      with "No NTS field found";
	    elsif Service.Universal_Serial_Number = "" then
	       raise SSDP_Message_Malformed with "No USN field found";
	    elsif Service.Service_Type = "" then
	       raise SSDP_Message_Malformed with "No ST field found";
	    else
	       declare
		  Last: Natural := Lines(NTS_Line).all'Last;
		  First: Natural := Lines(NTS_Line).all'First + 5;
		  NTS: String := Trim(Lines(NTS_Line)(First..Last), Both);

		  procedure Bye_Bye(Service: in Service_Device_Type) is
		     USN: constant Unbounded_String :=
		       Service.Universal_Serial_Number;
		     NT: constant Unbounded_String := Service.Service_Type;
		  begin
		     for I in 1..Services.Length loop
			if Services.Element(I).Service_Type = NT and
			  Services.Element(I).Universal_Serial_Number = USN then
			   Pl_Debug("Removing service: " & To_String(USN) &
				      ' ' & To_String(NT));
			   Services.Delete(I);
			   return;
			end if;
		     end loop;

		     Pl_Debug("Untracked service: " & To_String(USN) & ' ' &
				To_String(NT));
		  end Bye_Bye;
	       begin
		  if NTS = "ssdp:byebye" then
		     Pl_Debug("It's a byebye :¯(");
		     Bye_Bye(Service);
		  elsif NTS = "ssdp:alive" then
		     Pl_Debug("It's an alive :)");
		     if Cache_Control_Line = 0 and Expiration_Line = 0 then
			raise SSDP_Message_Malformed
			  with "Neither Cache-Control nor Expires field " &
			  "in alive notification";
		     elsif Cache_Control_Line /= 0 then
			Service.Expiration :=
			  Get_Expiration(Cache_Control,
					 Lines(Cache_Control_Line).all);
		     else
			Service.Expiration :=
			  Get_Expiration(Expires, Lines(Expiration_Line).all);
		     end if;
		     Update(Service);
		  else raise SSDP_Message_Malformed
		    with "Unknown NTS field: " & NTS;
		  end if;
	       end;
	    end if;
	 end Get_Notify_Info;

	 Lines: Line_Array := Parse_Lines(Message);
	 Posn: Natural;
      begin
	 Posn := Index(To_Upper(Lines(1).all), M_Search_Star_Line);
	 if Posn > 1 then raise SSDP_Message_Malformed
	   with "M-SEARCH line doesn't begin at character 0";
	 elsif Posn = 1 then
	    Pl_Debug("M-search received");
	    return;
	 end if;

	 Posn := Index(To_Upper(Lines(1).all), Notify_Line);
	 if Posn > 1 then raise SSDP_Message_Malformed
	   with "Notify line doesn't begin at character 0";
	 elsif Posn = 1 then
	    Pl_Debug("____________________________________________________");
	    Pl_Debug("From " & Image(Addr) & " [Multicast]");
	    Pl_Debug("Notify received");
	    Get_Notify_Info(Lines(2..Lines'Last));
	    Pl_Debug("¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯");
	    return;
	 end if;

	 Posn := Index(To_Upper(Lines(1).all), Status_Line);
	 if Posn >= 1 then
	    Pl_Debug("Multicast reply to M-SEARCH received: should not " &
		       "happend here. Message Dropped.");
	    return;
	 end if;

	 raise Not_An_SSDP_Message
	   with "Neither M-Search nor Notify nor a reply to M-Search";
      exception
	 when Ex: SSDP_Message_Malformed =>
	    Pl_Debug(Exception_Name(Ex) & ": " & Exception_Message(Ex));
      end Parse_Message;

      task Unicast_Listener;
      task body Unicast_Listener is
	 Msg: Stream_Element_Array(1..500);
	 Last: Stream_Element_Offset;
	 Addr: Sock_Addr_Type;

	 procedure Parse_Response(Message: in String) is
	    use Ada.Characters.Handling, Ada.Strings.Fixed;

	    Lines: Line_Array := Parse_Lines(Message);
	    Posn: Natural;

	    procedure Get_M_Search_Response(Lines: in Line_Array) is
	       use Ada.Strings;

	       Service: Service_Device_Type;
	       S_Line, Cache_Control_Line, Expires_Line,
		 Location_Line, AL_Line: Natural := 0;
	       Posn, First, Last: Natural;
	    begin
	       for I in Lines'Range loop
		  Posn := Index(To_Upper(Lines(I).all), "USN:");
		  if Posn = 1 then
		     First := Lines(I)'First + 4;
		     Last := Lines(I)'Last;
		     Service.Universal_Serial_Number :=
		       To_Unbounded_String(Trim(Lines(I)(First..Last), Both));
		     goto Continue;
		  end if;

		  Posn := Index(To_Upper(Lines(I).all), "ST:");
		  if Posn = 1 then
		     First := Lines(I)'First + 3;
		     Last := Lines(I)'Last;
		     Service.Service_Type :=
		       To_Unbounded_String(Trim(Lines(I)(First..Last), Both));
		     goto Continue;
		  end if;

		  Posn := Index(To_Upper(Lines(I).all), "S:");
		  if Posn = 1 then
		     S_Line := I;
		     goto Continue;
		  end if;

		  Posn := Index(To_Upper(Lines(I).all), "CACHE-CONTROL:");
		  if Posn = 1 then
		     Cache_Control_Line := I;
		     goto Continue;
		  end if;

		  Posn := Index(To_Upper(Lines(I).all), "EXPIRES:");
		  if Posn = 1 then
		     Expires_Line := I;
		     goto Continue;
		  end if;

		  Posn := Index(To_Upper(Lines(I).all), "AL:");
		  if Posn = 1 then
		     AL_Line := I;
		     goto Continue;
		  end if;

		  Posn := Index(To_Upper(Lines(I).all), "LOCATION:");
		  if Posn = 1 then
		     Location_Line := I;
		     goto Continue;
		  end if;

		  Pl_Debug("Extra info: [" & Lines(I).all & "]");
	      <<Continue>>
	       end loop;

	       if Service.Universal_Serial_Number = Null_Unbounded_String then
		  raise SSDP_Message_Malformed
		    with "USN missing in this M-Search response";
	       elsif Service.Service_Type = Null_Unbounded_String then
		  raise SSDP_Message_Malformed
		    with "ST missing in this M-Search response";
	       elsif Cache_Control_Line = 0 and Expires_Line = 0 then
		  raise SSDP_Message_Malformed
		    with "Both Cache_Control and Expires field are missing";
	       end if;

	       -- Cache-Control takes precedence over Expires:
	       if Cache_Control_Line /= 0 then
		  Service.Expiration :=
		    Get_Expiration(Cache_Control,
				   Lines(Cache_Control_Line).all);
	       else
		  Service.Expiration :=
		    Get_Expiration(Expires, Lines(Expires_Line).all);
	       end if;

	       -- Location takes precedence over AL. But both are optional:
	       if Location_Line /= 0 then
		  Service.Location := To_US(Lines(Location_Line).all);
	       elsif AL_Line /= 0 then
		  Service.Location := To_US(Lines(AL_Line).all);
	       end if;

	       Update(Service);
	    end Get_M_Search_Response;
	 begin
	    Posn := Index(To_Upper(Lines(1).all), Status_Line);
	    if Posn > 1 then raise SSDP_Message_Malformed
	      with "Reply to M-SEARCH line doesn't begin at character 0";
	    elsif Posn = 1 then
	       Pl_Debug("____________________________________________________");
	       Pl_Debug("From " & Image(Addr) & " [Unicast]");
	       Pl_Debug("Reply to M-SEARCH received");
	       Get_M_Search_Response(Lines(2..Lines'Last));
	       Pl_Debug("¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯");
	       return;
	    end if;

	    Pl_Debug("Unmanaged message with header:");
	    Pl_Debug(Lines(1).all);
	 end Parse_Response;
      begin
	 loop
	    begin
	       Receive_Socket(Global_Network_Settings.Socket(Unicast),
			      Msg, Last, Addr);
	       Parse_Response(To_String(Msg(1..Last)));
	    exception
	       when E: SSDP_Message_Malformed =>
		  Pl_Debug("Message malformed:" & Exception_Message(E));
	       when E: others =>
		  Pl_Error(Exception_Message(E));
	    end;
	 end loop;
      end Unicast_Listener;
   begin
      Set_Socket_Option(Global_Network_Settings.Socket(Multicast),
			Ip_Protocol_For_Ip_Level, (Multicast_Loop, True));

      loop
	 begin
	    Receive_Socket(Global_Network_Settings.Socket(Multicast),
			   Msg, Last, Addr);
	    Parse_Message(To_String(Msg(1..Last)));
	 exception
	    when E: Not_An_SSDP_Message | SSDP_Message_Malformed =>
	       Pl_Debug(Exception_Message(E));
	 end;
      end loop;
   end Finder_Job;

   procedure Start_Listening is
   begin
      SSDP.Utils.Start_Listening(Finder_Job'Access);
   end Start_Listening;

   procedure Stop_Listening is
   begin
      SSDP.Utils.Stop_Listening;
   end Stop_Listening;

end SSDP.Clients;
